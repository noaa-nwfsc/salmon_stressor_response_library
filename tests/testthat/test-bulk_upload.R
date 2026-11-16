# tests/testthat/test-bulk_upload.R
library(testthat)
library(DBI)
library(openxlsx)

# Source bulk_upload module with fallback paths
if (file.exists("modules/bulk_upload.R")) {
  source("modules/bulk_upload.R")
} else {
  source("../../modules/bulk_upload.R")
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. EXCEL TEMPLATE STRUCTURE TESTS
# ─────────────────────────────────────────────────────────────────────────────

test_that("validate_excel_structure checks sheet names and linking temp_id", {
  tmp_dir <- tempdir()
  
  # Helper to validate workbook structure
  check_bulk_excel <- function(file_path) {
    sheets <- openxlsx::getSheetNames(file_path)
    if (!all(c("Metadata", "Curve_Data") %in% sheets)) {
      return(list(valid = FALSE, message = "Missing required sheets"))
    }
    meta_df <- openxlsx::read.xlsx(file_path, sheet = "Metadata")
    curve_df <- openxlsx::read.xlsx(file_path, sheet = "Curve_Data")
    if (!"temp_id" %in% names(meta_df) || !"temp_id" %in% names(curve_df)) {
      return(list(valid = FALSE, message = "Missing temp_id column"))
    }
    list(valid = TRUE, meta = meta_df, curve = curve_df)
  }
  
  # 1. Missing Curve_Data sheet
  wb_bad_sheets <- createWorkbook()
  addWorksheet(wb_bad_sheets, "Metadata")
  writeData(wb_bad_sheets, "Metadata", data.frame(temp_id = 1, title = "Test"))
  path_bad1 <- tempfile(tmpdir = tmp_dir, fileext = ".xlsx")
  saveWorkbook(wb_bad_sheets, path_bad1, overwrite = TRUE)
  
  res_bad1 <- check_bulk_excel(path_bad1)
  expect_false(res_bad1$valid)
  expect_equal(res_bad1$message, "Missing required sheets")
  
  # 2. Missing temp_id column in Curve_Data
  wb_no_id <- createWorkbook()
  addWorksheet(wb_no_id, "Metadata")
  writeData(wb_no_id, "Metadata", data.frame(temp_id = 1, title = "Test"))
  addWorksheet(wb_no_id, "Curve_Data")
  writeData(wb_no_id, "Curve_Data", data.frame(stressor_x = 10, response_y = 0.5))
  path_bad2 <- tempfile(tmpdir = tmp_dir, fileext = ".xlsx")
  saveWorkbook(wb_no_id, path_bad2, overwrite = TRUE)
  
  res_bad2 <- check_bulk_excel(path_bad2)
  expect_false(res_bad2$valid)
  expect_equal(res_bad2$message, "Missing temp_id column")
  
  # 3. Valid Workbook
  wb_valid <- createWorkbook()
  addWorksheet(wb_valid, "Metadata")
  writeData(wb_valid, "Metadata", data.frame(temp_id = 1, title = "Valid Title"))
  addWorksheet(wb_valid, "Curve_Data")
  writeData(wb_valid, "Curve_Data", data.frame(temp_id = 1, stressor_x = 10, response_y = 0.5))
  path_good <- tempfile(tmpdir = tmp_dir, fileext = ".xlsx")
  saveWorkbook(wb_valid, path_good, overwrite = TRUE)
  
  res_good <- check_bulk_excel(path_good)
  expect_true(res_good$valid)
  expect_equal(nrow(res_good$meta), 1)
  expect_equal(nrow(res_good$curve), 1)
  
  # Cleanup temp files
  unlink(c(path_bad1, path_bad2, path_good))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. FUZZY DUPLICATE TITLE DETECTION (agrep)
# ─────────────────────────────────────────────────────────────────────────────

test_that("fuzzy duplicate title matching flags duplicates accurately", {
  existing_titles <- c(
    "Thermal tolerance of juvenile Chinook salmon in the Klamath River",
    "Streamflow effects on Coho salmon spawning"
  )
  
  check_duplicate <- function(title, existing, max_dist = 0.1) {
    if (is.na(title) || trimws(title) == "") return(FALSE)
    any(agrep(tolower(title), tolower(existing), max.distance = max_dist, ignore.case = TRUE))
  }
  
  # Exact match
  expect_true(check_duplicate(
    "Thermal tolerance of juvenile Chinook salmon in the Klamath River",
    existing_titles
  ))
  
  # Slight typo/variation (fuzzy duplicate)
  expect_true(check_duplicate(
    "Thermal tolerance of juvenile Chinook salmon in the Klamath River.",
    existing_titles
  ))
  
  # Completely different title
  expect_false(check_duplicate(
    "Turbidity impacts on Steelhead feeding in Puget Sound",
    existing_titles
  ))
  
  # NA or empty title
  expect_false(check_duplicate(NA_character_, existing_titles))
  expect_false(check_duplicate("", existing_titles))
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. METADATA AND CURVE DATA TEMP_ID MATCHING
# ─────────────────────────────────────────────────────────────────────────────

test_that("data_rows_found accurately links metadata and curve_data", {
  meta_df <- data.frame(
    temp_id = c(1, 2, 3),
    title = c("Study 1", "Study 2", "Study 3 (No Data)"),
    stringsAsFactors = FALSE
  )
  
  data_df <- data.frame(
    temp_id = c(1, 1, 1, 2), # 3 rows for id 1, 1 row for id 2, 0 rows for id 3
    stressor_x = c(10, 15, 20, 5),
    response_y = c(0.9, 0.8, 0.7, 0.95),
    stringsAsFactors = FALSE
  )
  
  # Count data rows matching each metadata temp_id
  meta_df$data_rows_found <- sapply(meta_df$temp_id, function(id) {
    sum(data_df$temp_id == id, na.rm = TRUE)
  })
  
  expect_equal(meta_df$data_rows_found, c(3, 1, 0))
  
  # Determine which are ready to upload (min 1 curve point)
  ready_to_upload <- meta_df$data_rows_found >= 1
  expect_equal(ready_to_upload, c(TRUE, TRUE, FALSE))
})

# ─────────────────────────────────────────────────────────────────────────────
# 4. DATABASE BATCH COMMIT WITH TRANSACTION ROLLBACK
# ─────────────────────────────────────────────────────────────────────────────

test_that("bulk_upload commits multiple records and curve rows with rollback", {
  with_test_transaction(function(con) {
    # 1. Prepare mock valid metadata (2 relationships)
    mock_tag <- as.numeric(Sys.time())
    title_1 <- paste0("BULK_TEST_ARTICLE_A_", mock_tag)
    title_2 <- paste0("BULK_TEST_ARTICLE_B_", mock_tag)
    
    meta_df <- data.frame(
      temp_id = c(101, 102),
      title = c(title_1, title_2),
      article_type = c("Peer-reviewed Journal Article", "Report"),
      broad_stressor_name = c("Water Quality", "Hydrology"),
      stressor_name = c("Water Temperature", "Streamflow"),
      specific_stressor_metric = c("Celsius", "cfs"),
      response = c("Survival", "Rearing Capacity"),
      species_common_name = c("Chinook salmon, Coho salmon", "Steelhead"),
      latin_name = c("Oncorhynchus tshawytscha, Oncorhynchus kisutch", "Oncorhynchus mykiss"),
      location_country = c("United States", "United States"),
      overview = c("Thermal bulk study overview.", "Flow bulk study overview."),
      citations = c("[]", "[]"),
      stringsAsFactors = FALSE
    )
    
    # 2. Prepare mock curve data
    curve_df <- data.frame(
      temp_id = c(101, 101, 102),
      curve_id = c("c1", "c1", "c1"),
      stressor_label = c("Temp", "Temp", "Flow"),
      stressor_x = c(12, 16, 50),
      units_x = c("C", "C", "cfs"),
      response_label = c("Surv", "Surv", "Cap"),
      response_y = c(0.9, 0.7, 0.85),
      units_y = c("%", "%", "%"),
      plot_type = c("curve", "curve", "scatter"),
      stringsAsFactors = FALSE
    )
    
    # 3. Simulate bulk commit logic from bulk_upload.R
    max_id_query <- "SELECT COALESCE(MAX(article_id), 0) AS max_id FROM stressor_responses;"
    current_max_id <- as.integer(DBI::dbGetQuery(con, max_id_query)$max_id[1])
    
    array_cols <- c("species_common_name", "latin_name", "location_country")
    
    inserted_article_ids <- integer(0)
    
    for (i in seq_len(nrow(meta_df))) {
      current_max_id <- current_max_id + 1L
      new_art_id <- current_max_id
      inserted_article_ids <- c(inserted_article_ids, new_art_id)
      
      row_meta <- meta_df[i, ]
      t_id <- row_meta$temp_id
      
      # Convert array columns
      for (col in array_cols) {
        val <- row_meta[[col]]
        parts <- trimws(strsplit(as.character(val), ",")[[1]])
        parts <- gsub('"', '\\"', parts, fixed = TRUE)
        row_meta[[col]] <- paste0('{', paste(sprintf('"%s"', parts), collapse = ","), '}')
      }
      
      # Insert stressor_responses record
      sql_meta <- "
        INSERT INTO stressor_responses (
          article_id, user_id, title, article_type, broad_stressor_name,
          stressor_name, specific_stressor_metric, response,
          species_common_name, latin_name, location_country,
          overview, citations
        ) VALUES (
          $1, 1, $2, $3, $4,
          $5, $6, $7,
          $8::text[], $9::text[], $10::text[],
          $11, $12::jsonb
        )
      "
      DBI::dbExecute(con, sql_meta, params = list(
        new_art_id, row_meta$title, row_meta$article_type, row_meta$broad_stressor_name,
        row_meta$stressor_name, row_meta$specific_stressor_metric, row_meta$response,
        row_meta$species_common_name, row_meta$latin_name, row_meta$location_country,
        row_meta$overview, row_meta$citations
      ))
      
      # Insert associated curve rows
      subset_curve <- curve_df[curve_df$temp_id == t_id, , drop = FALSE]
      for (r in seq_len(nrow(subset_curve))) {
        sql_curve <- "
          INSERT INTO csv_data (
            article_id, row_index, curve_id, stressor_label, stressor_x,
            units_x, response_label, response_y, units_y, plot_type
          ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
        "
        DBI::dbExecute(con, sql_curve, params = list(
          new_art_id,
          as.integer(r),
          subset_curve$curve_id[r],
          subset_curve$stressor_label[r],
          subset_curve$stressor_x[r],
          subset_curve$units_x[r],
          subset_curve$response_label[r],
          subset_curve$response_y[r],
          subset_curve$units_y[r],
          subset_curve$plot_type[r]
        ))
      }
    }
    
    # 4. Verify batch records and curve associations inside transaction
    res_meta <- DBI::dbGetQuery(
      con,
      "SELECT article_id, title FROM stressor_responses WHERE article_id IN ($1, $2) ORDER BY article_id",
      params = list(inserted_article_ids[1], inserted_article_ids[2])
    )
    expect_equal(nrow(res_meta), 2)
    expect_equal(res_meta$title, c(title_1, title_2))
    
    res_curves <- DBI::dbGetQuery(
      con,
      "SELECT article_id, count(*) as n_rows FROM csv_data WHERE article_id IN ($1, $2) GROUP BY article_id ORDER BY article_id",
      params = list(inserted_article_ids[1], inserted_article_ids[2])
    )
    expect_equal(res_curves$n_rows, c(2, 1)) # 2 points for study 101, 1 point for study 102
  })
})

test_that("verify bulk upload transactions leave zero residual records", {
  with_test_transaction(function(con) {
    leak_check <- DBI::dbGetQuery(
      con,
      "SELECT count(*) as count FROM stressor_responses WHERE title LIKE 'BULK_TEST_ARTICLE_%'"
    )
    expect_equal(leak_check$count, 0)
  })
})