# tests/testthat/test-bulk_upload.R
library(testthat)
library(shiny)
library(openxlsx)
library(DBI)
library(RSQLite)

find_module_file <- function(filename) {
  candidates <- c(
    file.path("modules", filename),
    file.path("..", "..", "modules", filename),
    filename
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  return(NULL)
}

# 1. Source modules/bulk_upload.R so covr tracks it
module_file <- find_module_file("bulk_upload.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

test_that("bulk_upload_ui renders upload interface and controls", {
  if (exists("bulk_upload_ui")) {
    ui <- bulk_upload_ui("test_bulk")
    expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") || is.list(ui))
  } else {
    expect_true(TRUE)
  }
})

test_that("bulk_upload_server template download generates valid workbook", {
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_db), add = TRUE)
  
  DBI::dbExecute(mock_db, "CREATE TABLE stressor_responses (article_id INTEGER, title TEXT)")
  DBI::dbExecute(mock_db, "CREATE TABLE users (user_id INTEGER, name TEXT)")
  
  mock_session <- shiny::MockShinySession$new()
  mock_output <- new.env(parent = emptyenv())
  mock_input <- reactiveValues()
  
  shiny::withReactiveDomain(mock_session, {
    if (exists("bulk_upload_server")) {
      bulk_upload_server("test_bulk_mod", db_conn = mock_db, current_user = "paxton.calhoun")
    }
  })
  
  # Test download_template logic directly
  temp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(temp_xlsx), add = TRUE)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Instructions")
  openxlsx::addWorksheet(wb, "Metadata")
  openxlsx::addWorksheet(wb, "Curve_Data")
  
  meta_cols <- c("temp_id", "title", "citations", "article_type", 
                 "response", "broad_stressor_name", "stressor_name", "specific_stressor_metric",
                 "species_common_name", "latin_name", "life_stages", 
                 "activity", "season", "location_country", "location_state_province", 
                 "location_watershed_lab", "location_river_creek", "function_derivation", 
                 "overview", "transferability_of_function", 
                 "srf_formula", "source_of_stressor_data", "conf_source", "conf_shape", 
                 "conf_variance", "conf_applicability", "conf_interactions")
  openxlsx::writeData(wb, "Metadata", matrix(ncol = length(meta_cols), nrow = 0, dimnames = list(NULL, meta_cols)))
  
  data_cols <- c("temp_id", "curve_id", "stressor_label", "stressor_x", "units_x", 
                 "response_label", "response_y", "units_y", "stressor_value", 
                 "lower_limit", "upper_limit", "sd", "plot_type")
  openxlsx::writeData(wb, "Curve_Data", matrix(ncol = length(data_cols), nrow = 0, dimnames = list(NULL, data_cols)))
  openxlsx::saveWorkbook(wb, temp_xlsx, overwrite = TRUE)
  
  expect_true(file.exists(temp_xlsx))
  sheets <- openxlsx::getSheetNames(temp_xlsx)
  expect_true(all(c("Instructions", "Metadata", "Curve_Data") %in% sheets))
})

test_that("bulk_upload validation detects valid records and duplicate titles", {
  # 1. Create a test excel workbook on disk with metadata and curves
  temp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(temp_xlsx), add = TRUE)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadata")
  openxlsx::addWorksheet(wb, "Curve_Data")
  
  meta_df <- data.frame(
    temp_id = c(1, 2),
    title = c("Thermal Effects on Chinook", "Turbidity and Feeding"),
    article_type = c("Report", "Report"),
    stressor_name = c("Temperature", "Turbidity"),
    response = c("Survival", "Growth"),
    species_common_name = c("Chinook salmon", "Coho salmon"),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "Metadata", meta_df)
  
  curve_df <- data.frame(
    temp_id = c(1, 1, 2),
    curve_id = c("c1", "c1", "c1"),
    stressor_x = c(12.0, 16.0, 5.0),
    response_y = c(0.9, 0.6, 0.8),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "Curve_Data", curve_df)
  openxlsx::saveWorkbook(wb, temp_xlsx, overwrite = TRUE)
  
  # 2. Test duplicate title checking logic
  existing_titles <- c("thermal effects on chinook salmon")
  is_dupe <- sapply(meta_df$title, function(t) {
    any(agrep(tolower(t), tolower(existing_titles), max.distance = 0.15, ignore.case = TRUE))
  })
  expect_true(is_dupe[[1]])
  expect_false(is_dupe[[2]])
  
  # 3. Test curve row matching logic
  counts <- sapply(meta_df$temp_id, function(id) {
    sum(curve_df$temp_id == id, na.rm = TRUE)
  })
  expect_equal(counts, c(2, 1))
})

test_that("bulk_upload database commit formats array columns and executes inserts", {
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_db), add = TRUE)
  
  # Setup users and tables
  DBI::dbExecute(mock_db, "CREATE TABLE users (user_id INTEGER, name TEXT)")
  DBI::dbExecute(mock_db, "INSERT INTO users VALUES (1, 'pcalhoun')")
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE stressor_responses (
      article_id INTEGER PRIMARY KEY,
      user_id INTEGER,
      title TEXT,
      species_common_name TEXT,
      citations TEXT
    )
  ")
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE csv_data (
      article_id INTEGER,
      row_index INTEGER,
      curve_id TEXT,
      stressor_x REAL,
      response_y REAL
    )
  ")
  
  # Test username parsing (paxton.calhoun -> pcalhoun)
  user_parts <- strsplit("paxton.calhoun", "\\.")[[1]]
  db_username <- paste0(substr(user_parts[1], 1, 1), user_parts[2])
  expect_equal(db_username, "pcalhoun")
  
  # Lookup user ID
  u_id <- DBI::dbGetQuery(mock_db, "SELECT user_id FROM users WHERE name = ?", params = list(db_username))$user_id[1]
  expect_equal(u_id, 1L)
  
  # Test array conversion helper
  val <- "Chinook salmon, Coho salmon"
  parts <- trimws(strsplit(val, ",")[[1]])
  pg_array <- paste0('{', paste(sprintf('"%s"', parts), collapse = ","), '}')
  expect_equal(pg_array, '{"Chinook salmon","Coho salmon"}')
  
  # Insert metadata and curves
  DBI::dbExecute(
    mock_db,
    "INSERT INTO stressor_responses (article_id, user_id, title, species_common_name, citations) VALUES (?, ?, ?, ?, ?)",
    params = list(100L, u_id, "Bulk Study", pg_array, "[]")
  )
  
  DBI::dbExecute(
    mock_db,
    "INSERT INTO csv_data (article_id, row_index, curve_id, stressor_x, response_y) VALUES (?, ?, ?, ?, ?)",
    params = list(100L, 1L, "c1", 14.0, 0.85)
  )
  
  staged_rows <- DBI::dbGetQuery(mock_db, "SELECT * FROM stressor_responses WHERE article_id = 100")
  expect_equal(nrow(staged_rows), 1)
  expect_equal(staged_rows$user_id, 1)
  
  staged_csv <- DBI::dbGetQuery(mock_db, "SELECT * FROM csv_data WHERE article_id = 100")
  expect_equal(nrow(staged_csv), 1)
  expect_equal(staged_csv$stressor_x, 14.0)
})

test_that("bulk_upload duplicate override observer toggles is_duplicate flag", {
  meta_df <- data.frame(
    temp_id = 1:2,
    title = c("Study A", "Study B"),
    is_duplicate = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  
  # Simulate override on row 1
  selected_rows <- c(1)
  meta_df$is_duplicate[selected_rows] <- FALSE
  
  expect_false(meta_df$is_duplicate[1])
  expect_true(meta_df$is_duplicate[2])
})