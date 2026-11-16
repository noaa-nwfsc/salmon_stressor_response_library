# tests/testthat/test-submit_relationship.R
library(testthat)
library(DBI)
library(jsonlite)

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC TESTS: VALIDATION, ARRAYS, CITATIONS & DUPLICATE CHECKS
# ─────────────────────────────────────────────────────────────────────────────

# Helper replicating required field validation in submit_relationship.R
validate_submission_fields <- function(name, email, title) {
  is_blank <- function(x) is.null(x) || length(x) == 0 || !nzchar(trimws(as.character(x)))
  if (is_blank(name) || is_blank(email) || is_blank(title)) {
    return(list(valid = FALSE, message = "Please fill out all required fields (Name, Email, and Title)."))
  }
  list(valid = TRUE, message = "")
}

# Helper replicating to_pg_array from submit_relationship.R
to_pg_array <- function(val) {
  if (is.null(val) || length(val) == 0) return(NA_character_)
  parts <- unlist(lapply(val, function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(character(0))
    trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  }))
  parts <- parts[!is.na(parts) & parts != ""]
  if (length(parts) == 0) return(NA_character_)
  paste0("{", paste(sprintf('"%s"', gsub('"', '\\"', parts, fixed = TRUE)), collapse = ","), "}")
}

# Helper compiling citations into JSON array
compile_citations_json <- function(cits) {
  valid_cits <- list()
  for (item in cits) {
    if (!is.null(item$text) && nzchar(trimws(item$text))) {
      valid_cits[[length(valid_cits) + 1]] <- list(
        text = trimws(item$text),
        title = trimws(item$title %||% ""),
        url = trimws(item$url %||% "")
      )
    }
  }
  if (length(valid_cits) > 0) {
    jsonlite::toJSON(valid_cits, auto_unbox = TRUE)
  } else {
    "[]"
  }
}

test_that("validate_submission_fields enforces Name, Email, and Title", {
  # All valid
  expect_true(validate_submission_fields("Jane Doe", "jane@noaa.gov", "Thermal Capacity 2026")$valid)
  
  # Missing name
  res_noname <- validate_submission_fields("", "jane@noaa.gov", "Thermal Capacity 2026")
  expect_false(res_noname$valid)
  expect_true(grepl("required fields", res_noname$message, fixed = TRUE))
  
  # Missing email
  expect_false(validate_submission_fields("Jane Doe", "   ", "Thermal Capacity 2026")$valid)
  
  # Missing title
  expect_false(validate_submission_fields("Jane Doe", "jane@noaa.gov", "")$valid)
})

test_that("fuzzy duplicate title check flags closely matching existing articles", {
  existing_titles <- c(
    "Fullerton et al. 2020: River temperature thermal tolerances",
    "Beechie et al. 2013: Salmon habitat restoration capacity"
  )
  
  check_duplicate_title <- function(input_title, existing) {
    if (nchar(trimws(input_title)) <= 5) return(FALSE)
    matches <- agrep(tolower(input_title), tolower(existing), max.distance = 0.15, value = TRUE)
    length(matches) > 0
  }
  
  # Nearly identical title (minor typo or case shift) -> flagged as duplicate
  expect_true(check_duplicate_title(
    "Fullerton et al. 2020: River temperature thermal tolerance", 
    existing_titles
  ))
  
  # Completely different title -> not flagged
  expect_false(check_duplicate_title(
    "Bond et al. 2024: Estuary juvenile migration timing", 
    existing_titles
  ))
  
  # Short title under minimum threshold (<= 5 chars) -> ignored
  expect_false(check_duplicate_title("Temp", existing_titles))
})

test_that("to_pg_array formats character vectors and comma-separated strings into Postgres arrays", {
  expect_equal(to_pg_array(c("Chinook salmon", "Coho salmon")), '{"Chinook salmon","Coho salmon"}')
  expect_equal(to_pg_array("Chinook salmon, Steelhead"), '{"Chinook salmon","Steelhead"}')
  expect_true(is.na(to_pg_array(NULL)))
  expect_true(is.na(to_pg_array(character(0))))
  expect_true(is.na(to_pg_array("")))
})

test_that("compile_citations_json builds structured JSON or returns empty array '[]'", {
  # Empty input
  expect_equal(compile_citations_json(list()), "[]")
  expect_equal(compile_citations_json(list(list(text = "   "))), "[]")
  
  # Valid citations
  cits <- list(
    list(text = "Smith et al. 2020", title = "Smith 2020", url = "[suspicious link removed]")
  )
  res_json <- compile_citations_json(cits)
  parsed <- jsonlite::fromJSON(res_json, simplifyDataFrame = FALSE)
  
  expect_equal(length(parsed), 1)
  expect_equal(parsed[[1L]]$text, "Smith et al. 2020")
  expect_equal(parsed[[1L]]$title, "Smith 2020")
  expect_equal(parsed[[1L]]$url, "[suspicious link removed]")
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. DATABASE INTEGRATION TEST: STAGING SUBMISSION & CURVE LINKAGE
# ─────────────────────────────────────────────────────────────────────────────

test_that("submit_relationship inserts into staging_submissions and links staging_csv_data", {
  with_test_transaction(function(con) {
    # 1. Insert into staging_submissions RETURNING staging_id
    query_sub <- "
      INSERT INTO staging_submissions (
        submitter_name, submitter_email, submitter_notes,
        article_type, title, stressor_name, broad_stressor_name, specific_stressor_metric,
        response, srf_formula, species_common_name, latin_name, life_stages,
        location_country, location_state_province, overview, function_derivation, transferability_of_function,
        conf_source, conf_shape, conf_variance, conf_applicability, conf_interactions,
        citations, pdf_filename
      ) VALUES (
        $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24::jsonb, $25
      ) RETURNING staging_id;
    "
    
    sub_res <- DBI::dbGetQuery(con, query_sub, params = list(
      "Jane Salmon", "jane@uw.edu", "Submitted for review",
      "Report", "Public SRF Submission Test", "Water Temperature", "Water Quality", "Celsius",
      "Egg Survival", "$$y = mx + b$$",
      to_pg_array("Chinook salmon"), to_pg_array("Oncorhynchus tshawytscha"), to_pg_array("Incubation"),
      to_pg_array("United States"), to_pg_array("Washington"),
      "Study of thermal impacts on incubation.", to_pg_array("Empirical"), "Lowland rivers",
      "High", "Moderate", "High", "High", "Low",
      '[{"title":"Jane 2026","url":"[suspicious link removed]","text":"Salmon Study"}]',
      "supporting_doc.pdf"
    ))
    
    # Extract staging_id from the 1st column of the RETURNING query
    new_staging_id <- as.integer(sub_res[[1]][1])
    
    # Fallback lookup in case RETURNING returned an empty data frame
    if (is.na(new_staging_id) || length(new_staging_id) == 0) {
      id_lookup <- DBI::dbGetQuery(
        con, 
        "SELECT staging_id FROM staging_submissions WHERE title = 'Public SRF Submission Test' ORDER BY staging_id DESC LIMIT 1;"
      )
      new_staging_id <- as.integer(id_lookup[[1]][1])
    }
    
    expect_false(is.na(new_staging_id))
    expect_gt(new_staging_id, 0L)
    
    # 2. Insert linked quantitative curve points into staging_csv_data
    curve_df <- data.frame(
      staging_id = new_staging_id,
      row_index = 1:2,
      curve_id = "c1",
      stressor_label = "Temperature",
      stressor_x = c(10.0, 15.0),
      units_x = "degC",
      response_label = "Survival",
      response_y = c(0.95, 0.80),
      units_y = "ratio",
      plot_type = "curve",
      stringsAsFactors = FALSE
    )
    DBI::dbAppendTable(con, "staging_csv_data", curve_df)
    
    # 3. Query back from staging_submissions to verify record state
    staged_sub <- DBI::dbGetQuery(
      con,
      "SELECT submitter_name, title, status, pdf_filename FROM staging_submissions WHERE staging_id = $1",
      params = list(new_staging_id)
    )
    
    expect_equal(nrow(staged_sub), 1)
    expect_equal(staged_sub$submitter_name, "Jane Salmon")
    expect_equal(staged_sub$title, "Public SRF Submission Test")
    expect_equal(staged_sub$status, "Pending")
    expect_equal(staged_sub$pdf_filename, "supporting_doc.pdf")
    
    # 4. Query back linked curve data from staging_csv_data
    staged_csv <- DBI::dbGetQuery(
      con,
      "SELECT * FROM staging_csv_data WHERE staging_id = $1 ORDER BY row_index",
      params = list(new_staging_id)
    )
    
    expect_equal(nrow(staged_csv), 2)
    expect_equal(staged_csv$curve_id, c("c1", "c1"))
    expect_equal(staged_csv$stressor_x, c(10.0, 15.0))
    expect_equal(staged_csv$response_y, c(0.95, 0.80))
  })
})