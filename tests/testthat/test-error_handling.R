# tests/testthat/test-error_handling.R
library(testthat)
library(DBI)
library(shiny)
library(rlang)

# Source modules/error_handling.R or define matching implementations
source("../../modules/error_handling.R", local = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC TESTS: INLINE HTML ALERTS & CSV ERROR GUIDANCE
# ─────────────────────────────────────────────────────────────────────────────

test_that("create_alert_html generates correct styles and icons for all 4 types", {
  # Error alert
  err_html <- create_alert_html("error", "Failed operation")
  expect_true(grepl("#f8d7da", err_html, fixed = TRUE))
  expect_true(grepl("⚠️", err_html, fixed = TRUE))
  expect_true(grepl("Failed operation", err_html, fixed = TRUE))
  
  # Warning alert
  warn_html <- create_alert_html("warning", "Careful")
  expect_true(grepl("#fff3cd", warn_html, fixed = TRUE))
  expect_true(grepl("⚡", warn_html, fixed = TRUE))
  
  # Success alert
  succ_html <- create_alert_html("success", "Saved")
  expect_true(grepl("#d4edda", succ_html, fixed = TRUE))
  expect_true(grepl("✓", succ_html, fixed = TRUE))
  
  # Info alert
  info_html <- create_alert_html("info", "Information")
  expect_true(grepl("#d1ecf1", info_html, fixed = TRUE))
  expect_true(grepl("ℹ️", info_html, fixed = TRUE))
  
  # Fallback to info for unrecognized type
  unknown_html <- create_alert_html("custom_unknown", "Fallback test")
  expect_true(grepl("#d1ecf1", unknown_html, fixed = TRUE))
  expect_true(grepl("ℹ️", unknown_html, fixed = TRUE))
})

test_that("create_alert_html formats detail bullet points cleanly", {
  details <- list("Missing column A", "Invalid value in row 4")
  alert <- create_alert_html("error", "Validation error", details = details)
  
  # Check for ul tag (with or without style attributes) and li bullet items
  expect_true(grepl("<ul", alert, fixed = TRUE))
  expect_true(grepl("</ul>", alert, fixed = TRUE))
  expect_true(grepl("<li>Missing column A</li>", alert, fixed = TRUE))
  expect_true(grepl("<li>Invalid value in row 4</li>", alert, fixed = TRUE))
})

test_that("get_csv_error_message handles valid validation results", {
  valid_input <- list(valid = TRUE, message = "All good", all_issues = list())
  res <- get_csv_error_message(valid_input)
  
  expect_equal(res$title, "Success")
  expect_equal(res$message, "CSV validation passed")
})

test_that("get_csv_error_message injects domain-specific guidance based on issues", {
  # Scenario 1: Missing required column
  missing_col_input <- list(
    valid = FALSE,
    message = "Column check failed",
    all_issues = list("Missing required column: 'stressor.x'")
  )
  res_col <- get_csv_error_message(missing_col_input)
  expect_equal(res_col$title, "❌ CSV Upload Validation Failed")
  expect_true(grepl("How to fix:</strong> Ensure your CSV contains these exactly named required columns", res_col$message, fixed = TRUE))
  expect_true(grepl("• stressor.x", res_col$message, fixed = TRUE))
  
  # Scenario 2: Non-numeric values
  non_num_input <- list(
    valid = FALSE,
    message = "Data check failed",
    all_issues = list("Column contains non-numeric values in row 2")
  )
  res_num <- get_csv_error_message(non_num_input)
  expect_true(grepl("Check that all numeric columns contain only numbers", res_num$message, fixed = TRUE))
  
  # Scenario 3: Single unique value violation
  unique_input <- list(
    valid = FALSE,
    message = "Data check failed",
    all_issues = list("Column 'units.x' must have exactly 1 unique value")
  )
  res_uniq <- get_csv_error_message(unique_input)
  expect_true(grepl("must have the exact same value in every row", res_uniq$message, fixed = TRUE))
  
  # Scenario 4: Empty values in required column
  empty_input <- list(
    valid = FALSE,
    message = "Empty check failed",
    all_issues = list("Column 'curve.id' has empty values")
  )
  res_empty <- get_csv_error_message(empty_input)
  expect_true(grepl("Fill in all required fields", res_empty$message, fixed = TRUE))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. LOGGING TESTS
# ─────────────────────────────────────────────────────────────────────────────

test_that("log_error emits formatted messages with context and metadata", {
  # Use capture_messages() from testthat to reliably intercept message()
  msgs <- testthat::capture_messages({
    log_error("CSV Upload", "Invalid format detected", list(filename = "test.csv", size_bytes = 4096))
  })
  
  combined_log <- paste(msgs, collapse = " ")
  
  expect_true(grepl("CSV Upload: Invalid format detected", combined_log, fixed = TRUE))
  expect_true(grepl("filename: test.csv", combined_log, fixed = TRUE))
  expect_true(grepl("size_bytes: 4096", combined_log, fixed = TRUE))
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. DATABASE INTEGRATION TESTS: TITLE DUPLICATE CHECKS
# ─────────────────────────────────────────────────────────────────────────────

test_that("check_title_duplicate detects identical and case-insensitive duplicates", {
  with_test_transaction(function(con) {
    # 1. Blank/NULL title inputs
    expect_equal(check_title_duplicate(NULL, con)$duplicate, FALSE)
    expect_equal(check_title_duplicate("", con)$duplicate, FALSE)
    expect_equal(check_title_duplicate(NULL, con)$message, "No title provided")
    
    # 2. Insert test article
    max_id_res <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(article_id), 0) as max_id FROM stressor_responses")
    base_id <- as.integer(max_id_res$max_id + 1L)
    
    insert_sql <- "
      INSERT INTO stressor_responses (
        article_id, user_id, article_type, title, broad_stressor_name,
        stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview
      ) VALUES (
        $1, 1, 'Report', 'Unique Thermal Tolerances 2026', 'Water Quality',
        'Temperature', 'Celsius', 'Survival',
        '{\"Chinook salmon\"}', '{\"Oncorhynchus tshawytscha\"}', '{\"USA\"}', 'Overview'
      )
    "
    DBI::dbExecute(con, insert_sql, params = list(base_id))
    
    # 3. Check exact duplicate
    res_exact <- check_title_duplicate("Unique Thermal Tolerances 2026", con)
    expect_true(res_exact$duplicate)
    expect_true(grepl("already exists in the database", res_exact$message, fixed = TRUE))
    
    # 4. Check case-insensitive duplicate with whitespace
    res_case <- check_title_duplicate("  unique thermal tolerances 2026  ", con)
    expect_true(res_case$duplicate)
    
    # 5. Check novel non-duplicate title
    res_novel <- check_title_duplicate("Completely Different Study 2026", con)
    expect_false(res_novel$duplicate)
    expect_equal(res_novel$message, "")
  })
})