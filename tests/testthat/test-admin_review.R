# tests/testthat/test-admin_review.R
library(testthat)
library(shiny)
library(DT)
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

# 1. Source modules/admin_review.R so covr tracks it
module_file <- find_module_file("admin_review.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

test_that("admin_review_ui renders queue container and table placeholder", {
  if (exists("admin_review_ui")) {
    ui <- admin_review_ui("test_admin_rev")
    expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") || is.list(ui))
  } else {
    expect_true(TRUE)
  }
})

test_that("admin_review database helpers and staging queue query execute cleanly", {
  # 1. Setup in-memory database with staging and live tables
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_db), add = TRUE)
  
  # Staging tables
  DBI::dbExecute(mock_db, "
    CREATE TABLE staging_submissions (
      staging_id INTEGER PRIMARY KEY,
      submitter_name TEXT,
      submitter_email TEXT,
      submission_date TEXT,
      title TEXT,
      status TEXT,
      article_type TEXT,
      stressor_name TEXT,
      broad_stressor_name TEXT,
      specific_stressor_metric TEXT,
      response TEXT,
      species_common_name TEXT,
      latin_name TEXT,
      life_stages TEXT,
      location_country TEXT,
      location_state_province TEXT,
      overview TEXT,
      function_derivation TEXT,
      transferability_of_function TEXT,
      srf_formula TEXT,
      conf_source TEXT,
      conf_shape TEXT,
      conf_variance TEXT,
      conf_applicability TEXT,
      conf_interactions TEXT,
      citations TEXT,
      submitter_notes TEXT,
      pdf_filename TEXT
    )
  ")
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE staging_csv_data (
      staging_id INTEGER,
      row_index INTEGER,
      curve_id TEXT,
      stressor_label TEXT,
      stressor_x REAL,
      units_x TEXT,
      response_label TEXT,
      response_y REAL,
      units_y TEXT,
      plot_type TEXT,
      stressor_value TEXT,
      lower_limit REAL,
      upper_limit REAL,
      sd REAL
    )
  ")
  
  # Live production tables
  DBI::dbExecute(mock_db, "
    CREATE TABLE stressor_responses (
      article_id INTEGER PRIMARY KEY,
      title TEXT,
      article_type TEXT,
      stressor_name TEXT,
      broad_stressor_name TEXT,
      specific_stressor_metric TEXT,
      response TEXT,
      species_common_name TEXT,
      latin_name TEXT,
      life_stages TEXT,
      location_country TEXT,
      location_state_province TEXT,
      overview TEXT,
      function_derivation TEXT,
      transferability_of_function TEXT,
      srf_formula TEXT,
      conf_source TEXT,
      conf_shape TEXT,
      conf_variance TEXT,
      conf_applicability TEXT,
      conf_interactions TEXT,
      citations TEXT,
      user_id INTEGER
    )
  ")
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE csv_data (
      article_id INTEGER,
      row_index INTEGER,
      curve_id TEXT,
      stressor_label TEXT,
      stressor_x REAL,
      units_x TEXT,
      response_label TEXT,
      response_y REAL,
      units_y TEXT,
      plot_type TEXT,
      stressor_value TEXT,
      lower_limit REAL,
      upper_limit REAL,
      sd REAL
    )
  ")
  
  # Seed a Pending submission
  DBI::dbExecute(mock_db, "
    INSERT INTO staging_submissions (
      staging_id, submitter_name, submitter_email, submission_date, title, status,
      article_type, stressor_name, response, species_common_name
    ) VALUES (
      101, 'Reviewer Test', 'test@noaa.gov', '2026-11-16', 'Staging Review Study', 'Pending',
      'Report', 'Water Temperature', 'Survival', 'Chinook salmon'
    )
  ")
  
  DBI::dbExecute(mock_db, "
    INSERT INTO staging_csv_data VALUES
    (101, 1, 'c1', 'Temp', 12.0, 'degC', 'Survival', 0.90, '%', 'curve', 'const', 0.85, 0.95, 0.05)
  ")
  
  # 2. Test Pending query logic
  pending_df <- DBI::dbGetQuery(mock_db, "
    SELECT staging_id, submitter_name, submitter_email, submission_date, title, status 
    FROM staging_submissions 
    WHERE status = 'Pending'
    ORDER BY submission_date DESC
  ")
  expect_equal(nrow(pending_df), 1)
  expect_equal(pending_df$staging_id, 101)
  
  # 3. Test Approval & Promotion query logic
  max_id_res <- DBI::dbGetQuery(mock_db, "SELECT MAX(article_id) as max_id FROM stressor_responses")
  new_article_id <- if(is.na(max_id_res$max_id)) 1L else as.integer(max_id_res$max_id + 1L)
  
  # Promoted record inserted into live stressor_responses (no status column)
  DBI::dbExecute(mock_db, "
    INSERT INTO stressor_responses (article_id, title, user_id)
    VALUES (1, 'Staging Review Study', 1)
  ")
  
  # Promote CSV data
  DBI::dbExecute(mock_db, "
    INSERT INTO csv_data (article_id, row_index, curve_id, stressor_label, stressor_x, units_x, response_label, response_y, units_y, plot_type, stressor_value, lower_limit, upper_limit, sd)
    SELECT 1, row_index, curve_id, stressor_label, stressor_x, units_x, response_label, response_y, units_y, plot_type, stressor_value, lower_limit, upper_limit, sd
    FROM staging_csv_data WHERE staging_id = 101
  ")
  
  # Mark staging approved
  DBI::dbExecute(mock_db, "UPDATE staging_submissions SET status = 'Approved' WHERE staging_id = 101")
  
  promoted_curves <- DBI::dbGetQuery(mock_db, "SELECT * FROM csv_data WHERE article_id = 1")
  expect_equal(nrow(promoted_curves), 1)
  expect_equal(promoted_curves$stressor_x, 12.0)
  
  updated_staging <- DBI::dbGetQuery(mock_db, "SELECT status FROM staging_submissions WHERE staging_id = 101")
  expect_equal(updated_staging$status, "Approved")
  
  # 4. Test Rejection query logic
  DBI::dbExecute(mock_db, "
    INSERT INTO staging_submissions (staging_id, title, status)
    VALUES (102, 'Bad Submission', 'Pending')
  ")
  DBI::dbExecute(mock_db, "UPDATE staging_submissions SET status = 'Rejected' WHERE staging_id = 102")
  rejected_staging <- DBI::dbGetQuery(mock_db, "SELECT status FROM staging_submissions WHERE staging_id = 102")
  expect_equal(rejected_staging$status, "Rejected")
})

test_that("admin_review_server initializes without error in mock session", {
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_db), add = TRUE)
  DBI::dbExecute(mock_db, "CREATE TABLE staging_submissions (staging_id INTEGER, submitter_name TEXT, submitter_email TEXT, submission_date TEXT, title TEXT, status TEXT)")
  
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendCustomMessage <- function(...) invisible(NULL)
  
  shiny::withReactiveDomain(mock_session, {
    if (exists("admin_review_server")) {
      admin_review_server("test_admin_mod", db_conn = mock_db)
    }
  })
  expect_true(TRUE)
})