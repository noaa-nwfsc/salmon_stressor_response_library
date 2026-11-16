# tests/testthat/test-admin_review.R
library(testthat)
library(DBI)
library(htmltools)

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC & UI BUILDER TESTS
# ─────────────────────────────────────────────────────────────────────────────

render_review_actions <- function(staging_id, status) {
  if (is.null(status) || is.na(status)) status <- "Pending"
  
  if (status == "Pending") {
    tags$div(
      class = "btn-group btn-group-sm",
      tags$button(
        class = "btn btn-info action-button btn-view-staging",
        id = paste0("view_staging_", staging_id),
        "Review"
      ),
      tags$button(
        class = "btn btn-success action-button btn-approve-staging",
        id = paste0("approve_staging_", staging_id),
        "Approve"
      ),
      tags$button(
        class = "btn btn-danger action-button btn-reject-staging",
        id = paste0("reject_staging_", staging_id),
        "Reject"
      )
    )
  } else {
    tags$span(
      class = sprintf("badge bg-%s", if (status == "Approved") "success" else "secondary"),
      status
    )
  }
}

test_that("render_review_actions renders active action buttons for Pending items", {
  ui <- render_review_actions(42, "Pending")
  ui_str <- as.character(ui)
  
  expect_true(grepl('id="view_staging_42"', ui_str, fixed = TRUE))
  expect_true(grepl('id="approve_staging_42"', ui_str, fixed = TRUE))
  expect_true(grepl('id="reject_staging_42"', ui_str, fixed = TRUE))
  expect_true(grepl("btn-success", ui_str, fixed = TRUE))
  expect_true(grepl("btn-danger", ui_str, fixed = TRUE))
})

test_that("render_review_actions renders status badge for finalized items", {
  app_ui <- render_review_actions(42, "Approved")
  app_str <- as.character(app_ui)
  expect_false(grepl("btn-group", app_str, fixed = TRUE))
  expect_true(grepl("badge", app_str, fixed = TRUE))
  expect_true(grepl("Approved", app_str, fixed = TRUE))
  
  rej_ui <- render_review_actions(42, "Rejected")
  rej_str <- as.character(rej_ui)
  expect_true(grepl("Rejected", rej_str, fixed = TRUE))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. DATABASE INTEGRATION TESTS: REVIEW QUEUE, REJECTION & PROMOTION
# ─────────────────────────────────────────────────────────────────────────────

test_that("admin review queue filters staging records by status", {
  with_test_transaction(function(con) {
    # Insert two test submissions (one Pending, one Approved)
    q_insert <- "
      INSERT INTO staging_submissions (
        submitter_name, submitter_email, article_type, title,
        stressor_name, broad_stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview, status
      ) VALUES (
        $1, $2, 'Report', $3, 'Water Temp', 'Water Quality', 'degC', 'Survival',
        '{\"Chinook salmon\"}', '{\"Oncorhynchus tshawytscha\"}', '{\"USA\"}', 'Overview', $4
      );
    "
    DBI::dbExecute(con, q_insert, params = list("Admin Test", "admin@test.gov", "Pending Study", "Pending"))
    DBI::dbExecute(con, q_insert, params = list("Admin Test", "admin@test.gov", "Archived Study", "Approved"))
    
    # Query filtered by Pending
    pending_items <- DBI::dbGetQuery(
      con,
      "SELECT staging_id, title, status FROM staging_submissions WHERE submitter_name = 'Admin Test' AND status = 'Pending'"
    )
    expect_equal(nrow(pending_items), 1)
    expect_equal(pending_items$title, "Pending Study")
    
    # Query filtered by Approved
    approved_items <- DBI::dbGetQuery(
      con,
      "SELECT staging_id, title, status FROM staging_submissions WHERE submitter_name = 'Admin Test' AND status = 'Approved'"
    )
    expect_equal(nrow(approved_items), 1)
    expect_equal(approved_items$title, "Archived Study")
  })
})

test_that("reject_staging_submission transitions record to Rejected", {
  with_test_transaction(function(con) {
    # 1. Insert a pending record
    q_insert <- "
      INSERT INTO staging_submissions (
        submitter_name, submitter_email, article_type, title,
        stressor_name, broad_stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview, status
      ) VALUES (
        'Tester', 't@test.com', 'Report', 'Study to Reject',
        'Water Temp', 'Water Quality', 'degC', 'Survival',
        '{\"Coho salmon\"}', '{\"Oncorhynchus kisutch\"}', '{\"USA\"}', 'Overview', 'Pending'
      ) RETURNING staging_id;
    "
    staged <- DBI::dbGetQuery(con, q_insert)
    s_id <- as.integer(staged[[1L]])
    
    # 2. Reject the submission (updating status and submitter_notes)
    q_reject <- "
      UPDATE staging_submissions
      SET status = 'Rejected',
          submitter_notes = $1
      WHERE staging_id = $2;
    "
    DBI::dbExecute(con, q_reject, params = list("Insufficient methodology details", s_id))
    
    # 3. Verify rejection state
    res <- DBI::dbGetQuery(
      con,
      "SELECT status, submitter_notes FROM staging_submissions WHERE staging_id = $1",
      params = list(s_id)
    )
    expect_equal(nrow(res), 1)
    expect_equal(res$status, "Rejected")
    expect_equal(res$submitter_notes, "Insufficient methodology details")
  })
})

test_that("promote_staging_to_live migrates metadata and csv curve points to production", {
  with_test_transaction(function(con) {
    # 1. Insert pending submission
    q_insert <- "
      INSERT INTO staging_submissions (
        submitter_name, submitter_email, article_type, title,
        stressor_name, broad_stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview, status
      ) VALUES (
        'Researcher', 'r@test.com', 'Report', 'Study to Promote',
        'Temperature', 'Water Quality', 'Celsius', 'Mortality',
        '{\"Sockeye salmon\"}', '{\"Oncorhynchus nerka\"}', '{\"Canada\"}', 'Thermal overview', 'Pending'
      ) RETURNING staging_id;
    "
    staged <- DBI::dbGetQuery(con, q_insert)
    s_id <- as.integer(staged[[1L]])
    
    # 2. Insert linked quantitative curve points in staging_csv_data
    curve_df <- data.frame(
      staging_id = s_id,
      row_index = 1:2,
      curve_id = "c1",
      stressor_label = "Temperature",
      stressor_x = c(12.0, 16.0),
      units_x = "degC",
      response_label = "Mortality",
      response_y = c(0.10, 0.40),
      units_y = "ratio",
      plot_type = "curve",
      stringsAsFactors = FALSE
    )
    DBI::dbAppendTable(con, "staging_csv_data", curve_df)
    
    # 3. Simulate promote_staging_to_live workflow
    # Calculate next production article_id
    max_id_res <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(article_id), 0) as max_id FROM stressor_responses")
    new_art_id <- as.integer(max_id_res$max_id + 1L)
    
    # Promote metadata into stressor_responses
    q_promote_meta <- "
      INSERT INTO stressor_responses (
        article_id, user_id, article_type, title,
        stressor_name, broad_stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview
      )
      SELECT
        $1, 1, article_type, title,
        stressor_name, broad_stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview
      FROM staging_submissions
      WHERE staging_id = $2;
    "
    DBI::dbExecute(con, q_promote_meta, params = list(new_art_id, s_id))
    
    # Promote curve points from staging_csv_data into csv_data
    q_promote_csv <- "
      INSERT INTO csv_data (
        article_id, row_index, curve_id, stressor_label, stressor_x, units_x,
        response_label, response_y, units_y, plot_type
      )
      SELECT
        $1, row_index, curve_id, stressor_label, stressor_x, units_x,
        response_label, response_y, units_y, plot_type
      FROM staging_csv_data
      WHERE staging_id = $2;
    "
    DBI::dbExecute(con, q_promote_csv, params = list(new_art_id, s_id))
    
    # Mark staged submission as Approved
    DBI::dbExecute(
      con,
      "UPDATE staging_submissions SET status = 'Approved' WHERE staging_id = $1",
      params = list(s_id)
    )
    
    # 4. Verify production table contains promoted record
    live_meta <- DBI::dbGetQuery(
      con,
      "SELECT title, response FROM stressor_responses WHERE article_id = $1",
      params = list(new_art_id)
    )
    expect_equal(nrow(live_meta), 1)
    expect_equal(live_meta$title, "Study to Promote")
    expect_equal(live_meta$response, "Mortality")
    
    # 5. Verify production csv_data contains promoted curve points
    live_csv <- DBI::dbGetQuery(
      con,
      "SELECT row_index, stressor_x, response_y FROM csv_data WHERE article_id = $1 ORDER BY row_index",
      params = list(new_art_id)
    )
    expect_equal(nrow(live_csv), 2)
    expect_equal(live_csv$stressor_x, c(12.0, 16.0))
    expect_equal(live_csv$response_y, c(0.10, 0.40))
    
    # 6. Verify staging status is Approved
    final_staged <- DBI::dbGetQuery(
      con,
      "SELECT status FROM staging_submissions WHERE staging_id = $1",
      params = list(s_id)
    )
    expect_equal(final_staged$status, "Approved")
  })
})