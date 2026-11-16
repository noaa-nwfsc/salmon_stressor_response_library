# tests/testthat/test-staging.R
library(testthat)
library(DBI)
library(jsonlite)

# Fallback helper for null coalescing
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ─────────────────────────────────────────────────────────────────────────────
# HELPER FUNCTIONS (matching submit_relationship.R and admin_review.R)
# ─────────────────────────────────────────────────────────────────────────────

to_pg_array <- function(val) {
  if (is.null(val) || length(val) == 0 || all(is.na(val))) return(NA_character_)
  
  # Ensure robust splitting on comma if passed as comma-separated strings or character vectors
  parts <- unlist(lapply(val, function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(character(0))
    trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
  }))
  
  parts <- parts[parts != "" & !is.na(parts)]
  if (length(parts) == 0) return(NA_character_)
  paste0("{", paste(sprintf('"%s"', gsub('"', '\\"', parts, fixed = TRUE)), collapse = ","), "}")
}

insert_staging_submission <- function(con, meta) {
  query <- "
    INSERT INTO staging_submissions (
      submitter_name, submitter_email, submitter_notes,
      article_type, title, stressor_name, broad_stressor_name, specific_stressor_metric,
      response, srf_formula, species_common_name, latin_name, life_stages,
      location_country, location_state_province, overview, function_derivation, transferability_of_function,
      conf_source, conf_shape, conf_variance, conf_applicability, conf_interactions,
      citations
    ) VALUES (
      $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24::jsonb
    ) RETURNING staging_id;
  "
  
  res <- DBI::dbGetQuery(con, query, params = list(
    meta$submitter_name,
    meta$submitter_email,
    meta$submitter_notes %||% NA_character_,
    meta$article_type %||% "Peer-reviewed Journal Article",
    meta$title,
    meta$stressor_name,
    meta$broad_stressor_name,
    meta$specific_stressor_metric %||% "General Metric",
    meta$response,
    meta$srf_formula %||% NA_character_,
    to_pg_array(meta$species_common_name),
    to_pg_array(meta$latin_name),
    to_pg_array(meta$life_stages),
    to_pg_array(meta$location_country),
    to_pg_array(meta$location_state_province),
    meta$overview %||% "Default overview for staging unit testing.",
    to_pg_array(meta$function_derivation),
    meta$transferability_of_function %||% NA_character_,
    meta$conf_source %||% NA_character_,
    meta$conf_shape %||% NA_character_,
    meta$conf_variance %||% NA_character_,
    meta$conf_applicability %||% NA_character_,
    meta$conf_interactions %||% NA_character_,
    if (!is.null(meta$citations) && nzchar(meta$citations)) meta$citations else "[]"
  ))
  
  as.integer(res$staging_id)
}

insert_staging_csv <- function(con, staging_id, csv_df) {
  df_csv <- csv_df
  df_csv$staging_id <- staging_id
  df_csv$row_index <- seq_len(nrow(df_csv))
  names(df_csv) <- gsub("\\.", "_", names(df_csv))
  DBI::dbAppendTable(con, "staging_csv_data", df_csv)
}

# ─────────────────────────────────────────────────────────────────────────────
# TESTS
# ─────────────────────────────────────────────────────────────────────────────

test_that("insert_staging_submission creates a Pending staging record", {
  with_test_transaction(function(con) {
    mock_title <- paste0("TEST_STAGING_SUBMISSION_", as.numeric(Sys.time()))
    
    meta <- list(
      submitter_name = "Jane Salmon Researcher",
      submitter_email = "jane.researcher@example.org",
      submitter_notes = "Submitting new thermal tolerance data.",
      article_type = "Peer-reviewed Journal Article",
      title = mock_title,
      stressor_name = "Water Temperature",
      broad_stressor_name = "Water Quality",
      specific_stressor_metric = "7DADM, Celsius",
      response = "Egg Survival",
      species_common_name = c("Chinook salmon", "Coho salmon"),
      latin_name = c("Oncorhynchus tshawytscha", "Oncorhynchus kisutch"),
      life_stages = "Incubation",
      location_country = "United States",
      location_state_province = "Washington",
      overview = "Empirical thermal incubation experiment in the Skagit basin.",
      citations = jsonlite::toJSON(list(list(
        text = "Researcher et al. 2026",
        title = "Thermal tolerances",
        url = "[suspicious link removed]"
      )), auto_unbox = TRUE)
    )
    
    # 1. Insert into staging
    staging_id <- insert_staging_submission(con, meta)
    expect_true(is.numeric(staging_id))
    expect_gt(staging_id, 0)
    
    # 2. Verify submission can be queried and defaults to 'Pending'
    staged_row <- DBI::dbGetQuery(
      con,
      "SELECT staging_id, submitter_name, title, status, citations 
       FROM staging_submissions WHERE staging_id = $1",
      params = list(staging_id)
    )
    
    expect_equal(nrow(staged_row), 1)
    expect_equal(staged_row$submitter_name, "Jane Salmon Researcher")
    expect_equal(staged_row$title, mock_title)
    expect_equal(staged_row$status, "Pending")
  })
})

test_that("staging_csv_data links correctly to staging_submissions", {
  with_test_transaction(function(con) {
    mock_title <- paste0("TEST_STAGING_CSV_", as.numeric(Sys.time()))
    
    staging_id <- insert_staging_submission(con, list(
      submitter_name = "Data Tester",
      submitter_email = "tester@noaa.gov",
      article_type = "Report",
      title = mock_title,
      stressor_name = "Streamflow",
      broad_stressor_name = "Hydrology",
      specific_stressor_metric = "cfs",
      response = "Rearing Capacity",
      overview = "Flow rearing capacity test."
    ))
    
    # Mock validated CSV curve points
    mock_csv <- data.frame(
      curve_id = c("c1", "c1", "c1"),
      stressor_label = rep("Flow", 3),
      stressor_x = c(50, 100, 150),
      units_x = rep("cfs", 3),
      response_label = rep("Capacity", 3),
      response_y = c(0.4, 0.8, 0.95),
      units_y = rep("proportion", 3),
      plot_type = c("curve", "curve", "curve"),
      stringsAsFactors = FALSE
    )
    
    insert_staging_csv(con, staging_id, mock_csv)
    
    # Query staging_csv_data
    saved_csv <- DBI::dbGetQuery(
      con,
      "SELECT staging_id, row_index, curve_id, stressor_x, response_y 
       FROM staging_csv_data WHERE staging_id = $1 ORDER BY row_index",
      params = list(staging_id)
    )
    
    expect_equal(nrow(saved_csv), 3)
    expect_equal(saved_csv$row_index, c(1, 2, 3))
    expect_equal(saved_csv$stressor_x, c(50, 100, 150))
    expect_equal(saved_csv$response_y, c(0.4, 0.8, 0.95))
  })
})

test_that("admin approval promotes staging record to live stressor_responses and csv_data", {
  with_test_transaction(function(con) {
    mock_title <- paste0("TEST_APPROVAL_FLOW_", as.numeric(Sys.time()))
    
    # 1. User submits to staging
    staging_id <- insert_staging_submission(con, list(
      submitter_name = "Contributor",
      submitter_email = "contributor@test.com",
      article_type = "Peer-reviewed Journal Article",
      title = mock_title,
      stressor_name = "Turbidity",
      broad_stressor_name = "Water Quality",
      specific_stressor_metric = "NTU",
      response = "Feeding Efficiency",
      species_common_name = "Steelhead",
      latin_name = "Oncorhynchus mykiss",
      location_country = "United States",
      overview = "Turbidity effects on juvenile feeding."
    ))
    
    insert_staging_csv(con, staging_id, data.frame(
      curve_id = "c1",
      stressor_label = "Turbidity",
      stressor_x = 25,
      units_x = "NTU",
      response_label = "Efficiency",
      response_y = 0.65,
      units_y = "%",
      plot_type = "scatter",
      stringsAsFactors = FALSE
    ))
    
    # 2. Admin Approves: calculate next article_id
    max_id_res <- DBI::dbGetQuery(con, "SELECT MAX(article_id) as max_id FROM stressor_responses")
    new_article_id <- if (is.na(max_id_res$max_id)) 1L else as.integer(max_id_res$max_id + 1L)
    
    # Fetch staging data to approve
    staged <- DBI::dbGetQuery(
      con,
      "SELECT * FROM staging_submissions WHERE staging_id = $1",
      params = list(staging_id)
    )
    
    # Insert promoted record into stressor_responses (matching admin_review.R)
    query_promote <- "
      INSERT INTO stressor_responses (
        article_id, title, article_type, stressor_name, broad_stressor_name, specific_stressor_metric,
        response, species_common_name, latin_name, location_country, overview, citations, user_id
      ) VALUES (
        $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12::jsonb, 1
      )
    "
    DBI::dbExecute(con, query_promote, params = list(
      new_article_id,
      staged$title,
      staged$article_type,
      staged$stressor_name,
      staged$broad_stressor_name,
      staged$specific_stressor_metric,
      staged$response,
      staged$species_common_name,
      staged$latin_name,
      staged$location_country,
      staged$overview,
      staged$citations
    ))
    
    # Copy staging_csv_data to live csv_data
    DBI::dbExecute(con, "
      INSERT INTO csv_data (article_id, row_index, curve_id, stressor_label, stressor_x, units_x, response_label, response_y, units_y, plot_type)
      SELECT $1, row_index, curve_id, stressor_label, stressor_x, units_x, response_label, response_y, units_y, plot_type
      FROM staging_csv_data WHERE staging_id = $2
    ", params = list(new_article_id, staging_id))
    
    # Mark submission as Approved
    DBI::dbExecute(con, "UPDATE staging_submissions SET status = 'Approved' WHERE staging_id = $1", params = list(staging_id))
    
    # 3. Verify promotion
    live_article <- DBI::dbGetQuery(con, "SELECT article_id, title FROM stressor_responses WHERE article_id = $1", params = list(new_article_id))
    live_csv <- DBI::dbGetQuery(con, "SELECT article_id, curve_id, response_y FROM csv_data WHERE article_id = $1", params = list(new_article_id))
    updated_status <- DBI::dbGetQuery(con, "SELECT status FROM staging_submissions WHERE staging_id = $1", params = list(staging_id))
    
    expect_equal(nrow(live_article), 1)
    expect_equal(live_article$title, mock_title)
    expect_equal(nrow(live_csv), 1)
    expect_equal(live_csv$response_y, 0.65)
    expect_equal(updated_status$status, "Approved")
  })
})

test_that("admin rejection updates staging status without modifying stressor_responses", {
  with_test_transaction(function(con) {
    mock_title <- paste0("TEST_REJECTION_FLOW_", as.numeric(Sys.time()))
    
    staging_id <- insert_staging_submission(con, list(
      submitter_name = "Spam Submitter",
      submitter_email = "spam@test.com",
      article_type = "Report",
      title = mock_title,
      stressor_name = "Unknown Stressor",
      broad_stressor_name = "Water Quality",
      specific_stressor_metric = "mg/L",
      response = "Growth",
      overview = "Incomplete study."
    ))
    
    # Admin rejects submission
    DBI::dbExecute(con, "UPDATE staging_submissions SET status = 'Rejected' WHERE staging_id = $1", params = list(staging_id))
    
    # Check that status is 'Rejected'
    status_res <- DBI::dbGetQuery(con, "SELECT status FROM staging_submissions WHERE staging_id = $1", params = list(staging_id))
    expect_equal(status_res$status, "Rejected")
    
    # Ensure no record was created in live stressor_responses
    live_res <- DBI::dbGetQuery(con, "SELECT count(*) as count FROM stressor_responses WHERE title = $1", params = list(mock_title))
    expect_equal(live_res$count, 0)
  })
})

test_that("verify all staging and review operations leave zero trace after rollback", {
  with_test_transaction(function(con) {
    leak_submissions <- DBI::dbGetQuery(
      con,
      "SELECT count(*) as count FROM staging_submissions WHERE title LIKE 'TEST_%'"
    )
    leak_stressor <- DBI::dbGetQuery(
      con,
      "SELECT count(*) as count FROM stressor_responses WHERE title LIKE 'TEST_%'"
    )
    
    expect_equal(leak_submissions$count, 0)
    expect_equal(leak_stressor$count, 0)
  })
})