# tests/testthat/test-edit_article.R
library(testthat)
library(DBI)
library(jsonlite)

# Helper for null coalescing
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ─────────────────────────────────────────────────────────────────────────────
# 1. HELPER LOGIC TESTS (matching edit_article.R)
# ─────────────────────────────────────────────────────────────────────────────

# Helper to parse citations/revision log from JSON
parse_json_cits <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "" || x == "[]") return(list())
  tryCatch({
    df <- jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
    if (is.data.frame(df)) {
      lapply(seq_len(nrow(df)), function(i) as.list(df[i, ]))
    } else {
      df
    }
  }, error = function(e) list())
}

# Helper to format PostgreSQL text array (bracket-safe using unlist)
to_pg_array <- function(val) {
  if (is.null(val) || length(val) == 0 || all(is.na(val))) return(NA_character_)
  parts <- unlist(lapply(val, function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(character(0))
    trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  }))
  parts <- parts[parts != "" & !is.na(parts)]
  if (length(parts) == 0) return(NA_character_)
  paste0("{", paste(sprintf('"%s"', gsub('"', '\\"', parts, fixed = TRUE)), collapse = ","), "}")
}

test_that("parse_json_cits correctly parses various JSON representations", {
  # Empty or null inputs
  expect_equal(parse_json_cits(NULL), list())
  expect_equal(parse_json_cits(NA_character_), list())
  expect_equal(parse_json_cits(""), list())
  expect_equal(parse_json_cits("[]"), list())
  
  # Valid single citation JSON
  json_single <- '[{"title":"Baker 1995","url":"[suspicious link removed]","text":"Baker et al."}]'
  res_single <- parse_json_cits(json_single)
  expect_equal(length(res_single), 1)
  
  idx_first <- 1L
  expect_equal(res_single[[idx_first]]$title, "Baker 1995")
  expect_equal(res_single[[idx_first]]$url, "[suspicious link removed]")
  
  # Malformed JSON should fail gracefully to empty list
  expect_equal(parse_json_cits("{bad_json}"), list())
})

test_that("revision_log compilation appends new entries to existing history", {
  # Existing revision history
  old_revs_json <- '[{"message":"Initial upload","user":"admin1","date":"2026-01-01"}]'
  old_revs <- parse_json_cits(old_revs_json)
  
  # Append new edit
  new_entry <- list(
    message = "Updated thermal curve upper limit",
    user = "aimee.fullerton",
    date = as.character(Sys.Date())
  )
  old_revs[[length(old_revs) + 1L]] <- new_entry
  updated_rev_json <- jsonlite::toJSON(old_revs, auto_unbox = TRUE)
  
  # Verify new JSON contains both revisions in sequence
  parsed_updated <- parse_json_cits(updated_rev_json)
  expect_equal(length(parsed_updated), 2)
  
  idx_1 <- 1L
  idx_2 <- 2L
  expect_equal(parsed_updated[[idx_1]]$message, "Initial upload")
  expect_equal(parsed_updated[[idx_2]]$message, "Updated thermal curve upper limit")
  expect_equal(parsed_updated[[idx_2]]$user, "aimee.fullerton")
})

test_that("to_pg_array formats character vectors and comma strings correctly", {
  # Standard vector
  expect_equal(
    to_pg_array(c("Chinook salmon", "Coho salmon")),
    '{"Chinook salmon","Coho salmon"}'
  )
  
  # Comma-separated string in single element
  expect_equal(
    to_pg_array("Chinook salmon, Steelhead"),
    '{"Chinook salmon","Steelhead"}'
  )
  
  # Quotes escaping
  expect_equal(
    to_pg_array('Fish "Quote" Test'),
    '{"Fish \\"Quote\\" Test"}'
  )
  
  # Empty or NA input
  expect_true(is.na(to_pg_array(character(0))))
  expect_true(is.na(to_pg_array(NA_character_)))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. DATABASE TRANSACTION TESTS (UPDATE METADATA & REPLACE CURVE DATA)
# ─────────────────────────────────────────────────────────────────────────────

test_that("update_article modifies stressor_responses metadata cleanly", {
  with_test_transaction(function(con) {
    # 1. Seed a temporary article to edit
    max_id_res <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(article_id), 0) as max_id FROM stressor_responses")
    art_id <- as.integer(max_id_res$max_id + 1L)
    init_title <- paste0("EDIT_TEST_ARTICLE_INIT_", as.numeric(Sys.time()))
    
    insert_sql <- "
      INSERT INTO stressor_responses (
        article_id, user_id, article_type, title, broad_stressor_name,
        stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country,
        overview, citations, revision_log
      ) VALUES (
        $1, 1, $2, $3, $4,
        $5, $6, $7,
        $8::text[], $9::text[], $10::text[],
        $11, $12::jsonb, $13::jsonb
      )
    "
    DBI::dbExecute(con, insert_sql, params = list(
      art_id,
      "Peer-reviewed Journal Article",
      init_title,
      "Water Quality",
      "Water Temperature",
      "Celsius",
      "Survival",
      '{"Chinook salmon"}',
      '{"Oncorhynchus tshawytscha"}',
      '{"United States"}',
      "Initial overview text.",
      "[]",
      '[{"message":"Initial entry","user":"test_user","date":"2026-01-01"}]'
    ))
    
    # 2. Execute UPDATE query matching edit_article.R
    updated_title <- paste0("EDIT_TEST_ARTICLE_UPDATED_", as.numeric(Sys.time()))
    updated_overview <- "Updated overview description after admin review."
    updated_species <- c("Chinook salmon", "Coho salmon")
    new_rev_log <- jsonlite::toJSON(list(
      list(message = "Initial entry", user = "test_user", date = "2026-01-01"),
      list(message = "Corrected species list and overview", user = "admin_tester", date = as.character(Sys.Date()))
    ), auto_unbox = TRUE)
    
    update_sql <- "
      UPDATE stressor_responses SET
        title = $1,
        species_common_name = $2::text[],
        overview = $3,
        revision_log = $4::jsonb
      WHERE article_id = $5
    "
    rows_affected <- DBI::dbExecute(con, update_sql, params = list(
      updated_title,
      to_pg_array(updated_species),
      updated_overview,
      new_rev_log,
      art_id
    ))
    expect_equal(rows_affected, 1)
    
    # 3. Query and assert updated fields
    res <- DBI::dbGetQuery(
      con,
      "SELECT title, species_common_name, overview, revision_log 
       FROM stressor_responses WHERE article_id = $1",
      params = list(art_id)
    )
    
    expect_equal(nrow(res), 1)
    expect_equal(res$title, updated_title)
    expect_equal(res$overview, updated_overview)
    
    revs <- parse_json_cits(res$revision_log)
    expect_equal(length(revs), 2)
    
    idx_admin <- 2L
    expect_equal(revs[[idx_admin]]$user, "admin_tester")
  })
})

test_that("edit_article curve replacement deletes old curve rows and appends new rows", {
  with_test_transaction(function(con) {
    max_id_res <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(article_id), 0) as max_id FROM stressor_responses")
    art_id <- as.integer(max_id_res$max_id + 1L)
    
    # 1. Insert parent article with all required NOT NULL columns
    DBI::dbExecute(con, "
      INSERT INTO stressor_responses (
        article_id, user_id, article_type, title, broad_stressor_name,
        stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview
      ) VALUES (
        $1, 1, 'Report', 'Curve Replacement Test', 'Hydrology',
        'Flow', 'cfs', 'Capacity',
        $2::text[], $3::text[], $4::text[], 'Desc'
      )
    ", params = list(
      art_id,
      '{"Chinook salmon"}',
      '{"Oncorhynchus tshawytscha"}',
      '{"United States"}'
    ))
    
    # 2. Seed initial 3 curve points into csv_data
    old_curve_df <- data.frame(
      article_id = art_id,
      row_index = 1:3,
      curve_id = "c1",
      stressor_label = "Flow",
      stressor_x = c(10, 20, 30),
      units_x = "cfs",
      response_label = "Capacity",
      response_y = c(0.2, 0.4, 0.6),
      units_y = "prop",
      plot_type = "curve",
      stringsAsFactors = FALSE
    )
    DBI::dbAppendTable(con, "csv_data", old_curve_df)
    
    # Verify initial rows exist
    count_before <- DBI::dbGetQuery(con, "SELECT count(*) as count FROM csv_data WHERE article_id = $1", params = list(art_id))$count
    expect_equal(count_before, 3)
    
    # 3. Simulate replacement: DELETE old curve rows, INSERT 2 new replacement rows
    DBI::dbExecute(con, "DELETE FROM csv_data WHERE article_id = $1", params = list(art_id))
    
    new_curve_df <- data.frame(
      article_id = art_id,
      row_index = 1:2,
      curve_id = "c1_v2",
      stressor_label = "Corrected Flow",
      stressor_x = c(15, 35),
      units_x = "cfs",
      response_label = "Capacity",
      response_y = c(0.35, 0.75),
      units_y = "prop",
      plot_type = "curve",
      stringsAsFactors = FALSE
    )
    DBI::dbAppendTable(con, "csv_data", new_curve_df)
    
    # 4. Verify replacement
    updated_rows <- DBI::dbGetQuery(
      con,
      "SELECT row_index, curve_id, stressor_label, stressor_x, response_y 
       FROM csv_data WHERE article_id = $1 ORDER BY row_index",
      params = list(art_id)
    )
    
    expect_equal(nrow(updated_rows), 2)
    expect_equal(updated_rows$curve_id, c("c1_v2", "c1_v2"))
    expect_equal(updated_rows$stressor_label, c("Corrected Flow", "Corrected Flow"))
    expect_equal(updated_rows$stressor_x, c(15, 35))
    expect_equal(updated_rows$response_y, c(0.35, 0.75))
  })
})

test_that("verify all edit operations leave zero residual records after rollback", {
  with_test_transaction(function(con) {
    leak_meta <- DBI::dbGetQuery(
      con,
      "SELECT count(*) as count FROM stressor_responses WHERE title LIKE 'EDIT_TEST_ARTICLE_%'"
    )
    expect_equal(leak_meta$count, 0)
  })
})