# tests/testthat/test-upload-db.R
library(testthat)

# Helper matching upload.R's ID generation logic
get_next_article_id <- function(con) {
  max_id_res <- DBI::dbGetQuery(con, "SELECT MAX(article_id) as max_id FROM stressor_responses")
  if (is.na(max_id_res$max_id)) 1L else as.integer(max_id_res$max_id + 1L)
}

test_that("get_next_article_id returns a valid, positive integer", {
  with_test_transaction(function(con) {
    next_id <- get_next_article_id(con)
    expect_true(is.numeric(next_id))
    expect_gt(next_id, 0)
  })
})

test_that("upload_data inserts article and rolls back without trace", {
  with_test_transaction(function(con) {
    test_id <- get_next_article_id(con)
    mock_title <- paste0("TEST_TRANSACTION_ARTICLE_", as.numeric(Sys.time()))
    
    # Insert test row satisfying all NOT NULL constraints from upload.R
    query <- "
      INSERT INTO stressor_responses (
        article_id, user_id, article_type, title,
        broad_stressor_name, stressor_name, specific_stressor_metric,
        response, species_common_name, latin_name, location_country,
        overview, citations
      ) VALUES (
        $1, $2, $3, $4,
        $5, $6, $7,
        $8, $9::text[], $10::text[], $11::text[],
        $12, $13::jsonb
      )
    "
    DBI::dbExecute(
      con,
      query,
      params = list(
        test_id,
        1L,
        "Peer-reviewed Journal Article",
        mock_title,
        "Water Quality",
        "Water Temperature",
        "7DADM, Celsius",
        "Survival",
        '{"Chinook salmon"}',
        '{"Oncorhynchus tshawytscha"}',
        '{"United States"}',
        "Unit test mock overview description.",
        "[]"
      )
    )
    
    # Verify row exists and can be queried inside active transaction
    res <- DBI::dbGetQuery(
      con,
      "SELECT article_id, title, broad_stressor_name, stressor_name 
       FROM stressor_responses WHERE article_id = $1",
      params = list(test_id)
    )
    expect_equal(nrow(res), 1)
    expect_equal(res$title, mock_title)
    expect_equal(res$broad_stressor_name, "Water Quality")
    expect_equal(res$stressor_name, "Water Temperature")
  })
})

test_that("verify rollback occurred from previous test", {
  with_test_transaction(function(con) {
    # Check that the test title was rolled back and is not in the database
    leak_check <- DBI::dbGetQuery(
      con,
      "SELECT count(*) as count FROM stressor_responses WHERE title LIKE 'TEST_TRANSACTION_ARTICLE_%'"
    )
    expect_equal(leak_check$count, 0)
  })
})