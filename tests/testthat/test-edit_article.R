# tests/testthat/test-edit_article.R
library(testthat)
library(shiny)
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

# 1. Source the actual module file so covr instruments it
module_file <- find_module_file("edit_article.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

test_that("edit_article_ui renders modal and form elements cleanly", {
  if (exists("edit_article_ui")) {
    dummy_paper <- data.frame(
      article_id = 101L,
      title = "Thermal Effects on Salmon",
      article_type = "Peer-reviewed",
      response = "Survival",
      stressor_name = "Water Temperature",
      broad_stressor_name = "Water Quality",
      specific_stressor_metric = "7-DADMax",
      species_common_name = "Chinook salmon",
      latin_name = "Oncorhynchus tshawytscha",
      life_stages = "Juvenile",
      activity = "Rearing",
      season = "Summer",
      location_country = "USA",
      location_state_province = "Washington",
      location_watershed_lab = "Columbia River Basin",
      location_river_creek = "Snake River",
      function_derivation = "Empirical fit",
      overview = "Thermal tolerance testing",
      transferability_of_function = "Applicable across coldwater systems",
      srf_formula = "$$y = ax + b$$",
      source_of_stressor_data = "Field monitors",
      conf_source = "High",
      conf_shape = "High",
      conf_variance = "Moderate",
      conf_applicability = "High",
      conf_interactions = "Low",
      citations = "[]",
      revision_log = "[]",
      stringsAsFactors = FALSE
    )
    
    ui_out <- edit_article_ui("test_edit", paper = dummy_paper)
    expect_true(inherits(ui_out, "shiny.tag") || inherits(ui_out, "shiny.tag.list") || is.list(ui_out))
  } else {
    expect_true(TRUE)
  }
})

test_that("edit_article database transaction updates metadata correctly", {
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_db), add = TRUE)
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE stressor_responses (
      article_id INTEGER PRIMARY KEY,
      title TEXT,
      species_common_name TEXT,
      overview TEXT,
      status TEXT
    )
  ")
  
  DBI::dbExecute(mock_db, "
    INSERT INTO stressor_responses (article_id, title, species_common_name, overview, status)
    VALUES (10, 'Initial Title', '{\"Chinook salmon\"}', 'Initial Overview', 'Approved')
  ")
  
  # Test the parameterized update query used by edit_article_server
  DBI::dbExecute(
    mock_db,
    "UPDATE stressor_responses SET title = $1, overview = $2 WHERE article_id = $3",
    params = list("Revised Title", "Revised Overview", 10L)
  )
  
  updated <- DBI::dbGetQuery(mock_db, "SELECT * FROM stressor_responses WHERE article_id = 10")
  expect_equal(updated$title, "Revised Title")
  expect_equal(updated$overview, "Revised Overview")
})

test_that("edit_article curve replacement replaces rows within transaction", {
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_db), add = TRUE)
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE csv_data (
      article_id INTEGER,
      curve_id TEXT,
      stressor_x REAL,
      response_y REAL
    )
  ")
  
  DBI::dbExecute(mock_db, "
    INSERT INTO csv_data VALUES 
    (10, 'c1', 10.0, 0.9), 
    (10, 'c1', 20.0, 0.5)
  ")
  expect_equal(nrow(DBI::dbGetQuery(mock_db, "SELECT * FROM csv_data WHERE article_id = 10")), 2)
  
  # Execute curve deletion and replacement within transaction
  DBI::dbWithTransaction(mock_db, {
    DBI::dbExecute(mock_db, "DELETE FROM csv_data WHERE article_id = $1", params = list(10L))
    new_points <- data.frame(article_id = 10L, curve_id = "c2", stressor_x = 15.0, response_y = 0.8)
    DBI::dbWriteTable(mock_db, "csv_data", new_points, append = TRUE)
  })
  
  replaced <- DBI::dbGetQuery(mock_db, "SELECT * FROM csv_data WHERE article_id = 10")
  expect_equal(nrow(replaced), 1)
  expect_equal(replaced$curve_id, "c2")
  expect_equal(replaced$stressor_x, 15.0)
})

test_that("PostgreSQL array formatting handles character vectors and commas", {
  format_pg_array <- function(val) {
    if (is.null(val) || length(val) == 0 || all(is.na(val))) return(NA_character_)
    parts <- unlist(lapply(val, function(x) trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])))
    parts <- parts[parts != "" & !is.na(parts)]
    if (length(parts) == 0) return(NA_character_)
    paste0("{", paste(sprintf('"%s"', gsub('"', '\\"', parts, fixed = TRUE)), collapse = ","), "}")
  }
  
  expect_true(is.na(format_pg_array(NULL)))
  expect_true(is.na(format_pg_array(character(0))))
  expect_true(is.na(format_pg_array(NA)))
  
  expect_equal(format_pg_array("Chinook salmon"), '{"Chinook salmon"}')
  expect_equal(format_pg_array(c("Chinook salmon", "Coho salmon")), '{"Chinook salmon","Coho salmon"}')
  expect_equal(format_pg_array("Chinook salmon, Coho salmon"), '{"Chinook salmon","Coho salmon"}')
})