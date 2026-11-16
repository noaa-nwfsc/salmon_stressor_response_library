# tests/testthat/test-render_article_server.R
library(testthat)
library(shiny)
library(plotly)
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
module_file <- find_module_file("render_article_server.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

test_that("render_article_server initializes and sets up all UI, table, and plot bindings", {
  expect_true(exists("render_article_server"))
  
  # Setup in-memory SQLite database with expected csv_data schema
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_db), add = TRUE)
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE csv_data (
      row_index INTEGER,
      article_id INTEGER,
      curve_id TEXT,
      stressor_label TEXT,
      stressor_x REAL,
      units_x TEXT,
      response_label TEXT,
      response_y REAL,
      units_y TEXT,
      stressor_value TEXT,
      lower_limit REAL,
      upper_limit REAL,
      sd REAL,
      plot_type TEXT
    )
  ")
  
  # Insert sample curve rows
  DBI::dbExecute(mock_db, "
    INSERT INTO csv_data VALUES 
    (1, 101, 'c1', 'Water Temp', 12.0, 'degC', 'Survival', 0.95, '%', 'constant', 0.90, 1.00, 0.05, 'curve'),
    (2, 101, 'c1', 'Water Temp', 16.0, 'degC', 'Survival', 0.85, '%', 'constant', 0.80, 0.90, 0.05, 'curve'),
    (3, 101, 'c1', 'Water Temp', 20.0, 'degC', 'Survival', 0.50, '%', 'constant', 0.45, 0.55, 0.05, 'curve')
  ")
  
  dummy_paper <- data.frame(
    article_id = 101L,
    title = "Thermal Effects on Salmon Survival",
    article_type = "Peer-reviewed",
    species_common_name = "Chinook salmon",
    latin_name = "Oncorhynchus tshawytscha",
    stressor_name = "Water Temperature",
    specific_stressor_metric = "7-DADMax",
    response = "Survival",
    life_stages = "Juvenile",
    overview = "Laboratory thermal exposure experiment",
    function_derivation = "Empirical fit",
    location_country = "USA",
    location_state_province = "Washington",
    location_watershed_lab = "Columbia River Basin",
    location_river_creek = "Snake River",
    transferability_of_function = "Applicable across coldwater streams",
    source_of_stressor_data = "Field sensor network",
    srf_formula = "$$y = ax + b$$",
    conf_source = "High",
    conf_shape = "High",
    conf_variance = "Moderate",
    conf_applicability = "High",
    conf_interactions = "Low",
    citations = '[{"text":"Smith et al. (2020)","title":"Smith 2020","url":"[suspicious link removed]"}]',
    stringsAsFactors = FALSE
  )
  dummy_paper$list_col <- list(c("tagA", "tagB"))
  
  mock_output <- new.env(parent = emptyenv())
  mock_input <- reactiveValues()
  mock_session <- shiny::MockShinySession$new()
  
  # Execute render_article_server (this runs through the module code)
  render_article_server(
    input = mock_input,
    output = mock_output,
    session = mock_session,
    paper_id = 101L,
    paper_row = dummy_paper,
    db = mock_db
  )
  
  # Check that output slots were registered
  expect_true(exists("article_title_101", envir = mock_output))
  expect_true(exists("confidence_table_101", envir = mock_output))
})

test_that("render_article_server handles papers with empty curve data cleanly", {
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_db), add = TRUE)
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE csv_data (
      row_index INTEGER,
      article_id INTEGER,
      curve_id TEXT,
      stressor_label TEXT,
      stressor_x REAL,
      units_x TEXT,
      response_label TEXT,
      response_y REAL,
      units_y TEXT,
      stressor_value TEXT,
      lower_limit REAL,
      upper_limit REAL,
      sd REAL,
      plot_type TEXT
    )
  ")
  
  dummy_paper <- data.frame(
    article_id = 202L,
    title = "Literature Review Only",
    citations = "[]",
    stringsAsFactors = FALSE
  )
  
  mock_output <- new.env(parent = emptyenv())
  mock_input <- reactiveValues()
  mock_session <- shiny::MockShinySession$new()
  
  expect_error({
    render_article_server(
      input = mock_input,
      output = mock_output,
      session = mock_session,
      paper_id = 202L,
      paper_row = dummy_paper,
      db = mock_db
    )
  }, NA)
  
  expect_true(exists("article_title_202", envir = mock_output))
})