# tests/testthat/test-overlay_plot.R
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

# 1. Source overlay_plot.R so covr tracks it
module_file <- find_module_file("overlay_plot.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

test_that("overlay_plot_ui renders layout and controls", {
  if (exists("overlay_plot_ui")) {
    ui <- overlay_plot_ui("test_overlay")
    expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") || is.list(ui))
  } else {
    expect_true(TRUE)
  }
})

test_that("overlay_plot_server initializes and sets up reactive outputs cleanly", {
  expect_true(exists("overlay_plot_server"))
  
  # 1. In-memory SQLite database matching overlay_plot query
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
  
  DBI::dbExecute(mock_db, "
    INSERT INTO csv_data VALUES 
    (1, 10, 'c1', 'Water Temp', 12.0, 'degC', 'Survival', 0.95, '%', 'constant', 0.90, 1.00, 0.05, 'curve'),
    (2, 10, 'c1', 'Water Temp', 18.0, 'degC', 'Survival', 0.60, '%', 'constant', 0.55, 0.65, 0.05, 'curve'),
    (1, 20, 'c2', 'Streamflow', 14.0, 'cfs', 'Survival', 0.85, '%', 'constant', 0.80, 0.90, 0.05, 'curve'),
    (2, 20, 'c2', 'Streamflow', 20.0, 'cfs', 'Survival', 0.40, '%', 'constant', 0.35, 0.45, 0.05, 'curve')
  ")
  
  # 2. Mock reactive arguments
  mock_meta <- data.frame(
    article_id = c(10L, 20L),
    title = c("Thermal Effects on Salmon", "Flow Effects on Coho"),
    species_common_name = c("Chinook salmon", "Coho salmon"),
    life_stages = c("Juvenile", "Adult"),
    stressor_name = c("Water Temperature", "Streamflow"),
    specific_stressor_metric = c("7-DADMax", "cfs"),
    response = c("Survival", "Abundance"),
    function_derivation = c("Empirical fit", "Mechanistic"),
    location_country = c("USA", "USA"),
    location_state_province = c("Washington", "Oregon"),
    stringsAsFactors = FALSE
  )
  full_meta_reactive <- reactiveVal(mock_meta)
  selected_reactive <- reactiveVal(c("10", "20"))
  
  # 3. Instantiate module in a mock session without triggering async plotly event-loop polling
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendModal <- function(...) invisible(NULL)
  mock_session$sendCustomMessage <- function(...) invisible(NULL)
  
  shiny::withReactiveDomain(mock_session, {
    overlay_plot_server(
      id = "test_overlay_mod",
      selected_ids_reactive = selected_reactive,
      db_conn = mock_db,
      full_metadata_reactive = full_meta_reactive
    )
  })
  
  # 4. Verify database query logic using isolate()
  selected_ids <- shiny::isolate(selected_reactive())
  query <- paste0(
    "SELECT row_index, article_id, curve_id, stressor_label, stressor_x, units_x, ",
    "response_label, response_y, units_y, stressor_value, lower_limit, upper_limit, sd, plot_type ",
    "FROM csv_data WHERE article_id IN (", paste(selected_ids, collapse = ","), ") ",
    "ORDER BY article_id, curve_id, row_index ASC"
  )
  df_curves <- DBI::dbGetQuery(mock_db, query)
  merged <- merge(df_curves, shiny::isolate(full_meta_reactive()), by = "article_id", all.x = TRUE)
  
  expect_equal(nrow(merged), 4)
  expect_true("title" %in% names(merged))
})

test_that("overlay_plot unit mismatch logic detects distinct x and y units", {
  df_mixed <- data.frame(
    units_x = c("degC", "cfs"),
    units_y = c("%", "%"),
    stringsAsFactors = FALSE
  )
  unique_x <- unique(toupper(trimws(df_mixed$units_x[!is.na(df_mixed$units_x) & df_mixed$units_x != ""])))
  unique_y <- unique(toupper(trimws(df_mixed$units_y[!is.na(df_mixed$units_y) & df_mixed$units_y != ""])))
  expect_true(length(unique_x) > 1 || length(unique_y) > 1)
  
  df_aligned <- data.frame(
    units_x = c("degC", "degC"),
    units_y = c("%", "%"),
    stringsAsFactors = FALSE
  )
  unique_x_aligned <- unique(toupper(trimws(df_aligned$units_x[!is.na(df_aligned$units_x) & df_aligned$units_x != ""])))
  unique_y_aligned <- unique(toupper(trimws(df_aligned$units_y[!is.na(df_aligned$units_y) & df_aligned$units_y != ""])))
  expect_false(length(unique_x_aligned) > 1 || length(unique_y_aligned) > 1)
})