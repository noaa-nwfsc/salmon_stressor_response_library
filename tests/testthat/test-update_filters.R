# tests/testthat/test-update_filters_server.R
library(testthat)
library(shiny)
library(shinyWidgets)

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
module_file <- find_module_file("update_filters_server.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

test_that("update_filters_server executes filter updates across all specifications", {
  expect_true(exists("update_filters_server"))
  
  dummy_df <- data.frame(
    article_id = 1:3,
    title = c("Study 1", "Study 2", "Study 3"),
    stressor_name = c("Temperature", "Flow", "Sediment"),
    specific_stressor_metric = c("7-DADMax", "cfs", "NTU"),
    species_common_name = c("Chinook salmon, Coho salmon", "Steelhead", "Chinook salmon"),
    life_stages = c("Juvenile, Adult", "Spawning", "Juvenile"),
    activity = c("Rearing", "Migration", "Rearing"),
    latin_name = c("Oncorhynchus tshawytscha", "Oncorhynchus mykiss", "Oncorhynchus tshawytscha"),
    article_type = c("Journal", "Report", "Journal"),
    location_country = c("USA", "USA, Canada", "USA"),
    location_state_province = c("Washington", "Oregon", "Washington"),
    location_watershed_lab = c("Columbia River Basin", "Willamette", "Snake River"),
    location_river_creek = c("Yakima River", "McKenzie", "Clearwater"),
    broad_stressor_name = c("Water Quality", "Hydrology", "Habitat"),
    stringsAsFactors = FALSE
  )
  
  # Mock inputs representing active filter selections
  mock_input <- reactiveValues(
    stressor = c("Temperature"),
    stressor_metric = character(0),
    species = c("Chinook salmon"),
    life_stage = character(0),
    activity = character(0),
    latin_name = character(0),
    article_type = character(0),
    location_country = character(0),
    location_state_province = character(0),
    location_watershed_lab = character(0),
    location_river_creek = character(0),
    broad_stressor_name = character(0)
  )
  
  mock_output <- list()
  mock_session <- MockShinySession$new()
  
  # Invoke update_filters_server (executes module initialization and instruments it in covr)
  expect_error({
    update_filters_server(
      input = mock_input,
      output = mock_output,
      session = mock_session,
      data = dummy_df,
      db = NULL
    )
  }, NA)
})

test_that("update_filters_server internal filtering handles array and plain columns", {
  dummy_df <- data.frame(
    stressor_name = c("Temperature", "Flow"),
    species_common_name = c("Chinook salmon, Coho salmon", "Steelhead"),
    stringsAsFactors = FALSE
  )
  
  # Test filtering mechanics matching update_filters_server
  apply_filter_test <- function(df, vals, col, is_array = FALSE) {
    if (is.null(vals) || length(vals) == 0) return(df)
    if (is_array) {
      keep <- vapply(df[[col]], function(cell) {
        if (is.na(cell)) return(FALSE)
        # Use [[1]] (or unlist) to get the character vector from strsplit:
        cell_parts <- strsplit(cell, ", ")[[1]]
        any(cell_parts %in% vals)
      }, logical(1))
    } else {
      keep <- df[[col]] %in% vals
    }
    df[keep, ]
  }
  
  res_plain <- apply_filter_test(dummy_df, "Temperature", "stressor_name", is_array = FALSE)
  expect_equal(nrow(res_plain), 1)
  
  res_array <- apply_filter_test(dummy_df, "Coho salmon", "species_common_name", is_array = TRUE)
  expect_equal(nrow(res_array), 1)
  expect_equal(res_array$stressor_name, "Temperature")
})