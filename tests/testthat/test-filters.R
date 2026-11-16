# tests/testthat/test-filters.R
library(testthat)
library(shiny)

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

# 1. Source modules/filters.R so covr instruments it
module_file <- find_module_file("filters.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

# Reusable mock dataset covering all 12 filterable attributes
make_test_data <- function() {
  data.frame(
    article_id = 1:3,
    title = c("Thermal Effects on Salmon", "Streamflow and Coho Abundance", "Sediment Impact Study"),
    article_type = c("Peer-reviewed", "Report", "Thesis"),
    broad_stressor_name = c("Water Quality", "Hydrology", "Physical Habitat"),
    stressor_name = c("Water Temperature", "Streamflow", "Turbidity"),
    specific_stressor_metric = c("7-DADMax", "cfs", "NTU"),
    response = c("Survival", "Abundance", "Growth Rate"),
    overview = c("Lab thermal trial", "Field weir counts", "Channel flume trial"),
    transferability_of_function = c("Coldwater streams", "Coastal streams", "Lab only"),
    source_of_stressor_data = c("Thermographs", "USGS gauge", "Turbidity probe"),
    species_common_name = c("Chinook salmon, Coho salmon", "Coho salmon", "Steelhead"),
    latin_name = c("Oncorhynchus tshawytscha, Oncorhynchus kisutch", "Oncorhynchus kisutch", "Oncorhynchus mykiss"),
    life_stages = c("Juvenile, Smolt", "Adult, Spawning", "Parr"),
    activity = c("Rearing", "Migration", "Feeding"),
    season = c("Summer", "Fall", "Spring"),
    location_country = c("USA, Canada", "USA", "USA"),
    location_state_province = c("Washington, Oregon", "Oregon", "Idaho"),
    location_watershed_lab = c("Columbia River Basin", "Willamette River", "Snake River Basin"),
    location_river_creek = c("Snake River", "McKenzie River", "Clearwater River"),
    function_derivation = c("Empirical fit", "Expert opinion", "Mechanistic"),
    stringsAsFactors = FALSE
  )
}

test_that("match_array_col and search_array_col helpers behave correctly", {
  expect_true(exists("match_array_col"))
  expect_true(exists("search_array_col"))
  
  col_sample <- c("Chinook salmon, Coho salmon", "Steelhead", NA, "")
  
  # match_array_col tests
  expect_equal(match_array_col(col_sample, c("Coho salmon")), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(match_array_col(col_sample, c("Steelhead")), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(match_array_col(col_sample, c("Sockeye")), c(FALSE, FALSE, FALSE, FALSE))
  
  # search_array_col tests
  expect_equal(search_array_col(col_sample, "chinook"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(search_array_col(col_sample, "salmon"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(search_array_col(col_sample, "nonexistent"), c(FALSE, FALSE, FALSE, FALSE))
})

test_that("filter_data_server passes through full data when no filters are set", {
  dummy_df <- make_test_data()
  mock_session <- MockShinySession$new()
  mock_input <- reactiveValues(
    species = NULL, life_stage = NULL, activity = NULL, latin_name = NULL,
    location_country = NULL, location_state_province = NULL,
    location_watershed_lab = NULL, location_river_creek = NULL,
    article_type = NULL, broad_stressor_name = NULL, stressor = NULL,
    stressor_metric = NULL, search = NULL
  )
  
  res_reactive <- filter_data_server(mock_input, dummy_df, mock_session)
  filtered <- shiny::isolate(res_reactive())
  expect_equal(nrow(filtered), 3)
})

test_that("filter_data_server filters across all array columns", {
  dummy_df <- make_test_data()
  mock_session <- MockShinySession$new()
  
  # 1. Species filter
  input_species <- reactiveValues(species = c("Chinook salmon"))
  res <- shiny::isolate(filter_data_server(input_species, dummy_df, mock_session)())
  expect_equal(res$article_id, 1)
  
  # 2. Life stage filter
  input_life <- reactiveValues(life_stage = c("Spawning"))
  res <- shiny::isolate(filter_data_server(input_life, dummy_df, mock_session)())
  expect_equal(res$article_id, 2)
  
  # 3. Activity filter
  input_act <- reactiveValues(activity = c("Feeding"))
  res <- shiny::isolate(filter_data_server(input_act, dummy_df, mock_session)())
  expect_equal(res$article_id, 3)
  
  # 4. Latin name filter
  input_latin <- reactiveValues(latin_name = c("Oncorhynchus mykiss"))
  res <- shiny::isolate(filter_data_server(input_latin, dummy_df, mock_session)())
  expect_equal(res$article_id, 3)
  
  # 5. Location Country filter
  input_country <- reactiveValues(location_country = c("Canada"))
  res <- shiny::isolate(filter_data_server(input_country, dummy_df, mock_session)())
  expect_equal(res$article_id, 1)
  
  # 6. Location State/Province filter
  input_state <- reactiveValues(location_state_province = c("Idaho"))
  res <- shiny::isolate(filter_data_server(input_state, dummy_df, mock_session)())
  expect_equal(res$article_id, 3)
  
  # 7. Location Watershed/Lab filter
  input_watershed <- reactiveValues(location_watershed_lab = c("Willamette River"))
  res <- shiny::isolate(filter_data_server(input_watershed, dummy_df, mock_session)())
  expect_equal(res$article_id, 2)
  
  # 8. Location River/Creek filter
  input_river <- reactiveValues(location_river_creek = c("Snake River"))
  res <- shiny::isolate(filter_data_server(input_river, dummy_df, mock_session)())
  expect_equal(res$article_id, 1)
})

test_that("filter_data_server filters across all plain text columns", {
  dummy_df <- make_test_data()
  mock_session <- MockShinySession$new()
  
  # 1. Article type
  input_type <- reactiveValues(article_type = c("Report"))
  res <- shiny::isolate(filter_data_server(input_type, dummy_df, mock_session)())
  expect_equal(res$article_id, 2)
  
  # 2. Broad stressor name
  input_broad <- reactiveValues(broad_stressor_name = c("Physical Habitat"))
  res <- shiny::isolate(filter_data_server(input_broad, dummy_df, mock_session)())
  expect_equal(res$article_id, 3)
  
  # 3. Stressor name
  input_stressor <- reactiveValues(stressor = c("Water Temperature"))
  res <- shiny::isolate(filter_data_server(input_stressor, dummy_df, mock_session)())
  expect_equal(res$article_id, 1)
  
  # 4. Stressor metric
  input_metric <- reactiveValues(stressor_metric = c("cfs"))
  res <- shiny::isolate(filter_data_server(input_metric, dummy_df, mock_session)())
  expect_equal(res$article_id, 2)
})

test_that("filter_data_server keyword search handles plain and array column matches", {
  dummy_df <- make_test_data()
  mock_session <- MockShinySession$new()
  
  # Match title (plain col)
  input_search1 <- reactiveValues(search = "Thermal")
  res1 <- shiny::isolate(filter_data_server(input_search1, dummy_df, mock_session)())
  expect_equal(res1$article_id, 1)
  
  # Match overview (plain col)
  input_search2 <- reactiveValues(search = "weir")
  res2 <- shiny::isolate(filter_data_server(input_search2, dummy_df, mock_session)())
  expect_equal(res2$article_id, 2)
  
  # Match species common name (array col)
  input_search3 <- reactiveValues(search = "Steelhead")
  res3 <- shiny::isolate(filter_data_server(input_search3, dummy_df, mock_session)())
  expect_equal(res3$article_id, 3)
  
  # Search with no match
  input_search_none <- reactiveValues(search = "NonexistentTermXYZ")
  res_none <- shiny::isolate(filter_data_server(input_search_none, dummy_df, mock_session)())
  expect_equal(nrow(res_none), 0)
})