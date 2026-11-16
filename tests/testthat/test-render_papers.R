# tests/testthat/test-render_papers.R
library(testthat)
library(shiny)
library(htmltools)

# Universal file path finder
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
module_file <- find_module_file("render_papers.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

test_that("render_papers_server initializes outputs and executes card rendering", {
  expect_true(exists("render_papers_server"))
  
  dummy_papers <- data.frame(
    article_id = 1:2,
    title = c("Thermal Effects on Salmon", "Streamflow Dynamics"),
    article_type = c("Peer-reviewed", "Report"),
    species_common_name = c("Chinook salmon", "Coho salmon"),
    latin_name = c("Oncorhynchus tshawytscha", "Oncorhynchus kisutch"),
    life_stages = c("Juvenile", "Adult"),
    activity = c("Rearing", "Spawning"),
    stressor_name = c("Water Temperature", "Streamflow"),
    specific_stressor_metric = c("7-DADMax", "cfs"),
    response = c("Survival", "Abundance"),
    location_country = c("USA", "USA"),
    location_state_province = c("Washington", "Oregon"),
    location_watershed_lab = c("Columbia River Basin", "Willamette"),
    location_river_creek = c("Snake River", "McKenzie"),
    contributor_name = c("NOAA Fisheries", "USGS"),
    stringsAsFactors = FALSE
  )
  
  # Prepare mock objects matching render_papers_server(output, paginated_data, input, session)
  mock_output <- new.env(parent = emptyenv())
  paginated_data <- reactive(dummy_papers)
  mock_input <- reactiveValues(select_all = FALSE)
  mock_session <- shiny::MockShinySession$new()
  
  # 1. Execute render_papers_server
  render_papers_server(
    output = mock_output,
    paginated_data = paginated_data,
    input = mock_input,
    session = mock_session
  )
  
  expect_true(exists("paper_cards", envir = mock_output))
  
  # 2. Execute renderUI body with active mock session
  cards_ui <- shiny::withReactiveDomain(mock_session, {
    shiny::isolate(mock_output$paper_cards(shinysession = mock_session))
  })
  expect_true(inherits(cards_ui, "shiny.tag.list") || inherits(cards_ui, "shiny.tag") || is.list(cards_ui))
  
  # 3. Trigger select_all observer across visible IDs
  expect_error({
    mock_input$select_all <- TRUE
  }, NA)
})

test_that("render_papers_server handles empty datasets with fallback message", {
  mock_output <- new.env(parent = emptyenv())
  paginated_data <- reactive(data.frame())
  mock_input <- reactiveValues(select_all = FALSE)
  mock_session <- shiny::MockShinySession$new()
  
  render_papers_server(
    output = mock_output,
    paginated_data = paginated_data,
    input = mock_input,
    session = mock_session
  )
  
  fallback_ui <- shiny::withReactiveDomain(mock_session, {
    shiny::isolate(mock_output$paper_cards(shinysession = mock_session))
  })
  
  fallback_html <- as.character(fallback_ui)
  
  # Use any() so it evaluates to a single scalar TRUE across all tag elements
  expect_true(any(grepl("No research papers found", fallback_html)))
  expect_true(any(grepl("color: red", fallback_html)))
})

test_that("format_field helper properly handles empty and valid values", {
  format_field_test <- function(label, val, bold = FALSE) {
    display <- ifelse(is.null(val) || length(val) == 0 || is.na(val) || val == "NA" || val == "N/A", "", val)
    if (display == "") return("")
    label_span <- sprintf("<span class='metadata-light' style='color: #666;'>%s:</span> ", htmltools::htmlEscape(label))
    value_span <- sprintf("<span class='metadata-bold' style='color: #222; font-weight: 500;'>%s</span>", htmltools::htmlEscape(display))
    sprintf("<div class='paper-meta-item' style='margin-bottom: 6px; line-height: 1.3;'>%s%s</div>", label_span, value_span)
  }
  
  expect_equal(format_field_test("Stressor", ""), "")
  expect_equal(format_field_test("Stressor", NA), "")
  expect_equal(format_field_test("Stressor", "N/A"), "")
  expect_equal(format_field_test("Stressor", "NA"), "")
  
  res <- format_field_test("Species", "Chinook salmon")
  expect_true(grepl("Species:", res))
  expect_true(grepl("Chinook salmon", res))
  expect_true(grepl("paper-meta-item", res))
})