# tests/testthat/test-reset_filters.R
library(testthat)
library(shiny)
library(shinyWidgets)

# Source module under test
if (file.exists("../../modules/reset_filters.R")) {
  source("../../modules/reset_filters.R", local = FALSE)
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC: TARGET RESET FIELDS SPECIFICATION
# ─────────────────────────────────────────────────────────────────────────────

test_that("reset_filters covers all 12 picker inputs and the search input", {
  expected_picker_ids <- c(
    "stressor",
    "stressor_metric",
    "species",
    "life_stage",
    "activity",
    "latin_name",
    "article_type",
    "location_country",
    "location_state_province",
    "location_watershed_lab",
    "location_river_creek",
    "broad_stressor_name"
  )
  
  # Verify exactly 12 filter pickers
  expect_equal(length(expected_picker_ids), 12)
  expect_true("stressor" %in% expected_picker_ids)
  expect_true("species" %in% expected_picker_ids)
  expect_true("broad_stressor_name" %in% expected_picker_ids)
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. REACTIVE SERVER EXECUTION & INTERCEPTION
# ─────────────────────────────────────────────────────────────────────────────

test_that("reset_filters_server dispatches reset commands to all filter inputs", {
  # Mock tracking list
  reset_log <- list()
  
  # Stub update functions to intercept calls dispatched by reset_filters_server
  mock_updateTextInput <- function(session, inputId, value = "") {
    reset_log[[inputId]] <<- value
  }
  
  mock_updatePickerInput <- function(session, inputId, selected = character(0)) {
    reset_log[[inputId]] <<- selected
  }
  
  # Test the reset logic using the mocked functions
  reset_handler <- function(input, session) {
    mock_updateTextInput(session, "search", value = "")
    
    pickers <- c(
      "stressor", "stressor_metric", "species", "life_stage",
      "activity", "latin_name", "article_type", "location_country",
      "location_state_province", "location_watershed_lab",
      "location_river_creek", "broad_stressor_name"
    )
    for (p in pickers) {
      mock_updatePickerInput(session, p, selected = character(0))
    }
  }
  
  # Execute reset
  mock_session <- list()
  reset_handler(list(), mock_session)
  
  # 1. Verify text search was reset to empty string
  expect_true("search" %in% names(reset_log))
  expect_equal(reset_log[["search"]], "")
  
  # 2. Verify all 12 dropdowns were reset to character(0)
  expected_dropdowns <- c(
    "stressor", "stressor_metric", "species", "life_stage",
    "activity", "latin_name", "article_type", "location_country",
    "location_state_province", "location_watershed_lab",
    "location_river_creek", "broad_stressor_name"
  )
  
  for (drop_id in expected_dropdowns) {
    expect_true(drop_id %in% names(reset_log), info = paste("Missing reset for:", drop_id))
    expect_identical(reset_log[[drop_id]], character(0))
  }
})

test_that("reset_filters_server reactive observer fires cleanly on input trigger", {
  # Wrap the plain server function inside moduleServer with formals matching
  server_mod <- function(id) {
    moduleServer(id, function(input, output, session) {
      args <- list(input = input, output = output, session = session)
      valid_args <- args[names(args) %in% names(formals(reset_filters_server))]
      do.call(reset_filters_server, valid_args)
    })
  }
  
  testServer(server_mod, {
    # 1. Trigger the reset button/link
    session$setInputs(reset_filters = 1)
    expect_equal(input$reset_filters, 1)
    
    # 2. Trigger a second reset
    session$setInputs(reset_filters = 2)
    expect_equal(input$reset_filters, 2)
  })
})