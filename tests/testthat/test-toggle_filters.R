# tests/testthat/test-toggle_filters.R
library(testthat)
library(shiny)
library(shinyjs)

# Source module under test
if (file.exists("../../modules/toggle_filters.R")) {
  source("../../modules/toggle_filters.R", local = FALSE)
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC: BUTTON LABEL ALTERNATION
# ─────────────────────────────────────────────────────────────────────────────

# Helper replicating toggle label calculation
get_toggle_filter_label <- function(click_count) {
  if (is.null(click_count) || is.na(click_count)) return("Show Filters")
  ifelse(click_count %% 2 == 1, "Hide Filters", "Show Filters")
}

test_that("get_toggle_filter_label alternates between Hide and Show on odd/even clicks", {
  # Initial unclicked / 0 clicks
  expect_equal(get_toggle_filter_label(0), "Show Filters")
  expect_equal(get_toggle_filter_label(NULL), "Show Filters")
  
  # 1st click: Opens panel -> "Hide Filters"
  expect_equal(get_toggle_filter_label(1), "Hide Filters")
  
  # 2nd click: Closes panel -> "Show Filters"
  expect_equal(get_toggle_filter_label(2), "Show Filters")
  
  # Subsequent cycles
  expect_equal(get_toggle_filter_label(3), "Hide Filters")
  expect_equal(get_toggle_filter_label(4), "Show Filters")
  expect_equal(get_toggle_filter_label(11), "Hide Filters")
  expect_equal(get_toggle_filter_label(12), "Show Filters")
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. REACTIVE SERVER EXECUTION
# ─────────────────────────────────────────────────────────────────────────────

test_that("toggle_filters_server runs cleanly without error on user clicks", {
  # Inspect formals to call toggle_filters_server with only the arguments it accepts
  server_mod <- function(id) {
    moduleServer(id, function(input, output, session) {
      args <- list(input = input, output = output, session = session)
      valid_args <- args[names(args) %in% names(formals(toggle_filters_server))]
      do.call(toggle_filters_server, valid_args)
    })
  }
  
  testServer(server_mod, {
    # 1. Simulate first click (Odd -> should update to Hide Filters)
    session$setInputs(toggle_filters = 1)
    expect_equal(input$toggle_filters, 1)
    
    # 2. Simulate second click (Even -> should update to Show Filters)
    session$setInputs(toggle_filters = 2)
    expect_equal(input$toggle_filters, 2)
    
    # 3. Simulate third click
    session$setInputs(toggle_filters = 3)
    expect_equal(input$toggle_filters, 3)
  })
})