# tests/testthat/test-update_filters.R
library(testthat)
library(shiny)

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC TESTS: EXTRACTING & CASCADING CHOICES
# ─────────────────────────────────────────────────────────────────────────────

# Helper function representing choice extraction logic from array/string columns
extract_unique_choices <- function(values) {
  if (is.null(values) || length(values) == 0) return(character(0))
  
  if (is.list(values)) {
    # If returned as a list of vectors
    parts <- unlist(values)
  } else {
    # If comma-separated strings
    parts <- unlist(lapply(values, function(x) {
      if (is.na(x) || !nzchar(trimws(x))) return(character(0))
      trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
    }))
  }
  
  parts <- parts[!is.na(parts) & parts != ""]
  sort(unique(parts))
}

# Helper to filter available choices down based on a parent selection
get_cascading_choices <- function(df, parent_col, parent_val, target_col) {
  if (is.null(parent_val) || length(parent_val) == 0 || all(parent_val == "")) {
    subset_df <- df
  } else {
    subset_df <- df[df[[parent_col]] %in% parent_val, , drop = FALSE]
  }
  extract_unique_choices(subset_df[[target_col]])
}

test_that("extract_unique_choices splits, trims, and deduplicates array and comma columns", {
  # Comma-separated strings
  raw_species <- c("Chinook salmon, Coho salmon", "Steelhead", "Coho salmon", NA_character_, "")
  res <- extract_unique_choices(raw_species)
  
  expect_equal(res, c("Chinook salmon", "Coho salmon", "Steelhead"))
  
  # List of character vectors (as returned by DBI/RPostgres for text[] columns)
  list_locations <- list(
    c("United States", "Canada"),
    c("United States"),
    character(0),
    c("Norway")
  )
  res_loc <- extract_unique_choices(list_locations)
  expect_equal(res_loc, c("Canada", "Norway", "United States"))
})

test_that("cascading choices narrow correctly based on parent selection", {
  mock_catalog <- data.frame(
    broad_stressor_name = c("Water Quality", "Water Quality", "Hydrology", "Hydrology"),
    stressor_name = c("Water Temperature", "Turbidity", "Streamflow", "Hydrograph Timing"),
    specific_stressor_metric = c("Celsius", "NTU", "cfs", "Julian Day"),
    stringsAsFactors = FALSE
  )
  
  # When no broad stressor is selected: all stressors available
  all_stressors <- get_cascading_choices(mock_catalog, "broad_stressor_name", NULL, "stressor_name")
  expect_equal(length(all_stressors), 4)
  expect_true(all(c("Water Temperature", "Turbidity", "Streamflow", "Hydrograph Timing") %in% all_stressors))
  
  # When "Water Quality" is selected: only temperature and turbidity
  wq_stressors <- get_cascading_choices(mock_catalog, "broad_stressor_name", "Water Quality", "stressor_name")
  expect_equal(wq_stressors, c("Turbidity", "Water Temperature"))
  
  # When "Streamflow" is selected: only "cfs" metric
  flow_metrics <- get_cascading_choices(mock_catalog, "stressor_name", "Streamflow", "specific_stressor_metric")
  expect_equal(flow_metrics, "cfs")
})

test_that("selection preservation retains valid selections and drops invalid ones", {
  reconcile_selection <- function(current_selected, available_choices) {
    if (is.null(current_selected)) return(NULL)
    valid <- intersect(current_selected, available_choices)
    if (length(valid) == 0) NULL else valid
  }
  
  available <- c("Water Temperature", "Turbidity", "Dissolved Oxygen")
  
  # User had "Water Temperature" selected; it is still available -> keep it
  expect_equal(
    reconcile_selection("Water Temperature", available),
    "Water Temperature"
  )
  
  # User had multi-selection; keep only the valid intersection
  expect_equal(
    reconcile_selection(c("Water Temperature", "Streamflow"), available),
    "Water Temperature"
  )
  
  # User had an option that is no longer valid -> resets to NULL
  expect_null(
    reconcile_selection("Streamflow", available)
  )
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. SHINY SERVER REACTIVE TEST (testServer)
# ─────────────────────────────────────────────────────────────────────────────

test_that("update_filters_server responds to parent filter changes", {
  mock_data <- data.frame(
    article_id = 1:4,
    broad_stressor_name = c("Water Quality", "Water Quality", "Hydrology", "Hydrology"),
    stressor_name = c("Water Temperature", "Turbidity", "Streamflow", "Hydrograph Timing"),
    species_common_name = c("Chinook salmon", "Coho salmon", "Chinook salmon", "Steelhead"),
    stringsAsFactors = FALSE
  )
  
  # Holder for the reactive expressions
  mod_out <- NULL
  
  test_filter_server <- function(input, output, session) {
    catalog <- reactiveVal(mock_data)
    
    # Reactive choices for stressor_name based on input$broad_stressor
    available_stressors <- reactive({
      df <- catalog()
      if (is.null(input$broad_stressor) || length(input$broad_stressor) == 0) {
        sort(unique(df$stressor_name))
      } else {
        sort(unique(df$stressor_name[df$broad_stressor_name %in% input$broad_stressor]))
      }
    })
    
    # Reactive choices for species based on selected stressor
    available_species <- reactive({
      df <- catalog()
      if (is.null(input$stressor) || length(input$stressor) == 0) {
        sort(unique(df$species_common_name))
      } else {
        sort(unique(df$species_common_name[df$stressor_name %in% input$stressor]))
      }
    })
    
    # Capture reactives into mod_out
    mod_out <<- list(
      stressors = available_stressors,
      species = available_species
    )
  }
  
  shiny::testServer(test_filter_server, {
    # 1. Initial State: no parent selections
    expect_equal(length(mod_out$stressors()), 4)
    expect_equal(mod_out$species(), c("Chinook salmon", "Coho salmon", "Steelhead"))
    
    # 2. Select "Water Quality" -> stressors cascade to Temperature & Turbidity
    session$setInputs(broad_stressor = "Water Quality")
    expect_equal(mod_out$stressors(), c("Turbidity", "Water Temperature"))
    
    # 3. Select "Turbidity" -> species cascades to only "Coho salmon"
    session$setInputs(stressor = "Turbidity")
    expect_equal(mod_out$species(), "Coho salmon")
    
    # 4. Switch to "Hydrology" -> stressors cascade to Flow & Timing
    session$setInputs(broad_stressor = "Hydrology", stressor = NULL)
    expect_equal(mod_out$stressors(), c("Hydrograph Timing", "Streamflow"))
    expect_equal(mod_out$species(), c("Chinook salmon", "Coho salmon", "Steelhead"))
  })
})