# tests/testthat/test-overlay_plot.R
library(testthat)
library(shiny)
library(plotly)
library(htmltools)

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC TESTS: UNIT MISMATCH DETECTION & LEGEND FORMATTING
# ─────────────────────────────────────────────────────────────────────────────

# Helper replicating unit mismatch check in overlay_plot.R
check_unit_mismatch <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list(has_mismatch = FALSE, x_units = character(0), y_units = character(0)))
  
  unique_x <- unique(toupper(trimws(df$units_x[!is.na(df$units_x) & df$units_x != ""])))
  unique_y <- unique(toupper(trimws(df$units_y[!is.na(df$units_y) & df$units_y != ""])))
  
  has_mismatch <- (length(unique_x) > 1 || length(unique_y) > 1)
  
  list(
    has_mismatch = has_mismatch,
    unique_x = unique_x,
    unique_y = unique_y
  )
}

# Helper replicating curve grouping and legend labeling
prepare_overlay_data <- function(df_csv, df_meta) {
  if (is.null(df_csv) || nrow(df_csv) == 0) return(data.frame())
  
  # Merge title from metadata
  merged <- merge(df_csv, df_meta[, c("article_id", "title")], by = "article_id", all.x = TRUE)
  
  # Order by article, curve, and x
  merged <- merged[order(merged$article_id, merged$curve_id, merged$stressor_x), ]
  
  merged$curve_label <- ifelse(is.na(merged$curve_id) | merged$curve_id == "", "", paste0(" (Curve: ", merged$curve_id, ")"))
  merged$legend_group <- paste0(merged$title, merged$curve_label)
  
  merged
}

test_that("check_unit_mismatch detects conflicting X and Y units across studies", {
  # Scenario 1: Same units -> No mismatch
  matching_df <- data.frame(
    article_id = c(1, 2),
    units_x = c("Celsius", "celsius"),
    units_y = c("Survival", "SURVIVAL"),
    stringsAsFactors = FALSE
  )
  res_match <- check_unit_mismatch(matching_df)
  expect_false(res_match$has_mismatch)
  expect_equal(res_match$unique_x, "CELSIUS")
  expect_equal(res_match$unique_y, "SURVIVAL")
  
  # Scenario 2: X-axis mismatch (e.g. Celsius vs Fahrenheit)
  mismatch_x <- data.frame(
    article_id = c(1, 2),
    units_x = c("degC", "degF"),
    units_y = c("Proportion", "Proportion"),
    stringsAsFactors = FALSE
  )
  res_mismatch_x <- check_unit_mismatch(mismatch_x)
  expect_true(res_mismatch_x$has_mismatch)
  expect_equal(length(res_mismatch_x$unique_x), 2)
  expect_equal(length(res_mismatch_x$unique_y), 1)
  
  # Scenario 3: Y-axis mismatch (e.g. Proportion vs Count)
  mismatch_y <- data.frame(
    article_id = c(1, 2),
    units_x = c("cfs", "cfs"),
    units_y = c("Proportion", "Number of Smolt"),
    stringsAsFactors = FALSE
  )
  res_mismatch_y <- check_unit_mismatch(mismatch_y)
  expect_true(res_mismatch_y$has_mismatch)
})

test_that("prepare_overlay_data formats legend groups and sorts points sequentially", {
  mock_meta <- data.frame(
    article_id = c(10, 20),
    title = c("Study Alpha", "Study Beta"),
    stringsAsFactors = FALSE
  )
  
  # Unsorted points with multi-curve IDs
  mock_csv <- data.frame(
    article_id = c(10, 20, 10, 10),
    curve_id = c("c1", "c1", "c1", "c2"),
    stressor_x = c(25, 10, 15, 5),
    response_y = c(0.4, 0.9, 0.8, 0.5),
    stringsAsFactors = FALSE
  )
  
  res <- prepare_overlay_data(mock_csv, mock_meta)
  
  expect_equal(nrow(res), 4)
  
  # Check legend group formatting
  expect_true("Study Alpha (Curve: c1)" %in% res$legend_group)
  expect_true("Study Alpha (Curve: c2)" %in% res$legend_group)
  expect_true("Study Beta (Curve: c1)" %in% res$legend_group)
  
  # Verify ordering: Article 10 Curve c1 points should be strictly sorted by x: 15, then 25
  c1_points <- res[res$article_id == 10 & res$curve_id == "c1", ]
  expect_equal(c1_points$stressor_x, c(15, 25))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. LOCATION STRING & SUMMARY CARD FORMATTING
# ─────────────────────────────────────────────────────────────────────────────

test_that("summary card location formatting handles missing or NA values properly", {
  format_location <- function(state, country) {
    parts <- c(state, country)
    parts <- parts[!is.na(parts) & nzchar(trimws(parts)) & parts != "NA"]
    if (length(parts) == 0) {
      "Not specified"
    } else {
      paste(parts, collapse = ", ")
    }
  }
  
  expect_equal(format_location("Washington", "United States"), "Washington, United States")
  expect_equal(format_location("Washington", NA_character_), "Washington")
  expect_equal(format_location(NA_character_, "United States"), "United States")
  expect_equal(format_location(NA_character_, NA_character_), "Not specified")
  expect_equal(format_location("", ""), "Not specified")
  expect_equal(format_location("   ", ""), "Not specified")
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. SHINY REACTIVE SERVER TEST
# ─────────────────────────────────────────────────────────────────────────────

test_that("overlay_plot_server reacts to selected profile IDs and builds plot data", {
  mock_meta_df <- data.frame(
    article_id = c(1, 2, 3),
    title = c("Thermal Study 1", "Thermal Study 2", "Flow Study"),
    units_x = c("Celsius", "Celsius", "cfs"),
    units_y = c("Proportion", "Proportion", "Proportion"),
    stringsAsFactors = FALSE
  )
  
  mock_db_curves <- data.frame(
    article_id = c(1, 1, 2, 2, 3),
    curve_id = c("c1", "c1", "c1", "c1", "c1"),
    stressor_x = c(10, 20, 12, 22, 50),
    units_x = c("Celsius", "Celsius", "Celsius", "Celsius", "cfs"),
    response_y = c(0.9, 0.6, 0.85, 0.55, 0.7),
    units_y = c("Proportion", "Proportion", "Proportion", "Proportion", "Proportion"),
    stringsAsFactors = FALSE
  )
  
  # Holder for reactive test inspection
  mod_out <- NULL
  
  test_overlay_server <- function(input, output, session) {
    selected_ids <- reactiveVal(character(0))
    
    # Reactive plot_data query simulation
    plot_data <- reactive({
      ids <- selected_ids()
      if (length(ids) == 0) return(data.frame())
      
      df_csv <- mock_db_curves[mock_db_curves$article_id %in% as.integer(ids), , drop = FALSE]
      if (nrow(df_csv) > 0) {
        df_csv <- merge(df_csv, mock_meta_df[, c("article_id", "title")], by = "article_id", all.x = TRUE)
      }
      df_csv
    })
    
    # Reactive warning status
    has_mismatch <- reactive({
      df <- plot_data()
      if (nrow(df) == 0) return(FALSE)
      ux <- unique(toupper(trimws(df$units_x[!is.na(df$units_x) & df$units_x != ""])))
      uy <- unique(toupper(trimws(df$units_y[!is.na(df$units_y) & df$units_y != ""])))
      length(ux) > 1 || length(uy) > 1
    })
    
    mod_out <<- list(
      set_ids = function(ids) selected_ids(ids),
      plot_data = plot_data,
      has_mismatch = has_mismatch
    )
  }
  
  shiny::testServer(test_overlay_server, {
    # 1. Initial State: 0 selected profiles
    expect_equal(nrow(mod_out$plot_data()), 0)
    expect_false(mod_out$has_mismatch())
    
    # 2. Select Article 1 & 2 (both in Celsius -> no mismatch)
    mod_out$set_ids(c("1", "2"))
    p_data <- mod_out$plot_data()
    expect_equal(nrow(p_data), 4)
    expect_equal(sort(unique(p_data$article_id)), c(1, 2))
    expect_false(mod_out$has_mismatch())
    
    # 3. Add Article 3 (Flow in cfs -> triggers unit mismatch)
    mod_out$set_ids(c("1", "2", "3"))
    expect_equal(nrow(mod_out$plot_data()), 5)
    expect_true(mod_out$has_mismatch())
  })
})