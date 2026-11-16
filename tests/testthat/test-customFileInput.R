# tests/testthat/test-customFileInput.R
library(testthat)
library(shiny)
library(htmltools)

# Source module under test
if (file.exists("../../modules/customFileInput.R")) {
  source("../../modules/customFileInput.R", local = FALSE)
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. CORE HTML STRUCTURE & ATTRIBUTE TESTS
# ─────────────────────────────────────────────────────────────────────────────

test_that("customFileInput generates valid HTML with correct id and class bindings", {
  ui <- customFileInput("csv_upload", "Upload Stressor Curve CSV")
  ui_str <- as.character(ui)
  
  # Outer container checks
  expect_true(grepl('class="form-group shiny-input-container"', ui_str, fixed = TRUE))
  
  # Label binding checks
  expect_true(grepl('<label', ui_str, fixed = TRUE))
  expect_true(grepl('for="csv_upload"', ui_str, fixed = TRUE))
  expect_true(grepl("Upload Stressor Curve CSV", ui_str, fixed = TRUE))
  
  # Input element checks
  expect_true(grepl('<input', ui_str, fixed = TRUE))
  expect_true(grepl('id="csv_upload"', ui_str, fixed = TRUE))
  expect_true(grepl('name="csv_upload"', ui_str, fixed = TRUE))
  expect_true(grepl('type="file"', ui_str, fixed = TRUE))
  expect_true(grepl('style="position: absolute !important;', ui_str, fixed = TRUE) ||
                grepl('style="display: none;"', ui_str, fixed = TRUE) ||
                grepl('shiny-input-file', ui_str, fixed = TRUE))
})

test_that("customFileInput handles custom button labels and placeholder text", {
  ui_default <- customFileInput("f1", "Choose File")
  ui_default_str <- as.character(ui_default)
  
  # Defaults (Browse... and No file selected)
  expect_true(grepl("Browse...", ui_default_str, fixed = TRUE) ||
                grepl("Upload", ui_default_str, fixed = TRUE))
  expect_true(grepl("No file selected", ui_default_str, fixed = TRUE))
  
  # Custom button and placeholder (avoid unescaped '&' which renders as '&amp;')
  ui_custom <- customFileInput(
    "f2", "Choose File",
    buttonLabel = "Select Document...",
    placeholder = "Select or drop PDF here"
  )
  ui_custom_str <- as.character(ui_custom)
  expect_true(grepl("Select Document...", ui_custom_str, fixed = TRUE))
  expect_true(grepl("Select or drop PDF here", ui_custom_str, fixed = TRUE))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. CONSTRAINTS: ACCEPT TYPES & MULTIPLE FILES
# ─────────────────────────────────────────────────────────────────────────────

test_that("customFileInput binds accept attribute for single and multiple MIME types", {
  # Single extension
  ui_single <- customFileInput("csv_file", "Upload", accept = ".csv")
  expect_true(grepl('accept=".csv"', as.character(ui_single), fixed = TRUE))
  
  # Multiple extensions / MIME types
  ui_multi_accept <- customFileInput("doc_file", "Upload", accept = c(".csv", ".tsv", "text/plain"))
  ui_multi_str <- as.character(ui_multi_accept)
  
  # Should format as comma-separated list
  expect_true(
    grepl('accept=".csv, .tsv, text/plain"', ui_multi_str, fixed = TRUE) ||
      grepl('accept=".csv,.tsv,text/plain"', ui_multi_str, fixed = TRUE)
  )
})

test_that("customFileInput handles multiple attribute correctly", {
  # Single file (multiple = FALSE by default)
  ui_single <- customFileInput("f_single", "Upload", multiple = FALSE)
  expect_false(grepl('multiple="multiple"', as.character(ui_single), fixed = TRUE))
  expect_false(grepl('multiple ', as.character(ui_single), fixed = TRUE))
  
  # Multiple files (multiple = TRUE)
  ui_multi <- customFileInput("f_multi", "Upload", multiple = TRUE)
  expect_true(
    grepl('multiple="multiple"', as.character(ui_multi), fixed = TRUE) ||
      grepl('multiple', as.character(ui_multi), fixed = TRUE)
  )
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. WIDTH & STYLING LOGIC
# ─────────────────────────────────────────────────────────────────────────────

test_that("customFileInput applies container width when specified", {
  # Default width
  ui_nowidth <- customFileInput("w1", "Upload")
  
  # Explicit pixel width (htmltools::css outputs width:450px without space)
  ui_width <- as.character(customFileInput("w2", "Upload", width = "450px"))
  expect_true(grepl("width:\\s*450px", ui_width) || grepl("450px", ui_width))
  
  # Percentage width
  ui_pct <- as.character(customFileInput("w3", "Upload", width = "80%"))
  expect_true(grepl("width:\\s*80%", ui_pct) || grepl("80%", ui_pct))
})