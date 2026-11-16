# tests/testthat/test-csv_template.R
library(testthat)

# Source the modules under test
source("../../modules/csv_template.R", local = FALSE)
if (file.exists("../../modules/csv_validation.R")) {
  source("../../modules/csv_validation.R", local = FALSE)
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. TEMPLATE STRUCTURE & COLUMNS
# ─────────────────────────────────────────────────────────────────────────────

test_that("get_csv_template returns a valid data frame with all required and optional columns", {
  df <- get_csv_template()
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5)
  
  # Required columns expected by csv_validation.R
  required_cols <- c(
    "curve.id", "stressor.label", "stressor.x", "units.x",
    "response.label", "response.y", "units.y"
  )
  for (col in required_cols) {
    expect_true(col %in% names(df), info = paste("Missing required column:", col))
  }
  
  # Optional columns expected by csv_validation.R
  optional_cols <- c("plot.type", "stressor.value", "lower.limit", "upper.limit", "sd")
  for (col in optional_cols) {
    expect_true(col %in% names(df), info = paste("Missing optional column:", col))
  }
  
  # Exactly 12 columns in total
  expect_equal(ncol(df), 12)
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. DATA TYPES & LOGICAL INTEGRITY
# ─────────────────────────────────────────────────────────────────────────────

test_that("get_csv_template column types and values conform to validation rules", {
  df <- get_csv_template()
  
  # Character columns
  expect_type(df$curve.id, "character")
  expect_type(df$stressor.label, "character")
  expect_type(df$units.x, "character")
  expect_type(df$response.label, "character")
  expect_type(df$units.y, "character")
  expect_type(df$plot.type, "character")
  expect_type(df$stressor.value, "character")
  
  # Numeric columns
  expect_type(df$stressor.x, "double")
  expect_type(df$response.y, "double")
  expect_type(df$lower.limit, "double")
  expect_type(df$upper.limit, "double")
  expect_type(df$sd, "double")
  
  # Single unique value requirements for label/unit columns
  expect_equal(length(unique(df$stressor.label)), 1)
  expect_equal(length(unique(df$response.label)), 1)
  expect_equal(length(unique(df$units.x)), 1)
  expect_equal(length(unique(df$units.y)), 1)
  
  # Numeric bounding logic: lower <= response <= upper
  expect_true(all(df$lower.limit <= df$response.y))
  expect_true(all(df$response.y <= df$upper.limit))
  
  # Allowed plot types: curve or scatter
  expect_true(all(df$plot.type %in% c("curve", "scatter", "bar")))
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. CSV EXPORT & FILE GENERATION
# ─────────────────────────────────────────────────────────────────────────────

test_that("write_csv_template creates a clean CSV file without row names", {
  tmp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_file), add = TRUE)
  
  write_csv_template(tmp_file)
  
  expect_true(file.exists(tmp_file))
  expect_gt(file.info(tmp_file)$size, 0)
  
  # Re-read file to verify formatting
  read_back <- read.csv(tmp_file, stringsAsFactors = FALSE)
  
  expect_equal(nrow(read_back), 5)
  expect_equal(ncol(read_back), 12)
  
  # Ensure write.csv did not include an unnamed row-number column ("X")
  expect_false("X" %in% names(read_back))
  expect_equal(names(read_back), names(get_csv_template()))
  
  # Coordinates preserved accurately
  expect_equal(read_back$stressor.x, c(10, 15, 20, 25, 30))
  expect_equal(read_back$response.y, c(0.95, 0.85, 0.70, 0.50, 0.30))
})

# ─────────────────────────────────────────────────────────────────────────────
# 4. COMPATIBILITY WITH CSV VALIDATION PIPELINE
# ─────────────────────────────────────────────────────────────────────────────

test_that("the generated template passes comprehensive csv_validation without issues", {
  tmp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_file), add = TRUE)
  
  write_csv_template(tmp_file)
  
  mock_input <- list(
    name = "SRF_template.csv",
    datapath = tmp_file,
    type = "text/csv"
  )
  
  val_res <- validate_csv_upload(mock_input)
  
  expect_true(val_res$valid)
  expect_equal(length(val_res$all_issues), 0)
  expect_equal(nrow(val_res$data), 5)
  expect_equal(val_res$message, "CSV validation passed")
})