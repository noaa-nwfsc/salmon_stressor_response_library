# tests/testthat/test-csv_validation.R
library(testthat)

# Source the validation modules
if (file.exists("modules/csv_validation.R")) {
  source("modules/csv_validation.R")
  source("modules/file_validation.R")
} else {
  source("../../modules/csv_validation.R")
  source("../../modules/file_validation.R")
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. SECURITY & FORMULA INJECTION TESTS
# ─────────────────────────────────────────────────────────────────────────────

test_that("sanitize_csv_cells neutralizes Excel/Sheets formula prefixes", {
  # Test formula injection strings individually or across rows
  dangerous_df <- data.frame(
    curve_id = c("c1", "c2", "c3", "c4"),
    stressor_label = c("=cmd|' /C calc'!A0", "+SUM(A1:A10)", "-SUM(A1:A10)", "@SUM(B1:B5)"),
    safe_col = c("Safe Label", "Another Safe", "Normal", "123"),
    numeric_col = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  
  sanitized <- sanitize_csv_cells(dangerous_df)
  
  # Dangerous formula prefixes must be escaped with a leading single quote
  expect_equal(sanitized$stressor_label[1], "'=cmd|' /C calc'!A0")
  expect_equal(sanitized$stressor_label[2], "'+SUM(A1:A10)")
  expect_equal(sanitized$stressor_label[3], "'-SUM(A1:A10)")
  expect_equal(sanitized$stressor_label[4], "'@SUM(B1:B5)")
  
  # Safe text and numeric columns should remain untouched
  expect_equal(sanitized$safe_col[1], "Safe Label")
  expect_equal(sanitized$safe_col[2], "Another Safe")
  expect_equal(sanitized$numeric_col, c(1, 2, 3, 4))
})

test_that("check_suspicious_patterns catches SQL injection and code patterns", {
  # Clean data
  clean_df <- data.frame(
    title = c("Temperature effects on Chinook", "Flow survival"),
    notes = c("Field study in Sacramento River", "Lab experiment"),
    stringsAsFactors = FALSE
  )
  res_clean <- check_suspicious_patterns(clean_df)
  expect_true(res_clean$valid)
  expect_equal(length(res_clean$warnings), 0)
  
  # SQL Injection attempts
  sql_df <- data.frame(
    title = "Chinook'; DROP TABLE stressor_responses; --",
    notes = "UNION SELECT * FROM users",
    stringsAsFactors = FALSE
  )
  res_sql <- check_suspicious_patterns(sql_df)
  expect_false(res_sql$valid)
  expect_gte(length(res_sql$warnings), 2)
  
  # XSS and Command injection attempts
  xss_df <- data.frame(
    title = "<script>alert('pwned')</script>",
    notes = "system('rm -rf /')",
    stringsAsFactors = FALSE
  )
  res_xss <- check_suspicious_patterns(xss_df)
  expect_false(res_xss$valid)
  expect_gte(length(res_xss$warnings), 2)
})

test_that("detect_binary_content flags executables and non-text files", {
  tmp_dir <- tempdir()
  
  # 1. Valid plain text file
  valid_txt <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  writeLines("curve.id,stressor.x\nc1,10", valid_txt)
  expect_true(detect_binary_content(valid_txt)$valid)
  
  # 2. Null byte detection (0x00)
  null_bin <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  writeBin(as.raw(c(0x61, 0x62, 0x00, 0x63)), null_bin)
  res_null <- detect_binary_content(null_bin)
  expect_false(res_null$valid)
  expect_match(res_null$message, "binary content", ignore.case = TRUE)
  
  # 3. Windows PE header (MZ = 0x4D, 0x5A) - non-null tail so it hits executable check
  mz_bin <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  writeBin(as.raw(c(0x4D, 0x5A, 0x90, 0x20)), mz_bin)
  res_mz <- detect_binary_content(mz_bin)
  expect_false(res_mz$valid)
  # Passes if either "binary content" or "executable" is flagged
  expect_true(grepl("binary content|executable", res_mz$message, ignore.case = TRUE))
  
  # 4. ZIP header (PK = 0x50, 0x4B)
  zip_bin <- tempfile(tmpdir = tmp_dir, fileext = ".csv")
  writeBin(as.raw(c(0x50, 0x4B, 0x03, 0x04)), zip_bin)
  res_zip <- detect_binary_content(zip_bin)
  expect_false(res_zip$valid)
  expect_true(grepl("binary content|zip archive", res_zip$message, ignore.case = TRUE))
  
  # Cleanup
  unlink(c(valid_txt, null_bin, mz_bin, zip_bin))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. FILE ATTRIBUTES & SHINY INPUT VALIDATION
# ─────────────────────────────────────────────────────────────────────────────

test_that("validate_file_extension and validate_mime_type check extensions and types", {
  # Valid CSV
  valid_file <- list(name = "test_data.csv", type = "text/csv")
  expect_true(validate_file_extension(valid_file)$valid)
  expect_true(validate_mime_type(valid_file)$valid)
  
  # Valid alternative CSV MIME types
  expect_true(validate_mime_type(list(type = "application/vnd.ms-excel"))$valid)
  expect_true(validate_mime_type(list(type = "text/plain"))$valid)
  
  # Invalid extension
  bad_ext <- list(name = "test_data.xlsx", type = "text/csv")
  res_ext <- validate_file_extension(bad_ext)
  expect_false(res_ext$valid)
  expect_match(res_ext$message, "Invalid file extension", ignore.case = TRUE)
  
  # Invalid MIME type
  bad_mime <- list(name = "test_data.csv", type = "application/x-sh")
  res_mime <- validate_mime_type(bad_mime)
  expect_false(res_mime$valid)
  expect_match(res_mime$message, "Invalid file type", ignore.case = TRUE)
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. CSV COLUMN STRUCTURE TESTS
# ─────────────────────────────────────────────────────────────────────────────

test_that("validate_csv_columns accepts valid columns (case-insensitive)", {
  valid_df <- data.frame(
    `Curve.ID` = "c1",
    `stressor.label` = "Temperature",
    `Stressor.X` = 15,
    `units.x` = "degC",
    `response.label` = "Survival",
    `Response.Y` = 0.85,
    `units.y` = "proportion",
    `Plot.Type` = "curve"
  )
  
  res <- validate_csv_columns(valid_df)
  expect_true(res$valid)
  expect_equal(length(res$issues), 0)
  expect_equal(res$col_map$curve_id, "curve.id")
  expect_equal(res$col_map$plot_type, "plot.type")
})

test_that("validate_csv_columns rejects missing required columns", {
  incomplete_df <- data.frame(
    curve.id = "c1",
    stressor.x = 15,
    response.y = 0.85
  )
  
  res <- validate_csv_columns(incomplete_df)
  expect_false(res$valid)
  expect_true(any(grepl("Missing required column: 'stressor.label'", res$issues)))
  expect_true(any(grepl("Missing required column: 'units.x'", res$issues)))
  expect_true(any(grepl("Missing required column: 'response.label'", res$issues)))
  expect_true(any(grepl("Missing required column: 'units.y'", res$issues)))
})

# ─────────────────────────────────────────────────────────────────────────────
# 4. DATA CONTENT & FIELD VALIDATION
# ─────────────────────────────────────────────────────────────────────────────

test_that("validate_numeric_column handles numeric conversions and NA options", {
  # Strict numeric (no NAs allowed)
  expect_true(validate_numeric_column(c(1, 2.5, 3), "test_col", allow_na = FALSE)$valid)
  
  # Strict numeric with NA should fail
  res_na <- validate_numeric_column(c(1, NA, 3), "test_col", allow_na = FALSE)
  expect_false(res_na$valid)
  expect_match(res_na$issues[[1]], "empty or missing values")
  
  # allow_na = TRUE with NA should pass
  expect_true(validate_numeric_column(c(1, NA, 3), "test_col", allow_na = TRUE)$valid)
  
  # Non-numeric text strings should fail
  res_char <- validate_numeric_column(c(1, "bad_val", 3), "test_col", allow_na = TRUE)
  expect_false(res_char$valid)
  expect_match(res_char$issues[[1]], "contains non-numeric values")
})

test_that("validate_single_unique_value ensures uniform labels across the file", {
  # Exactly 1 unique value
  expect_true(validate_single_unique_value(c("degC", "degC", "degC"), "units.x")$valid)
  
  # Multiple distinct values should fail
  res_multi <- validate_single_unique_value(c("degC", "Fahrenheit"), "units.x")
  expect_false(res_multi$valid)
  expect_match(res_multi$issues[[1]], "multiple unique values")
  
  # All NA / empty values should fail
  res_empty <- validate_single_unique_value(c("", NA, "NA"), "units.x")
  expect_false(res_empty$valid)
  expect_match(res_empty$issues[[1]], "no non-NA values")
})

test_that("validate_curve_data_points enforces MIN_VALID_POINTS_PER_CURVE", {
  col_map <- list(curve_id = "curve.id", stressor_x = "stressor.x", response_y = "response.y")
  
  # 1 curve with 2 valid points
  valid_curve_df <- data.frame(
    curve.id = c("c1", "c1"),
    stressor.x = c(10, 15),
    response.y = c(0.9, 0.8)
  )
  expect_true(validate_curve_data_points(valid_curve_df, col_map)$valid)
  
  # Curve with missing response.y resulting in 0 valid pairs
  invalid_curve_df <- data.frame(
    curve.id = c("c1", "c2"),
    stressor.x = c(10, 15),
    response.y = c(NA, 0.8)
  )
  res_invalid <- validate_curve_data_points(invalid_curve_df, col_map)
  expect_false(res_invalid$valid)
  expect_match(res_invalid$issues[[1]], "Curve 'c1' has only 0 valid data point")
})

test_that("validate_csv_data checks plot.type and limit bounds", {
  col_map <- list(
    curve_id = "curve.id", stressor_label = "stressor.label",
    stressor_x = "stressor.x", units_x = "units.x",
    response_label = "response.label", response_y = "response.y",
    units_y = "units.y", plot_type = "plot.type",
    lower_limit = "lower.limit", upper_limit = "upper.limit",
    sd = "sd"
  )
  
  # Valid data with allowed plot.types ('scatter', 'curve', 'bar', or NA)
  base_df <- data.frame(
    curve.id = rep("c1", 3),
    stressor.label = rep("Temp", 3),
    stressor.x = c(10, 15, 20),
    units.x = rep("C", 3),
    response.label = rep("Surv", 3),
    response.y = c(0.9, 0.8, 0.7),
    units.y = rep("%", 3),
    plot.type = c("curve", "scatter", "bar"),
    lower.limit = c(0.85, 0.75, 0.65),
    upper.limit = c(0.95, 0.85, 0.75),
    sd = c(0.05, 0.05, 0.05),
    stringsAsFactors = FALSE
  )
  
  expect_true(validate_csv_data(base_df, col_map)$valid)
  
  # Invalid plot.type
  bad_plot_df <- base_df
  bad_plot_df$plot.type[1] <- "pie_chart"
  res_plot <- validate_csv_data(bad_plot_df, col_map)
  expect_false(res_plot$valid)
  expect_true(any(grepl("contains invalid values: pie_chart", res_plot$issues)))
  
  # Lower limit exceeds upper limit
  bad_limits_df <- base_df
  bad_limits_df$lower.limit[1] <- 0.99
  bad_limits_df$upper.limit[1] <- 0.50
  res_limits <- validate_csv_data(bad_limits_df, col_map)
  expect_false(res_limits$valid)
  expect_true(any(grepl("Lower limit exceeds upper limit", res_limits$issues)))
})

# ─────────────────────────────────────────────────────────────────────────────
# 5. DATA TRANSFORMATION: ADDING OPTIONAL COLUMNS
# ─────────────────────────────────────────────────────────────────────────────

test_that("add_missing_optional_columns appends missing columns as NA and orders them", {
  raw_df <- data.frame(
    curve.id = "c1",
    stressor.label = "Temp",
    stressor.x = 10,
    units.x = "C",
    response.label = "Surv",
    response.y = 0.9,
    units.y = "%"
  )
  
  col_map <- list(
    curve_id = "curve.id", stressor_label = "stressor.label",
    stressor_x = "stressor.x", units_x = "units.x",
    response_label = "response.label", response_y = "response.y",
    units_y = "units.y", plot_type = NA_character_,
    stressor_value = NA_character_, lower_limit = NA_character_,
    upper_limit = NA_character_, sd = NA_character_
  )
  
  completed <- add_missing_optional_columns(raw_df, col_map)
  
  expected_cols <- c(
    "curve.id", "stressor.label", "stressor.x", "units.x",
    "response.label", "response.y", "units.y",
    "plot.type", "stressor.value", "lower.limit", "upper.limit", "sd"
  )
  
  expect_equal(names(completed), expected_cols)
  expect_true(is.na(completed$plot.type[1]))
  expect_true(is.na(completed$lower.limit[1]))
  expect_true(is.na(completed$upper.limit[1]))
  expect_true(is.na(completed$sd[1]))
})

# ─────────────────────────────────────────────────────────────────────────────
# 6. PDF VALIDATION TESTS (file_validation.R)
# ─────────────────────────────────────────────────────────────────────────────

test_that("validate_pdf_upload checks extension, MIME type, and size", {
  tmp_dir <- tempdir()
  
  # Null input (optional PDF upload)
  expect_true(validate_pdf_upload(NULL)$valid)
  
  # Non-PDF extension
  bad_ext <- list(name = "document.docx", datapath = tempfile(), type = "application/pdf")
  res_ext <- validate_pdf_upload(bad_ext)
  expect_false(res_ext$valid)
  expect_true(any(grepl("File extension must be .pdf", res_ext$issues)))
  
  # Valid small PDF
  good_pdf_path <- tempfile(tmpdir = tmp_dir, fileext = ".pdf")
  writeBin(as.raw(c(0x25, 0x50, 0x44, 0x46)), good_pdf_path) # %PDF header
  good_pdf <- list(name = "study.pdf", datapath = good_pdf_path, type = "application/pdf")
  expect_true(validate_pdf_upload(good_pdf)$valid)
  
  unlink(good_pdf_path)
})