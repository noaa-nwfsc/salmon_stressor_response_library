# tests/testthat/test-file_validation.R
library(testthat)

# Sourced directly or defined to test modules/file_validation.R logic
MAX_PDF_SIZE_BYTES <- 5 * 1024 * 1024 # 5 MB

validate_pdf_upload <- function(file_input) {
  if (is.null(file_input)) {
    return(list(valid = TRUE, message = "", issues = list()))
  }
  
  fname <- file_input$name
  fpath <- file_input$datapath
  ftype <- file_input$type
  
  issues <- list()
  
  # Extension check
  ext <- tolower(tools::file_ext(fname))
  if (ext != "pdf") {
    issues <- c(issues, "File extension must be .pdf")
  }
  
  # MIME type check (best-effort)
  if (!is.null(ftype) && ftype != "application/pdf") {
    issues <- c(issues, sprintf("Unexpected MIME type: %s", ftype))
  }
  
  # Size check
  if (!file.exists(fpath)) {
    issues <- c(issues, "Uploaded file not found on disk")
  } else {
    sz <- file.info(fpath)$size
    if (!is.na(sz) && sz > MAX_PDF_SIZE_BYTES) {
      MAX_PDF_SIZE_MB <- MAX_PDF_SIZE_BYTES / 1e6
      issues <- c(issues, sprintf("PDF exceeds maximum allowed size (%.0f MB)", MAX_PDF_SIZE_MB))
    }
  }
  
  list(valid = length(issues) == 0, message = paste(issues, collapse = "; "), issues = issues)
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. OPTIONAL UPLOAD & NULL INPUTS
# ─────────────────────────────────────────────────────────────────────────────

test_that("validate_pdf_upload permits NULL inputs for optional PDF uploads", {
  res <- validate_pdf_upload(NULL)
  
  expect_true(res$valid)
  expect_equal(res$message, "")
  expect_equal(length(res$issues), 0)
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. VALID PDF UPLOAD & CASE INSENSITIVITY
# ─────────────────────────────────────────────────────────────────────────────

test_that("validate_pdf_upload accepts valid small PDF files", {
  tmp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_pdf), add = TRUE)
  
  # Write a minimal dummy file
  writeBin(charToRaw("%PDF-1.4 mock pdf content"), tmp_pdf)
  
  mock_input <- list(
    name = "research_paper.pdf",
    datapath = tmp_pdf,
    type = "application/pdf"
  )
  
  res <- validate_pdf_upload(mock_input)
  expect_true(res$valid)
  expect_equal(res$message, "")
  expect_equal(length(res$issues), 0)
})

test_that("validate_pdf_upload handles uppercase extension .PDF cleanly", {
  tmp_pdf <- tempfile(fileext = ".PDF")
  on.exit(unlink(tmp_pdf), add = TRUE)
  writeBin(charToRaw("%PDF-1.4 uppercase test"), tmp_pdf)
  
  mock_input <- list(
    name = "STUDY_REPORT.PDF",
    datapath = tmp_pdf,
    type = "application/pdf"
  )
  
  res <- validate_pdf_upload(mock_input)
  expect_true(res$valid)
  expect_equal(length(res$issues), 0)
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. EXTENSION & MIME TYPE CHECKS
# ─────────────────────────────────────────────────────────────────────────────

test_that("validate_pdf_upload rejects non-PDF file extensions", {
  tmp_file <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp_file), add = TRUE)
  writeBin(charToRaw("dummy content"), tmp_file)
  
  mock_input <- list(
    name = "report.docx",
    datapath = tmp_file,
    type = "application/pdf"
  )
  
  res <- validate_pdf_upload(mock_input)
  expect_false(res$valid)
  expect_true(any(grepl("File extension must be .pdf", res$issues, fixed = TRUE)))
})

test_that("validate_pdf_upload flags unexpected MIME types", {
  tmp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_pdf), add = TRUE)
  writeBin(charToRaw("%PDF-1.4 dummy"), tmp_pdf)
  
  mock_input <- list(
    name = "report.pdf",
    datapath = tmp_pdf,
    type = "application/octet-stream"
  )
  
  res <- validate_pdf_upload(mock_input)
  expect_false(res$valid)
  expect_true(any(grepl("Unexpected MIME type: application/octet-stream", res$issues, fixed = TRUE)))
})

# ─────────────────────────────────────────────────────────────────────────────
# 4. DISK PRESENCE & FILE SIZE LIMITS
# ─────────────────────────────────────────────────────────────────────────────

test_that("validate_pdf_upload catches missing files not found on disk", {
  mock_input <- list(
    name = "missing_paper.pdf",
    datapath = "/nonexistent/path/to/missing_paper.pdf",
    type = "application/pdf"
  )
  
  res <- validate_pdf_upload(mock_input)
  expect_false(res$valid)
  expect_true(any(grepl("Uploaded file not found on disk", res$issues, fixed = TRUE)))
})

test_that("validate_pdf_upload rejects files exceeding MAX_PDF_SIZE_BYTES (5 MB)", {
  tmp_large <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_large), add = TRUE)
  
  # Create a file slightly over 5 MB (5.2 MB)
  target_size <- (5 * 1024 * 1024) + (200 * 1024)
  con <- file(tmp_large, "wb")
  # Write in 1 MB chunks
  chunk <- raw(1024 * 1024)
  for (i in 1:5) {
    writeBin(chunk, con)
  }
  writeBin(raw(200 * 1024), con)
  close(con)
  
  expect_gt(file.info(tmp_large)$size, MAX_PDF_SIZE_BYTES)
  
  mock_input <- list(
    name = "massive_supplement.pdf",
    datapath = tmp_large,
    type = "application/pdf"
  )
  
  res <- validate_pdf_upload(mock_input)
  expect_false(res$valid)
  expect_true(any(grepl("PDF exceeds maximum allowed size", res$issues, fixed = TRUE)))
})

test_that("validate_pdf_upload aggregates multiple simultaneous issues", {
  # Non-existent file AND invalid extension
  mock_input <- list(
    name = "malicious_script.sh",
    datapath = "/nonexistent/path/script.sh",
    type = "application/x-sh"
  )
  
  res <- validate_pdf_upload(mock_input)
  expect_false(res$valid)
  expect_gte(length(res$issues), 2)
  expect_true(any(grepl("File extension must be .pdf", res$issues, fixed = TRUE)))
  expect_true(any(grepl("Uploaded file not found on disk", res$issues, fixed = TRUE)))
  expect_true(grepl(";", res$message, fixed = TRUE))
})