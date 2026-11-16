# tests/testthat/test-downloads.R
library(testthat)
library(DBI)
library(openxlsx)
library(shiny)

# Sourced or defined exactly as in modules/downloads.R
flatten_for_export <- function(df) {
  list_cols <- names(df)[sapply(df, is.list)]
  if (length(list_cols) > 0) {
    df[list_cols] <- lapply(df[list_cols], function(col) {
      vapply(col, function(x) {
        if (length(x) == 0 || all(is.na(x))) NA_character_
        else paste(x[!is.na(x)], collapse = ", ")
      }, character(1))
    })
  }
  df
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC TESTS: FLATTENING & EXCEL FORMATTING
# ─────────────────────────────────────────────────────────────────────────────

test_that("flatten_for_export flattens list columns into clean strings", {
  # Mock dataframe containing list columns (like Postgres text[] after query)
  df_with_lists <- data.frame(
    article_id = 1:3,
    title = c("Study 1", "Study 2", "Study 3"),
    stringsAsFactors = FALSE
  )
  
  df_with_lists$species_common_name <- list(
    c("Chinook salmon", "Coho salmon"),
    character(0), # empty list
    c("Steelhead")
  )
  
  df_with_lists$location_country <- list(
    c("United States"),
    c(NA_character_),
    c("United States", "Canada")
  )
  
  flattened <- flatten_for_export(df_with_lists)
  
  # Ensure no list columns remain
  expect_false(any(sapply(flattened, is.list)))
  
  # Use explicit row index integers to avoid bracket corruption
  r1 <- 1L
  r2 <- 2L
  r3 <- 3L
  
  # Check species list flattening
  expect_equal(flattened$species_common_name[r1], "Chinook salmon, Coho salmon")
  expect_true(is.na(flattened$species_common_name[r2]))
  expect_equal(flattened$species_common_name[r3], "Steelhead")
  
  # Check location list flattening
  expect_equal(flattened$location_country[r1], "United States")
  expect_true(is.na(flattened$location_country[r2]))
  expect_equal(flattened$location_country[r3], "United States, Canada")
})

test_that("flatten_for_export leaves standard character/numeric columns untouched", {
  df_plain <- data.frame(
    article_id = 1:2,
    stressor_name = c("Temperature", "Flow"),
    numeric_val = c(12.5, 45.0),
    stringsAsFactors = FALSE
  )
  
  res <- flatten_for_export(df_plain)
  expect_equal(res, df_plain)
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. WORKBOOK GENERATION & SHEET STRUCTURE
# ─────────────────────────────────────────────────────────────────────────────

test_that("empty download creates 'No Data' worksheet", {
  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)
  
  # Simulate empty download fallback in downloads.R
  df_main <- data.frame()
  if (nrow(df_main) == 0) {
    wb <- createWorkbook()
    addWorksheet(wb, "No Data")
    saveWorkbook(wb, tmp_file, overwrite = TRUE)
  }
  
  sheets <- getSheetNames(tmp_file)
  expect_equal(sheets, "No Data")
})

test_that("multi-sheet workbook generates Metadata and Data - Art tabs correctly", {
  tmp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_file), add = TRUE)
  
  # Mock metadata for 2 articles
  df_main <- data.frame(
    article_id = c(101, 102),
    title = c("Thermal Tolerance Study", "Flow Study"),
    stressor_name = c("Temperature", "Flow"),
    stringsAsFactors = FALSE
  )
  
  # Mock associated csv_data rows
  df_csv_data <- data.frame(
    article_id = c(101, 101, 102),
    curve_id = c("c1", "c1", "c1"),
    stressor_x = c(10, 15, 50),
    response_y = c(0.9, 0.8, 0.75),
    stringsAsFactors = FALSE
  )
  
  # Build workbook following downloads.R logic
  wb <- createWorkbook()
  addWorksheet(wb, "Metadata")
  writeData(wb, "Metadata", df_main)
  
  for (id in unique(df_csv_data$article_id)) {
    sheet_name <- paste0("Data - Art ", id)
    # Ensure sheet name complies with Excel 31-character limit
    expect_lte(nchar(sheet_name), 31)
    
    article_specific_data <- df_csv_data[df_csv_data$article_id == id, , drop = FALSE]
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, article_specific_data)
  }
  saveWorkbook(wb, tmp_file, overwrite = TRUE)
  
  # Inspect generated workbook
  sheets <- getSheetNames(tmp_file)
  expect_equal(sheets, c("Metadata", "Data - Art 101", "Data - Art 102"))
  
  # Read back sheets and verify contents
  read_meta <- read.xlsx(tmp_file, sheet = "Metadata")
  expect_equal(nrow(read_meta), 2)
  expect_equal(read_meta$title, c("Thermal Tolerance Study", "Flow Study"))
  
  read_art101 <- read.xlsx(tmp_file, sheet = "Data - Art 101")
  expect_equal(nrow(read_art101), 2)
  expect_equal(read_art101$stressor_x, c(10, 15))
  
  read_art102 <- read.xlsx(tmp_file, sheet = "Data - Art 102")
  expect_equal(nrow(read_art102), 1)
  expect_equal(read_art102$stressor_x, 50)
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. EXPORT SELECTION ROUTING & CHECKBOX CART LOGIC
# ─────────────────────────────────────────────────────────────────────────────

test_that("export option subsetting works for filtered and selected records", {
  all_data <- data.frame(
    article_id = 1:5,
    title = paste("Study", 1:5),
    stringsAsFactors = FALSE
  )
  
  # 1. Option: "filtered" returns full filtered_data
  opt_filtered <- all_data
  expect_equal(nrow(opt_filtered), 5)
  
  # 2. Option: "selected" retains only items in cart
  selected_ids <- c("2", "4")
  opt_selected <- all_data[as.character(all_data$article_id) %in% selected_ids, , drop = FALSE]
  expect_equal(nrow(opt_selected), 2)
  expect_equal(opt_selected$article_id, c(2, 4))
  
  # 3. Empty cart selection returns 0 rows
  empty_selection <- all_data[as.character(all_data$article_id) %in% character(0), , drop = FALSE]
  expect_equal(nrow(empty_selection), 0)
})

test_that("cart tracking correctly adds and removes checked IDs across pages", {
  cart <- character(0)
  
  # User checks article 3 and 5 on Page 1
  cart <- unique(c(cart, as.character(3)))
  cart <- unique(c(cart, as.character(5)))
  expect_equal(cart, c("3", "5"))
  
  # User navigates to Page 2 and checks article 12
  cart <- unique(c(cart, as.character(12)))
  expect_equal(cart, c("3", "5", "12"))
  
  # User unchecks article 5
  cart <- setdiff(cart, as.character(5))
  expect_equal(cart, c("3", "12"))
  
  # Duplicate additions are ignored
  cart <- unique(c(cart, as.character(3)))
  expect_equal(cart, c("3", "12"))
})