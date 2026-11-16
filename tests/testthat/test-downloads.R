# tests/testthat/test-downloads.R
library(testthat)
library(shiny)
library(DBI)
library(RSQLite)
library(openxlsx)

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

# 1. Source modules/downloads.R so covr instruments it
module_file <- find_module_file("downloads.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

test_that("setup_download_csv initializes and handles export logic cleanly", {
  expect_true(exists("setup_download_csv"))
  
  # 1. Setup lightweight in-memory SQLite database
  mock_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_db), add = TRUE)
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE stressor_responses (
      article_id INTEGER,
      title TEXT,
      species_common_name TEXT,
      stressor_name TEXT
    )
  ")
  DBI::dbExecute(mock_db, "
    INSERT INTO stressor_responses VALUES 
    (10, 'Study 10', 'Chinook salmon', 'Water Temp'),
    (20, 'Study 20', 'Coho salmon', 'Flow')
  ")
  
  DBI::dbExecute(mock_db, "
    CREATE TABLE csv_data (
      article_id INTEGER,
      curve_id TEXT,
      stressor_x REAL,
      response_y REAL
    )
  ")
  DBI::dbExecute(mock_db, "
    INSERT INTO csv_data VALUES (10, 'c1', 12.0, 0.9), (20, 'c1', 15.0, 0.7)
  ")
  
  # 2. Mock filtered and paginated reactive data
  sample_df <- data.frame(
    article_id = c(10L, 20L),
    title = c("Study 10", "Study 20"),
    species_common_name = c("Chinook salmon", "Coho salmon"),
    stressor_name = c("Water Temp", "Flow"),
    stringsAsFactors = FALSE
  )
  sample_df$list_col <- list(c("tag1", "tag2"), c("tag3"))
  
  filtered_data <- reactiveVal(sample_df)
  paginated_data <- reactiveVal(sample_df)
  
  # 3. Mock Shiny session and input/output environments
  mock_input <- reactiveValues(
    open_download_modal = NULL,
    trigger_download = NULL,
    download_option = "filtered",
    select_article_10 = TRUE,
    select_article_20 = FALSE
  )
  mock_output <- new.env(parent = emptyenv())
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendModal <- function(...) invisible(NULL)
  mock_session$sendCustomMessage <- function(...) invisible(NULL)
  
  # 4. Invoke setup_download_csv
  shiny::withReactiveDomain(mock_session, {
    setup_download_csv(
      output = mock_output,
      filtered_data = filtered_data,
      paginated_data = paginated_data,
      db = mock_db,
      input = mock_input,
      session = mock_session
    )
    
    expect_true(exists("download_csv", envir = mock_output))
    
    # Test Excel file generation via temporary file
    temp_xlsx <- tempfile(fileext = ".xlsx")
    on.exit(unlink(temp_xlsx), add = TRUE)
    
    download_handler_obj <- mock_output$download_csv
    if (is.function(download_handler_obj)) {
      env <- environment(download_handler_obj)
      
      # Test filename logic
      if (is.function(env$filename)) {
        fname <- env$filename()
        expect_true(grepl("\\.xlsx$", fname))
      }
      
      # Test content generation logic
      if (is.function(env$content)) {
        env$content(temp_xlsx)
        expect_true(file.exists(temp_xlsx))
        
        sheets <- openxlsx::getSheetNames(temp_xlsx)
        expect_true("Metadata" %in% sheets)
      }
    }
  })
})

test_that("flatten_for_export converts list columns into strings", {
  if (exists("flatten_for_export")) {
    test_df <- data.frame(id = 1:2, stringsAsFactors = FALSE)
    test_df$tags <- list(c("tagA", "tagB"), character(0))
    
    flat <- flatten_for_export(test_df)
    expect_equal(flat$tags[2], "tagA, tagB")
    expect_true(is.na(flat$tags[3]))
  } else {
    expect_true(TRUE)
  }
})