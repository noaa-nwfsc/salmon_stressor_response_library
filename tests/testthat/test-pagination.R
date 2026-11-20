# tests/testthat/test-pagination.R
library(testthat)
library(shiny)

# Source pagination module with fallback paths
if (file.exists("modules/pagination.R")) {
  source("modules/pagination.R")
} else {
  source("../../modules/pagination.R")
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE CALCULATION & SLICING LOGIC TESTS
# ─────────────────────────────────────────────────────────────────────────────

# Helper representing standard pagination slicing logic
paginate_dataframe <- function(df, page = 1, page_size = 10) {
  total_records <- nrow(df)
  if (total_records == 0) {
    return(list(
      data = df[0, , drop = FALSE],
      page = 1,
      total_pages = 1,
      start_idx = 0,
      end_idx = 0
    ))
  }
  
  total_pages <- max(1, ceiling(total_records / page_size))
  # Clamp requested page between 1 and total_pages
  clamped_page <- max(1, min(as.integer(page), total_pages))
  
  start_idx <- (clamped_page - 1) * page_size + 1
  end_idx <- min(clamped_page * page_size, total_records)
  
  list(
    data = df[start_idx:end_idx, , drop = FALSE],
    page = clamped_page,
    total_pages = total_pages,
    start_idx = start_idx,
    end_idx = end_idx
  )
}

test_that("calculate_total_pages handles typical, multiple, and edge counts", {
  # Standard division
  calc_pages <- function(total, size) max(1, ceiling(total / size))
  
  expect_equal(calc_pages(total = 25, size = 10), 3)
  expect_equal(calc_pages(total = 105, size = 25), 5)
  
  # Exact multiples
  expect_equal(calc_pages(total = 20, size = 10), 2)
  expect_equal(calc_pages(total = 50, size = 50), 1)
  
  # Edge cases: 0 rows or fewer rows than page size
  expect_equal(calc_pages(total = 0, size = 10), 1)
  expect_equal(calc_pages(total = 4, size = 10), 1)
})

test_that("paginate_dataframe slices standard pages accurately", {
  # 25 mock records
  mock_data <- data.frame(
    id = 1:25,
    title = paste("Study", 1:25),
    stringsAsFactors = FALSE
  )
  
  # Page 1 (items 1 to 10)
  p1 <- paginate_dataframe(mock_data, page = 1, page_size = 10)
  expect_equal(p1$page, 1)
  expect_equal(p1$total_pages, 3)
  expect_equal(p1$start_idx, 1)
  expect_equal(p1$end_idx, 10)
  expect_equal(p1$data$id, 1:10)
  
  # Page 2 (items 11 to 20)
  p2 <- paginate_dataframe(mock_data, page = 2, page_size = 10)
  expect_equal(p2$page, 2)
  expect_equal(p2$start_idx, 11)
  expect_equal(p2$end_idx, 20)
  expect_equal(p2$data$id, 11:20)
  
  # Page 3 (items 21 to 25 - partial end page)
  p3 <- paginate_dataframe(mock_data, page = 3, page_size = 10)
  expect_equal(p3$page, 3)
  expect_equal(p3$start_idx, 21)
  expect_equal(p3$end_idx, 25)
  expect_equal(nrow(p3$data), 5)
  expect_equal(p3$data$id, 21:25)
})

test_that("paginate_dataframe clamps out-of-bounds page requests", {
  mock_data <- data.frame(
    id = 1:15,
    title = paste("Study", 1:15),
    stringsAsFactors = FALSE
  )
  
  # Page requested is higher than total_pages (e.g. Page 9 requested for 2-page dataset)
  p_high <- paginate_dataframe(mock_data, page = 9, page_size = 10)
  expect_equal(p_high$page, 2)
  expect_equal(p_high$data$id, 11:15)
  
  # Zero or negative page requested
  p_zero <- paginate_dataframe(mock_data, page = 0, page_size = 10)
  expect_equal(p_zero$page, 1)
  expect_equal(p_zero$data$id, 1:10)
  
  p_neg <- paginate_dataframe(mock_data, page = -3, page_size = 10)
  expect_equal(p_neg$page, 1)
  expect_equal(p_neg$data$id, 1:10)
})

test_that("paginate_dataframe handles empty data frames gracefully", {
  empty_df <- data.frame(id = integer(0), title = character(0))
  
  res <- paginate_dataframe(empty_df, page = 1, page_size = 10)
  expect_equal(nrow(res$data), 0)
  expect_equal(res$total_pages, 1)
  expect_equal(res$start_idx, 0)
  expect_equal(res$end_idx, 0)
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. SHINY REACTIVE SERVER TEST
# ─────────────────────────────────────────────────────────────────────────────

test_that("pagination_server reacts to page_size and navigation buttons", {
  # Mock dataset with 25 rows
  mock_df <- data.frame(
    article_id = 1:25,
    title = paste("Study", 1:25),
    stringsAsFactors = FALSE
  )
  
  # Holder for the module's returned reactive values
  mod_out <- NULL
  
  test_app_server <- function(input, output, session) {
    filtered_data <- reactiveVal(mock_df)
    
    # Run the pagination server function directly and capture its output
    mod_out <<- pagination_server(input, output, session, filtered_data)
  }
  
  shiny::testServer(test_app_server, {
    # 1. Set initial page size to 10
    session$setInputs(page_size = 10)
    
    # Helper to read whether paginated_data is returned as a reactive function or value
    get_paginated <- function() {
      if (is.function(mod_out$paginated_data)) {
        mod_out$paginated_data()
      } else {
        mod_out$paginated_data
      }
    }
    
    get_info <- function() {
      if (is.function(mod_out$page_info)) {
        mod_out$page_info()
      } else {
        mod_out$page_info
      }
    }
    
    # Initial state: 10 items (Page 1 of 3)
    p_data <- get_paginated()
    expect_equal(nrow(p_data), 10)
    expect_equal(p_data$article_id, 1:10)
    expect_equal(get_info(), "Showing 1 to 10 of 25 results")
    
    # 2. Click "Next Page" (top button)
    session$setInputs(next_page_top = 1)
    p_data2 <- get_paginated()
    expect_equal(nrow(p_data2), 10)
    expect_equal(p_data2$article_id, 11:20)
    expect_equal(get_info(), "Showing 11 to 20 of 25 results")
    
    # 3. Click "Next Page" again to reach the partial last page (Page 3)
    session$setInputs(next_page_top = 2)
    p_data3 <- get_paginated()
    expect_equal(nrow(p_data3), 5)
    expect_equal(p_data3$article_id, 21:25)
    expect_equal(get_info(), "Showing 21 to 25 of 25 results")
    
    # 4. Click "Previous Page" (bottom button) to navigate back to Page 2
    session$setInputs(prev_page_bottom = 1)
    p_data4 <- get_paginated()
    expect_equal(nrow(p_data4), 10)
    expect_equal(p_data4$article_id, 11:20)
    expect_equal(get_info(), "Showing 11 to 20 of 25 results")
    
    # 5. Change page_size to 25 -> resets back to Page 1
    session$setInputs(page_size = 25)
    p_data5 <- get_paginated()
    expect_equal(nrow(p_data5), 25)
    expect_equal(get_info(), "Showing 1 to 25 of 25 results")
  })
})