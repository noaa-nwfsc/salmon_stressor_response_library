# tests/testthat/test-filters.R
library(testthat)

# Source modules with robust fallback paths
if (file.exists("modules/filters.R")) {
  source("modules/filters.R")
  source("modules/downloads.R")
} else {
  source("../../modules/filters.R")
  source("../../modules/downloads.R")
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. MATCH_ARRAY_COL TESTS (Comma-separated exact matching)
# ─────────────────────────────────────────────────────────────────────────────

test_that("match_array_col matches single and multiple items accurately", {
  species_col <- c(
    "Chinook salmon",
    "Coho salmon, Chinook salmon",
    "Steelhead trout",
    "Coho salmon, Sockeye salmon"
  )
  
  # Single selection
  match_chinook <- match_array_col(species_col, selected = c("Chinook salmon"))
  expect_equal(match_chinook, c(TRUE, TRUE, FALSE, FALSE))
  
  # Multiple selections (OR logic across selected items)
  match_multi <- match_array_col(species_col, selected = c("Steelhead trout", "Sockeye salmon"))
  expect_equal(match_multi, c(FALSE, FALSE, TRUE, TRUE))
  
  # Selection that does not exist in any row
  match_none <- match_array_col(species_col, selected = c("Atlantic salmon"))
  expect_equal(match_none, c(FALSE, FALSE, FALSE, FALSE))
})

test_that("match_array_col prevents partial substring false-positives", {
  species_col <- c("Chinook salmon", "Chinook", "Coho salmon")
  
  # "Chinook" should NOT match "Chinook salmon"
  match_exact <- match_array_col(species_col, selected = c("Chinook"))
  expect_equal(match_exact, c(FALSE, TRUE, FALSE))
  
  # "salmon" should NOT match "Chinook salmon"
  match_sub <- match_array_col(species_col, selected = c("salmon"))
  expect_equal(match_sub, c(FALSE, FALSE, FALSE))
})

test_that("match_array_col safely handles NAs, blanks, and empty inputs", {
  messy_col <- c("Chinook salmon", NA, "", "Coho salmon", "NA")
  
  res <- match_array_col(messy_col, selected = c("Chinook salmon"))
  expect_equal(res, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  
  # Empty or NULL selection should return FALSE for all rows
  expect_equal(match_array_col(messy_col, selected = character(0)), rep(FALSE, 5))
  expect_equal(match_array_col(messy_col, selected = NULL), rep(FALSE, 5))
  
  # All NA column
  all_na <- c(NA_character_, NA_character_)
  expect_equal(match_array_col(all_na, selected = c("Chinook salmon")), c(FALSE, FALSE))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. SEARCH_ARRAY_COL TESTS (Substring searching)
# ─────────────────────────────────────────────────────────────────────────────

test_that("search_array_col performs case-insensitive substring search", {
  text_col <- c(
    "Sacramento River, Feather River",
    "Columbia River Basin",
    "Puget Sound lowland streams",
    NA
  )
  
  # Lowercase query against MixedCase data
  expect_equal(search_array_col(text_col, "feather"), c(TRUE, FALSE, FALSE, FALSE))
  
  # Substring query matching multiple rows
  expect_equal(search_array_col(text_col, "river"), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(search_array_col(text_col, "RIVER"), c(TRUE, TRUE, FALSE, FALSE))
  
  # Non-matching query
  expect_equal(search_array_col(text_col, "Fraser"), c(FALSE, FALSE, FALSE, FALSE))
})

test_that("search_array_col handles regex and edge values cleanly", {
  test_col <- c("Temp (degC)", "Flow [cfs]", "7DADM", "")
  
  # Searching with parentheses or metric codes
  expect_equal(search_array_col(test_col, "degc"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(search_array_col(test_col, "7dadm"), c(FALSE, FALSE, TRUE, FALSE))
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. FILTER PIPELINE INTEGRATION
# ─────────────────────────────────────────────────────────────────────────────

test_that("Filter pipeline accurately combines plain and array column criteria", {
  # Mock catalog of articles
  catalog <- data.frame(
    article_id = 1:4,
    title = c(
      "Thermal tolerance of juvenile Chinook",
      "Streamflow requirements for Coho rearing",
      "Temperature effects on Adult Chinook and Coho",
      "Turbidity impacts on Steelhead feeding"
    ),
    stressor_name = c("Temperature", "Flow", "Temperature", "Turbidity"),
    species_common_name = c(
      "Chinook salmon",
      "Coho salmon",
      "Chinook salmon, Coho salmon",
      "Steelhead trout"
    ),
    life_stages = c("Juvenile", "Parr", "Adult", "Juvenile"),
    stringsAsFactors = FALSE
  )
  
  # 1. Filter by plain text: stressor_name == 'Temperature'
  f_temp <- catalog[catalog$stressor_name %in% "Temperature", ]
  expect_equal(f_temp$article_id, c(1, 3))
  
  # 2. Add array filter: species == 'Coho salmon'
  f_coho <- f_temp[match_array_col(f_temp$species_common_name, "Coho salmon"), ]
  expect_equal(f_coho$article_id, 3)
  
  # 3. Full-text search across titles and species
  search_term <- "juvenile"
  plain_match <- grepl(search_term, tolower(catalog$title))
  array_match <- search_array_col(catalog$life_stages, search_term)
  search_res <- catalog[plain_match | array_match, ]
  expect_equal(search_res$article_id, c(1, 4))
})