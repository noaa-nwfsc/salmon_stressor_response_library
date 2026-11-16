# tests/testthat/test-eda.R
library(testthat)
library(DBI)
library(ggplot2)
library(plotly)

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC & GGPLOT/PLOTLY PIPELINE TESTS
# ─────────────────────────────────────────────────────────────────────────────

# Helper replicating the ggplot builder used across all 4 outputs in eda.R
build_eda_plot <- function(df, x_col, label_name, bar_fill = "#F49D5C", text_color = "#005f5f") {
  if (is.null(df) || nrow(df) == 0) {
    p <- ggplot() + 
      annotate("text", x = 1, y = 1, label = "No data available") +
      theme_void()
    return(ggplotly(p))
  }
  
  p <- ggplot(df, aes(x = reorder(.data[[x_col]], n), y = n, text = paste0(label_name, ": ", .data[[x_col]], "<br>Count: ", n))) +
    geom_col(fill = bar_fill) +
    coord_flip() +
    labs(title = paste("Top", nrow(df), label_name, "Covered"), x = NULL, y = "Count") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = text_color),
      axis.text = element_text(size = 12, color = text_color),
      axis.title = element_text(size = 14, color = text_color),
      panel.grid.major.x = element_blank()
    )
  
  ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
}

test_that("eda ggplotly pipeline constructs valid interactive charts", {
  # Mock aggregated data matching top_n_counts output
  mock_df <- data.frame(
    value = c("Water Temperature", "Streamflow", "Turbidity"),
    n = c(45, 30, 12),
    stringsAsFactors = FALSE
  )
  # Ensure n is numeric as handled in eda.R
  mock_df$n <- as.numeric(mock_df$n)
  
  plt <- build_eda_plot(mock_df, "value", "Stressor")
  
  expect_s3_class(plt, "plotly")
  expect_equal(length(plt$x$data), 1)
  # Verify config has displayModeBar set to FALSE
  expect_false(plt$x$config$displayModeBar)
})

test_that("numeric conversion handles integer64 from PostgreSQL counts safely", {
  # Properly instantiate 64-bit integers as returned by RPostgres
  if (requireNamespace("bit64", quietly = TRUE)) {
    int64_vals <- bit64::as.integer64(c(100, 50, 20))
  } else {
    int64_vals <- c(100, 50, 20)
  }
  
  converted <- as.numeric(int64_vals)
  
  expect_type(converted, "double")
  expect_equal(converted, c(100, 50, 20))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. DATABASE AGGREGATION & LATERAL UNNEST QUERIES
# ─────────────────────────────────────────────────────────────────────────────

test_that("top_n_counts returns top categories in descending order and ignores blanks", {
  with_test_transaction(function(con) {
    max_id_res <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(article_id), 0) as max_id FROM stressor_responses")
    base_id <- as.integer(max_id_res$max_id + 1L)
    
    # 1. Seed 4 records: 2 "Water Temperature", 1 "Streamflow", 1 blank/whitespace
    seed_sql <- "
      INSERT INTO stressor_responses (
        article_id, user_id, article_type, title, broad_stressor_name,
        stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview
      ) VALUES (
        $1, 1, 'Report', $2, 'Stressor',
        $3, 'Metric', 'Resp',
        '{\"Chinook salmon\"}', '{\"Oncorhynchus tshawytscha\"}', '{\"USA\"}', 'Desc'
      )
    "
    
    # Record 1: Water Temperature
    DBI::dbExecute(con, seed_sql, params = list(base_id, "EDA Test 1", "Water Temperature"))
    # Record 2: Water Temperature
    DBI::dbExecute(con, seed_sql, params = list(base_id + 1L, "EDA Test 2", "Water Temperature"))
    # Record 3: Streamflow
    DBI::dbExecute(con, seed_sql, params = list(base_id + 2L, "EDA Test 3", "Streamflow"))
    # Record 4: Blank/whitespace stressor (should be excluded)
    DBI::dbExecute(con, seed_sql, params = list(base_id + 3L, "EDA Test 4", "   "))
    
    # 2. Execute query replicating top_n_counts from eda.R
    query <- "
      SELECT stressor_name AS value, COUNT(*) AS n
      FROM stressor_responses
      WHERE stressor_name IS NOT NULL AND TRIM(stressor_name) <> ''
        AND title LIKE 'EDA Test %'
      GROUP BY stressor_name
      ORDER BY n DESC
      LIMIT 10
    "
    res <- DBI::dbGetQuery(con, query)
    res$n <- as.numeric(res$n)
    
    expect_equal(nrow(res), 2)
    
    # Top item must be "Water Temperature" with count 2
    idx_top <- 1L
    expect_equal(res$value[idx_top], "Water Temperature")
    expect_equal(res$n[idx_top], 2)
    
    # Second item must be "Streamflow" with count 1
    idx_second <- 2L
    expect_equal(res$value[idx_second], "Streamflow")
    expect_equal(res$n[idx_second], 1)
  })
})

test_that("LATERAL unnest correctly flattens and tallies text array elements", {
  with_test_transaction(function(con) {
    max_id_res <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(article_id), 0) as max_id FROM stressor_responses")
    base_id <- as.integer(max_id_res$max_id + 1L)
    
    # Seed 2 articles with overlapping species arrays:
    # Article 1: Chinook salmon, Coho salmon
    # Article 2: Chinook salmon, Steelhead
    seed_sql <- "
      INSERT INTO stressor_responses (
        article_id, user_id, article_type, title, broad_stressor_name,
        stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview
      ) VALUES (
        $1, 1, 'Report', $2, 'Stressor',
        'Stressor Name', 'Metric', 'Resp',
        $3::text[], '{\"Oncorhynchus\"}', '{\"USA\"}', 'Desc'
      )
    "
    
    DBI::dbExecute(con, seed_sql, params = list(
      base_id, "EDA Unnest 1", '{"Chinook salmon", "Coho salmon"}'
    ))
    DBI::dbExecute(con, seed_sql, params = list(
      base_id + 1L, "EDA Unnest 2", '{"Chinook salmon", "Steelhead"}'
    ))
    
    # Run the exact LATERAL unnest query from modules/eda.R
    query <- "
      SELECT TRIM(unnested) AS raw, COUNT(*) AS n
      FROM stressor_responses,
        LATERAL unnest(species_common_name) AS unnested
      WHERE species_common_name IS NOT NULL
        AND array_length(species_common_name, 1) > 0
        AND title LIKE 'EDA Unnest %'
      GROUP BY raw
      ORDER BY n DESC
      LIMIT 10
    "
    res <- DBI::dbGetQuery(con, query)
    res$n <- as.numeric(res$n)
    
    expect_equal(nrow(res), 3)
    
    # "Chinook salmon" appears in both articles -> n = 2
    idx_chinook <- which(res$raw == "Chinook salmon")
    expect_equal(res$n[idx_chinook], 2)
    
    # "Coho salmon" and "Steelhead" appear once each -> n = 1
    idx_coho <- which(res$raw == "Coho salmon")
    expect_equal(res$n[idx_coho], 1)
    
    idx_steelhead <- which(res$raw == "Steelhead")
    expect_equal(res$n[idx_steelhead], 1)
  })
})