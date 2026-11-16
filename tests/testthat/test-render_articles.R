# tests/testthat/test-render_article.R
library(testthat)
library(DBI)
library(jsonlite)
library(htmltools)

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC & HELPER TESTS
# ─────────────────────────────────────────────────────────────────────────────

# Helper replicating parse_jsonb from render_article_server.R
parse_jsonb <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x)) || x == "[]") {
    return(NULL)
  }
  parsed <- tryCatch(jsonlite::fromJSON(x, simplifyDataFrame = FALSE), error = function(e) NULL)
  if (is.null(parsed) || length(parsed) == 0) {
    return(NULL)
  }
  if (is.data.frame(parsed)) {
    parsed <- lapply(seq_len(nrow(parsed)), function(i) as.list(parsed[i, ]))
  }
  parsed
}

# Helper replicating safe_get from render_article_server.R
safe_get <- function(df, col) {
  if (col %in% names(df)) {
    val <- df[[col]]
    if (is.null(val) || length(val) == 0 || all(is.na(val))) {
      return("Not provided")
    }
    return(ifelse(is.na(val), "Not provided", val))
  }
  return("Not provided")
}

# Helper replicating smart_round from render_article_server.R
smart_round <- function(col) {
  col <- suppressWarnings(as.numeric(col))
  if (all(col %% 1 == 0, na.rm = TRUE)) {
    return(as.integer(col))
  }
  return(round(col, 2))
}

test_that("parse_jsonb handles valid, empty, and malformed inputs gracefully", {
  # Null/empty cases
  expect_null(parse_jsonb(NULL))
  expect_null(parse_jsonb(character(0)))
  expect_null(parse_jsonb(NA_character_))
  expect_null(parse_jsonb(""))
  expect_null(parse_jsonb("   "))
  expect_null(parse_jsonb("[]"))
  expect_null(parse_jsonb("{invalid_json: true}"))
  
  # Valid single citation JSON
  valid_json <- '[{"title":"Test Paper","url":"[suspicious link removed]","text":"Author et al. 2020"}]'
  parsed <- parse_jsonb(valid_json)
  expect_type(parsed, "list")
  expect_equal(length(parsed), 1)
  
  idx_1 <- 1L
  expect_equal(parsed[[idx_1]]$title, "Test Paper")
  expect_equal(parsed[[idx_1]]$url, "[suspicious link removed]")
})

test_that("safe_get retrieves existing values and defaults cleanly to 'Not provided'", {
  df <- data.frame(
    title = "Thermal Study",
    empty_val = NA_character_,
    stringsAsFactors = FALSE
  )
  
  expect_equal(safe_get(df, "title"), "Thermal Study")
  expect_equal(safe_get(df, "empty_val"), "Not provided")
  expect_equal(safe_get(df, "nonexistent_column"), "Not provided")
})

test_that("smart_round formats integer sequences and rounds decimals appropriately", {
  # Whole numbers converted to integer
  whole_nums <- c(10.0, 20.0, 30.0)
  expect_identical(smart_round(whole_nums), c(10L, 20L, 30L))
  
  # Decimals rounded to 2 places
  decimal_nums <- c(10.1234, 20.5678)
  expect_equal(smart_round(decimal_nums), c(10.12, 20.57))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. CITATIONS & CONFIDENCE TABLE RENDERING
# ─────────────────────────────────────────────────────────────────────────────

# Helper replicating render_citations UI generator
build_citations_ui <- function(cits_json) {
  parsed <- parse_jsonb(cits_json)
  if (is.null(parsed) || length(parsed) == 0) {
    return(tags$p("No citations available."))
  }
  
  ui_items <- lapply(parsed, function(item) {
    link_ui <- if (!is.null(item$url) && nzchar(trimws(item$url))) {
      tags$a(href = item$url, target = "_blank", class = "btn btn-primary btn-sm", "View Link")
    } else {
      NULL
    }
    tags$div(
      class = "citation-item",
      tags$strong(item$title %||% "Untitled"),
      tags$p(item$text %||% ""),
      link_ui
    )
  })
  
  tags$div(class = "citations-container", ui_items)
}

test_that("build_citations_ui generates structured HTML and external link buttons", {
  # Empty citation fallback
  empty_ui <- build_citations_ui("[]")
  expect_true(grepl("No citations available.", as.character(empty_ui), fixed = TRUE))
  
  # Populated citations with URL
  cits_json <- '[{"title":"Fullerton 2026","url":"[suspicious link removed]","text":"Fullerton et al. Thermal curves."}]'
  rendered_ui <- as.character(build_citations_ui(cits_json))
  
  expect_true(grepl("Fullerton 2026", rendered_ui, fixed = TRUE))
  expect_true(grepl("Fullerton et al. Thermal curves.", rendered_ui, fixed = TRUE))
  expect_true(grepl('href="[suspicious link removed]"', rendered_ui, fixed = TRUE))
  expect_true(grepl('class="btn btn-primary btn-sm"', rendered_ui, fixed = TRUE))
})

test_that("confidence table formats all 5 standard categories", {
  build_confidence_table <- function(article_row) {
    categories <- c(
      "Source Quality" = safe_get(article_row, "conf_source"),
      "Curve Shape" = safe_get(article_row, "conf_shape"),
      "Variance / Error" = safe_get(article_row, "conf_variance"),
      "Applicability" = safe_get(article_row, "conf_applicability"),
      "Interactions" = safe_get(article_row, "conf_interactions")
    )
    data.frame(
      Category = names(categories),
      Rating = unname(categories),
      stringsAsFactors = FALSE
    )
  }
  
  mock_row <- data.frame(
    conf_source = "High",
    conf_shape = "Medium",
    conf_variance = NA_character_,
    conf_applicability = "High",
    conf_interactions = "Low",
    stringsAsFactors = FALSE
  )
  
  conf_df <- build_confidence_table(mock_row)
  
  expect_equal(nrow(conf_df), 5)
  expect_equal(conf_df$Rating[1L], "High")
  expect_equal(conf_df$Rating[2L], "Medium")
  expect_equal(conf_df$Rating[3L], "Not provided")
  expect_equal(conf_df$Rating[4L], "High")
  expect_equal(conf_df$Rating[5L], "Low")
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. QUANTITATIVE CURVE DATA EXTRACTION & MULTI-CURVE DETECTION
# ─────────────────────────────────────────────────────────────────────────────

# Helper replicating curve data preparation logic from render_article_server.R
prepare_article_curves <- function(df_csv) {
  if (is.null(df_csv) || nrow(df_csv) == 0) return(NULL)
  
  # Normalize column names to dot-notation expected by rendering methods
  colnames(df_csv) <- tolower(gsub("_", ".", colnames(df_csv), fixed = TRUE))
  
  req_cols <- c("stressor.x", "response.y")
  if (!all(req_cols %in% names(df_csv))) return(NULL)
  
  df_csv$stressor.x <- suppressWarnings(as.numeric(df_csv$stressor.x))
  df_csv$response.y <- suppressWarnings(as.numeric(df_csv$response.y))
  
  # Filter out rows missing coordinates
  df_csv <- df_csv[!is.na(df_csv$stressor.x) & !is.na(df_csv$response.y), , drop = FALSE]
  if (nrow(df_csv) == 0) return(NULL)
  
  has_multiple_curves <- FALSE
  if ("curve.id" %in% names(df_csv)) {
    valid_curves <- unique(df_csv$curve.id[!is.na(df_csv$curve.id) & df_csv$curve.id != ""])
    if (length(valid_curves) > 1) {
      has_multiple_curves <- TRUE
    }
  }
  
  list(
    data = df_csv,
    has_multiple_curves = has_multiple_curves,
    point_count = nrow(df_csv)
  )
}

test_that("prepare_article_curves cleans coordinates and detects single vs multi-curve data", {
  # Single curve data with an invalid NA row
  raw_csv_1 <- data.frame(
    row_index = 1:3,
    curve_id = c("c1", "c1", "c1"),
    stressor_x = c("10.5", "20.5", "invalid_num"),
    response_y = c(0.8, 0.4, 0.2),
    plot_type = "curve",
    stringsAsFactors = FALSE
  )
  
  res_1 <- prepare_article_curves(raw_csv_1)
  expect_false(res_1$has_multiple_curves)
  expect_equal(res_1$point_count, 2)
  expect_equal(res_1$data$stressor.x, c(10.5, 20.5))
  
  # Multiple curves (c1 and c2)
  raw_csv_2 <- data.frame(
    row_index = 1:4,
    curve_id = c("c1", "c1", "c2", "c2"),
    stressor_x = c(10, 20, 10, 20),
    response_y = c(0.9, 0.5, 0.7, 0.3),
    plot_type = "curve",
    stringsAsFactors = FALSE
  )
  
  res_2 <- prepare_article_curves(raw_csv_2)
  expect_true(res_2$has_multiple_curves)
  expect_equal(res_2$point_count, 4)
})

# ─────────────────────────────────────────────────────────────────────────────
# 4. DATABASE INTEGRATION TEST WITH AUTOMATIC ROLLBACK
# ─────────────────────────────────────────────────────────────────────────────

test_that("render_article queries metadata and linked curve points accurately", {
  with_test_transaction(function(con) {
    max_id_res <- DBI::dbGetQuery(con, "SELECT COALESCE(MAX(article_id), 0) as max_id FROM stressor_responses")
    art_id <- as.integer(max_id_res$max_id + 1L)
    
    # 1. Insert article with citations and confidence scores
    cits_json <- '[{"title":"Test Study 2026","url":"[suspicious link removed]","text":"Reference text"}]'
    insert_sql <- "
      INSERT INTO stressor_responses (
        article_id, user_id, article_type, title, broad_stressor_name,
        stressor_name, specific_stressor_metric, response,
        species_common_name, latin_name, location_country, overview,
        citations, conf_source, conf_shape
      ) VALUES (
        $1, 1, 'Journal Article', 'Detailed Article Test', 'Water Quality',
        'Temperature', 'Celsius', 'Survival',
        '{\"Chinook salmon\"}', '{\"Oncorhynchus tshawytscha\"}', '{\"United States\"}', 'Detailed overview',
        $2::jsonb, 'High', 'Medium'
      )
    "
    DBI::dbExecute(con, insert_sql, params = list(art_id, cits_json))
    
    # 2. Insert 2 curve points into csv_data
    curve_df <- data.frame(
      article_id = art_id,
      row_index = 1:2,
      curve_id = "c1",
      stressor_label = "Temperature",
      stressor_x = c(12, 18),
      units_x = "degC",
      response_label = "Survival",
      response_y = c(0.95, 0.70),
      units_y = "ratio",
      plot_type = "curve",
      stringsAsFactors = FALSE
    )
    DBI::dbAppendTable(con, "csv_data", curve_df)
    
    # 3. Query article metadata exactly as in render_article_server.R
    meta_res <- DBI::dbGetQuery(
      con,
      "SELECT * FROM stressor_responses WHERE article_id = $1",
      params = list(art_id)
    )
    expect_equal(nrow(meta_res), 1)
    expect_equal(meta_res$title, "Detailed Article Test")
    expect_equal(meta_res$conf_source, "High")
    
    parsed_cits <- parse_jsonb(meta_res$citations)
    idx_1 <- 1L
    expect_equal(parsed_cits[[idx_1]]$title, "Test Study 2026")
    
    # 4. Query linked curve data
    csv_res <- DBI::dbGetQuery(
      con,
      "SELECT * FROM csv_data WHERE article_id = $1 ORDER BY row_index",
      params = list(art_id)
    )
    
    prepared <- prepare_article_curves(csv_res)
    expect_false(prepared$has_multiple_curves)
    expect_equal(prepared$point_count, 2)
    expect_equal(prepared$data$stressor.x, c(12, 18))
    expect_equal(prepared$data$response.y, c(0.95, 0.70))
  })
})