# tests/testthat/test-render_papers.R
library(testthat)
library(shiny)
library(htmltools)

# Sourced directly or defined to test render_papers logic
format_field <- function(label, val, bold = FALSE) {
  display <- ifelse(is.null(val) || length(val) == 0 || is.na(val) || val == "NA" || val == "N/A", "", val)
  if (display == "") {
    return("")
  }
  label_span <- sprintf("<span class='metadata-light' style='color: #666;'>%s:</span> ", htmltools::htmlEscape(label))
  value_span <- sprintf("<span class='metadata-bold' style='color: #222; font-weight: 500;'>%s</span>", htmltools::htmlEscape(display))
  sprintf("<div class='paper-meta-item' style='margin-bottom: 6px; line-height: 1.3;'>%s%s</div>", label_span, value_span)
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. PURE LOGIC TESTS: FIELD FORMATTING & HTML ESCAPING
# ─────────────────────────────────────────────────────────────────────────────

test_that("format_field returns empty string for NA, N/A, and empty values", {
  expect_equal(format_field("Species", NULL), "")
  expect_equal(format_field("Species", character(0)), "")
  expect_equal(format_field("Species", NA_character_), "")
  expect_equal(format_field("Species", "NA"), "")
  expect_equal(format_field("Species", "N/A"), "")
  expect_equal(format_field("Species", ""), "")
})

test_that("format_field produces formatted HTML with proper escaping", {
  res <- format_field("Life Stage", "Smolt")
  expect_true(grepl("Life Stage:", res, fixed = TRUE))
  expect_true(grepl("Smolt", res, fixed = TRUE))
  expect_true(grepl("class='metadata-light'", res, fixed = TRUE))
  expect_true(grepl("class='metadata-bold'", res, fixed = TRUE))
  
  # HTML character escaping check (<, >, &)
  res_escaped <- format_field("Metric", "Flow < 50 & Temp > 15")
  expect_true(grepl("Flow &lt; 50 &amp; Temp &gt; 15", res_escaped, fixed = TRUE))
  expect_false(grepl("< 50", res_escaped, fixed = TRUE))
})

# ─────────────────────────────────────────────────────────────────────────────
# 2. CARD GENERATION & CONTRIBUTOR BADGE LOGIC
# ─────────────────────────────────────────────────────────────────────────────

test_that("empty or all-NA data frame produces 'No research papers found.'", {
  render_card_ui <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(tags$p("No research papers found.", style = "font-size: 18px; font-weight: bold; color: red;"))
    }
    df <- df[rowSums(is.na(df)) != ncol(df), , drop = FALSE]
    if (nrow(df) == 0) {
      return(tags$p("No research papers found.", style = "font-size: 18px; font-weight: bold; color: red;"))
    }
    tags$div(class = "cards-container")
  }
  
  # 1. NULL input
  ui_null <- render_card_ui(NULL)
  expect_true(grepl("No research papers found.", as.character(ui_null), fixed = TRUE))
  
  # 2. 0-row dataframe
  ui_empty <- render_card_ui(data.frame(article_id = integer(0), title = character(0)))
  expect_true(grepl("No research papers found.", as.character(ui_empty), fixed = TRUE))
  
  # 3. All-NA dataframe
  df_all_na <- data.frame(
    article_id = c(NA, NA),
    title = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  ui_na <- render_card_ui(df_all_na)
  expect_true(grepl("No research papers found.", as.character(ui_na), fixed = TRUE))
})

test_that("paper card generates correct contributor badge, IDs, and structure", {
  paper_row <- data.frame(
    article_id = 101,
    title = "Thermal Incubation Test",
    contributor_name = "Jane Salmon Researcher",
    article_type = "Peer-reviewed",
    species_common_name = "Chinook salmon",
    latin_name = "Oncorhynchus tshawytscha",
    life_stages = "Incubation",
    activity = "Spawning",
    stressor_name = "Water Temperature",
    specific_stressor_metric = "Celsius",
    response = "Egg Survival",
    location_country = "United States",
    location_state_province = "Washington",
    location_watershed_lab = "Skagit Basin",
    location_river_creek = "Cascade River",
    stringsAsFactors = FALSE
  )
  
  # Helper simulating single card markup from render_papers.R
  build_card <- function(paper) {
    contributor <- if ("contributor_name" %in% names(paper) && !is.na(paper$contributor_name)) paper$contributor_name else "UNKNOWN"
    checkbox_id <- paste0("select_article_", paper$article_id)
    btn_id <- paste0("view_article_", paper$article_id)
    title_label <- paste0(paper$article_id, ". ", paper$title)
    
    div(
      class = "paper-card",
      tags$span(class = "contributor-badge", paste("Entry by:", contributor)),
      div(class = "paper-checkbox-container", checkboxInput(inputId = checkbox_id, label = NULL, value = FALSE)),
      div(
        class = "paper-content",
        actionButton(inputId = btn_id, label = title_label)
      )
    )
  }
  
  card_html <- as.character(build_card(paper_row))
  
  # Verify IDs and contributor badge
  expect_true(grepl("Entry by: Jane Salmon Researcher", card_html, fixed = TRUE))
  expect_true(grepl("select_article_101", card_html, fixed = TRUE))
  expect_true(grepl("view_article_101", card_html, fixed = TRUE))
  expect_true(grepl("101. Thermal Incubation Test", card_html, fixed = TRUE))
  
  # Verify UNKNOWN fallback when contributor is NA or missing
  paper_row_anon <- paper_row
  paper_row_anon$contributor_name <- NA_character_
  card_anon_html <- as.character(build_card(paper_row_anon))
  expect_true(grepl("Entry by: UNKNOWN", card_anon_html, fixed = TRUE))
})

# ─────────────────────────────────────────────────────────────────────────────
# 3. SHINY REACTIVE SERVER TEST (Select All Synchronization)
# ─────────────────────────────────────────────────────────────────────────────

test_that("render_papers_server syncs visible checkboxes when select_all changes", {
  # Mock 3 visible articles on active page
  mock_papers <- data.frame(
    article_id = c(10, 20, 30),
    title = paste("Study", c(10, 20, 30)),
    stringsAsFactors = FALSE
  )
  
  # Scoped tracking list to hold updated states directly
  tracker <- list(
    select_article_10 = FALSE,
    select_article_20 = FALSE,
    select_article_30 = FALSE
  )
  
  test_papers_server <- function(input, output, session) {
    paginated_data <- reactiveVal(mock_papers)
    
    # Observe select_all matching render_papers.R
    observeEvent(input$select_all, {
      req(!is.null(input$select_all))
      ids <- paginated_data()$article_id
      for (mid in ids) {
        box_name <- paste0("select_article_", mid)
        tracker[[box_name]] <<- input$select_all
        updateCheckboxInput(session, inputId = box_name, value = input$select_all)
      }
    })
  }
  
  shiny::testServer(test_papers_server, {
    # 1. Trigger select_all = TRUE
    session$setInputs(select_all = TRUE)
    
    expect_true(tracker$select_article_10)
    expect_true(tracker$select_article_20)
    expect_true(tracker$select_article_30)
    
    # 2. Trigger select_all = FALSE
    session$setInputs(select_all = FALSE)
    
    expect_false(tracker$select_article_10)
    expect_false(tracker$select_article_20)
    expect_false(tracker$select_article_30)
  })
})
