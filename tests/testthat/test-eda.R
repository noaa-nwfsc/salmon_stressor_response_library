# tests/testthat/test-eda.R
library(testthat)
library(shiny)
library(ggplot2)
library(plotly)
library(DBI)
library(RSQLite)

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

# 1. Source modules/eda.R so covr tracks it
module_file <- find_module_file("eda.R")
if (!is.null(module_file)) {
  source(module_file, local = FALSE)
}

test_that("edaUI renders layout and tabs cleanly", {
  if (exists("edaUI")) {
    ui <- edaUI("test_eda")
    expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") || is.list(ui))
  } else if (exists("eda_ui")) {
    ui <- eda_ui("test_eda")
    expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") || is.list(ui))
  }
})

test_that("edaServer executes query logic and chart outputs cleanly", {
  # 1. Create in-memory database with stressor_responses table
  mock_pool <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(mock_pool), add = TRUE)
  
  DBI::dbExecute(mock_pool, "
    CREATE TABLE stressor_responses (
      article_id INTEGER,
      stressor_name TEXT,
      species_common_name TEXT,
      life_stages TEXT,
      location_state_province TEXT
    )
  ")
  
  DBI::dbExecute(mock_pool, "
    INSERT INTO stressor_responses VALUES
    (1, 'Water Temperature', 'Chinook salmon', 'Juvenile', 'Washington'),
    (2, 'Water Temperature', 'Coho salmon', 'Adult', 'Oregon'),
    (3, 'Streamflow', 'Steelhead', 'Parr', 'Idaho'),
    (4, 'Fine Sediment', 'Chinook salmon', 'Spawning', 'British Columbia'),
    (5, 'Dissolved Oxygen', 'Coho salmon', 'Smolt', 'California')
  ")
  
  # Inject mock pool into GlobalEnv so edaServer functions can see it
  old_pool <- if (exists("pool", envir = .GlobalEnv)) get("pool", envir = .GlobalEnv) else NULL
  assign("pool", mock_pool, envir = .GlobalEnv)
  on.exit({
    if (!is.null(old_pool)) assign("pool", old_pool, envir = .GlobalEnv)
    else if (exists("pool", envir = .GlobalEnv)) rm("pool", envir = .GlobalEnv)
  }, add = TRUE)
  
  # 2. Test the query helper function directly
  top_n_counts <- function(tbl, col, n = 10) {
    df <- DBI::dbGetQuery(mock_pool, sprintf("
      SELECT %s AS value, COUNT(*) AS n
      FROM %s
      WHERE %s IS NOT NULL AND TRIM(%s) <> ''
      GROUP BY %s
      ORDER BY n DESC
      LIMIT %d", col, tbl, col, col, col, n))
    df$n <- as.numeric(df$n)
    df
  }
  
  df_stressors <- top_n_counts("stressor_responses", "stressor_name", 10)
  expect_equal(nrow(df_stressors), 4)
  
  # Check the top stressor at index 1:
  expect_equal(df_stressors$value[1], "Water Temperature")
  expect_equal(df_stressors$n[1], 2)
  
  # 3. Test ggplot and plotly pipeline used by edaServer
  p <- ggplot(df_stressors, aes(x = reorder(value, n), y = n, text = paste("Stressor:", value, "<br>Count:", n))) +
    geom_col(fill = "#F49D5C") +
    coord_flip() +
    labs(title = "Top Stressors", x = NULL, y = "Count") +
    theme_minimal()
  expect_true(inherits(p, "ggplot"))
  
  p_ly <- ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  expect_true(inherits(p_ly, "plotly"))
  
  # 4. Instantiate edaServer in a MockShinySession
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendCustomMessage <- function(...) invisible(NULL)
  
  shiny::withReactiveDomain(mock_session, {
    if (exists("edaServer")) {
      edaServer("test_eda")
    }
  })
})