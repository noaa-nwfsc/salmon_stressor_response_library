# tests/testthat/setup.R
library(testthat)
library(DBI)
library(pool)
library(withr)

# 1. Helper to find project root directory
find_project_root <- function() {
  if (file.exists("global.R")) {
    return(getwd())
  } else if (file.exists("../../global.R")) {
    return(normalizePath("../.."))
  }
  stop("Could not locate project root containing global.R")
}

proj_root <- find_project_root()

# 2. Check if a pool exists AND is open/valid
is_pool_active <- function() {
  if (!exists("pool", envir = .GlobalEnv)) return(FALSE)
  p <- get("pool", envir = .GlobalEnv)
  if (is.null(p) || !inherits(p, "Pool")) return(FALSE)
  tryCatch(pool::dbIsValid(p), error = function(e) FALSE)
}

# 3. Source global.R if pool is missing or closed (using local = FALSE for .GlobalEnv)
withr::with_dir(proj_root, {
  if (!is_pool_active()) {
    message("Initializing database connection pool from global.R...")
    source("global.R", local = FALSE)
  }
  source("modules/upload.R", local = FALSE)
})

# 4. Helper for isolated transaction rollbacks
with_test_transaction <- function(code) {
  skip_if(
    !is_pool_active(),
    "Database pool is not available or closed. Skipping DB write tests."
  )
  
  pool_obj <- get("pool", envir = .GlobalEnv)
  
  # Check out dedicated connection
  con <- pool::poolCheckout(pool_obj)
  
  # Ensure the connection returns to the pool when the test block exits
  withr::defer(pool::poolReturn(con), envir = parent.frame())
  
  # Ensure schema search path matches db_config
  schema <- Sys.getenv("DB_SCHEMA", "stressor_responses")
  DBI::dbExecute(con, sprintf("SET search_path TO %s, public", schema))
  
  # Start isolated transaction
  DBI::dbBegin(con)
  
  # Ensure transaction is rolled back before connection is returned
  withr::defer(DBI::dbRollback(con), envir = parent.frame())
  
  # Run the test assertions
  force(code(con))
}