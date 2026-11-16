# tests/testthat/test-static_views.R
library(testthat)
library(shiny)

# Universal file path finder that checks both project root and ../../
find_module_file <- function(filename) {
  candidates <- c(
    file.path("modules", filename),
    file.path("..", "..", "modules", filename)
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  return(NULL)
}

# Locate project root where data/ lives
find_project_root <- function() {
  candidates <- c(".", "../..", "..")
  for (cand in candidates) {
    if (file.exists(file.path(cand, "data", "user_guide", "metadata_fields.md"))) {
      return(cand)
    }
  }
  return(".")
}

test_that("static informational modules render valid UI tags", {
  static_files <- c(
    "about_us.R",
    "acknowledgment.R",
    "acknowledgement.R",
    "user_guide.R"
  )
  
  loaded_count <- 0
  for (fname in static_files) {
    fpath <- find_module_file(fname)
    if (!is.null(fpath)) {
      source(fpath, local = FALSE)
      loaded_count <- loaded_count + 1
    }
  }
  
  expect_gt(loaded_count, 0)
  
  # 1. About Us UI
  if (exists("about_us")) {
    res_about <- about_us("about_us")
    expect_true(inherits(res_about, "shiny.tag") || inherits(res_about, "shiny.tag.list") || is.list(res_about))
  }
  
  # 2. Acknowledgment UI
  if (exists("acknowledgement_ui")) {
    res_ack <- acknowledgement_ui("acknowledgement")
    expect_true(inherits(res_ack, "shiny.tag") || inherits(res_ack, "shiny.tag.list") || is.list(res_ack))
  } else if (exists("acknowledgment_ui")) {
    res_ack <- acknowledgment_ui("acknowledgment")
    expect_true(inherits(res_ack, "shiny.tag") || inherits(res_ack, "shiny.tag.list") || is.list(res_ack))
  }
  
  # 3. User Guide UI (temporarily switch to project root so includeMarkdown finds the file)
  if (exists("userGuideUI")) {
    proj_root <- find_project_root()
    res_guide <- withr::with_dir(proj_root, {
      userGuideUI("user_guide")
    })
    expect_true(inherits(res_guide, "shiny.tag") || inherits(res_guide, "shiny.tag.list") || is.list(res_guide))
  }
})