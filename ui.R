# nolint start
library(shinyjs)
library(shiny)
library(shinyWidgets)

# Source all necessary modules
source("modules/upload.R", local = TRUE)
source("modules/about_us.R", local = TRUE)
source("modules/acknowledgement.R", local = TRUE)
source("modules/eda.R", local = TRUE)
source("modules/user_guide.R", local = TRUE)
source("modules/submit_relationship.R", local = TRUE)

# Static resource for team images
addResourcePath("teamimg", "modules/images")

# UI
ui <- navbarPage(
  id = "main_navbar",
  title = "Pacific Salmonid Stressor-Response eLibrary",
  selected = "about",
  
  # Removed bslib theme argument entirely. Falling back to native Shiny.

  # About Tab
  tabPanel(
    title = "About",
    value = "about",
    fluidPage(
      useShinyjs(),
      tags$head(
        includeCSS("www/custom.css"),
        tags$style(HTML("
          /* WCAG AA 4.5:1 Contrast Fix for Active Navbar Links against #e7e7e7 */
          .navbar-default .navbar-nav > .active > a, 
          .navbar-default .navbar-nav > .active > a:focus, 
          .navbar-default .navbar-nav > .active > a:hover {
              color: #005f9e !important;
              font-weight: 600;
              background-color: #e7e7e7 !important;
          }
          #back_to_top_fab {
            position: fixed;
            bottom: 30px;
            right: 30px;
            z-index: 9999;
          }
          .dropdown-menu {
            padding: 20px;
          }
          .radio label {
            font-size: 16px;
            font-weight: 500;
          }
        ")),
        tags$script(HTML("
          Shiny.addCustomMessageHandler('download_csv', function(data) {
            document.getElementById(data.id).click();
          });
        "))
      ),
      h1("Welcome to the Pacific Salmonid Stressor-Response e-Library"),
      tags$div(
        about_us("about_us")
      ),
      tags$hr(),
      tags$div(
        acknowledgement_ui("acknowledgement", n = 20)
      )
    )
  ),

  # User Guide Tab
  tabPanel("User Guide", userGuideUI("user_guide")),

  # Analyze Data Tab
  tabPanel("Topics Covered", edaUI("eda")),

  # Dashboard Tab
  tabPanel(
    title = "SRF Dashboard",
    value = "dashboard",
    fluidPage(
      useShinyjs(),
      conditionalPanel(
        condition = "!window.location.search.includes('article_id')",
        fluidRow(
          column(8, textInput("search", "Search All Text", placeholder = "Type keywords...")),
          # Removed icon("filter")
          column(4, actionButton("toggle_filters", "Show Filters"))
        ),
        shinyjs::hidden(
          fluidRow(
            column(6, numericInput("page", NULL, value = 1, min = 1)),
            column(6, numericInput("page_size_hidden", NULL, value = 10, min = 1))
          )
        ),
        
        # ── RESTORED PICKER INPUTS WITH SHINYJS HIDDEN FIX ──
        shinyjs::hidden(
          div(
            id = "filter_panel",
            fluidRow(
              column(3, pickerInput("stressor", "Stressor Name",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              )),
              column(3, pickerInput("stressor_metric", "Stressor Metric",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              )),
              column(3, pickerInput("species", "Species Common Name",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              )),
              column(3, pickerInput("broad_stressor_name", "Broad Stressor Name",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              ))
            ),
            fluidRow(
              column(3, pickerInput("life_stage", "Life Stage",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              )),
              column(3, pickerInput("activity", "Activity",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              )),
              column(3, pickerInput("latin_name", "Latin Name",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              ))
            ),
            fluidRow(
              column(3, pickerInput("article_type", "Article Type",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              )),
              column(3, pickerInput("location_country", "Country",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              )),
              column(3, pickerInput("location_state_province", "State / Province",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              )),
              column(3, pickerInput("location_watershed_lab", "Watershed / Lab",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              ))
            ),
            fluidRow(
              column(3, pickerInput("location_river_creek", "River / Creek",
                choices = list(), multiple = TRUE,
                options = list("actions-box" = TRUE, "live-search" = TRUE)
              ))
            ),
            fluidRow(
              column(12, div(
                style = "text-align: right;",
                actionLink("reset_filters", "Reset Filters",
                  style = "color: #005f9e; font-size: 14px; text-decoration: underline; margin-right: 10px;",
                  "aria-label" = "Reset all selected search filters"
                )
              ))
            )
          )
        ),

        # ── 1. Top Pagination Controls ──
        fluidRow(
          column(12, align = "center",
            style = "margin-top: 10px; margin-bottom: 15px;",
            
            selectInput("page_size", "Articles per page:", 
                        choices = c(5, 10, 25, 50), 
                        selected = 10, 
                        width = "150px"),
            br(),
            
            actionButton("prev_page_top", "← Previous", class = "btn-primary btn-sm", "aria-label" = "Go to previous page"),
            span(textOutput("page_info_top", inline = TRUE), style = "margin: 0 15px; font-weight: bold;"),
            actionButton("next_page_top", "Next →", class = "btn-primary btn-sm", "aria-label" = "Go to next page")
          )
        ),
        
        # Removed icon("download")
        actionButton(
          "open_download_modal", 
          "Download Data", 
          class = "btn-primary"
        ),

        # Component for displaying the papers
        fluidRow(
          column(8, offset = 2, uiOutput("selection_action_bar")),
          column(6, offset = 3, uiOutput("paper_cards"))
        ),
        br(), br(),
        
        # ── 2. Bottom Pagination Controls ──
        fluidRow(
          column(12, align = "center",
            style = "margin-top: 20px; margin-bottom: 20px;",
            
            actionButton("prev_page_bottom", "← Previous", class = "btn-primary", "aria-label" = "Go to previous page (bottom)"),
            span(textOutput("page_info_bottom", inline = TRUE), style = "margin: 0 15px; font-weight: bold;"),
            actionButton("next_page_bottom", "Next →", class = "btn-primary", "aria-label" = "Go to next page (bottom)")
          )
        ),
        
        # Back to Top floating button using text arrow instead of font icon
        tags$div(
          id = "back_to_top_fab",
          actionButton(
            inputId = "back_to_top",
            label = "↑",
            "aria-label" = "Back to top of page",
            class = "btn btn-default rounded-circle",
            style = "width: 50px; height: 50px; font-size: 26px; font-weight: bold; line-height: 1; padding: 0;"
          )
        ),
        tags$script(HTML("
            $(document).on('click', '#back_to_top', function() {
              $('html, body').animate({ scrollTop: 0 }, 'slow');
            });
          ")),
        tags$script(HTML("
          $(document).on('click', '#next_page_top, #prev_page_top, #next_page_bottom, #prev_page_bottom', function() {
            $('html, body').animate({ scrollTop: 0 }, 'smooth');
          });
        "))
      )
    )
  ),

  # Submit a Relationship Tab
  tabPanel(
    title = "Submit a Relationship",
    value = "submit_relationship",
    submit_relationship_ui("submit_relationship")
  )
)
# nolint end
