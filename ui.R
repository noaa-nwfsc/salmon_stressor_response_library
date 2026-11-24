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
        
        # ── VirtualSelect Custom Reset Handler ──────────────────────────────
        tags$script(HTML("
          $(document).on('shiny:connected', function() {
            Shiny.addCustomMessageHandler('reset-all-virtual-selects', function(message) {
              if (message.ids && Array.isArray(message.ids)) {
                message.ids.forEach(function(id) {
                  var el = document.getElementById(id);
                  if (el && el.reset) {
                    el.reset(); // Clears VirtualSelect values & selection tags on the client
                  }
                });
              }
            });
          });
        ")),
        
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

          $(document).ready(function() {
            // 1. Fix bootstrap-select tabindex='null' & remove tabindex='0' from giant tab containers
            function fixAccessibilityTabs() {
              // Remove tabindex from giant layout containers (fixes Tab #3 reading the whole page)
              $('.tab-pane, #filter_panel, .well').removeAttr('tabindex');

              // Fix pickerInput buttons that get tabindex='null' after selection/update
              $('.bootstrap-select > .dropdown-toggle').each(function() {
                var ti = $(this).attr('tabindex');
                if (!ti || ti === 'null') {
                  $(this).attr('tabindex', '0');
                }
              });

              // Ensure the hidden native <select> inside pickerInput doesn't trap tabbing
              $('.bootstrap-select > select').attr('tabindex', '-1');
            }

              // Run initially and whenever Shiny updates inputs, switches tabs, or renders UI
              setTimeout(fixAccessibilityTabs, 300);
              $(document).on('shown.bs.tab shiny:value shiny:inputchanged shiny:bound loaded.bs.select refreshed.bs.select changed.bs.select', function() {
                setTimeout(fixAccessibilityTabs, 50);
            });

            // 2. Make Article Popups (Modals) announce properly to Screen Readers
            var lastFocusedElement = null;
            $(document).on('show.bs.modal', '.modal', function() {
              lastFocusedElement = document.activeElement;
            });

            $(document).on('shown.bs.modal', '.modal', function() {
              var $modal = $(this);
              $modal.attr({
                'role': 'dialog',
                'aria-modal': 'true'
              });

              // Find the article title (h3) or modal-title and link it via aria-labelledby
              var $heading = $modal.find('h3, .modal-title').first();
              if ($heading.length) {
                if (!$heading.attr('id')) {
                  $heading.attr('id', 'modal-article-heading');
                }
                $modal.attr('aria-labelledby', $heading.attr('id'));
                
                // Move screen reader focus directly to the article title so it reads aloud immediately!
                $heading.attr('tabindex', '-1').focus();
              } else {
                $modal.find('.modal-content').attr('tabindex', '-1').focus();
              }
            });

            // Return keyboard focus back to the article card button when the popup closes
            $(document).on('hidden.bs.modal', '.modal', function() {
              if (lastFocusedElement) {
                $(lastFocusedElement).focus();
              }
            });
          });
        ")),
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
        
        # ── VIRTUAL SELECT INPUTS (BS5-READY & ACCESSIBLE) ──
        shinyjs::hidden(
          div(
            id = "filter_panel",
            fluidRow(
              column(3, virtualSelectInput("stressor", "Stressor Name",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              )),
              column(3, virtualSelectInput("stressor_metric", "Stressor Metric",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              )),
              column(3, virtualSelectInput("species", "Species Common Name",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              )),
              column(3, virtualSelectInput("broad_stressor_name", "Broad Stressor Name",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              ))
            ),
            fluidRow(
              column(3, virtualSelectInput("life_stage", "Life Stage",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              )),
              column(3, virtualSelectInput("activity", "Activity",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              )),
              column(3, virtualSelectInput("latin_name", "Latin Name",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              ))
            ),
            fluidRow(
              column(3, virtualSelectInput("article_type", "Article Type",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              )),
              column(3, virtualSelectInput("location_country", "Country",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              )),
              column(3, virtualSelectInput("location_state_province", "State / Province",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              )),
              column(3, virtualSelectInput("location_watershed_lab", "Watershed / Lab",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
              ))
            ),
            fluidRow(
              column(3, virtualSelectInput("location_river_creek", "River / Creek",
                choices = NULL, multiple = TRUE, search = TRUE, width = "100%",
                showValueAsTags = FALSE, optionsCount = 6
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
