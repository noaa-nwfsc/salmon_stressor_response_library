# nolint start
render_papers_server <- function(output, paginated_data, input, session) {
  output$paper_cards <- renderUI({
    data_to_display <- paginated_data()

    format_field <- function(label, val, bold = FALSE) {
      # Expanded the check to catch "N/A" and length-0 vectors just in case
      display <- ifelse(is.null(val) || length(val) == 0 || is.na(val) || val == "NA" || val == "N/A", "", val)
      if (display == "") {
        return("")
      }
      label_span <- sprintf("<span class='metadata-light' style='color: #666;'>%s:</span> ", htmltools::htmlEscape(label))
      value_span <- sprintf("<span class='metadata-bold' style='color: #222; font-weight: 500;'>%s</span>", htmltools::htmlEscape(display))
      
      # Added a slight bottom margin to give the items breathing room
      sprintf("<div class='paper-meta-item' style='margin-bottom: 6px; line-height: 1.3;'>%s%s</div>", label_span, value_span)
    }

    if (is.null(data_to_display) || nrow(data_to_display) == 0) {
      return(tags$p(
        "No research papers found.",
        style = "font-size: 18px; font-weight: bold; color: red;"
      ))
    }

    # Remove rows where all values are NA
    data_to_display <- data_to_display[rowSums(is.na(data_to_display)) != ncol(data_to_display), ]

    tagList(
      lapply(seq_len(nrow(data_to_display)), function(i) {
        paper <- data_to_display[i, ]

        article_url <- paste0("?article_id=", paper$article_id)
        checkbox_id <- paste0("select_article_", paper$article_id)
        badge_id    <- paste0("card_badge_", paper$article_id)
        meta_id     <- paste0("card_meta_", paper$article_id)

        # Safely pull the userid for the badge
        contributor <- if ("contributor_name" %in% names(paper) && !is.na(paper$contributor_name)) paper$contributor_name else "UNKNOWN"

        div(
          class = "paper-card",
          role = "article",
          "aria-labelledby" = paste0("view_article_", paper$article_id),

          # 1. THE CONTRIBUTOR BADGE (Given an ID so the button can read it)
          tags$span(
            id = badge_id,
            class = "contributor-badge", 
            paste("Entry by:", contributor)
          ),

          # 2. THE CHECKBOX
          div(
            class = "paper-checkbox-container",
            tags$div(
              class = "form-group shiny-input-container",
              style = "width: 40px; margin-bottom: 0; display: flex; justify-content: center;",
              tags$div(
                class = "checkbox",
                style = "margin: 0;",
                tags$label(
                  "for" = checkbox_id,
                  style = "padding: 10px; margin: 0; cursor: pointer; display: flex; align-items: center; justify-content: center;", 
                  tags$input(
                    id = checkbox_id,
                    type = "checkbox",
                    class = "shiny-input-checkbox",
                    style = "position: static; margin: 0;"
                  ),
                  tags$span(
                    style = "position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0, 0, 0, 0); border: 0;",
                    paste("Select profile", paper$article_id, paper$title)
                  )
                )
              )
            )
          ),

          # 3. THE MAIN CONTENT
          div(
            class = "paper-content",
            
            # Title Button (Linked to the metadata and badge via aria-describedby)
            div(
              style = "margin-bottom: 15px; width: 100%; border-bottom: 2px solid #f0f0f0; padding-bottom: 10px;", 
              actionButton(
                inputId = paste0("view_article_", paper$article_id),
                label = paste0(paper$article_id, ". ", paper$title),
                class = "paper-card-title btn-link",
                "aria-describedby" = paste(badge_id, meta_id),
                style = "display: block; text-align: left; padding: 0; white-space: normal !important; word-break: break-word; max-width: calc(100% - 160px); border: none; background: none; font-size: 1.15em; font-weight: bold; color: #0056b3;"
              )
            ),

            # ── NEW SECTIONED METADATA LAYOUT (Wrapped with id = meta_id) ──
            fluidRow(
              id = meta_id,
              style = "font-size: 0.95em;",
              
              # Section 1: Biological & Study Info
              column(4,
                div(style = "border-right: 1px solid #eaeaea; padding-right: 15px; height: 100%;",
                  tags$strong("Biological Profile:", style = "color: #5577ab; display: block; border-bottom: 1px solid #eaeaea; padding-bottom: 4px; margin-bottom: 10px; text-transform: uppercase; font-size: 0.85em; letter-spacing: 0.5px;"),
                  HTML(format_field("Type", paper$article_type)),
                  HTML(format_field("Common Name", paper$species_common_name)),
                  HTML(format_field("Latin Name", paper$latin_name)),
                  HTML(format_field("Life Stage", paper$life_stages)),
                  HTML(format_field("Activity", paper$activity))
                )
              ),
              
              # Section 2: Stressor Details
              column(4,
                div(style = "border-right: 1px solid #eaeaea; padding-left: 5px; padding-right: 15px; height: 100%;",
                  tags$strong("Stressor & Response:", style = "color: #5577ab; display: block; border-bottom: 1px solid #eaeaea; padding-bottom: 4px; margin-bottom: 10px; text-transform: uppercase; font-size: 0.85em; letter-spacing: 0.5px;"),
                  HTML(format_field("Stressor", paper$stressor_name)),
                  HTML(format_field("Stressor Metric", paper$specific_stressor_metric)),
                  HTML(format_field("Response", paper$response))
                )
              ),
              
              # Section 3: Location
              column(4,
                div(style = "padding-left: 5px; height: 100%;",
                  tags$strong("Location:", style = "color: #5577ab; display: block; border-bottom: 1px solid #eaeaea; padding-bottom: 4px; margin-bottom: 10px; text-transform: uppercase; font-size: 0.85em; letter-spacing: 0.5px;"),
                  HTML(format_field("Country", paper$location_country)),
                  HTML(format_field("State/Province", paper$location_state_province)),
                  HTML(format_field("Watershed/Lab", paper$location_watershed_lab)),
                  HTML(format_field("River/Creek", paper$location_river_creek))
                )
              )
            )
          )
        )
      })
    )
  })

  # Sync all checkboxes with "Select All"
  observeEvent(input$select_all, {
    ids <- paginated_data()$article_id
    for (mid in ids) {
      updateCheckboxInput(
        session,
        inputId = paste0("select_article_", mid),
        value = input$select_all
      )
    }
  })
}
# nolint end
