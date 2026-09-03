# modules/admin_review.R
# nolint start
library(shiny)
library(DT)
library(DBI)
library(jsonlite)

admin_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Admin Review Queue", style = "color: #2c3e50; margin-bottom: 20px;"),
    p("Review pending user submissions below. Click 'Review' to edit metadata and approve or reject the submission."),
    DTOutput(ns("staging_table"))
  )
}

admin_review_server <- function(id, db_conn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── 1. Fetch Pending Submissions ──
    refresh_trigger <- reactiveVal(0)
    
    staging_data <- reactive({
      refresh_trigger()
      dbGetQuery(db_conn, "
        SELECT 
          staging_id, submitter_name, submitter_email, 
          submission_date, title, status 
        FROM staging_submissions 
        WHERE status = 'Pending'
        ORDER BY submission_date DESC
      ")
    })
    
    # ── 2. Render the Table with Action Buttons ──
    output$staging_table <- renderDT({
      df <- staging_data()
      
      if (nrow(df) > 0) {
        df$Actions <- sprintf(
          '<button id="review_btn_%s" type="button" class="btn btn-primary btn-sm" onclick="Shiny.setInputValue(\'%s\', %s, {priority: \'event\'})">Review</button>',
          df$staging_id, ns("review_click"), df$staging_id
        )
      } else {
        df$Actions <- character(0)
      }
      
      datatable(
        df[, c("staging_id", "submission_date", "submitter_name", "title", "Actions")],
        escape = FALSE, 
        selection = "none",
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE)
      )
    })
    
    # ── 3. Handle Review Button Click (Open Massive Modal) ──
    observeEvent(input$review_click, {
      req(input$review_click)
      sid <- input$review_click
      
      sub_data <- dbGetQuery(db_conn, "SELECT * FROM staging_submissions WHERE staging_id = $1", params = list(sid))
      req(nrow(sub_data) > 0)
      sub_row <- sub_data[1, ]
      
      # Helpers to handle strings and RPostgres list-arrays cleanly
      safe_val <- function(v) if (is.null(v) || is.na(v)) "" else as.character(v)
      safe_arr <- function(v) {
        if (is.null(v) || is.na(v) || length(v) == 0) return(character(0))
        if (is.list(v)) v <- unlist(v)
        # Fallback if it comes as a raw "{a,b}" string instead of a parsed list
        if (is.character(v) && length(v) == 1 && grepl("^\\{.*\\}$", v)) {
          v <- strsplit(gsub("^\\{|\\}$|\"", "", v), ",")[[1]]
        }
        return(trimws(v))
      }
      
      # Arrays
      arr_species <- safe_arr(sub_row$species_common_name)
      arr_latin <- safe_arr(sub_row$latin_name)
      arr_life <- safe_arr(sub_row$life_stages)
      arr_country <- safe_arr(sub_row$location_country)
      arr_state <- safe_arr(sub_row$location_state_province)
      arr_deriv <- safe_arr(sub_row$function_derivation)

      showModal(modalDialog(
        title = paste("Reviewing Submission ID:", sid, "| From:", safe_val(sub_row$submitter_name)),
        size = "xl",
        easyClose = FALSE,
        
        tagList(
          if (safe_val(sub_row$submitter_notes) != "") {
            div(style = "background-color: #fff3cd; padding: 15px; border-left: 5px solid #ffeeba; margin-bottom: 20px;",
                strong("Submitter Notes:"), p(sub_row$submitter_notes))
          },
          
          # ── Core Metadata ──
          h4("Article & Function Metadata", style = "border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
          fluidRow(
            column(12, textInput(ns("rev_title"), "Article Title *", value = safe_val(sub_row$title), width = "100%"))
          ),
          fluidRow(
            column(6, textInput(ns("rev_article_type"), "Article Type *", value = safe_val(sub_row$article_type), width = "100%")),
            column(6, textInput(ns("rev_response"), "Response *", value = safe_val(sub_row$response), width = "100%"))
          ),
          
          # ── Stressor & Species ──
          h4("Stressor & Species", style = "margin-top: 20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
          fluidRow(
            column(4, textInput(ns("rev_stressor"), "Stressor Name *", value = safe_val(sub_row$stressor_name), width = "100%")),
            column(4, textInput(ns("rev_broad_stressor"), "Broad Stressor Name *", value = safe_val(sub_row$broad_stressor_name), width = "100%")),
            column(4, textInput(ns("rev_metric"), "Specific Stressor Metric *", value = safe_val(sub_row$specific_stressor_metric), width = "100%"))
          ),
          fluidRow(
            column(4, selectizeInput(ns("rev_species"), "Species Common Name *", choices = arr_species, selected = arr_species, multiple = TRUE, options = list(create = TRUE), width = "100%")),
            column(4, selectizeInput(ns("rev_latin"), "Latin Name *", choices = arr_latin, selected = arr_latin, multiple = TRUE, options = list(create = TRUE), width = "100%")),
            column(4, selectizeInput(ns("rev_life"), "Life Stages", choices = arr_life, selected = arr_life, multiple = TRUE, options = list(create = TRUE), width = "100%"))
          ),
          
          # ── Location & Descriptions ──
          h4("Location & Overviews", style = "margin-top: 20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
          fluidRow(
            column(6, selectizeInput(ns("rev_country"), "Country *", choices = arr_country, selected = arr_country, multiple = TRUE, options = list(create = TRUE), width = "100%")),
            column(6, selectizeInput(ns("rev_state"), "State / Province", choices = arr_state, selected = arr_state, multiple = TRUE, options = list(create = TRUE), width = "100%"))
          ),
          fluidRow(
            column(12, selectizeInput(ns("rev_deriv"), "Function Derivation", choices = arr_deriv, selected = arr_deriv, multiple = TRUE, options = list(create = TRUE), width = "100%"))
          ),
          fluidRow(
            column(12, textAreaInput(ns("rev_overview"), "Overview Description *", value = safe_val(sub_row$overview), height = "100px", width = "100%")),
            column(12, textAreaInput(ns("rev_transfer"), "Transferability of Function", value = safe_val(sub_row$transferability_of_function), height = "60px", width = "100%")),
            column(12, textAreaInput(ns("rev_formula"), "SRF Formula (LaTeX allowed)", value = safe_val(sub_row$srf_formula), height = "60px", width = "100%"))
          ),
          
          # ── Confidence Rankings ──
          h4("Confidence Rankings", style = "margin-top: 20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
          fluidRow(
            column(6, textInput(ns("rev_conf_source"), "Data Source", value = safe_val(sub_row$conf_source), width = "100%")),
            column(6, textInput(ns("rev_conf_shape"), "Shape of SR Function", value = safe_val(sub_row$conf_shape), width = "100%")),
            column(6, textInput(ns("rev_conf_var"), "Data Variance/Consistency", value = safe_val(sub_row$conf_variance), width = "100%")),
            column(6, textInput(ns("rev_conf_app"), "Applicability to System", value = safe_val(sub_row$conf_applicability), width = "100%")),
            column(6, textInput(ns("rev_conf_int"), "Potential Stressor Interactions", value = safe_val(sub_row$conf_interactions), width = "100%"))
          )
        ),
        
        footer = tagList(
          actionButton(ns("reject_sub"), "❌ Reject", class = "btn-danger", style = "float: left;"),
          modalButton("Cancel"),
          actionButton(ns("approve_sub"), "✅ Approve & Publish", class = "btn-success")
        )
      ))
      
      session$userData$current_review_id <- sid
      session$userData$current_review_data <- sub_row
    })
    
    # ── 4. Approve & Publish Logic ──
    observeEvent(input$approve_sub, {
      sid <- session$userData$current_review_id
      sub_row <- session$userData$current_review_data
      
      # Array formatting helper for PG
      to_pg_array <- function(val) {
        if (is.null(val) || length(val) == 0) return(NA_character_)
        parts <- unlist(lapply(val, function(x) trimws(strsplit(x, ",")[[1]])))
        parts <- parts[parts != ""]
        if (length(parts) == 0) return(NA_character_)
        paste0("{", paste(sprintf('"%s"', gsub('"', '\\"', parts, fixed = TRUE)), collapse = ","), "}")
      }
      
      tryCatch({
        # Generate new article_id
        max_id_res <- dbGetQuery(db_conn, "SELECT MAX(article_id) as max_id FROM stressor_responses")
        new_article_id <- if(is.na(max_id_res$max_id[1])) 1 else as.integer(max_id_res$max_id[1] + 1)
        
        # INSERT full data into live stressor_responses
        query_insert <- "
          INSERT INTO stressor_responses (
            article_id, title, article_type, stressor_name, broad_stressor_name, specific_stressor_metric,
            response, species_common_name, latin_name, life_stages,
            location_country, location_state_province, overview, function_derivation, transferability_of_function, srf_formula,
            conf_source, conf_shape, conf_variance, conf_applicability, conf_interactions, citations, user_id
          ) VALUES (
            $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22::jsonb, 1
          )
        "
        
        # Safe handling for JSON citations in case they are null
        cit_json <- if (is.null(sub_row$citations) || is.na(sub_row$citations)) "[]" else as.character(sub_row$citations)

        dbExecute(db_conn, query_insert, params = list(
          new_article_id, input$rev_title, input$rev_article_type, input$rev_stressor, input$rev_broad_stressor, input$rev_metric,
          input$rev_response, to_pg_array(input$rev_species), to_pg_array(input$rev_latin), to_pg_array(input$rev_life),
          to_pg_array(input$rev_country), to_pg_array(input$rev_state), input$rev_overview, to_pg_array(input$rev_deriv), input$rev_transfer, input$rev_formula,
          input$rev_conf_source, input$rev_conf_shape, input$rev_conf_var, input$rev_conf_app, input$rev_conf_int, cit_json
        ))
        
        # Move CSV Data over
        dbExecute(db_conn, "
          INSERT INTO csv_data (article_id, row_index, curve_id, stressor_label, stressor_x, units_x, response_label, response_y, units_y, plot_type, stressor_value, lower_limit, upper_limit, sd)
          SELECT $1, row_index, curve_id, stressor_label, stressor_x, units_x, response_label, response_y, units_y, plot_type, stressor_value, lower_limit, upper_limit, sd
          FROM staging_csv_data WHERE staging_id = $2
        ", params = list(new_article_id, sid))
        
        # Mark as Approved in staging table
        dbExecute(db_conn, "UPDATE staging_submissions SET status = 'Approved' WHERE staging_id = $1", params = list(sid))
        
        removeModal()
        showNotification(paste("✅ Submission published successfully! Assigned Article ID:", new_article_id), type = "message")
        refresh_trigger(refresh_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("❌ Error publishing:", e$message), type = "error")
      })
    })
    
    # ── 5. Reject Logic ──
    observeEvent(input$reject_sub, {
      sid <- session$userData$current_review_id
      tryCatch({
        dbExecute(db_conn, "UPDATE staging_submissions SET status = 'Rejected' WHERE staging_id = $1", params = list(sid))
        removeModal()
        showNotification("Submission marked as Rejected.", type = "warning")
        refresh_trigger(refresh_trigger() + 1)
      }, error = function(e) {
        showNotification(paste("❌ Error rejecting:", e$message), type = "error")
      })
    })
    
  })
}
# nolint end
