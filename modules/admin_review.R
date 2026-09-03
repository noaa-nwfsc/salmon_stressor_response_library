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
    # We use a reactiveVal as a trigger to refresh the table after an action
    refresh_trigger <- reactiveVal(0)
    
    staging_data <- reactive({
      refresh_trigger() # Take dependency
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
        # Inject HTML buttons into the dataframe
        df$Actions <- sprintf(
          '<button id="review_btn_%s" type="button" class="btn btn-primary btn-sm" onclick="Shiny.setInputValue(\'%s\', %s, {priority: \'event\'})">Review</button>',
          df$staging_id, ns("review_click"), df$staging_id
        )
      } else {
        df$Actions <- character(0)
      }
      
      datatable(
        df[, c("staging_id", "submission_date", "submitter_name", "title", "Actions")],
        escape = FALSE, # Required to render the HTML buttons
        selection = "none",
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE)
      )
    })
    
    # ── 3. Handle Review Button Click (Open Modal) ──
    observeEvent(input$review_click, {
      req(input$review_click)
      sid <- input$review_click
      
      # Fetch the full row for this submission
      sub_data <- dbGetQuery(db_conn, "SELECT * FROM staging_submissions WHERE staging_id = $1", params = list(sid))
      req(nrow(sub_data) > 0)
      sub_row <- sub_data[1, ]
      
      # Helper to pre-fill arrays
      safe_val <- function(v) if (is.null(v) || is.na(v)) "" else as.character(v)
      
      showModal(modalDialog(
        title = paste("Reviewing Submission ID:", sid),
        size = "xl",
        easyClose = FALSE,
        
        tagList(
          # Submitter Notes Warning
          if (safe_val(sub_row$submitter_notes) != "") {
            div(style = "background-color: #fff3cd; padding: 15px; border-left: 5px solid #ffeeba; margin-bottom: 20px;",
                strong("Submitter Notes:"), p(sub_row$submitter_notes))
          },
          
          # Core Fields to Review (You can expand this to include all fields just like edit_article.R)
          fluidRow(
            column(12, textInput(ns("rev_title"), "Article Title *", value = safe_val(sub_row$title), width = "100%"))
          ),
          fluidRow(
            column(6, textInput(ns("rev_stressor"), "Stressor Name *", value = safe_val(sub_row$stressor_name), width = "100%")),
            column(6, textInput(ns("rev_response"), "Response *", value = safe_val(sub_row$response), width = "100%"))
          ),
          fluidRow(
            column(12, textAreaInput(ns("rev_overview"), "Overview", value = safe_val(sub_row$overview), height = "100px", width = "100%"))
          )
        ),
        
        footer = tagList(
          actionButton(ns("reject_sub"), "❌ Reject", class = "btn-danger", style = "float: left;"),
          modalButton("Cancel"),
          actionButton(ns("approve_sub"), "✅ Approve & Publish", class = "btn-success")
        )
      ))
      
      # Store current staging_id for the approve/reject observers
      session$userData$current_review_id <- sid
      session$userData$current_review_data <- sub_row
    })
    
    # ── 4. Approve & Publish Logic ──
    observeEvent(input$approve_sub, {
      sid <- session$userData$current_review_id
      sub_row <- session$userData$current_review_data
      
      tryCatch({
        # Generate new article_id
        max_id_res <- dbGetQuery(db_conn, "SELECT MAX(article_id) as max_id FROM stressor_responses")
        new_article_id <- if(is.na(max_id_res$max_id[1])) 1 else as.integer(max_id_res$max_id[1] + 1)
        
        # INSERT into live stressor_responses
        query_insert <- "
          INSERT INTO stressor_responses (
            article_id, title, stressor_name, response, overview,
            user_id -- (We default to 1 or lookup based on admin taking action)
          ) VALUES ($1, $2, $3, $4, $5, 1)
        "
        dbExecute(db_conn, query_insert, params = list(
          new_article_id, input$rev_title, input$rev_stressor, input$rev_response, input$rev_overview
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
        refresh_trigger(refresh_trigger() + 1) # Reload table
        
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
        refresh_trigger(refresh_trigger() + 1) # Reload table
      }, error = function(e) {
        showNotification(paste("❌ Error rejecting:", e$message), type = "error")
      })
    })
    
  })
}
# nolint end
