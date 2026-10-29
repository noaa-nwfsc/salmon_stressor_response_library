# nolint start
library(shiny)
library(openxlsx)
library(DBI)
library(DT)
library(pool)

bulk_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3(icon("layer-group"), " Bulk Literature Upload"),
        p("Upload multiple stressor-response functions at once using the standardized Excel template. The workbook must contain two sheets: ", strong("Metadata"), " and ", strong("Curve_Data"), "."),
        downloadButton(ns("download_template"), "Download Excel Template", class = "btn-info"),
        hr(),
        fileInput(ns("bulk_file"), "Upload Completed Template (.xlsx)", accept = c(".xlsx")),
        uiOutput(ns("staging_area"))
      )
    )
  )
}

bulk_upload_server <- function(id, db_conn, current_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── 1. Generate Blank Template ───────────────────────────────────────────
    output$download_template <- downloadHandler(
      filename = function() {
        paste0("Bulk_Upload_Template_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- createWorkbook()
        
        # Metadata Sheet (Matches Schema Exactly)
        addWorksheet(wb, "Metadata")
        meta_cols <- c("temp_id", "title", "citations", "article_type", 
                       "response", "broad_stressor_name", "stressor_name", "specific_stressor_metric",
                       "species_common_name", "latin_name", "life_stages", 
                       "activity", "season", "location_country", "location_state_province", 
                       "location_watershed_lab", "location_river_creek", "function_derivation", 
                       "overview", "transferability_of_function", 
                       "srf_formula", "source_of_stressor_data", "conf_source", "conf_shape", 
                       "conf_variance", "conf_applicability", "conf_interactions")
        writeData(wb, "Metadata", matrix(ncol = length(meta_cols), nrow = 0, dimnames = list(NULL, meta_cols)))
        
        # Curve Data Sheet
        addWorksheet(wb, "Curve_Data")
        data_cols <- c("temp_id", "curve_id", "stressor_label", "stressor_x", "units_x", 
                       "response_label", "response_y", "units_y", "stressor_value", 
                       "lower_limit", "upper_limit", "sd", "plot_type")
        writeData(wb, "Curve_Data", matrix(ncol = length(data_cols), nrow = 0, dimnames = list(NULL, data_cols)))
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # ── 2. Read and Validate Uploaded File ───────────────────────────────────
    staged_data <- reactiveVal(NULL)
    
    observeEvent(input$bulk_file, {
      req(input$bulk_file)
      
      tryCatch({
        sheets <- getSheetNames(input$bulk_file$datapath)
        
        if (!all(c("Metadata", "Curve_Data") %in% sheets)) {
          showNotification("Invalid file structure. Must contain 'Metadata' and 'Curve_Data' sheets.", type = "error")
          return()
        }
        
        meta_df <- read.xlsx(input$bulk_file$datapath, sheet = "Metadata")
        data_df <- read.xlsx(input$bulk_file$datapath, sheet = "Curve_Data")
        
        if (!"temp_id" %in% names(meta_df) || !"temp_id" %in% names(data_df)) {
          showNotification("Both sheets must contain a 'temp_id' column to link the data.", type = "error")
          return()
        }
        
        # Duplicate Checking Logic using title
        existing_titles <- dbGetQuery(db_conn, "SELECT title FROM stressor_responses")$title
        meta_df$is_duplicate <- sapply(meta_df$title, function(t) {
          if (is.na(t) || t == "") return(FALSE)
          any(agrep(tolower(t), tolower(existing_titles), max.distance = 0.1, ignore.case = TRUE))
        })
        
        meta_df$data_rows_found <- sapply(meta_df$temp_id, function(id) {
          sum(data_df$temp_id == id, na.rm = TRUE)
        })
        
        staged_data(list(meta = meta_df, data = data_df))
        
      }, error = function(e) {
        showNotification(paste("Error reading Excel file:", e$message), type = "error")
      })
    })
    
    # ── 3. Render Staging UI ─────────────────────────────────────────────────
    output$staging_area <- renderUI({
      req(staged_data())
      meta <- staged_data()$meta
      
      num_ready <- sum(!meta$is_duplicate & meta$data_rows_found >= 1)
      num_dupes <- sum(meta$is_duplicate)
      num_missing_data <- sum(meta$data_rows_found < 1)
      
      tagList(
        hr(),
        h4("Validation Report"),
        fluidRow(
          column(4, div(class = "alert alert-success", paste(num_ready, "Ready to Upload"))),
          column(4, div(class = "alert alert-warning", paste(num_dupes, "Potential Duplicates Found"))),
          column(4, div(class = "alert alert-danger", paste(num_missing_data, "Missing Required Curve Data (Min 1 row)"))),
        ),
        DTOutput(ns("staging_table")),
        br(),
        actionButton(ns("commit_bulk"), "Commit Valid Records to Database", class = "btn-success btn-lg text-white", icon = icon("database"))
      )
    })
    
    output$staging_table <- renderDT({
      req(staged_data())
      meta <- staged_data()$meta
      
      # Format table for review
      df_display <- meta[, c("temp_id", "title", "species_common_name", "stressor_name", "data_rows_found", "is_duplicate")]
      
      datatable(df_display, 
                options = list(pageLength = 10, dom = 'tp'), 
                selection = 'none',
                rownames = FALSE) %>%
        formatStyle(
          'is_duplicate',
          target = 'row',
          backgroundColor = styleEqual(c(TRUE), c('#fff3cd'))
        ) %>%
        formatStyle(
          'data_rows_found',
          target = 'row',
          backgroundColor = styleInterval(c(0.9), c('#f8d7da', 'transparent'))
        )
    })
    
    # ── 4. Commit to Database ────────────────────────────────────────────────
    observeEvent(input$commit_bulk, {
      req(staged_data())
      meta <- staged_data()$meta
      curve_data <- staged_data()$data
      
      valid_meta <- meta[!meta$is_duplicate & meta$data_rows_found >= 1, ]
      
      if (nrow(valid_meta) == 0) {
        showNotification("No valid records to commit.", type = "warning")
        return()
      }
      
      showNotification(paste("Beginning upload of", nrow(valid_meta), "records..."), type = "message")
      
      conn <- pool::poolCheckout(db_conn)
      on.exit(pool::poolReturn(conn), add = TRUE)
      
      tryCatch({
        dbBegin(conn)
        
        # All columns required as text[] (_text) by the schema
        array_cols <- c("species_common_name", "latin_name", "life_stages", 
                        "activity", "season", "location_country", 
                        "location_state_province", "location_watershed_lab", 
                        "location_river_creek", "function_derivation")
        
        for (i in 1:nrow(valid_meta)) {
          row_meta <- valid_meta[i, ]
          t_id <- row_meta$temp_id
          
          # Remove staging helper columns
          cols_to_remove <- c("temp_id", "is_duplicate", "data_rows_found")
          insert_meta <- row_meta[, !names(row_meta) %in% cols_to_remove, drop = FALSE]
          
          insert_meta$user_id <- current_user
          
          # Parse comma-separated Excel strings into PostgreSQL arrays
          for (col in array_cols) {
            if (col %in% names(insert_meta)) {
              val <- insert_meta[[col]]
              if (is.na(val) || trimws(val) == "") {
                insert_meta[[col]] <- "{}" 
              } else {
                parts <- trimws(strsplit(as.character(val), ",")[[1]])
                insert_meta[[col]] <- paste0('{', paste(sprintf('"%s"', parts), collapse = ","), '}')
              }
            }
          }
          
          if ("citations" %in% names(insert_meta)) {
             cit_val <- insert_meta[["citations"]]
             if (is.na(cit_val) || trimws(cit_val) == "") {
                 insert_meta[["citations"]] <- "[]"
             }
          }
          
          dbAppendTable(conn, "stressor_responses", insert_meta)
          
          # Fetch the auto-generated article_id based on title
          new_id_query <- "SELECT article_id FROM stressor_responses WHERE title = $1 ORDER BY article_id DESC LIMIT 1"
          new_article_id <- dbGetQuery(conn, new_id_query, params = list(insert_meta$title))$article_id
          
          # Isolate curve data for this relationship
          subset_curve <- curve_data[curve_data$temp_id == t_id, ]
          subset_curve$article_id <- new_article_id
          subset_curve$row_index <- 1:nrow(subset_curve)
          subset_curve$temp_id <- NULL
          
          dbAppendTable(conn, "csv_data", subset_curve)
        }
        
        dbCommit(conn)
        showNotification("Upload complete! Refresh the dashboard to see new data.", type = "message")
        staged_data(NULL)
        shinyjs::reset("bulk_file")
        
      }, error = function(e) {
        dbRollback(conn)
        showNotification(paste("Database commit failed. No changes were made.", e$message), type = "error")
      })
    })
    
  })
}
# nolint end
