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
        
        # 1. Instructions / Readme Sheet
        addWorksheet(wb, "Instructions")
        
          instructions_df <- data.frame(
          Field_or_Topic = c(
            "General Rules",
            "temp_id (Crucial)",
            "citations",
            "Array Columns (Species, Location, etc.)",
            "srf_formula",
            "Missing Data"
          ),
          Formatting_Guide = c(
            "Fill out both 'Metadata' and 'Curve_Data' sheets. Do NOT alter or delete the column headers.",
            "This number links the two sheets. If a relationship has temp_id = 1 in the Metadata sheet, all of its curve points must also have temp_id = 1 in the Curve_Data sheet.",
            'Must be formatted as a JSON array of objects. Example: [{"title":"Bjornn & Reiser 1991","url":"https://www.for.gov.bc.ca/hfd/LIBRARY/FFIP/Bjornn_TC1991.pdf","text":"Bjornn, T. C., & Reiser, D. W. (1991). Habitat requirements of salmonids in streams. In American Fisheries Society Special Publication (Vol. 19, pp. 83–138)."}] or for multiple citations [{"title":"Rempel et al. 2012","url":"https://publications.gc.ca/collections/collection_2014/mpo-dfo/Fs97-6-2991-eng.pdf","text":"Rempel, L. L. , Healey, K. , & Lewis, F. J. A. (2012). Lower Fraser River juvenile fish habitat suitability criteria. Ecosystem Management Branch, Fisheries and Oceans Canada."}]',
            "No special database syntax needed. Simply type values separated by commas. Example: Chinook Salmon, Coho Salmon",
            "Accepts standard text or LaTeX equations. If you want the equation to appear in LaTeX, wrap the equation in double dollar signs. Example: $$S = \\frac{1}{1 + e^{-(\\alpha + \\beta T)}}$$",
            "Leave cells blank for missing values. Do not type 'NA' or 'NULL'."
          ),
          stringsAsFactors = FALSE
        )
        
        # Write instructions and style them for readability
        header_style <- createStyle(textDecoration = "bold", bgFill = "#DCE6F1", border = "Bottom")
        writeData(wb, "Instructions", instructions_df, headerStyle = header_style)
        setColWidths(wb, "Instructions", cols = 1:2, widths = c(35, 120))
        
        # 2. Metadata Sheet
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
        
        # 3. Curve Data Sheet
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
        # ── NEW: Instructions and Override Button ──
        if (num_dupes > 0) tagList(
          p(icon("info-circle"), " If a record is falsely flagged as a duplicate, select the row(s) below and override it."),
          actionButton(ns("override_dupe"), "Override Selected Duplicates", class = "btn-warning btn-sm", icon = icon("unlock")),
          br(), br()
        ),
        DTOutput(ns("staging_table")),
        br(),
        actionButton(ns("commit_bulk"), "Commit Valid Records to Database", class = "btn-success btn-lg text-white", icon = icon("database"))
      )
    })
    
    output$staging_table <- renderDT({
      req(staged_data())
      meta <- staged_data()$meta
      
      df_display <- meta[, c("temp_id", "title", "species_common_name", "stressor_name", "data_rows_found", "is_duplicate")]
      
      # ── NEW: Changed selection to 'multiple' so rows can be clicked ──
      datatable(df_display, 
                options = list(pageLength = 10, dom = 'tp'), 
                selection = 'multiple',
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
    
    # ── NEW: Override Duplicate Observer ─────────────────────────────────────
    observeEvent(input$override_dupe, {
      selected_rows <- input$staging_table_rows_selected
      req(length(selected_rows) > 0)
      
      current_data <- staged_data()
      
      # Flip the is_duplicate flag to FALSE for the selected rows
      current_data$meta$is_duplicate[selected_rows] <- FALSE
      
      # Save the updated data back to the reactive value
      staged_data(current_data)
      showNotification("Selected rows marked as 'Ready to Upload'.", type = "message")
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
        user_parts <- strsplit(current_user, "\\.")[[1]]
        if (length(user_parts) == 2) {
          db_username <- paste0(substr(user_parts[1], 1, 1), user_parts[2])
        } else {
          db_username <- current_user 
        }
        
        uploader_id_df <- dbGetQuery(conn, "SELECT user_id FROM users WHERE name = $1 LIMIT 1", params = list(db_username))
        
        if (nrow(uploader_id_df) == 0) {
          showNotification(
            paste("Upload blocked: Your username (", db_username, ") is not registered in the database."), 
            type = "error", 
            duration = 10
          )
          return() 
        }
        
        actual_user_id <- uploader_id_df$user_id[1]
        
        dbBegin(conn)
        
        max_id_query <- "SELECT COALESCE(MAX(article_id), 0) AS max_id FROM stressor_responses;"
        current_max_id <- as.integer(dbGetQuery(conn, max_id_query)$max_id[1])
        
        array_cols <- c("species_common_name", "latin_name", "life_stages", 
                        "activity", "season", "location_country", 
                        "location_state_province", "location_watershed_lab", 
                        "location_river_creek", "function_derivation")
        
        for (i in 1:nrow(valid_meta)) {
          row_meta <- valid_meta[i, ]
          t_id <- row_meta$temp_id
          
          current_max_id <- current_max_id + 1L
          new_article_id <- current_max_id
          
          cols_to_remove <- c("temp_id", "is_duplicate", "data_rows_found")
          insert_meta <- row_meta[, !names(row_meta) %in% cols_to_remove, drop = FALSE]
          
          insert_meta$article_id <- as.integer(new_article_id)
          insert_meta$user_id <- as.integer(actual_user_id)
          
          for (col in array_cols) {
            if (col %in% names(insert_meta)) {
              val <- insert_meta[[col]]
              if (is.na(val) || trimws(val) == "") {
                insert_meta[[col]] <- "{}" 
              } else {
                parts <- trimws(strsplit(as.character(val), ",")[[1]])
                parts <- gsub('"', '\\"', parts, fixed = TRUE)
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
          
          col_names <- names(insert_meta)
          placeholders <- sapply(seq_along(col_names), function(idx) {
            c_name <- col_names[idx]
            if (c_name %in% array_cols) {
              paste0("$", idx, "::text[]")
            } else if (c_name %in% c("citations", "revision_log")) {
              paste0("$", idx, "::jsonb")
            } else if (c_name %in% c("user_id", "article_id")) {
              paste0("$", idx, "::integer")
            } else {
              paste0("$", idx)
            }
          })
          
          meta_sql <- sprintf(
            "INSERT INTO stressor_responses (%s) VALUES (%s);",
            paste(col_names, collapse = ", "),
            paste(placeholders, collapse = ", ")
          )
          
          dbExecute(conn, meta_sql, params = unname(as.list(insert_meta[1, ])))
          
          subset_curve <- curve_data[curve_data$temp_id == t_id, , drop = FALSE]
          subset_curve$article_id <- as.integer(new_article_id)
          subset_curve$row_index <- as.integer(1:nrow(subset_curve))
          subset_curve$temp_id <- NULL
          
          curve_cols <- names(subset_curve)
          curve_placeholders <- paste0("$", seq_along(curve_cols))
          curve_sql <- sprintf(
            "INSERT INTO csv_data (%s) VALUES (%s);",
            paste(curve_cols, collapse = ", "),
            paste(curve_placeholders, collapse = ", ")
          )
          
          for (r in 1:nrow(subset_curve)) {
            dbExecute(conn, curve_sql, params = unname(as.list(subset_curve[r, ])))
          }
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
