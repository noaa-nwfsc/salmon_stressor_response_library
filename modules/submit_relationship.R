# nolint start
source("modules/csv_validation.R")
source("modules/csv_template.R")
source("modules/file_validation.R")
source("modules/error_handling.R")
source("modules/customFileInput.R")

submit_relationship_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      includeCSS("www/custom.css")
    ),
    
    div(
      id = ns("submit_relationship_form"),
      
      fluidRow(
        column(8, offset = 2,
          h2("Submit a Relationship", style = "text-align: center; color: #6082B6;"),
          p("Use the form below to suggest a new relationship between a stressor and a response. This submission will be placed in a queue for our team to review before being published to the live database.", style = "text-align: center; color: #555;"),
          div(style = "text-align: center; margin-bottom: 25px;",
            actionButton(ns("show_readme"), "ℹ️ Formatting Guide (Read Me)", class = "btn-info")
          )
        )
      ),
      
      # ── Submitter Info ──
      fluidRow(
        column(8, offset = 2, h4("1. About You", style = "border-bottom: 1px solid #ddd; padding-bottom: 5px;"))
      ),
      fluidRow(
        column(4, offset = 2, textInput(ns("submitter_name"), "Full Name *", width = "100%")),
        column(4, textInput(ns("submitter_email"), "Email Address *", width = "100%"))
      ),
      fluidRow(
        column(8, offset = 2, textAreaInput(ns("submitter_notes"), "Notes for Reviewer", placeholder = "Describe why you are submitting this function...", height = "80px", width = "100%"))
      ),
      
      # ── Core Metadata ──
      fluidRow(
        column(8, offset = 2, h4("2. Article & Function Metadata", style = "margin-top: 20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"))
      ),
      fluidRow(
        column(8, offset = 2, textInput(ns("title"), "Article Title *", placeholder = "Format: Author et al. Year: Function description", width = "100%"),
        uiOutput(ns("title_warning"))
        )
      ),
      fluidRow(
        column(4, offset = 2, selectizeInput(ns("article_type"), "Article Type *", choices = NULL, options = list(create = TRUE), width = "100%")),
        column(4, selectizeInput(ns("response"), "Response *", choices = NULL, options = list(create = TRUE), width = "100%"))
      ),
      
      # ── CSV & PDF Uploads ──
      fluidRow(
        column(8, offset = 2, wellPanel(
          style = "background-color: #f9f9f9; border-color: #ccc; margin-top: 15px; margin-bottom: 25px;",
          strong("Data Uploads"),
          div(id = ns("csv_wrapper"), customFileInput(ns("sr_csv_file"), "Optional: Stressor-Response Curve Data CSV", accept = ".csv")),
          uiOutput(ns("csv_validation_status")),
          downloadButton(ns("download_csv_template"), "Download CSV Template", class = "btn btn-info mb-2"),
          hr(),
          div(id = ns("pdf_wrapper"), customFileInput(ns("supporting_pdf"), "Optional: Supporting PDF", accept = c(".pdf", "application/pdf")))
        ))
      ),
      
      # ── Stressor & Species ──
      fluidRow(
        column(4, offset = 2, selectizeInput(ns("stressor_name"), "Stressor Name *", choices = NULL, options = list(create = TRUE), width = "100%")),
        column(4, selectizeInput(ns("broad_stressor_name"), "Broad Stressor Name *", choices = NULL, options = list(create = TRUE), width = "100%"))
      ),
      fluidRow(
        column(4, offset = 2, selectizeInput(ns("specific_stressor_metric"), "Specific Stressor Metric *", choices = NULL, options = list(create = TRUE), width = "100%")),
        column(4, selectizeInput(ns("species_common_name"), "Species Common Name *", choices = NULL, multiple = TRUE, options = list(create = TRUE), width = "100%"))
      ),
      fluidRow(
        column(4, offset = 2, selectizeInput(ns("latin_name"), "Latin Name *", choices = NULL, multiple = TRUE, options = list(create = TRUE), width = "100%")),
        column(4, selectizeInput(ns("life_stages"), "Life Stages", choices = NULL, multiple = TRUE, options = list(create = TRUE), width = "100%"))
      ),
      
      # ── Location & Descriptions ──
      fluidRow(
        column(4, offset = 2, selectizeInput(ns("location_country"), "Country *", choices = NULL, multiple = TRUE, options = list(create = TRUE), width = "100%")),
        column(4, selectizeInput(ns("location_state_province"), "State / Province", choices = NULL, multiple = TRUE, options = list(create = TRUE), width = "100%"))
      ),
      fluidRow(
        column(8, offset = 2, selectizeInput(ns("function_derivation"), "Function Derivation", choices = NULL, multiple = TRUE, options = list(create = TRUE), width = "100%"))
      ),
      fluidRow(
        column(8, offset = 2, textAreaInput(ns("overview"), "Overview Description *", placeholder = "Describe the context of the study and how the function was derived...", height = "120px", width = "100%")),
        column(8, offset = 2, textAreaInput(ns("transferability_of_function"), "Transferability of Function", placeholder = "In what geographic regions or system types is this function applicable? How generalizable is the function? This can be a high-level description and doesn't have to be comprehensive", height = "80px", width = "100%")),
        column(8, offset = 2, textAreaInput(ns("srf_formula"), "SRF Formula (LaTeX allowed)", placeholder = "If this paper used a formula/equation to derive the function, describe it here. LaTeX formatting allowed. e.g., $$y = mx + b$$  OR  $$y = \\alpha e^{\\beta x}$$",  height = "80px", width = "100%"))
      ),
      
      # ── Confidence Rankings ──
      fluidRow(
        column(8, offset = 2, h4("3. Confidence Rankings", style = "margin-top: 20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"))
      ),
      fluidRow(
        column(8, offset = 2, textInput(ns("conf_source"), "Data Source", placeholder = "e.g., High (Primary empirical data), Low (Proxy species)", width = "100%")),
        column(8, offset = 2, textInput(ns("conf_shape"), "Shape of SR Function", placeholder = "e.g., High (Strong fit), Moderate (Wide confidence intervals)", width = "100%")),
        column(8, offset = 2, textInput(ns("conf_variance"), "Data Variance/Consistency", placeholder = "e.g., High consistency across multiple years", width = "100%")),
        column(8, offset = 2, textInput(ns("conf_applicability"), "Applicability to System", placeholder = "e.g., Specific to Puget Sound lowland streams", width = "100%")),
        column(8, offset = 2, textInput(ns("conf_interactions"), "Potential Stressor Interactions", placeholder = "e.g., Synergistic effects noted with low dissolved oxygen", width = "100%"))
      ),
      
      # ── Citations (Dynamic) ──
      fluidRow(
        column(8, offset = 2, h4("4. Citations *", style = "margin-top: 20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"))
      ),
      fluidRow(
        column(8, offset = 2,
          div(
            id = ns("citation_block_1"),
            style = "border: 1px solid #e3e3e3; padding: 15px; margin-bottom: 10px; border-radius: 5px; background-color: #fafafa;",
            textAreaInput(ns("citation_text_1"), "Citation 1 (Text)", placeholder = "e.g., Smith et al. (2020)...", height = "70px", width = "100%"),
            fluidRow(
              column(6, textInput(ns("citation_title_1"), "Link Title", placeholder = "e.g., Baker et al. 1995", width = "100%")),
              column(6, textInput(ns("citation_url_1"), "URL", placeholder = "https://doi.org/...", width = "100%"))
            )
          ),
          tags$div(id = ns("extra_citations_container")) 
        )
      ),
      fluidRow(
        column(8, offset = 2, actionButton(ns("add_citation"), "Add Another Citation", icon = icon("plus"), class = "btn-sm", style = "margin-bottom: 30px;"))
      ),
      
      # ── Submit Button ──
      fluidRow(
        column(8, offset = 2, align = "center",
          actionButton(ns("submit_relationship"), "Submit Relationship for Review", class = "btn-primary btn-lg", style = "width: 100%; margin-bottom: 50px; font-weight: bold; background-color: #0073e6; border-color: #0073e6;")
        )
      )
    )
  )
}

submit_relationship_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    db_conn <- pool
    
    # ── Formatting Guide Modal ──
    observeEvent(input$show_readme, {
      showModal(modalDialog(
        title = "Formatting & Metadata Guide",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          h4("Metadata Consistency"),
          p("To help maintain a clean database, please click the dropdown arrows first to see if your term already exists before typing a new one."),
          hr(),
          h4("Confidence Rankings"),
          p("For each of the 5 confidence categories, please assign a rank of ", strong("High, Moderate, or Low"), ", along with a brief explanation if needed."),
          tags$ul(
            tags$li(strong("Data Source:"), " How well the data represents the stressor/response of interest."),
            tags$li(strong("Shape:"), " How well-supported the functional curve form is."),
            tags$li(strong("Variance:"), " Consistency and noise in the underlying dataset."),
            tags$li(strong("Applicability:"), " Fit to the specific system in question."),
            tags$li(strong("Interactions:"), " Potential influence of unmeasured interacting stressors.")
          )
        )
      ))
    })

    # ── Populate Dropdowns from Live Database ──
    observe({
      get_distinct <- function(col) {
        tryCatch({
          res <- dbGetQuery(db_conn, sprintf("SELECT DISTINCT %s AS val FROM stressor_responses WHERE %s IS NOT NULL", col, col))
          c("", sort(res$val[res$val != ""]))
        }, error = function(e) "")
      }
      get_distinct_array <- function(col) {
        tryCatch({
          res <- dbGetQuery(db_conn, sprintf("SELECT DISTINCT unnest(%s) AS val FROM stressor_responses WHERE %s IS NOT NULL", col, col))
          sort(res$val[res$val != ""])
        }, error = function(e) character(0))
      }

      updateSelectizeInput(session, "article_type", choices = get_distinct("article_type"), server = FALSE, options = list(create = TRUE))
      updateSelectizeInput(session, "response", choices = get_distinct("response"), server = FALSE, options = list(create = TRUE))
      updateSelectizeInput(session, "stressor_name", choices = get_distinct("stressor_name"), server = FALSE, options = list(create = TRUE))
      updateSelectizeInput(session, "broad_stressor_name", choices = get_distinct("broad_stressor_name"), server = FALSE, options = list(create = TRUE))
      updateSelectizeInput(session, "specific_stressor_metric", choices = get_distinct("specific_stressor_metric"), server = FALSE, options = list(create = TRUE))

      updateSelectizeInput(session, "species_common_name", choices = get_distinct_array("species_common_name"), server = FALSE, options = list(create = TRUE))
      updateSelectizeInput(session, "latin_name", choices = get_distinct_array("latin_name"), server = FALSE, options = list(create = TRUE))
      updateSelectizeInput(session, "life_stages", choices = get_distinct_array("life_stages"), server = FALSE, options = list(create = TRUE))
      updateSelectizeInput(session, "location_country", choices = get_distinct_array("location_country"), server = FALSE, options = list(create = TRUE))
      updateSelectizeInput(session, "location_state_province", choices = get_distinct_array("location_state_province"), server = FALSE, options = list(create = TRUE))
      updateSelectizeInput(session, "function_derivation", choices = get_distinct_array("function_derivation"), server = FALSE, options = list(create = TRUE))
    })

    # ── Handle Citations ──
    citation_count <- reactiveVal(1)
    observeEvent(input$add_citation, {
      new_count <- citation_count() + 1
      citation_count(new_count)
      insertUI(
        selector = paste0("#", ns("extra_citations_container")),
        where = "beforeEnd",
        ui = div(
          id = ns(paste0("citation_block_", new_count)),
          style = "border: 1px solid #e3e3e3; padding: 15px; margin-bottom: 10px; border-radius: 5px; background-color: #fafafa;",
          textAreaInput(ns(paste0("citation_text_", new_count)), paste("Citation", new_count, "(Text)"), height = "70px", width = "100%"),
          fluidRow(
            column(6, textInput(ns(paste0("citation_title_", new_count)), "Link Title", width = "100%")),
            column(6, textInput(ns(paste0("citation_url_", new_count)), "URL", width = "100%"))
          )
        )
      )
    })

    # ── CSV Validation ──
    observeEvent(input$sr_csv_file, {
      req(input$sr_csv_file)
      csv_validation_result <- validate_csv_upload(input$sr_csv_file)
      output$csv_validation_status <- renderUI({
        if (csv_validation_result$valid) {
          HTML(create_alert_html("success", "CSV is valid", list(sprintf("Total rows: %d", nrow(csv_validation_result$data)))))
        } else {
          err <- get_csv_error_message(csv_validation_result)
          HTML(create_alert_html("error", err$message, err$issues))
        }
      })
    })
        
    # ── Live Duplicate Title Check ──
    observeEvent(input$title, {
      req(nchar(input$title) > 5)
      
      existing_titles <- dbGetQuery(db_conn, "SELECT title FROM stressor_responses WHERE title IS NOT NULL")$title
      matches <- agrep(tolower(input$title), tolower(existing_titles), max.distance = 0.15, value = TRUE)
      
      output$title_warning <- renderUI({
        if (length(matches) > 0) {
          div(style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; padding: 10px; border-radius: 5px; margin-top: -10px; margin-bottom: 15px;",
              icon("exclamation-triangle"), 
              strong(" Potential Duplicate:"), " A similarly titled article already exists in the database. Please verify before submitting."
          )
        } else {
          NULL
        }
      })
    })
        
    # ── Submit to Staging Database ──
    observeEvent(input$submit_relationship, {
      # Basic required field checks
      if (trimws(input$submitter_name) == "" || trimws(input$submitter_email) == "" || trimws(input$title) == "") {
        show_error_modal(session, "Missing Fields", "Please fill out all required fields (Name, Email, and Title).")
        return()
      }

      # Initialize empty CSV result
      csv_res <- list(valid = FALSE, data = data.frame())
        
      # Only validate if a CSV was actually uploaded
      if (!is.null(input$sr_csv_file)) {
        csv_res <- validate_csv_upload(input$sr_csv_file)
        if (!csv_res$valid) {
          show_error_modal(session, "Invalid CSV", "Please fix your CSV file before submitting.")
          return()
        }
      }
      
      # Compile citations to JSON
      citations_list <- list()
      for (i in 1:citation_count()) {
        c_text <- input[[paste0("citation_text_", i)]]
        if (!is.null(c_text) && trimws(c_text) != "") {
          citations_list[[length(citations_list) + 1]] <- list(
            text = trimws(c_text),
            title = trimws(input[[paste0("citation_title_", i)]]),
            url = trimws(input[[paste0("citation_url_", i)]])
          )
        }
      }
      citation_json <- if (length(citations_list) > 0) jsonlite::toJSON(citations_list, auto_unbox = TRUE) else "[]"

      # Array Helper
      to_pg_array <- function(val) {
        if (is.null(val) || length(val) == 0) return(NA_character_)
        parts <- unlist(lapply(val, function(x) trimws(strsplit(x, ",")[[1]])))
        parts <- parts[parts != ""]
        if (length(parts) == 0) return(NA_character_)
        paste0("{", paste(sprintf('"%s"', gsub('"', '\\"', parts, fixed = TRUE)), collapse = ","), "}")
      }

      # Optional PDF processing
      pdf_binary <- NULL
      pdf_name <- NA_character_
      if (!is.null(input$supporting_pdf)) {
        pdf_path <- input$supporting_pdf$datapath
        if (file.exists(pdf_path)) {
          pdf_binary <- readBin(pdf_path, "raw", file.info(pdf_path)$size)
          pdf_name <- input$supporting_pdf$name
        }
      }

      tryCatch({
        # 1. Insert into staging_submissions RETURNING staging_id
        query <- "
          INSERT INTO staging_submissions (
            submitter_name, submitter_email, submitter_notes,
            article_type, title, stressor_name, broad_stressor_name, specific_stressor_metric,
            response, srf_formula, species_common_name, latin_name, life_stages,
            location_country, location_state_province, overview, function_derivation, transferability_of_function,
            conf_source, conf_shape, conf_variance, conf_applicability, conf_interactions,
            citations, supporting_pdf, pdf_filename
          ) VALUES (
            $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24::jsonb, $25, $26
          ) RETURNING staging_id;"
          
        res <- dbGetQuery(db_conn, query, params = list(
          input$submitter_name, input$submitter_email, input$submitter_notes,
          input$article_type, input$title, input$stressor_name, input$broad_stressor_name, input$specific_stressor_metric,
          input$response, input$srf_formula, to_pg_array(input$species_common_name), to_pg_array(input$latin_name), to_pg_array(input$life_stages),
          to_pg_array(input$location_country), to_pg_array(input$location_state_province), input$overview, to_pg_array(input$function_derivation), input$transferability_of_function,
          input$conf_source, input$conf_shape, input$conf_variance, input$conf_applicability, input$conf_interactions,
          citation_json, 
          list(pdf_binary),
          pdf_name
        ))
        
        new_staging_id <- res$staging_id

        # 2. Insert into staging_csv_data (Only if a valid CSV was provided)
        if (csv_res$valid && nrow(csv_res$data) > 0) {
          df_csv <- csv_res$data
          df_csv$staging_id <- new_staging_id
          df_csv$row_index <- 1:nrow(df_csv)
          names(df_csv) <- gsub("\\.", "_", names(df_csv))
          dbAppendTable(db_conn, "staging_csv_data", df_csv)
        }

        show_success_modal(session, "Submission Successful", "Thank you! Your relationship has been added to our staging queue for administrative review.")
        shinyjs::reset(ns("submit_relationship_form"))
        
        # ── Send Lightweight Email Notification ──
        smtp_host <- Sys.getenv("SMTP_HOST")
        smtp_port <- as.integer(Sys.getenv("SMTP_PORT"))
        smtp_from <- Sys.getenv("SMTP_FROM")
        admin_to <- Sys.getenv("ADMIN_EMAIL")

        if (nzchar(smtp_host) && nzchar(smtp_from) && nzchar(admin_to)) {
          if (requireNamespace("emayili", quietly = TRUE) && requireNamespace("promises", quietly = TRUE)) {
            
            email_text <- paste0(
              "A new stressor-response relationship has been submitted to the Staging Queue.\n\n",
              "Submitter: ", input$submitter_name, " (", input$submitter_email, ")\n",
              "Title: ", input$title, "\n\n",
              "Please log into the Salmonid e-Library and check the Admin Review Queue to approve or reject this submission."
            )

            email_env <- emayili::envelope() %>%
              emayili::from(smtp_from) %>%
              emayili::to(admin_to) %>%
              emayili::subject("New SRF Submission - Admin Review Required") %>%
              emayili::text(email_text)

            promises::future_promise({
              smtp <- emayili::server(host = smtp_host, port = smtp_port, helo = "noaa.gov")
              smtp(email_env)
            }, seed = TRUE)
          }
        }
      }, error = function(e) {
        show_error_modal(session, "Database Error", paste("Failed to save submission:", e$message))
      })
    })
  })
}
# nolint end
