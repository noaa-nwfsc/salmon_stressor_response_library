# nolint start
about_us <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
      
      # Hero Section
      div(
        style = "background: linear-gradient(135deg, #fdfbfb 0%, #ebedee 100%); 
                 padding: 40px 30px; border-radius: 15px; text-align: center; 
                 margin-bottom: 40px; box-shadow: 0 4px 12px rgba(0,0,0,0.05);",
        h2("About this Dashboard", style = "color: #2c3e50; font-weight: 700; margin-bottom: 20px;"),
        p("The Pacific Salmonid Stressor-Response eLibrary is an open-source, centralized resource designed to support researchers and modelers working with life cycle models (LCMs). It consolidates and organizes published quantitative relationships between environmental stressors and salmonid life stages, making it easier to access and apply relevant data.", 
          style = "font-size: 18px; color: #444; max-width: 900px; margin: 0 auto 15px auto;"),
        p("This e-library serves as a decision-support tool, helping ensure that life cycle modeling efforts are built on a shared foundation of empirical data. While the provided relationships are drawn from peer-reviewed literature and vetted studies, users should critically assess the data’s applicability to their specific models and consider factors such as regional differences, study limitations, and context-specific variables.",
          style = "font-size: 18px; color: #444; max-width: 900px; margin: 0 auto;")
      ),
      
      # Side-by-side Info Cards using native fluidRow and column
      fluidRow(
        
        # Left Card: Conservation Efforts
        column(6,
          div(
            style = "background-color: white; border-radius: 8px; padding: 25px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); border-top: 4px solid #2ecc71; height: 100%;",
            h4("How This Tool Supports Conservation", style = "margin-top: 0; color: #333; font-weight: 600; border-bottom: 1px solid #eee; padding-bottom: 10px;"),
            tags$ul(
              style = "list-style-type: none; padding-left: 0; margin-top: 15px; font-size: 16px; line-height: 1.6;",
              tags$li(style = "margin-bottom: 15px;", icon("circle-check", style = "color: #28a745; font-size: 1.5rem;"), strong(" Improve Model Accuracy:"), " Ensuring researchers and decision-makers have access to consistent, high-quality data reduces uncertainty in life cycle modeling."),
              tags$li(style = "margin-bottom: 15px;", icon("lightbulb", style = "color: #ffc107; font-size: 1.5rem;"), strong(" Identify Knowledge Gaps:"), " Highlighting areas where data is lacking can guide future research and funding priorities."),
              tags$li(style = "margin-bottom: 15px;", icon("chart-bar", style = "color: #17a2b8; font-size: 1.5rem;"), strong(" Support Policy & Management:"), " Reliable data on stressor-response functions can inform habitat restoration, water management, and conservation policy."),
              tags$li(style = "margin-bottom: 15px;", icon("globe", style = "color: #007bff; font-size: 1.5rem;"), strong(" Encourage Open Science:"), " By making key data easily accessible, the e-library fosters collaboration and transparency within the life cycle modeling community.")
            )
          )
        ),
        
        # Right Card: Using the App & Credits
        column(6,
          div(
            style = "background-color: white; border-radius: 8px; padding: 25px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); border-top: 4px solid #6082B6; height: 100%;",
            h4("Using the e-Library", style = "margin-top: 0; color: #333; font-weight: 600; border-bottom: 1px solid #eee; padding-bottom: 10px;"),
            p("If relevant studies appear, review the metadata and study details to determine whether the findings align with your research needs.", style = "margin-top: 15px; font-size: 16px;"),
            p("If no studies are returned, this may indicate:", style = "font-size: 16px;"),
            tags$ul(
              style = "font-size: 16px; line-height: 1.6;",
              tags$li("The topic has not yet been researched by our team, AND/OR"),
              tags$li("A gap exists in the scientific literature, highlighting an area for future study.")
            ),
            p("By providing a one-stop, transparent repository of stressor-response functions, this e-library supports informed decision-making and fosters an open-science approach within the LCM community.", style = "font-size: 16px;"),
            hr(style = "margin: 20px 0;"),
            p(
              style = "font-size: 15px; color: #555;",
              "This R/Shiny app was developed by a team of Seattle University data science students (see acknowledgements below). It was modeled after an existing Drupal app created by Matthew Bayly. We owe a great deal of gratitude to Matthew and his colleagues for generating the original app and for allowing us to emulate its function here. Matthew's app can be found ",
              tags$a(href = "https://mjbayly.com/stressor-response", "here.", target = "_blank")
            ),
            p(
              style = "font-size: 15px; color: #555;",
              "This app is still under active development and we welcome feedback about the user experience (to aimee.fullerton at noaa dot gov). We will be adding additional relationships on a rolling basis over the next several years."
            )
          )
        )
      ),
      
      # ── Where Our Data Comes From (Full-width Block) ──
      div(
        style = "margin-top: 40px; background-color: white; border-radius: 8px; padding: 30px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); border-top: 4px solid #f39c12;",
        h3("Where Our Data Comes From", style = "margin-top: 0; color: #2c3e50; border-bottom: 1px solid #eee; padding-bottom: 10px; font-weight: 600;"),
        p("This library represents a collaborative effort spanning multiple agencies, universities, and international partnerships. Our functional relationships are continually aggregated from several foundational sources and active literature reviews:", style = "font-size: 16px; margin-bottom: 20px; color: #444;"),
        
        tags$ul(style = "line-height: 1.8; font-size: 15px; color: #444;",
          
          # Foundational Aggregation
          tags$li(style = "margin-bottom: 15px;",
            strong("Foundational Libraries: "),
            "In collaboration with our Canadian colleagues, we seeded the database by transitioning 112 relationships from Matthew Bayly’s Drupal library (see link above). This included expert elicitations, Government of Alberta reports, and foundational literature (e.g., Cramer 2001, Bjornn & Reiser 1991, Honea et al. 2016). Simultaneously, Aimee Fullerton led a team of interns in compiling early E2F Temperature, Sediment, and Flow survival relationships via GitHub, which are now fully integrated here."
          ),
          
          # Regional Literature Reviews
          tags$li(style = "margin-bottom: 15px;",
            strong("Regional Literature Reviews: "),
            "To build comprehensive spatial coverage, our team partnered with NOAA Central Librarians to screen over 1,800 papers focusing specifically on California’s freshwater Pacific salmonids. These actively derived relationships currently make up a significant portion of our library. In tandem, our Canadian colleagues conducted a massive 30,000-paper review spanning the remainder of the Pacific Northwest (WA, ID, OR, B.C., and Yukon), which we plan to ingest following their publication."
          ),
          
          # Specialized Topics & Databases
          tags$li(style = "margin-bottom: 15px;",
            strong("Toxicological Functions: "),
            "Toxicological functional relationships have been derived and integrated based on source material provided by Julann Spromberg (NOAA Ecotoxicologist). We will continue to add toxicology functional relationships as they appear."
          ),
          tags$li(style = "margin-bottom: 15px;",
            strong("Related Sister Databases: "),
            "We are actively mining and cross-referencing functional relationships from two large literature reviews:",
            tags$ul(style = "margin-top: 8px; list-style-type: circle;",
              tags$li(
                "Lisa Crozier's Climate Change Vulnerability Database — ",
                tags$a(href = "https://connect.fisheries.noaa.gov/ClimateSalmonLiterature/", "Explore Database", target = "_blank"), " | ",
                tags$a(href = "https://doi.org/10.1038/s42003-021-01734-w", "Read Paper", target = "_blank")
              ),
              tags$li(
                "John McMillan's Thermal Stress Database — ",
                tags$a(href = "https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Ffme.12643&file=fme12643-sup-0002-AppendixS2.html", "Explore Database", target = "_blank"), " | ",
                tags$a(href = "https://doi.org/10.1111/fme.12643", "Read Paper", target = "_blank")
              )
            )
          ),
          
          # Future Scope
          tags$li(style = "margin-bottom: 10px;",
            strong("Future Expansion: "),
            "While currently focused on freshwater environments, we are charting paths to incorporate more downstream relationships encompassing estuary and ocean life stages."
          )
        )
      )
    )
  )
}
# nolint end
