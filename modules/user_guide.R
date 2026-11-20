# nolint start
library(shiny)

# Reusable function that outputs Section 508 compliant tables
render_508_table <- function(headers, rows) {
  tags$div(
    class = "table-responsive",
    style = "margin: 20px 0;",
    tags$table(
      class = "table table-striped table-bordered",
      style = "width: 100%; border-collapse: collapse; font-size: 15px; color: #222;",
      # ── Table Header with scope='col' ──
      tags$thead(
        style = "background-color: #f1f5f9; color: #2c3e50; border-bottom: 2px solid #cbd5e1;",
        tags$tr(
          lapply(headers, function(h) {
            tags$th(scope = "col", h, style = "padding: 12px; font-weight: 700; text-align: left;")
          })
        )
      ),
      # ── Table Body ──
      tags$tbody(
        lapply(rows, function(r) {
          tags$tr(
            tags$td(
              style = "padding: 10px 12px; font-weight: 600; white-space: nowrap; vertical-align: top; width: 25%;",
              r[[3]]
            ),
            tags$td(
              style = "padding: 10px 12px; vertical-align: top; line-height: 1.5;",
              r[[4]]
            )
          )
        })
      )
    )
  )
}

userGuideUI <- function(id) {
  ns <- NS(id)
  
  # Metadata fields exactly as defined in your dictionary
  metadata_rows <- list(
    list("Main ID", "Identifier for the SR function. Links metadata to the associated dataset"),
    list("Citation(s)", "Full citation of the primary research article, including citations for additional sources if the SR function draws on multiple papers"),
    list("Stressor Title", "Title of the SR function. Format: Author et al. Year: Function description. Example: Honea et al. 2016: Chinook egg-to-fry survival vs incubation temperature"),
    list("Species Common", "Common species name (e.g., Chinook salmon, Sockeye salmon)"),
    list("Species Latin Name", "Genus species name (e.g., Oncorhynchus tshawytscha)"),
    list("Life Stage", "Life stage of fish (e.g., juvenile, fry, smolt, adult, egg)"),
    list("Activity", "Fish activity examined (e.g., incubation, spawning, rearing, migration)"),
    list("Season", "Season relevant to the study (e.g., spring, summer, fall, winter)"),
    list("Stressor Name", "Primary stressor evaluated (e.g., Water Temperature, Flow, Fine Sediment)"),
    list("Broad Stressor Name", "High-level classification of the stressor category (e.g., Physical, Chemical, Biological)"),
    list("Specific Stressor Metric", "Measurement metric for the stressor (e.g., 7DADM, NTU, Celsius, cfs)"),
    list("Response", "Biological response evaluated (e.g., Survival, Growth rate, Migration timing)"),
    list("Response Units", "Measurement units of the response (e.g., %, proportion, g/day)"),
    list("Location Country", "Country where the empirical data were gathered or modeled"),
    list("Location State / Province", "State or province (e.g., Washington, Oregon, California, British Columbia)"),
    list("Location Watershed / Lab", "Watershed name, basin, or notation if the study was conducted in a laboratory"),
    list("Location River / Creek", "Specific river, stream reach, or estuary site")
  )

  extracted_rows <- list(
    list("curve.id", "Identifier distinguishing distinct curves or sub-treatments within the same function entry (e.g., c1, c2)"),
    list("stressor.label", "Human-readable label for the stressor variable plotted on the horizontal axis"),
    list("stressor.x", "Numeric value of the environmental stressor (x-coordinate)"),
    list("units.x", "Measurement unit for the stressor values (e.g., degC, mg/L, cfs)"),
    list("response.label", "Human-readable label for the biological response variable plotted on the vertical axis"),
    list("response.y", "Numeric value of the biological response (y-coordinate)"),
    list("units.y", "Measurement unit for the response values (e.g., proportion, %, count)"),
    list("plot.type", "Display specification for the coordinate series ('curve' for lines, 'scatter' for points)"),
    list("stressor.value", "Categorical or sub-condition label for multi-curve sets (e.g., constant, fluctuating)"),
    list("lower.limit", "Lower bound of the uncertainty envelope or confidence interval (if available)"),
    list("upper.limit", "Upper bound of the uncertainty envelope or confidence interval (if available)"),
    list("sd", "Reported standard deviation or standard error associated with the data point")
  )

  tagList(
    div(
      class = "page-container",
      style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
      
      h1("User Guide", style = "color: #2c3e50; font-weight: 700; margin-bottom: 20px;"),
      h2("Stressor Response Metadata and Data Documentation", style = "color: #2c3e50; font-size: 24px; font-weight: 600;"),
      p("This e-library is designed to support life cycle modelers, resource managers, and scientists by making stressor-response (SR) functions discoverable, transparent, and reusable. Each SR function describes the quantitative relationship between a stressor (e.g., temperature, flow, harvest, contaminants) and a biological response (e.g., survival, growth, migration timing, capacity, productivity).",
        style = "font-size: 16px; color: #333; line-height: 1.6;"),
      hr(style = "margin: 25px 0;"),
      
      p("The library contains these main components:", style = "font-size: 16px; font-weight: 600; color: #2c3e50;"),
      tags$ol(
        style = "font-size: 16px; color: #333; line-height: 1.8;",
        tags$li(strong("Metadata"), " — Standardized fields describing the stressor-response function, its source, biological context, and supporting details."),
        tags$li(strong("Extracted data"), " — Numerical values digitized or obtained from the article (or supplemental datasets), formatted for reuse, plotting, or integration into models."),
        tags$li(strong("Confidence Rankings"), " — Qualitative assessments of uncertainty and support for each stressor-response function, based on the CEMPRA Stressor-Response Function Guidance.")
      ),
      hr(style = "margin: 25px 0;"),
      
      # ── Section 1: Metadata Fields Table ──
      h2("Metadata Fields", style = "color: #2c3e50; font-size: 22px; font-weight: 600; margin-bottom: 15px;"),
      p("Each entry has an identifier and descriptive metadata fields.", style = "font-size: 16px; color: #333; margin-bottom: 15px;"),
      
      render_508_table(
        headers = c("Field", "Description"),
        rows = metadata_rows
      ),
      
      hr(style = "margin: 35px 0;"),

      # ── Section 2: Extracted Data Table ──
      h2("Extracted Data", style = "color: #2c3e50; font-size: 22px; font-weight: 600; margin-bottom: 15px;"),
      p("When you download an SR function as an Excel Spreadsheet, you receive:", style = "font-size: 16px; color: #333;"),
      tags$ol(
        style = "font-size: 16px; color: #333; line-height: 1.8;",
        tags$li("Metadata (all fields above), and"),
        tags$li("Extracted data table — numerical data pulled from figures, tables, or supplementary files.")
      ),
      p("The extracted spreadsheet data have the following standardized columns:", style = "font-size: 16px; color: #333; margin-top: 15px; margin-bottom: 15px;"),
      
      render_508_table(
        headers = c("Column Name", "Description"),
        rows = extracted_rows
      ),
      
      tags$div(
        style = "margin: 25px 0;",
        tags$button(
          type = "button",
          class = "btn btn-link",
          style = "padding: 0; font-size: 16px; color: #1a56db; text-decoration: underline; font-weight: 600;",
          onclick = "document.querySelector('a[data-value=\"submit_relationship\"]').click(); return false;",
          "aria-label" = "Navigate to the Submit a Relationship form",
          "Suggest a Relationship — go to the submission form"
        )
      ),
      p(
        style = "font-style: italic; font-size: 15px; color: #444; line-height: 1.6; background-color: #f8f9fa; padding: 15px; border-left: 4px solid #005f9e; border-radius: 4px;",
        "Note: All extracted data entries include numeric Stressor (x) and Response (y) values and are associated with a single stressor per SRF entry. Each entry is fully labeled with a Stressor Label, Response Label, and corresponding units to ensure interpretability. Where multiple curves are present for the same stressor, they are distinguished using a curve identifier and, when applicable, a stressor value describing the curve. Early entries contributed by Canadian partners (e.g., CEMPRA, Joe Model) reported the biological response as Mean System Capacity, which scaled response values from 0% to 100%. Since NOAA assumed responsibility for stewardship of the library and expanded its scope, response values have been retained in the original units and formats reported in each source study, rather than standardized across entries. Extracted data may originate from reported tables, supplemental datasets, or digitized figures. When data are digitized from figures, small uncertainties may be introduced; these are documented where possible."
      ),
      hr(style = "margin: 35px 0;"),
      
      # ── Section 3: Overlay Plots ──
      h2("Comparing Profiles (Overlay Plots)", style = "color: #2c3e50; font-size: 22px; font-weight: 600; margin-bottom: 15px;"),
      p("To facilitate multi-study comparisons, the e-library allows users to visually overlay stressor-response curves from multiple articles onto a single interactive plot. This feature is especially useful when trying to determine which empirical relationships are most relevant for a specific study system or to visualize underlying sensitivity across different geographic regions.",
        style = "font-size: 16px; color: #333; line-height: 1.6;"),
      tags$ol(
        style = "font-size: 16px; color: #333; line-height: 1.8;",
        tags$li("Use the ", strong("Show Filters"), " and search bar functionality on the main dashboard to isolate a specific stressor or metric of interest."),
        tags$li("Check the box located on the left side of any relevant article card to add it to your selection."),
        tags$li("Click the ", strong("Overlay Plots"), " button in the action bar persistently located at the top of the search results."),
        tags$li("An interactive chart will appear displaying all selected quantitative curves, along with a summary of the metadata for each selected profile.")
      ),
      hr(style = "margin: 35px 0;"),
      
      # ── Section 4: Confidence Rankings ──
      h2("Confidence Rankings", style = "color: #2c3e50; font-size: 22px; font-weight: 600; margin-bottom: 15px;"),
      p("Each stressor-response function in the e-library includes a set of confidence rankings that help users evaluate the strength, reliability, and applicability of the underlying relationship. These rankings identify five key areas where uncertainty may arise when deriving or applying a stressor-response function.",
        style = "font-size: 16px; color: #333; line-height: 1.6;"),
      p(
        "For each of the five categories below, authors provide a qualitative ranking — ",
        strong("Low"), ", ", strong("Moderate"), ", or ", strong("High"),
        " — along with an optional rationale. These rankings are intended to support transparency and help users determine how appropriate a given stressor-response function may be for their system, model, or decision context.",
        style = "font-size: 16px; color: #333; line-height: 1.6;"
      ),
      
      h3("1. Data Source for Stressor-Response Function", style = "color: #2c3e50; font-size: 18px; font-weight: 600; margin-top: 25px;"),
      p("This ranking reflects how well the data used to derive the stressor-response function represent the stressor and response of interest.", style = "font-size: 15px; color: #333;"),
      tags$ul(
        style = "font-size: 15px; color: #333; line-height: 1.6;",
        tags$li("High confidence typically indicates that data were directly measured, collected using consistent methods, and represent the relevant species, life stage, and geographic context."),
        tags$li("Low confidence may arise when data are sparse, indirect, extrapolated, or compiled from heterogeneous sources.")
      ),
      
      h3("2. Shape of Stressor-Response Function", style = "color: #2c3e50; font-size: 18px; font-weight: 600; margin-top: 25px;"),
      p("This ranking describes how well-supported the functional form is (e.g., linear, threshold, dome-shaped).", style = "font-size: 15px; color: #333;"),
      tags$ul(
        style = "font-size: 15px; color: #333; line-height: 1.6;",
        tags$li("High confidence suggests the curve shape is clearly supported by data patterns and model diagnostics."),
        tags$li("Low confidence reflects ambiguous patterns, multiple plausible curve shapes, or models sensitive to parameterization.")
      ),
      
      h3("3. Data Variance / Consistency", style = "color: #2c3e50; font-size: 18px; font-weight: 600; margin-top: 25px;"),
      p("This ranking summarizes the variability, noise, or inconsistency in the underlying dataset.", style = "font-size: 15px; color: #333;"),
      tags$ul(
        style = "font-size: 15px; color: #333; line-height: 1.6;",
        tags$li("High confidence reflects consistent results across studies, years, or sampling programs."),
        tags$li("Low confidence indicates high variance, conflicting trends, or limited replication.")
      ),
      
      h3("4. Applicability to System", style = "color: #2c3e50; font-size: 18px; font-weight: 600; margin-top: 25px;"),
      p("This ranking assesses how applicable the underlying data and derived function are to the specific system or context where it is currently being applied.", style = "font-size: 15px; color: #333;"),
      tags$ul(
        style = "font-size: 15px; color: #333; line-height: 1.6;",
        tags$li("High confidence means the function was derived using data highly representative of the specific system in question (matching species, life stage, habitat type, and stressor regime)."),
        tags$li("Low confidence indicates that the function is being applied to a system with notable biological or environmental differences from the original study context.")
      ),
      
      h3("5. Potential Stressor Interactions", style = "color: #2c3e50; font-size: 18px; font-weight: 600; margin-top: 25px;"),
      p("This ranking considers whether the stressor-response function may be influenced by interactions with other environmental stressors (e.g., temperature × flow).", style = "font-size: 15px; color: #333;"),
      tags$ul(
        style = "font-size: 15px; color: #333; line-height: 1.6;",
        tags$li("High confidence means the original study controlled for or examined relevant interacting stressors."),
        tags$li("Low confidence reflects missing covariates or systems where interactions are likely but unmeasured.")
      ),
      
      h3("How to Use These Rankings", style = "color: #2c3e50; font-size: 18px; font-weight: 600; margin-top: 30px;"),
      p("Confidence rankings are not intended to exclude stressor-response functions from use; instead, they help users:", style = "font-size: 15px; color: #333;"),
      tags$ul(
        style = "font-size: 15px; color: #333; line-height: 1.6;",
        tags$li("Interpret model output appropriately,"),
        tags$li("Understand sources of uncertainty,"),
        tags$li("Compare functions when multiple relationships exist,"),
        tags$li("Identify where caution or system-specific calibration may be needed,"),
        tags$li("Recognize areas where further research would be valuable.")
      ),
      p("Users should consider these rankings together with stressor-response function metadata, extracted data, and any accompanying notes to determine how well a function fits their modeling or decision context.",
        style = "font-size: 15px; color: #333; line-height: 1.6;"),
      
      hr(style = "margin: 35px 0;"),
      h2("Attribution", style = "color: #2c3e50; font-size: 22px; font-weight: 600; margin-bottom: 10px;"),
      p("If you use data from this library, please cite the original research article(s).", style = "font-size: 16px; color: #333;")
    )
  )
}
# nolint end
