# nolint start
acknowledgement_ui <- function(id, n = NULL) {
  ns <- NS(id)

  # DYNAMIC IMAGE FIX: Look inside the folder and only get valid image names!
  valid_pics <- list.files("modules/images/highlight", pattern = "(?i)\\.(jpg|jpeg|png)$", full.names = FALSE)

  tagList(
    div(
      style = "max-width: 1200px; margin: 0 auto; padding: 20px;",

      # ── Photos Section ──
      fluidRow(
        column(12, h3("Field & Research Photos", style = "text-align: center; color: #2c3e50; font-weight: 600; margin-top: 20px; margin-bottom: 30px;"))
      ),

      # Dynamic Pictures Section (Reads directly from folder contents)
      fluidRow(
        style = "display: flex; justify-content: center; flex-wrap: wrap; gap: 15px; margin-top: 10px;",
        lapply(valid_pics, function(pic_name) {
          # Clean file name into readable alt text (e.g., "chinook_smolt.jpg" -> "chinook smolt")
          clean_alt <- tools::file_path_sans_ext(pic_name)
          clean_alt <- gsub("[_-]", " ", clean_alt)
          
          tags$div(
            style = "width: 180px; height: 180px; overflow: hidden; border-radius: 12px; box-shadow: 0 4px 10px rgba(0,0,0,0.15); transition: transform 0.2s;",
            # FIXED: Added descriptive alt text for each dynamic highlight image
            tags$img(
              src = paste0("teamimg/highlight/", pic_name),
              alt = paste("Pacific salmon research photo:", clean_alt),
              style = "width: 100%; height: 100%; object-fit: cover;"
            )
          )
        })
      ),

      hr(style = "margin: 50px 0; border-top: 1px solid #ddd;"),

      # ── Acknowledgements Heading ──
      fluidRow(
        column(12, h2("Acknowledgements", style = "text-align: center; color: #2c3e50; font-weight: 700; margin-bottom: 35px;"))
      ),

      # ── NOAA Team-Sponsors Heading ──
      fluidRow(
        column(12, h3("NOAA Team-Sponsors", style = "text-align: center; color: #2c3e50; font-size: 20px; font-weight: 600; margin-bottom: 25px;"))
      ),

      # Sponsors' Photos
      fluidRow(
        style = "display: flex; justify-content: center; gap: 40px; flex-wrap: wrap; margin-bottom: 40px;",
        tags$div(
          style = "text-align: center; width: 160px;",
          # FIXED: Added alt text to sponsor headshot
          tags$img(
            src = "teamimg/Aimee.jpg",
            alt = "Photograph of Aimee Fullerton, Research Fishery Biologist",
            style = "width: 130px; height: 130px; border-radius: 50%; object-fit: cover; box-shadow: 0 4px 10px rgba(0,0,0,0.15); border: 3px solid white; margin-bottom: 12px;"
          ),
          tags$p("Aimee Fullerton", style = "font-weight: 600; font-size: 16px; margin: 0; color: #222;")
        ),
        tags$div(
          style = "text-align: center; width: 160px;",
          tags$img(
            src = "teamimg/paxton.jpg",
            alt = "Photograph of Paxton Calhoun, Research Fishery Biologist",
            style = "width: 130px; height: 130px; border-radius: 50%; object-fit: cover; box-shadow: 0 4px 10px rgba(0,0,0,0.15); border: 3px solid white; margin-bottom: 12px;"
          ),
          tags$p("Paxton Calhoun", style = "font-weight: 600; font-size: 16px; margin: 0; color: #222;")
        ),
        tags$div(
          style = "text-align: center; width: 160px;",
          tags$img(
            src = "teamimg/Morgan.jpg",
            alt = "Photograph of Morgan Bond, Research Fishery Biologist",
            style = "width: 130px; height: 130px; border-radius: 50%; object-fit: cover; box-shadow: 0 4px 10px rgba(0,0,0,0.15); border: 3px solid white; margin-bottom: 12px;"
          ),
          tags$p("Morgan Bond", style = "font-weight: 600; font-size: 16px; margin: 0; color: #222;")
        ),
        tags$div(
          style = "text-align: center; width: 160px;",
          tags$img(
            src = "teamimg/Danielle.jpg",
            alt = "Photograph of Danielle Andrews, Developer",
            style = "width: 130px; height: 130px; border-radius: 50%; object-fit: cover; box-shadow: 0 4px 10px rgba(0,0,0,0.15); border: 3px solid white; margin-bottom: 12px;"
          ),
          tags$p("Danielle Andrews", style = "font-weight: 600; font-size: 16px; margin: 0; color: #222;")
        )
      ),

      # ── Original Designers / Canadian Collaborators ──
      fluidRow(
        column(
          12,
          tags$div(
            style = "background-color: #f8f9fa; border-radius: 10px; padding: 25px; max-width: 800px; margin: 0 auto 50px auto; text-align: center; box-shadow: 0 2px 8px rgba(0,0,0,0.05);",
            # FIXED: Changed from h5 to h3 to maintain proper hierarchy
            h3("Original Design & Canadian Collaborators", style = "color: #2c3e50; margin-top: 0; margin-bottom: 15px; font-size: 18px; font-weight: 600;"),
            p("The original design and layout for this e-library came from the following collaborators in Canada:", style = "color: #333; margin-bottom: 12px; font-size: 15px;"),
            tags$ul(
              # FIXED: Darkened text color from #555 to #333 for contrast
              style = "list-style-type: none; padding: 0; text-align: center; color: #333; line-height: 1.8; font-size: 16px;",
              tags$li("Sierra Sullivan (The University of British Columbia)"),
              tags$li("Jordan Rosenfeld (British Columbia Ministry of Environment)"),
              tags$li("Matthew Bayly (M.J. Bayly Analytics Ltd.)"),
              tags$li("Matthew Bakken (The University of British Columbia)"),
              tags$li("Alexandra Tekatch (ESSA Technologies Ltd.)")
            )
          )
        )
      ),

      # ── Student Team Section ──
      fluidRow(
        # FIXED: Promoted from h4 to h3
        column(12, h3("Student Development Team", style = "text-align: center; color: #2c3e50; font-size: 20px; font-weight: 600; margin-bottom: 25px;"))
      ),
      fluidRow(
        style = "display: flex; justify-content: center; align-items: flex-start; flex-wrap: wrap; gap: 50px; margin-bottom: 50px;",

        tags$div(
          style = "width: 350px; text-align: center;",
          # FIXED: Added descriptive alt text for team photo
          tags$img(
            src = "teamimg/Team.jpg",
            alt = "Seattle University Student Development Team: Maelice Yamdjieu, Garrett Ringler, Lavanya Bunadri, and Ruqhaiya Syeda",
            style = "width: 100%; height: auto; border-radius: 12px; box-shadow: 0 6px 15px rgba(0,0,0,0.15);"
          ),
          tags$p("SU Student Development Team", style = "margin: 15px 0 0 0; font-weight: 600; font-size: 16px; color: #222;"),
          # FIXED: Darkened color from #666 to #444 to pass 4.5:1 contrast
          tags$p("From left to right: Maelice Yamdjieu, Garrett Ringler, Lavanya Bunadri, and Ruqhaiya Syeda", style = "font-size: 14px; font-style: italic; color: #444; margin-top: 5px;")
        ),

        tags$div(
          style = "width: 250px; text-align: center;",
          # FIXED: Added descriptive alt text for advisor photo
          tags$img(
            src = "teamimg/Mendible.jpg",
            alt = "Photograph of Dr. Ariana Mendible, Professor of Mathematics at Seattle University",
            style = "width: 100%; height: auto; border-radius: 12px; box-shadow: 0 6px 15px rgba(0,0,0,0.15);"
          ),
          tags$p("Dr. Ariana Mendible", style = "font-weight: 600; font-size: 16px; margin: 15px 0 0 0; color: #222;"),
          # FIXED: Darkened color from #666 to #444
          tags$p("Professor of Mathematics at Seattle University", style = "font-size: 14px; color: #444; margin-top: 5px;")
        )
      ),

      hr(style = "margin: 40px 0; border-top: 1px solid #eee;"),

      # ── Additional Resources Section ──
      fluidRow(
        # FIXED: Promoted from h5 to h3
        column(12, h3("Additional Resources", style = "text-align: left; color: #2c3e50; font-weight: 600; font-size: 18px;"))
      ),
      fluidRow(
        column(12,
          tags$ul(
            style = "list-style-type: none; padding: 0; display: flex; gap: 20px; font-size: 16px; margin-bottom: 40px;",
            # FIXED: Darkened link color to #1a56db (> 7.5:1 contrast ratio), added underline and aria-label
            tags$li(
              tags$a(
                href = "https://www.noaa.gov",
                "NOAA Official Website",
                target = "_blank",
                rel = "noopener noreferrer",
                "aria-label" = "Visit the official NOAA website (opens in new tab)",
                style = "color: #1a56db; text-decoration: underline; font-weight: 500;"
              )
            ),
            tags$li(
              tags$a(
                href = "https://www.fisheries.noaa.gov/region/west-coast/northwest-science",
                "NOAA Fisheries Northwest Fisheries Science Center",
                target = "_blank",
                rel = "noopener noreferrer",
                "aria-label" = "Visit the NOAA Fisheries Northwest Fisheries Science Center website (opens in new tab)",
                style = "color: #1a56db; text-decoration: underline; font-weight: 500;"
              )
            )
          )
        )
      )
    )
  )
}
# nolint end
