# nolint start
reset_filters_server <- function(input, session) {
  observeEvent(input$reset_filters, {
    updateTextInput(session, "search", value = "")

    filter_ids <- c(
      "stressor", "stressor_metric", "species", "life_stage",
      "activity", "latin_name", "article_type", "location_country",
      "location_state_province", "location_watershed_lab",
      "location_river_creek", "broad_stressor_name"
    )

    for (f_id in filter_ids) {
      updateVirtualSelect(session = session, inputId = f_id, selected = character(0))
    }
  })
}
# nolint end
