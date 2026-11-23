# nolint start
reset_filters_server <- function(input, session) {
  observeEvent(input$reset_filters, {
    # 1. Clear text search
    updateTextInput(session, "search", value = "")

    # 2. Reset all VirtualSelect dropdowns on the client side simultaneously
    # This prevents reactivity thrashing between dropdowns
    session$sendCustomMessage("reset-all-virtual-selects", list(
      ids = c(
        "stressor", "stressor_metric", "species", "life_stage",
        "activity", "latin_name", "article_type", "location_country",
        "location_state_province", "location_watershed_lab",
        "location_river_creek", "broad_stressor_name"
      )
    ))
  })
}
# nolint end
