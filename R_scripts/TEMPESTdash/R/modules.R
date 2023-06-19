# Test modules

mapsTabUI <- function(id) {
    # `NS(id)` returns a namespace function, which was save as `ns` and will
    # invoke later.
    ns <- NS(id)

    tabItem(
        tabName = "maps",
        selectInput("map_plot",
                    "Plot:",
                    choices = c("Control", "Freshwater", "Saltwater", "Shoreline"),
                    selected = "Control"),
        radioGroupButtons("mapitems",
                          label = "Data to show:",
                          choices = c("Sapflow status" = "map_sapflow",
                                      "TEROS status" = "map_teros"),
                          selected = "map_teros"),
        checkboxGroupInput("map_overlays",
                           label = "Overlay:",
                           choices = c("Compass rose" = "map_rose", "Trees" = "map_trees"),
                           selected = "map_rose",
                           inline = TRUE),
        plotOutput("status_map", height = "600px"),
        selectInput("data_map_variable",
                    "TEROS variable:",
                    choices = c("TSOIL", "VWC", "EC"),
                    selected = "VWC"),
        selectInput("teros_depth",
                    "TEROS depth:",
                    choices = c("All", "5", "15", "30"),
                    selected = "All"),
        plotOutput("data_map", height = "600px")
    )
}
