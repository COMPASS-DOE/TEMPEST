# This is the user-interface definition of the TEMPEST data dashboard
# June 2022

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(dygraphs)
library(shinyWidgets)
library(dygraphs)
library(shinybusy)

ui <- dashboardPage(

    skin = if_else(TESTING, "red-light", "black-light"),
    dashboardHeader(
        title = if_else(TESTING, "TEST Dashboard", "TEMPEST Dashboard")
    ),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("compass")),
            menuItem("Sapflow", tabName = "sapflow", icon = icon("tree")),
            menuItem("TEROS", tabName = "teros", icon = icon("temperature-high")),
            menuItem("AquaTroll", tabName = "troll", icon = icon("water")),
            menuItem("Battery", tabName = "battery", icon = icon("car-battery")),
            menuItem("Maps", tabName = "maps", icon = icon("map-location-dot")),
            menuItem("Alerts", tabName = "alerts", icon = icon("comment-dots"))
        )
    ),
    dashboardBody(
        tags$head(tags$style(".shiny-notification {position: fixed; top: 30% ;left: 50%; width: 300px")),
        tabItems(
            tabItem(
                tabName = "dashboard",
                fluidRow(
                    # Front page badges; their attributes are computed by the server
                    valueBoxOutput("sapflow_bdg", width = 3),
                    valueBoxOutput("teros_bdg", width = 3),
                    valueBoxOutput("aquatroll_bdg", width = 3),
                    valueBoxOutput("battery_bdg", width = 3),
                ),
                fluidRow(
                    column(6,
                           progress_circle(value = 0, shiny_id = "circle",
                                           color = "#00B0CA", stroke_width = 15,
                                           trail_color = "#BBE7E6"),
                           tags$h3("Flood Progress", align = "center")
                    ),
                    column(width = 6,
                           tabBox(width = 12,
                                  tabPanel(
                                      title = "Sapflow",
                                      dataTableOutput("sapflow_bad_sensors")
                                  ),
                                  tabPanel(
                                      title = "TEROS",
                                      dataTableOutput("teros_bad_sensors")
                                  ),
                                  tabPanel(
                                      title = "AquaTroll",
                                      dataTableOutput("troll_bad_sensors")

                                  ),
                                  tabPanel(
                                      title = "Battery",
                                      dataTableOutput("batt_bad_sensors")
                                  )
                           )

                    )
                ),
                fluidRow(
                    tabBox(width = 12,
                           tabPanel(
                               title = "Sapflow",
                               plotlyOutput("sapflow_plot", height = "400px")
                           ),
                           tabPanel(
                               title = "TEROS",
                               plotlyOutput("teros_plot", height = "400px")
                           ),
                           tabPanel(
                               title = "AquaTroll",
                               plotlyOutput("aquatroll_plot", height = "400px")
                           ),
                           tabPanel(
                               title = "Battery",
                               plotlyOutput("battery_plot", height = "400px")
                           )
                    )
                )

            ),
            tabItem(
                tabName = "sapflow",
                DT::dataTableOutput("sapflow_table"),
                selectInput("plot",
                            "Plot:",
                            choices = c("Control", "Freshwater", "Seawater", "Shoreline"),
                            selected = "Freshwater"),
                plotlyOutput("sapflow_detail_graph")
            ),
            tabItem(
                tabName = "teros",
                dataTableOutput("teros_table")
            ),
            tabItem(
                tabName = "aquatroll",
                dataTableOutput("troll600_table"),
                dataTableOutput("troll200_table")
            ),
            tabItem(
                tabName = "battery",
                dataTableOutput("btable")#,
                # actionButton("press", "press me"),
                # textOutput("number")
            ),
            tabItem(
                tabName = "maps",
                selectInput("map_plot",
                            "Plot:",
                            choices = c("Control", "Freshwater", "Seawater", "Shoreline"),
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
                            "Variable:",
                            choices = c("TSOIL", "VWC", "EC", "xxx"),
                            selected = "VWC"),
                plotOutput("data_map", height = "600px")
            )#,
            # tabItem(
            #     tabName = "alerts",
            #     textInput(inputId = "phone-number",
            #               label = "Phone number:",
            #               value = "(301) 555-5555"),
            #
            #     # Input: Selector for choosing dataset ----
            #     selectInput(inputId = "carrier",
            #                 label = "Select your carrier:",
            #                 choices = c("Verizon", "AT&T", "T Mobile")),
            #
            #     submitButton("Receive Text Alerts")
            # )

        )
    )
)
