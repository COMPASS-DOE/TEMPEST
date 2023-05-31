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
library(shinyalert)
library(gmailr)

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
                               plotlyOutput("teros_plot")
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
                DT::dataTableOutput("teros_table"),
                selectInput("plot",
                            "Plot:",
                            choices = c("Control", "Freshwater", "Seawater", "Shoreline"),
                            selected = "Freshwater"),
                plotlyOutput("teros_detail_graph")
            ),
            tabItem(
                tabName = "troll",
                DT::dataTableOutput("troll_table"),
                plotlyOutput("troll_detail_graph")
            ),
            tabItem(
                tabName = "battery",
                dataTableOutput("btable")
            ),
            tabItem(
                tabName = "maps",
                selectInput("map_plot",
                            "Plot:",
                            choices = c("Control", "Freshwater", "Seawater", "Shoreline"),
                            selected = "Control"),
                checkboxGroupInput("mapitems", "Data to show",
                                   choices = c("Sapflow status" = "map_sapflow",
                                     "TEROS status" = "map_teros",
                                     "Aquatroll status" = "map_aquatroll",
                                     "Tree locations" = "map_trees",
                                     "Compass rose" = "map_rose"),
                                   selected = c("map_rose"),
                                   inline = TRUE),
                plotOutput("map", height = "600px")
            ),
            tabItem(
                tabName = "alerts",
                textInput(inputId = "phone_number",
                          label = "Phone number:",
                          placeholder = "301-555-5555"),

                # Input: Selector for choosing carrier ----
                selectInput(inputId = "carrier",
                            label = "Select your carrier:",
                            choices = c("Verizon", "AT&T", "T-Mobile", "Sprint")),

                actionButton("txt_alert","Receive Text Alerts")
            )
        )
    )
)
