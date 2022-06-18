#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#

#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(dygraphs)
library(shinyWidgets)
# Dummy variable
# sapflow_data <- data.frame(Logger = NA, Plot = NA)

ui <- dashboardPage(
    skin = "black-light",
    dashboardHeader(title = "TEMPEST Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("compass")),
            menuItem("Sapflow", tabName = "sapflow", icon = icon("tree")),
            menuItem(
                "TEROS",
                tabName = "teros",
                icon = icon("temperature-high")
            ),
            menuItem("AquaTroll", tabName = "troll", icon = icon("water")),
            menuItem(
                "Battery",
                tabName = "battery",
                icon = icon("car-battery")
            ),
            menuItem(
                "Alerts",
                tabName = "alerts",
                icon = icon("comment-dots")
            )
        )
    ),
    dashboardBody(id = "nav",
                  tags$head(
                      tags$style(
                          ".shiny-notification {position: fixed; top: 30% ;left: 50%; width: 300px"
                      )
                  ),
                  tabItems(
                      tabItem(
                          tabName = "dashboard",
                          fluidRow(
                              # Frontpage - boxes
                              valueBox(
                                  "98%",
                                  "Sapflow",
                                  color = "green",
                                  icon = icon("tree"),
                                  width = 3
                              ),
                              valueBox(
                                  "90%",
                                  "TEROS",
                                  color = "yellow",
                                  icon = icon("exclamation-circle"),
                                  width = 3
                              ),
                              valueBox(
                                  "75%",
                                  "AquaTroll",
                                  color = "red",
                                  icon = icon("exclamation-triangle"),
                                  width = 3
                              ),
                              valueBox(
                                  "100%",
                                  "Battery",
                                  color = "green",
                                  icon = icon("car-battery"),
                                  width = 3
                              ),
                          ),
                          fluidRow(tabBox(
                              width = 12,
                              #uiOutput("plotSelector"),
                              tabPanel(title = "Sapflow",
                                       dygraphOutput("sfsensor_timeseries", height = "200px")),
                              tabPanel(title = "Teros"),
                              tabPanel(title = "AquaTroll"),
                              tabPanel(title = "Battery",
                                       plotlyOutput("battery", height = "200px"))
                          ))

                          # fluidRow(
                          #     tabBox(
                          #         width = 12,
                          #         tabPanel(
                          #             status = "primary",
                          #             title = "Sapflow",
                          #             plotOutput("sf_timeseries", height = "800px")
                          #         )
                          #     )
                          # ),
                          # actionButton("update", "Update Data"),
                          # actionButton("refreshButton",
                          #              label = "Refresh",
                          #              class = "btn-success"),

                      ),
                      tabItem(
                          tabName = "sapflow",
                          dataTableOutput("table"),
                          selectInput("plot",
                                      "Plot:",
                                      choices = c("Control", "Freshwater", "Seawater", "Shoreline"),
                                      selected = "Freshwater"),
                          plotlyOutput("splot")
                      ),
                      tabItem(
                          tabName = "teros",
                          dataTableOutput("teros_table")
                      ),
                      tabItem(
                          tabName = "aquatroll"
                      ),
                      tabItem(
                          tabName = "battery",
                          DT::dataTableOutput("btable"),
                          actionButton("press", "press me"),
                          textOutput("number")
                      ),
                      tabItem(
                          tabName = "alerts",
                          textInput(
                              inputId = "phone-number",
                              label = "Phone number:",
                              value = "3015555555"
                          ),

                          # Input: Selector for choosing dataset ----
                          selectInput(
                              inputId = "carrier",
                              label = "Select your carrier:",
                              choices = c("Verizon", "AT&T", "T Mobile")
                          ),

                          submitButton("Recieve Text Alerts")
                      )

                      # tabItem(
                      #        tabName = "sapflow",
                      #       fluidRow(
                      #           box(uiOutput("plotSelector"),
                      #               plotOutput("sf_timeseries", height = "800px"),
                      #               width = 12),
                      #           box(uiOutput("sensorSelector"),
                      #               dygraphOutput("sfsensor_timeseries", height = "800px"),
                      #               width = 12
                      #           ),
                      #           uiOutput("dataloggerSelector"),
                      #           dataTableOutput("table")
                      #           ),
                      # )#,
                      # tabItem(
                      #     tabName = "teros",
                      #    # uiOutput("sensorSelectorT"),
                      #     fluidRow(
                      #         uiOutput("plotSelectorT"),
                      #         box(plotOutput("teros_timeseries", height = "800px"), width = 12),
                      #         uiOutput("dataloggerSelector"),
                      #         dataTableOutput("teros_table")
                      #     )
                      #  )
                  ))
)
