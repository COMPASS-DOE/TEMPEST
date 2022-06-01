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
library(dplyr)
library(shinyWidgets)
# Dummy variable
# sapflow_data <- data.frame(Logger = NA, Plot = NA)

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(
        title = "TEMPEST Dashboard"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("compass")),
            menuItem("Sapflow", tabName = "sapflow", icon = icon("tree")),
            menuItem("TEROS", tabName = "teros", icon = icon("temperature-high"))
            )
    ),
    dashboardBody(
        tags$head(tags$style(".shiny-notification {position: fixed; top: 30% ;left: 50%; width: 300px")),
        tabItems(
          tabItem(
                tabName = "dashboard",
                fluidRow(
                    # Frontpage - boxes - start -----------------------------------------------
                    valueBox(
                        "—", "Tweets Today",
                        color = "purple",
                        icon = icon("comment-dots"))),
                # actionButton("update", "Update Data"),
                # actionButton("refreshButton",
                #              label = "Refresh",
                #              class = "btn-success"),
                uiOutput("dataloggerSelector"),
                dataTableOutput("table"),
                dataTableOutput("teros_table")
          ),

          tabItem(
                 tabName = "sapflow",
                fluidRow(
                    box(uiOutput("plotSelector"),
                        plotOutput("sf_timeseries", height = "800px"),
                        width = 12),
                    box(uiOutput("sensorSelector"),
                        dygraphOutput("sfsensor_timeseries", height = "800px"),
                        width = 12
                    )
                    ),
          ),
          tabItem(
              tabName = "teros",
              uiOutput("plotSelectorT"),
             # uiOutput("sensorSelectorT"),
              fluidRow(
                  box(plotOutput("teros_timeseries", height = "800px"), width = 12)
              )
          )
        )
    )
)

