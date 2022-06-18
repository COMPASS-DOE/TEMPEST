#
# This is the user-interface definition of the TEMPEST data dashboard
# June 2022

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(shinyWidgets)
library(dygraphs)

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
                    # selectInput(inputId = "plot",
                    #             label = "Plot:",
                    #             choices = c("Control", "Seawater", "Freshwater")),
                    tabBox(width = 12,
                           #uiOutput("plotSelector"),
                           tabPanel(
                               title = "Sapflow",
                               plotlyOutput("sapflow_plot", height = "200px")
                           ),
                           tabPanel(
                               title = "TEROS",
                               plotlyOutput("teros_plot", height = "200px")
                           ),
                           tabPanel(
                               title = "AquaTroll",
                               plotlyOutput("aquatroll_plot", height = "200px")
                           ),
                           tabPanel(
                               title = "Battery",
                               plotlyOutput("battery_plot", height = "200px")
                           )
                    )
                )

            ),
            tabItem(
                tabName = "alerts",
                textInput(inputId = "phone-number",
                          label = "Phone number:",
                          value = "(301) 555-5555"),

                # Input: Selector for choosing dataset ----
                selectInput(inputId = "carrier",
                            label = "Select your carrier:",
                            choices = c("Verizon", "AT&T", "T Mobile")),

                submitButton("Receive Text Alerts")
            )

        )
    )
)
