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

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(
        title = "TEMPEST Dashboard"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("compass")),
            menuItem("Stats", tabName = "stats", icon = icon("chart-pie"))
            )
    ),
    dashboardBody(
        tabItem(
            tabName = "dashboard"
        ),
        tabItem(
            tabName = "stats"
        ),
        sidebarPanel(
            selectInput("plot", "Plot:",
                        choices = unique(sapflow$Plot))),
        fluidRow(
            box(plotOutput("sf_timeseries"), width = 8)
        )
    )
)
