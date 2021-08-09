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
            menuItem("Graphs", tabName = "graphs", icon = icon("chart-pie"))
            )
    ),
    dashboardBody(
        tabItems(
          tabItem(
                tabName = "dashboard",
                dataTableOutput("data")
          ),

          tabItem(
                tabName = "graphs",
                sidebarPanel(
                    selectInput("plot", "Plot:",
                                choices = unique(sapflow$Plot))),
                fluidRow(
                    box(plotOutput("sf_timeseries"), width = 8))
          )
        )
    )
)

