
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(rdrop2)
library(dygraphs)
library(xts)
library(dygraphs)

source("global.R")
source("flag_sensors.R")

#datadir <- "TEMPEST_PNNL_Data/Current_Data"
if(!TESTING) {
    token <- readRDS("droptoken.rds")
    cursor <- drop_dir(datadir, cursor = TRUE, dtoken = token)
}
last_update <- NA

server <- function(input, output) {

    autoInvalidate <- reactiveTimer(15 * 60 * 1000)

    reactive_df <- reactive({

        autoInvalidate()

        if(TESTING) {
            sapflow <- readRDS("test-data/sapflow")
            teros <- readRDS("test-data/teros")
            battery <- readRDS("test-data/battery")
        } else {
            sapflow <- withProgress(process_sapflow(token), message = "Updating sapflow...")
            teros <- withProgress(process_teros(token), message = "Updating TEROS...")
            battery <- select(sapflow, Timestamp, BattV_Avg, Plot)
        }

        # Do limits testing and compute data needed for badges
        sapflow_bdg <- flag_sensors(sapflow$Value, limits = SAPFLOW_RANGE)

        list(sapflow = sapflow,
             teros = teros,
             battery = battery,
             sapflow_bdg = sapflow_bdg)
    })

    output$battery <- renderPlotly({
        # Battery voltages, from the sapflow data
        # This graph is shown when users click the "Battery" tab on the dashboard
        battery <- reactive_df()$battery
        latest_ts <- max(battery$Timestamp)

        if(nrow(battery)) {
            battery %>%
                ggplot(aes(Timestamp, BattV_Avg, color = Plot)) +
                geom_point() +
                labs(y = "Battery (V)") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }

        plotly::ggplotly(b)
    })

    output$teros_timeseries <- renderPlotly({
        # Average TEROS data by plot and 15 minute interval,
        # one facet per sensor (temperature, moisture, conductivity)
        # This graph is shown when users click the "TEROS" tab on the dashboard

        teros <- reactive_df()$teros
        latest_ts <- max(teros$TIMESTAMP)

        if(nrow(teros)) {
            teros %>%
                mutate(Timestamp_rounded = round_date(TIMESTAMP, "15 minutes")) %>%
                group_by(Plot, variable, Logger, Timestamp_rounded) %>%
                summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, value, color = Plot, group = Logger)) +
                facet_grid(variable~., scales = "free") +
                geom_line() +
                xlab("") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts))->
                b
        } else {
            b <- NO_DATA_GRAPH
        }

        plotly::ggplotly(b)
    })

    output$sfsensor_timeseries <- renderPlotly({
        # Average sapflow data by plot and 15 minute interval
        # This graph is shown when users click the "Sapflow" tab on the dashboard

        sapflow <- reactive_df()$sapflow
        latest_ts <- max(sapflow$Timestamp)

        if(nrow(sapflow)) {
            sapflow %>%
                mutate(Timestamp_rounded = round_date(Timestamp, "15 minutes")) %>%
                group_by(Plot, Logger, Timestamp_rounded) %>%
                summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, Value, color = Plot, group = Logger)) +
                geom_line() +
                xlab("") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts))->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })

    # Dashboard badges
    output$sapflow_bdg <- renderValueBox({
        valueBox(reactive_df()$sapflow_bdg$percent_in[1],
                 "Sapflow",
                 color = reactive_df()$sapflow_bdg$color[1],
                 icon = icon("tree")
        )
    })

}
