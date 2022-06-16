
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(rdrop2)
library(dygraphs)
library(xts)
library(dygraphs)

source("global.R")

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
        list(sapflow = sapflow, teros = teros, battery = battery)
    })

    #
    #     output$dataloggerSelector <- renderUI({
    #
    #         sapflow_data <- reactive_df()$sapflow
    #
    #         pickerInput("logger-filter", "Loggers",
    #                     choices = unique(sapflow_data$Logger),
    #                     selected = "11",
    #                     multiple = TRUE)
    #     })
    #
    #
    #
    # output$plotSelector <- renderUI({
    #     autoInvalidate()
    #     sapflow_data <- reactive_df()$sapflow
    #
    #     selectInput("plot", "Plot:",
    #                 choices = unique(sapflow_data$Plot),
    #                 selected = "C")
    # })
    #
    #     output$sensorSelector <- renderUI({
    #         autoInvalidate()
    #         sapflow_data <- reactive_df()$sapflow
    #
    #         pickerInput("sensor", "Sensor",
    #                     choices = unique(sapflow_data$Tree_Code),
    #                     selected = "F11",
    #                     multiple = TRUE)
    #     })
    #
    #     output$plotSelectorT <- renderUI({
    #         autoInvalidate()
    #         teros_data <- reactive_df()$teros
    #
    #         selectInput("tPlot", "Plot:",
    #                     choices = unique(substr(teros_data$Plot, 1, 1)),
    #                     selected = "C")
    #     })
    #
    #      output$table <- renderDataTable({
    #
    #         # input$refreshButton
    #
    #          autoInvalidate()
    #          sapflow_data <- reactive_df()$sapflow
    #
    #          # filelist <- drop_dir(datadir, cursor = cursor, dtoken = token)
    #          # update_needed <- nrow(filelist) > 0
    #          #
    #          # if(update_needed) {
    #          #     showNotification("Updating data...", duration = 3)
    #          #
    #          #     # Read dropbox files
    #          #     sapflow_data <<- process_sapflow(token)
    #          #
    #          #     # Update the cursor tracking directory state
    #          #     cursor <<- drop_dir(drop_dir, cursor = TRUE, dtoken = token)
    #          #     last_update <<- Sys.time()
    #          # }
    #
    #          if(nrow(sapflow_data)) {
    #              if(is.null(input$`logger-filter`)) {  # initial state before update
    #                  sdata <- sapflow_data
    #              } else {
    #                  sdata <- filter(sapflow_data, Logger %in% input$`logger-filter`)
    #              }
    #              sdata %>%
    #                  group_by(Tree_Code) %>%
    #                  do(tail(., 10)) %>%
    #                  select(Timestamp, Tree_Code, Value, Logger, Grid_Square) %>%
    #                  pivot_wider(id_cols = Tree_Code ,names_from = "Timestamp", values_from = "Value")
    #          }
    #     })
    #
    #      output$teros_table <- renderDataTable({
    #
    #          # input$refreshButton
    #
    #          autoInvalidate()
    #          teros_data <- reactive_df()$teros
    #
    #          if(nrow(teros_data)) {
    #
    #              if(is.null(input$`logger-filter`)) {  # initial state before update
    #                  tdata <- teros_data
    #              } else {
    #                  tdata <- filter(teros_data, Logger %in% input$`logger-filter`)
    #              }
    #              tdata %>%
    #                  group_by(ID, variable) %>%
    #                  do(tail(., 10)) %>%
    #                  select(TIMESTAMP, ID, value, Logger, `Grid Square`) %>%
    #                  pivot_wider(id_cols = c("variable", "ID") ,names_from = "TIMESTAMP", values_from = "value")
    #          }
    #      })
    #
    #      output$sf_timeseries <- renderPlot({
    #          #input$plot
    #
    #          autoInvalidate()
    #          sapflow_data <- reactive_df()$sapflow
    #
    #          if(is.null(input$plot)) {  # initial state before update
    #              sdata <- sapflow_data
    #          } else {
    #              sdata <- filter(sapflow_data, Plot == input$plot)
    #          }
    #
    #          sdata %>%
    #              ggplot(aes(x = Timestamp, y = Value, group = Tree_Code)) +
    #              geom_line() +
    #              facet_wrap(~Species, scales = "free") +
    #              theme(axis.text.x = element_text(angle = 90)) +
    #              theme_minimal() #+
    #              # annotate(geom = "rect",
    #              #          xmin = ymd_hms("2021-09-09 00:07:00", tz = "EST"),
    #              #          xmax = ymd_hms("2021-09-09 17:00:00", tz = "EST"),
    #              #          ymin = -Inf, ymax = Inf,
    #              #          alpha = 0.2, fill = "deepskyblue")
    #      })
    #

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

}
