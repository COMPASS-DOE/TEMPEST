#
# This is the server of the TEMPEST data dashboard
# June 2022

library(dplyr)
library(shiny)
library(DT)
library(readr)
library(rdrop2)
library(dygraphs)
library(xts)
library(shinybusy)


source("global.R")
source("flag_sensors.R")

# The server normally access the SERC Dropbox to download data
# If we are TESTING, however, skip this and use local test data only
if(!TESTING) {
    datadir <- "TEMPEST_PNNL_Data/Current_Data"
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
            aquatroll <- readRDS("test-data/aquatroll")
            battery <- readRDS("test-data/battery")
        } else {
            sapflow <- withProgress(process_sapflow(token, datadir), message = "Updating sapflow...")
            teros <- withProgress(process_teros(token, datadir), message = "Updating TEROS...")
            aquatroll <- withProgress(process_aquatroll(token, datadir), message = "Updating AquaTroll...")
            sapflow %>%
                select(Timestamp, BattV_Avg, Plot, Logger) %>%
                group_by(Plot, Logger, Timestamp) %>%
                summarise(BattV_Avg = mean(BattV_Avg), .groups = "drop") ->
                battery
        }

        latest_ts <- Sys.time()

        # Do limits testing and compute data needed for badges
        sapflow %>%
            filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < latest_ts) %>%
            summarise(flag_sensors(Value, limits = SAPFLOW_RANGE)) ->
            sapflow_bdg
        # TEROS is awkward, because we only have one badge, but three
        # variables within a single dataset. We compute out-of-limits for each
        # variable, and then combine to a single value and badge color
        teros %>%
            filter(TIMESTAMP > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
                   TIMESTAMP < latest_ts) %>%
            left_join(TEROS_RANGE, by = "variable") %>%
            group_by(variable) %>%
            summarise(flag_sensors(value, limits = c(low[1], high[1]))) %>%
            summarise(fraction_in = weighted.mean(fraction_in, n)) %>%
            mutate(percent_in = paste0(round(fraction_in * 100, 0), "%"),
                   color = badge_color(1 - fraction_in)) ->
            teros_bdg

        aquatroll %>%
            filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < latest_ts) %>%
            summarise(flag_sensors(Temp, limits = AQUATROLL_TEMP_RANGE)) ->
            aquatroll_bdg

        battery %>%
            filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < latest_ts) %>%
            summarise(flag_sensors(BattV_Avg, limits = VOLTAGE_RANGE)) ->
            battery_bdg

        # Return data and badge information
        list(sapflow = sapflow,
             teros = teros,
             aquatroll = aquatroll,
             battery = battery,
             sapflow_bdg = sapflow_bdg,
             teros_bdg = teros_bdg,
             aquatroll_bdg = aquatroll_bdg,
             battery_bdg = battery_bdg)
    })

    # ------------------ Dashboard graphs -----------------------------

    observeEvent(autoInvalidate(), {

        update_progress("circle", {
            round(as.numeric(difftime(Sys.time(), EVENT_START, units = "hours")) / 10, 2)
        })
    })

    output$sapflow_sensors <- renderDataTable({
        reactive_df()$sapflow %>%
            filter(Timestamp > Sys.time() - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < Sys.time()) -> sapflow

        bad_sensors(sapflow, sapflow$Value, "Tree_Code", limits = SAPFLOW_RANGE) -> vals

        datatable(vals, options = list(paging = FALSE, searching = FALSE))
    })

    output$batt_sensors <- renderDataTable({
        reactive_df()$battery %>%
            filter(Timestamp > Sys.time() - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < Sys.time()) -> battery

        bad_sensors(battery, battery$BattV_Avg, "Logger", limits = VOLTAGE_RANGE) -> vals

        datatable(vals, options = list(paging = FALSE, searching = FALSE))
    })


    output$sapflow_plot <- renderPlotly({
        # Average sapflow data by plot and 15 minute interval
        # This graph is shown when users click the "Sapflow" tab on the dashboard

        sapflow <- reactive_df()$sapflow

        if(nrow(sapflow)) {
            latest_ts <- Sys.time()
            sapflow %>%
                filter(Timestamp > latest_ts - GRAPH_TIME_WINDOW * 60 * 60,
                       Timestamp < latest_ts) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot, Logger, Timestamp_rounded) %>%
                summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, Value, color = Plot, group = Logger)) +
                geom_line() +
                xlab("") +
                geom_hline(yintercept = SAPFLOW_RANGE, linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })

    output$teros_plot <- renderPlotly({
        # Average TEROS data by plot and 15 minute interval,
        # one facet per sensor (temperature, moisture, conductivity)
        # This graph is shown when users click the "TEROS" tab on the dashboard

        teros <- reactive_df()$teros

        if(nrow(teros) > 1) {
            latest_ts <- Sys.time()
            teros %>%
                filter(TIMESTAMP > latest_ts - GRAPH_TIME_WINDOW * 60 * 60,
                       TIMESTAMP < latest_ts) %>%
                mutate(Timestamp_rounded = round_date(TIMESTAMP, GRAPH_TIME_INTERVAL)) %>%
                mutate(Timestamp_rounded = round_date(TIMESTAMP, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot, variable, Logger, Timestamp_rounded) %>%
                summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, value, color = Plot, group = Logger)) +
                facet_grid(variable~., scales = "free") +
                geom_line() +
                xlab("") +
                geom_hline(data = TEROS_RANGE, aes(yintercept = low), linetype = 2) +
                geom_hline(data = TEROS_RANGE, aes(yintercept = high), linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }

        plotly::ggplotly(b)
    })

    output$aquatroll_plot <- renderPlotly({
        # AquaTroll data plot
        # TODO: this currently just shows temperature; we may want to have multiple
        # variables, in which case the badge status computation would be like
        # that of TEROS
        # This graph is shown when users click the "Battery" tab on the dashboard
        aquatroll <- reactive_df()$aquatroll

        if(nrow(aquatroll) > 1) {
            latest_ts <- Sys.time()
            aquatroll %>%
                filter(Timestamp > latest_ts - GRAPH_TIME_WINDOW * 60 * 60,
                       Timestamp < latest_ts) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot_long, Probe_ShortName, Timestamp_rounded) %>%
                summarise(Plot = Plot_long,
                          Temp = mean(Temp, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, Temp, color = Plot, group = Probe_ShortName)) +
                geom_line() +
                xlab("") +
                geom_hline(yintercept = AQUATROLL_TEMP_RANGE, linetype = 2)  ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }

        plotly::ggplotly(b)
    })

    output$battery_plot <- renderPlotly({
        # Battery voltages, from the sapflow data
        # This graph is shown when users click the "Battery" tab on the dashboard
        battery <- reactive_df()$battery

        if(nrow(battery)) {
            latest_ts <- Sys.time()
            battery %>%
                filter(Timestamp > latest_ts - GRAPH_TIME_WINDOW * 60 * 60,
                       Timestamp < latest_ts) %>%
                ggplot(aes(Timestamp, BattV_Avg, color = as.factor(Logger))) +
                geom_line() +
                labs(x = "", y = "Battery (V)") +
                geom_hline(yintercept = VOLTAGE_RANGE, linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }

        plotly::ggplotly(b)
    })

    # output$plotSelector <- renderUI({
    #
    # })


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

    # output$plotSelector <- renderUI({
    #     sapflow_data <- reactive_df()$sapflow
    #
    #     selectInput("plot",
    #                 "Plot:",
    #                 choices = unique(sapflow_data$Plot),
    #                 selected = "Freshwater")
    # })

    output$sapflow_table <- DT::renderDataTable(datatable({
        autoInvalidate()
        sapflow_data <- reactive_df()$sapflow

        # if(is.null(input$plotSelector)) {
        #     sdata <- sapflow_data
        # }
        # if(length(input$plotSelector) > 0){
        #     sdata <- filter(sapflow_data, Plot %in% input$plotSelector)
        # }

        sapflow_data %>% select(Tree_Code, Plot) -> plots

        sapflow_data %>%
            group_by(Tree_Code) %>%
            do(tail(., 10)) %>%
            select(Timestamp, Plot, Tree_Code, Value, Logger, Grid_Square) %>%
            pivot_wider(
                id_cols = c("Tree_Code", "Plot", "Grid_Square") ,
                names_from = "Timestamp",
                values_from = "Value"
            )

    }))

    output$y11 <- renderPrint(input$sapflow_table_rows_selected)

     output$teros_table <- renderDataTable({

         # input$refreshButton

         autoInvalidate()
         teros_data <- reactive_df()$teros

         # if(nrow(teros_data)) {
         #
         #     if(is.null(input$`logger-filter`)) {  # initial state before update
         #         tdata <- teros_data
         #     } else {
         #         tdata <- filter(teros_data, Logger %in% input$`logger-filter`)
         #     }
         teros_data %>%
                 group_by(ID, variable) %>%
                 do(tail(., 10)) %>%
                 select(TIMESTAMP, ID, value, Logger, `Grid Square`) %>%
                 pivot_wider(id_cols = c("variable", "ID") ,names_from = "TIMESTAMP", values_from = "value")
         #}
     })

     observeEvent(input$press, {
         output$number <- print("testing")
         #print(input$btable_rows_selected)
     })

     output$btable <- DT::renderDataTable({
         autoInvalidate()
         reactive_df()$battery %>%
             select(Timestamp, BattV_Avg, Plot, Logger) %>%
             filter(Timestamp > "2022-06-13", Timestamp < "2022-07-01") %>%
             group_by(Plot, Logger) %>%
             distinct() %>%
             do(tail(., 10)) %>%
             pivot_wider(id_cols = c("Plot", "Logger"), names_from = "Timestamp", values_from = "BattV_Avg") %>%
             datatable()

     })

    # ------------------ Dashboard badges -----------------------------

    output$sapflow_bdg <- renderValueBox({
        valueBox(reactive_df()$sapflow_bdg$percent_in[1],
                 "Sapflow",
                 color = reactive_df()$sapflow_bdg$color[1],
                 icon = icon("tree")
        )
    })
    output$teros_bdg <- renderValueBox({
        valueBox(reactive_df()$teros_bdg$percent_in[1],
                 "TEROS",
                 color = reactive_df()$teros_bdg$color[1],
                 icon = icon("temperature-high")
        )
    })
    output$aquatroll_bdg <- renderValueBox({
        valueBox(reactive_df()$aquatroll_bdg$percent_in[1],
                 "AquaTroll",
                 color = reactive_df()$aquatroll_bdg$color[1],
                 icon = icon("water")
        )
    })
    output$battery_bdg <- renderValueBox({
        valueBox(reactive_df()$battery_bdg$percent_in[1],
                 "Battery",
                 color = reactive_df()$battery_bdg$color[1],
                 icon = icon("car-battery")
        )
    })

}
