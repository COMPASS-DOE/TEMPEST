
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(rdrop2)
library(dygraphs)
library(xts)


source("global.R")
token <- readRDS("droptoken.rds")

datadir <- "TEMPEST_PNNL_Data/Current_Data"
cursor <- drop_dir(datadir, cursor = TRUE, dtoken = token)
last_update <- NA

server <- function(input, output) {

    autoInvalidate <- reactiveTimer(15 * 60 * 1000)

    reactive_df <- reactive({

        autoInvalidate()

        sapflow <- withProgress(process_sapflow(token), message = "Updating sapflow...")
        teros <- withProgress(process_teros(token), message = "Updating TEROS...")
        list(sapflow = sapflow, teros = teros)
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

    s <- reactive({
        reactive_df()$sapflow %>%
            filter(Plot == input$plot)
    })

    output$splot <- renderPlotly({

        s() %>%
            ggplot(aes(x = Timestamp, y = Value, color = Tree_Code)) +
            geom_line() + theme_minimal() -> s

        ggplotly(s)

    })

    output$table <- renderDataTable(datatable({
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
         browser()
         output$number <- print("testing")
         #print(input$btable_rows_selected)
     })

     output$btable <- DT::renderDataTable({

         autoInvalidate()
         reactive_df()$sapflow %>%
             select(Timestamp, BattV_Avg, Plot, Logger) %>%
             filter(Timestamp > "2022-06-13", Timestamp < "2022-07-01") %>%
             group_by(Plot, Logger) %>%
             distinct() %>%
             do(tail(., 10)) %>%
             pivot_wider(id_cols = c("Plot", "Logger"), names_from = "Timestamp", values_from = "BattV_Avg") -> bdf

             datatable(bdf)

     })

     # output$selected <- renderText({
     #     row <- input$btable_rows_selected
     #     row <- as.numeric(row)
     #     paste0("You Selected Row: ", row)
     #     })

    output$battery <- renderPlotly({

        battery_data <- reactive_df()$sapflow %>%
            select(Timestamp, BattV_Avg, Plot, Logger)

        battery_data %>%
            filter(Timestamp > "2022-06-13", Timestamp < "2022-07-01") %>%
            ggplot(aes(Timestamp, BattV_Avg, color = Plot, group = Logger)) +
            geom_line() +
            labs(y = "Battery (V)") +
            theme_minimal() -> b

        plotly::ggplotly(b)

    })

     sf_xts <- reactive({
         autoInvalidate()
         sapflow_data <- reactive_df()$sapflow

         # if(is.null(input$sensor)) {  # initial state before update
         #     sf_filtered <- sapflow_data %>% filter(Plot == "C")
         # } else {
         #     sf_filtered <- filter(sapflow_data, Tree_Code == input$sensor)
         # }

         sapflow_data %>%
             group_by(Plot, Timestamp) %>%
             summarise(Value = mean(Value)) %>%
             select(Timestamp, Plot, Value) %>%
             pivot_wider(names_from = "Plot", values_from = "Value") -> sf_formatted

         xts(x = sf_formatted[,-1], order.by = sf_formatted$Timestamp)
     })

     output$sfsensor_timeseries <- renderDygraph({
         #input$tree
         autoInvalidate()
         dygraph(sf_xts()) %>% dyRangeSelector()
     })

}
