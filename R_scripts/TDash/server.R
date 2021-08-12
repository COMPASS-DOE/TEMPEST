
library(dplyr)
library(shiny)
library(DT)
library(readr)

server <- function(input, output) {

    source("../read_sapflow.R")
    source("../process_sapflow.R")


    output$test <- renderTable({
        invalidateLater(1000 * 30)

        sapflow <<- process_sapflow()

        sapflow %>%
            filter(Logger %in% input$`logger-filter`) %>%
            group_by(Tree_Code) %>%
            do(tail(., 10)) %>%
            select(Timestamp, Tree_Code, Value, Logger, Grid_Square) %>%
            pivot_wider(id_cols = Tree_Code ,names_from = "Timestamp", values_from = "Value")
        })


    #  reactive({
    #      invalidateLater(1000 * 30, session)
    #
    #      test_df <-read_csv("~/Desktop/test_reactive.csv")
    # })
    #
    # output$test <- renderTable({
    #     test_df
    # })

    # output$data <- renderDataTable({
    #     sapflow %>%
    #         filter(Logger %in% input$`logger-filter`) %>%
    #         group_by(Tree_Code) %>%
    #         do(tail(., 10)) %>%
    #         select(Timestamp, Tree_Code, Value, Logger, Grid_Square) %>%
    #         pivot_wider(id_cols = Tree_Code ,names_from = "Timestamp", values_from = "Value")
    # })

    output$sf_timeseries <- renderPlot({

         sapflow %>%
             filter(Plot == input$plot) %>%
             ggplot(aes(x = Timestamp, y = Value)) +
             geom_line() +
             facet_wrap(~Tree_Code, scales = "free") +
             theme(axis.text.x = element_text(angle = 90))
    })

}
