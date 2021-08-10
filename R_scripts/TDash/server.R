
library(dplyr)
library(shiny)
library(DT)
library(readr)

server <- function(input, output) {

    observeEvent(input$update, {
        source("../process_data.R")
    })

    output$data <- renderDataTable({
        sapflow %>%
            filter(Logger == input$`logger-filter`) %>%
            group_by(Tree_Code) %>%
            do(tail(., 10)) %>%
            select(Timestamp, Tree_Code, Value, Logger, Grid_Square) %>%
            pivot_wider(id_cols = Timestamp ,names_from = "Tree_Code", values_from = "Value")
    })

    output$sf_timeseries <- renderPlot({

         sapflow %>%
             filter(Plot == input$plot) %>%
             ggplot(aes(x = Timestamp, y = Value)) +
             geom_line() +
             facet_wrap(~Tree_Code, scales = "free") +
             theme(axis.text.x = element_text(angle = 90))
    })

}
