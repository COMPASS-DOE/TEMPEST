
library(shiny)
library(DT)
library(readr)

server <- function(input, output) {

    output$data <- renderDataTable({
        test()
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
