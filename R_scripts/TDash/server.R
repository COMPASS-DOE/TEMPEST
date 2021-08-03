#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

server <- function(input, output) {


    output$sf_timeseries <- renderPlot({
        sapflow <- read_csv("~/Desktop/sapflow.csv")
        sapflow %>%
            filter(Plot == "C") %>%
            ggplot(aes(x = Timestamp, y = Value)) +
            geom_line() +
            facet_wrap(~Tree_Code, scales = "free") +
            theme(axis.text.x = element_text(angle = 90))
    })
    }
