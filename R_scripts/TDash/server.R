
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(rdrop2)

source("../read_sapflow.R")
source("../process_sapflow.R")
token <- readRDS("../droptoken.rds")

datadir <- "TEMPEST_PNNL_Data/Current_Data"
cursor <- drop_dir(datadir, cursor = TRUE, dtoken = token)
last_update <- NA

# Start off by downloading all data
sapflow_data <- process_sapflow(token)

server <- function(input, output) {

    autoInvalidate <- reactiveTimer(5 * 1000)

    output$table <- renderTable({

        input$refreshButton
        autoInvalidate()

        filelist <- drop_dir(datadir, cursor = cursor, dtoken = token)
        update_needed <- nrow(filelist) > 0

        if(update_needed) {
            showNotification("Updating data...", duration = 3)

            # Read dropbox files
            sapflow_data <<- process_sapflow(token)

            # Update the cursor tracking directory state
            cursor <<- drop_dir(drop_dir, cursor = TRUE, dtoken = token)
            last_update <<- Sys.time()
        }

        if(nrow(sapflow_data)) {
            if(is.null(input$`logger-filter`)) {  # initial state before update
                sdata <- sapflow_data
            } else {
                sdata <- filter(sapflow_data, Logger %in% input$`logger-filter`)
            }
            sdata %>%
                group_by(Tree_Code) %>%
                do(tail(., 10)) %>%
                select(Timestamp, Tree_Code, Value, Logger, Grid_Square) %>%
                pivot_wider(id_cols = Tree_Code ,names_from = "Timestamp", values_from = "Value")
        }
    })

    output$dataloggerSelector <- renderUI({
        pickerInput("logger-filter", "Loggers",
                    choices = unique(sapflow_data$Logger),
                    selected = "11",
                    multiple = TRUE)
    })

    output$plotSelector <- renderUI({
        selectInput("plot", "Plot:",
                    choices = unique(sapflow_data$Plot),
                    selected = "C")
    })

    output$sf_timeseries <- renderPlot({
        input$plot

        if(is.null(input$plot)) {  # initial state before update
            sdata <- sapflow_data
        } else {
            sdata <- filter(sapflow_data, Plot == input$plot)
        }

        sdata %>%
            ggplot(aes(x = Timestamp, y = Value)) +
            geom_line() +
            facet_wrap(~Tree_Code, scales = "free") +
            theme(axis.text.x = element_text(angle = 90))
    })
}
