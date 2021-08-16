
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(rdrop2)

source("../read_sapflow.R")
source("../process_sapflow.R")
token <- readRDS("../droptoken.rds")

drop_dir = "TEMPEST_PNNL_Data/Current_Data"
cursor <- drop_dir(drop_dir, cursor = TRUE, dtoken = token)
last_update <- NA
force_update <- TRUE
sapflow_data <- data.frame()

server <- function(input, output) {

    autoInvalidate <- reactiveTimer(5 * 1000)

    sapflow_data_display <- reactive({

        input$refreshButton
        autoInvalidate()

        filelist <- drop_dir(drop_dir, cursor = cursor, dtoken = token)
        update_needed <- nrow(filelist) > 0

        if(update_needed || force_update) {
            showNotification("Updating data...", duration = 3)

            # Read dropbox file
            sapflow_data <<- process_sapflow(token)

            # Update the cursor
            cursor <<- drop_dir(drop_dir, cursor = TRUE, dtoken = token)
            force_update <<- FALSE
            last_update <<- Sys.time()
        }
    })

    output$table <- renderTable({

        if(nrow(sapflow_data)) {
            sapflow_data %>%
                filter(Logger %in% input$`logger-filter`) %>%
                group_by(Tree_Code) %>%
                do(tail(., 10)) %>%
                select(Timestamp, Tree_Code, Value, Logger, Grid_Square) %>%
                pivot_wider(id_cols = Tree_Code ,names_from = "Timestamp", values_from = "Value")
        }
    })

    output$sf_timeseries <- renderPlot({

        sapflow_data %>%
            filter(Plot == input$plot) %>%
            ggplot(aes(x = Timestamp, y = Value)) +
            geom_line() +
            facet_wrap(~Tree_Code, scales = "free") +
            theme(axis.text.x = element_text(angle = 90))
    })
}
