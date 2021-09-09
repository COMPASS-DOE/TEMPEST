
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(rdrop2)

source("read_sapflow.R")
source("process_sapflow.R")
source("process_teros.R")
source("teros_fileread.R")
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


    output$dataloggerSelector <- renderUI({

        sapflow_data <- reactive_df()$sapflow

        pickerInput("logger-filter", "Loggers",
                    choices = unique(sapflow_data$Logger),
                    selected = "11",
                    multiple = TRUE)
    })



    output$plotSelector <- renderUI({

        sapflow_data <- reactive_df()$sapflow

        selectInput("plot", "Plot:",
                    choices = unique(sapflow_data$Plot),
                    selected = "C")
    })

    output$plotSelectorT <- renderUI({

        teros_data <- reactive_df()$teros

        selectInput("tPlot", "Plot:",
                    choices = unique(substr(teros_data$Plot, 1, 1)),
                    selected = "C")
    })

     output$table <- renderDataTable({

        # input$refreshButton

         autoInvalidate()
         sapflow_data <- reactive_df()$sapflow

         # filelist <- drop_dir(datadir, cursor = cursor, dtoken = token)
         # update_needed <- nrow(filelist) > 0
         #
         # if(update_needed) {
         #     showNotification("Updating data...", duration = 3)
         #
         #     # Read dropbox files
         #     sapflow_data <<- process_sapflow(token)
         #
         #     # Update the cursor tracking directory state
         #     cursor <<- drop_dir(drop_dir, cursor = TRUE, dtoken = token)
         #     last_update <<- Sys.time()
         # }

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

     output$teros_table <- renderDataTable({

         # input$refreshButton

         autoInvalidate()
         teros_data <- reactive_df()$teros

         if(nrow(teros_data)) {

             if(is.null(input$`logger-filter`)) {  # initial state before update
                 tdata <- teros_data
             } else {
                 tdata <- filter(teros_data, Logger %in% input$`logger-filter`)
             }
             tdata %>%
                 group_by(ID, variable) %>%
                 do(tail(., 10)) %>%
                 select(TIMESTAMP, ID, value, Logger, `Grid Square`) %>%
                 pivot_wider(id_cols = c("variable", "ID") ,names_from = "TIMESTAMP", values_from = "value")
         }
     })

     output$sf_timeseries <- renderPlot({
         #input$plot

         autoInvalidate()
         sapflow_data <- reactive_df()$sapflow

         if(is.null(input$plot)) {  # initial state before update
             sdata <- sapflow_data
         } else {
             sdata <- filter(sapflow_data, Plot == input$plot)
         }

         sdata %>%
             ggplot(aes(x = Timestamp, y = Value, group = Tree_Code)) +
             geom_line() +
             facet_wrap(~Species, scales = "free") +
             theme(axis.text.x = element_text(angle = 90)) +
             theme_minimal() +
             annotate(geom = "rect",
                      xmin = ymd_hms("2021-09-09 00:07:00", tz = "EST"),
                      xmax = ymd_hms("2021-09-09 17:00:00", tz = "EST"),
                      ymin = -Inf, ymax = Inf,
                      alpha = 0.2, fill = "deepskyblue")
     })

     output$teros_timeseries <- renderPlot({
         #input$plot

         autoInvalidate()
         tdata <- reactive_df()$teros

         if(is.null(input$tPlot)) {  # initial state before update
             tdata <- tdata
         } else {
             tdata <- filter(tdata, substr(Plot, 1, 1) == input$tPlot)
         }

         display_bounds <- tibble(variable = c("EC", "TSOIL", "VWC"),
                                  min_val = c(0, 15, 2000),
                                  max_val = c(500, 30, 4500))

         tdata %>%
             left_join(display_bounds, by = "variable") %>%
             filter(value >= min_val, value <= max_val) %>%
             ggplot(aes(x = TIMESTAMP, y = value, group = ID)) +
             geom_line() +
             facet_wrap(variable~Depth, scales = "free") +
             theme(axis.text.x = element_text(angle = 90)) +
             theme_minimal() +
             annotate(geom = "rect",
                      xmin = ymd_hms("2021-09-09 00:07:00", tz = "EST"),
                      xmax = ymd_hms("2021-09-09 17:00:00", tz = "EST"),
                      ymin = -Inf, ymax = Inf,
                      alpha = 0.2, fill = "deepskyblue")
     })

}
