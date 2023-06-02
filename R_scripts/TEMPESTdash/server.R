# This is the server of the TEMPEST data dashboard
# June 2022

source("global.R")
source("flag_sensors.R")
source("maps.R")

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
            atroll <- withProgress(process_aquatroll(token, datadir), message = "Updating AquaTroll...")
            aquatroll <- list(
                aquatroll_600 = filter(atroll, Instrument == "TROLL600"),
                aquatroll_200 = filter(atroll, Instrument == "TROLL200")
            )
            sapflow %>%
                select(Timestamp, BattV_Avg, Plot, Logger) %>%
                group_by(Plot, Logger, Timestamp) %>%
                summarise(BattV_Avg = mean(BattV_Avg), .groups = "drop") ->
                battery
       }

        latest_ts <- with_tz(Sys.time(), tzone = "EST")

        # Do limits testing and compute data needed for badges
        sapflow %>%
            filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < latest_ts) %>%
            summarise(flag_sensors(Value, limits = SAPFLOW_RANGE)) ->
            sapflow_bdg

        sapflow %>%
            group_by(Tree_Code) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, Plot, Tree_Code, Value, Logger, Grid_Square) %>%
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("Tree_Code", "Plot", "Grid_Square") ,
                names_from = "Timestamp",
                values_from = "Value") ->
            sapflow_table_data

        # TEROS is awkward, because we only have one badge, but three
        # variables within a single dataset. We compute out-of-limits for each
        # variable, and then combine to a single value and badge color
        teros %>%
            filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < latest_ts) %>%
            left_join(TEROS_RANGE, by = "variable") ->
            teros_filtered

        teros_filtered %>%
            group_by(variable) %>%
            summarise(flag_sensors(value, limits = c(low[1], high[1]))) %>%
            summarise(fraction_in = weighted.mean(fraction_in, n)) %>%
            mutate(percent_in = paste0(round(fraction_in * 100, 0), "%"),
                   color = badge_color(1 - fraction_in)) ->
            teros_bdg

        teros_filtered %>%
            group_by(variable) %>%
            mutate(bad_sensor = which_outside_limits(value,
                                                     left_limit = low[1],
                                                     right_limit = high[1]),
                   .keep = "all") %>%
            filter(bad_sensor) %>%
            select(ID, Logger, Grid_Square) %>%
            distinct(ID, Logger) -> teros_bad_sensors

        # Aquatroll is similar: one badge, two datasets
        aquatroll$aquatroll_600 %>%
            select(Timestamp, Logger_ID, Well_Name, Temp) %>%
            mutate(Sensor = 600) -> a600

        aquatroll$aquatroll_200 %>%
            select(Timestamp, Logger_ID, Well_Name, Temp) %>%
            mutate(Sensor = 200) %>%
            bind_rows(a600) -> aquatroll_temp

        aquatroll_temp %>%
            filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < latest_ts) %>%
            mutate(bad_sensor = which_outside_limits(Temp,
                                                     left_limit = AQUATROLL_TEMP_RANGE[1],
                                                     right_limit = AQUATROLL_TEMP_RANGE[2])) %>%
            filter(bad_sensor) %>%
            select(Well_Name, Logger_ID) %>%
            arrange(Well_Name, Logger_ID) ->
            aquatroll_bad_sensors

        aquatroll_temp %>%
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
             sapflow_bdg = sapflow_bdg,
             sapflow_table_data = sapflow_table_data,

             teros = teros,
             teros_bad_sensors = teros_bad_sensors,
             teros_bdg = teros_bdg,

             aquatroll_600 = aquatroll$aquatroll_600,
             aquatroll_200 = aquatroll$aquatroll_200,
             aquatroll_temp = aquatroll_temp,
             aquatroll_bad_sensors = aquatroll_bad_sensors,
             aquatroll_bdg = aquatroll_bdg,

             battery = battery,
             battery_bdg = battery_bdg)
    })

    # ------------------ Dashboard graphs -----------------------------

    observeEvent(autoInvalidate(), {

        circleval <- round(as.numeric(difftime(with_tz(Sys.time(), tzone = "EST"),
                                               EVENT_START,
                                               units = "hours")) / EVENT_HOURS, 2)

        # Don't show a flood progress indicator if too far beyond the end
        if(circleval > 1.05) circleval <- NA

        update_progress("circle", circleval)
    })

    output$sapflow_bad_sensors <- DT::renderDataTable({

        reactive_df()$sapflow %>%
            filter(Timestamp > with_tz(Sys.time(), tzone = "EST") - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < with_tz(Sys.time(), tzone = "EST")) -> sapflow

        bad_sensors(sapflow, sapflow$Value, "Tree_Code", limits = SAPFLOW_RANGE) -> vals

        datatable(vals, options = list(searching = FALSE, pageLength = 5))
    })

    output$teros_bad_sensors <- DT::renderDataTable({
        reactive_df()$teros_bad_sensors %>%
            datatable(options = list(searching = FALSE, pageLength = 5))
    })

    output$troll_bad_sensors <- DT::renderDataTable({
        reactive_df()$aquatroll_bad_sensors %>%
            datatable(options = list(searching = FALSE, pageLength = 5))
    })

    output$batt_bad_sensors <- DT::renderDataTable({
        reactive_df()$battery %>%
            filter(Timestamp > with_tz(Sys.time(), tzone = "EST") - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < with_tz(Sys.time(), tzone = "EST"))  -> battery

        bad_sensors(battery, battery$BattV_Avg, "Logger", limits = VOLTAGE_RANGE) -> vals

        datatable(vals, options = list(searching = FALSE, pageLength = 5))
    })


    output$sapflow_plot <- renderPlotly({
        # Average sapflow data by plot and 15 minute interval
        # This graph is shown when users click the "Sapflow" tab on the dashboard

        sapflow <- reactive_df()$sapflow

        if(nrow(sapflow)) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            sapflow %>%
                filter(Timestamp > latest_ts - GRAPH_TIME_WINDOW * 60 * 60,
                       Timestamp < latest_ts) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot, Logger, Timestamp_rounded) %>%
                summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, Value, color = Plot, group = Logger)) +
                SAPFLOW_EVENT_RECT +
                geom_line() +
                xlab("") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                geom_hline(yintercept = SAPFLOW_RANGE, color = "grey", linetype = 2)  ->
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
            latest_ts <- with_tz(Sys.time(), tzone = "EST")
            teros %>%
                left_join(TEROS_RANGE, by = "variable") %>%
                filter(Timestamp > latest_ts - GRAPH_TIME_WINDOW * 60 * 60,
                       Timestamp < latest_ts) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot, variable, Logger, Timestamp_rounded) %>%
                summarise(value = mean(value, na.rm = TRUE), .groups = "drop") -> t

            ggplot(t) +
                geom_rect(data = TEROS_RANGE, group = 1,
                          aes(xmin = EVENT_START, xmax = EVENT_STOP, ymin = low, ymax = high), fill = "#BBE7E6", alpha = 0.7, col = "#BBE7E6") +
                facet_grid(variable~., scales = "free") +
                geom_line(aes(Timestamp_rounded, value, color = Plot, group = Logger)) +
                xlab("") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                geom_hline(data = TEROS_RANGE, aes(yintercept = low), color = "grey", linetype = 2) +
                geom_hline(data = TEROS_RANGE, aes(yintercept = high), color = "grey", linetype = 2) ->
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
        reactive_df()$aquatroll_200 %>%
            pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity"), names_to = "variable", values_to = "value") -> aq200

        reactive_df()$aquatroll_600 %>%
            pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity", "DO_mgl"), names_to = "variable", values_to = "value") %>%
            bind_rows(aq200) -> full_trolls_long

        if(nrow(full_trolls_long) > 1) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            full_trolls_long %>%
                filter(Timestamp > latest_ts - GRAPH_TIME_WINDOW * 60 * 60,
                       Timestamp < latest_ts) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Logger_ID, Well_Name, Timestamp_rounded, variable) %>%
                summarise(Well_Name = Well_Name,
                          value = mean(value, na.rm = TRUE), .groups = "drop") %>%
                ggplot(aes(Timestamp_rounded, value, color = Well_Name)) +
                geom_line() + facet_wrap(~variable, scales = "free") +
                # annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                #          xmin = EVENT_START, xmax = EVENT_STOP,
                #          ymin = min(AQUATROLL_TEMP_RANGE), ymax = max(AQUATROLL_TEMP_RANGE)) +
                xlab("") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) ->
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
            latest_ts <- with_tz(Sys.time(), tzone = "EST")
            battery %>%
                filter(Timestamp > latest_ts - GRAPH_TIME_WINDOW * 60 * 60,
                       Timestamp < latest_ts) %>%
                ggplot(aes(Timestamp, BattV_Avg, color = as.factor(Logger))) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                         xmin = EVENT_START, xmax = EVENT_STOP,
                         ymin = min(VOLTAGE_RANGE), ymax = max(VOLTAGE_RANGE)) +
                geom_line() +
                labs(x = "", y = "Battery (V)") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                geom_hline(yintercept = VOLTAGE_RANGE, color = "grey", linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }

        plotly::ggplotly(b)
    })

    output$sapflow_table <- DT::renderDataTable(datatable({
        autoInvalidate()

        reactive_df()$sapflow_table_data
    }))

    output$sapflow_detail_graph <- renderPlotly({

        if(length(input$sapflow_table_rows_selected)) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            reactive_df()$sapflow_table_data %>%
                slice(input$sapflow_table_rows_selected) %>%
                pull(Tree_Code) ->
                trees_selected

            reactive_df()$sapflow %>%
                filter(Tree_Code %in% trees_selected) %>%
                ggplot(aes(Timestamp, Value, group = Tree_Code, color = Plot)) +
                SAPFLOW_EVENT_RECT +
                geom_line() +
                xlab("") +
                xlim(c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                geom_hline(yintercept = SAPFLOW_RANGE, color = "grey", linetype = 2) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
})

    output$teros_table <- renderDataTable({
        autoInvalidate()

        reactive_df()$teros %>%
            group_by(ID, variable) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, ID, variable, value, Logger, Grid_Square) %>%
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("ID", "variable", "Grid_Square"), names_from = "Timestamp", values_from = "value")
        #}
    })

    output$teros_detail_graph <- renderPlotly({

        if(length(input$teros_table_rows_selected)) {
            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            reactive_df()$teros %>%
                group_by(ID, variable) %>%
                slice_tail(n = 10) %>%
                ungroup() %>%
                select(Timestamp, ID, variable, value, Logger, Grid_Square) %>%
                arrange(Timestamp) %>%
                pivot_wider(id_cols = c("ID", "variable", "Grid_Square"), names_from = "Timestamp", values_from = "value") %>%
                slice(input$teros_table_rows_selected) %>%
                select(variable, ID) ->
                tsensor_selected

            reactive_df()$teros %>%
                filter(ID %in% tsensor_selected$ID, variable %in% tsensor_selected$variable) %>%
                ggplot(aes(Timestamp, value, group = interaction(ID, variable), color = ID)) +
                geom_line() +
                xlab("") +
                xlim(c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })

    output$troll_table <- renderDataTable({
        autoInvalidate()

        reactive_df()$aquatroll_200 %>%
            pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity"), names_to = "variable", values_to = "value") %>%
            group_by(Well_Name, variable) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) -> aq200_long

        reactive_df()$aquatroll_600 %>%
            pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity", "DO_mgl"), names_to = "variable", values_to = "value") %>%
            group_by(Well_Name, variable) %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) %>%
            bind_rows(aq200_long) -> trolls

        trolls %>%
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("Well_Name", "variable", "Plot", "Instrument"), names_from = "Timestamp", values_from = "value")
    })

    output$troll_detail_graph <- renderPlotly({
        if(length(input$troll_table_rows_selected)) {

            latest_ts <- with_tz(Sys.time(), tzone = "EST")

            reactive_df()$aquatroll_200 %>%
                pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity"), names_to = "variable", values_to = "value") -> aq200

            reactive_df()$aquatroll_600 %>%
                pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity", "DO_mgl"), names_to = "variable", values_to = "value") %>%
                bind_rows(aq200) -> full_trolls_long

            reactive_df()$aquatroll_200 %>%
                pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity"), names_to = "variable", values_to = "value") %>%
                group_by(Well_Name, variable) %>%
                slice_tail(n = 10) %>%
                ungroup() %>%
                select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) -> aq200_long

            reactive_df()$aquatroll_600 %>%
                pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity", "DO_mgl"), names_to = "variable", values_to = "value") %>%
                group_by(Well_Name, variable) %>%
                slice_tail(n = 10) %>%
                ungroup() %>%
                select(Timestamp, Well_Name, Instrument, variable, value, Logger_ID, Plot) %>%
                bind_rows(aq200_long) -> trolls

            trolls %>%
                pivot_wider(id_cols = c("Well_Name", "variable", "Plot", "Instrument"), names_from = "Timestamp", values_from = "value") %>%
                slice(input$troll_table_rows_selected) %>%
                select(variable, Well_Name) ->
                aqsensor_selected

            full_trolls_long %>%
                filter(Well_Name %in% aqsensor_selected$Well_Name, variable %in% aqsensor_selected$variable) %>%
                ggplot(aes(Timestamp, value, group = interaction(Well_Name, variable), color = Well_Name)) +
                geom_line() +
                xlab("") +
                xlim(c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) ->
                b
        } else {
            b <- NO_DATA_GRAPH
        }
        plotly::ggplotly(b)
    })

    output$btable <- DT::renderDataTable({
        autoInvalidate()

        reactive_df()$battery %>%
            select(Timestamp, BattV_Avg, Plot, Logger) %>%
            group_by(Plot, Logger) %>%
            distinct() %>%
            slice_tail(n = 10) %>%
            ungroup() %>%
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("Plot", "Logger"), names_from = "Timestamp", values_from = "BattV_Avg") %>%
            datatable()
    })

    output$map <- renderPlot({
        make_plot_map(input$map_plot, map_items = input$mapitems)
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

# ------------------ Text alerts -----------------------------

    observeEvent(input$txt_alert, {


        # gm_auth_configure(path = "PATH TO JSON HERE")
        # gm_oauth_app()

        carrier_email <- if(input$carrier == "Verizon") {
            carrier_email <- "@vtext.com"
        } else if(input$carrier == "AT&T") {
            carrier_email <- "@txt.att.net"
        } else if(input$carrier == "Sprint") {
            carrier_email <- "@messaging.sprintpcs.com"
        } else if(input$carrier == "T-Mobile") {
            carrier_email <- "@tmomail.net"
        }

        phone_number <- parse_number(input$phone_number, locale = locale(grouping_mark = "-"))

        text_msg <- gm_mime() %>%
            gm_to(paste0(phone_number, carrier_email)) %>%
            gm_from("compassfme.tools@gmail.com") %>%
            gm_text_body("Gmailr is a very handy package!")

        # need to add how often to send, right now only once
        gm_send_message(text_msg)

        shinyalert("Message sent", type = "success")

    })



}
