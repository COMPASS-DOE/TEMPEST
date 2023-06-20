# This is the server of the TEMPEST data dashboard
# June 2022

source("global.R")
source("flag_sensors.R")

server <- function(input, output) {

    autoInvalidate <- reactiveTimer(15 * 60 * 1000)
    alertInvalidate <- reactiveTimer(60 * 60 * 1000)

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
                   Timestamp < latest_ts) ->
            sapflow_filtered

        sapflow_filtered %>%
            summarise(flag_sensors(Value, limits = SAPFLOW_RANGE)) ->
            sapflow_bdg

        sapflow_filtered %>%
            mutate(bad_sensor = which_outside_limits(Value,
                                                     left_limit = SAPFLOW_RANGE[1],
                                                     right_limit = SAPFLOW_RANGE[2])) %>%
            filter(bad_sensor) %>%
            select(Plot, Tree_Code, Logger, Grid_Square, Out_Of_Plot) %>%
            distinct(Tree_Code, Logger, .keep_all = TRUE) ->
            sapflow_bad_sensors

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
            select(Plot, ID, Depth, Logger, Grid_Square) %>%
            distinct(ID, Logger, .keep_all = TRUE) ->
            teros_bad_sensors

        # Aquatroll is similar: one badge, two datasets
        aquatroll$aquatroll_600 %>%
            select(Timestamp, Logger_ID, Well_Name, Temp) %>%
            mutate(Sensor = 600) -> a600

        aquatroll$aquatroll_200 %>%
            select(Timestamp, Logger_ID, Well_Name, Temp) %>%
            mutate(Sensor = 200) %>%
            bind_rows(a600) %>%
            filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < latest_ts) ->
            aquatroll_filtered

        aquatroll_filtered %>%
            mutate(bad_sensor = which_outside_limits(Temp,
                                                     left_limit = AQUATROLL_TEMP_RANGE[1],
                                                     right_limit = AQUATROLL_TEMP_RANGE[2])) %>%
            filter(bad_sensor) %>%
            select(Well_Name, Logger_ID) %>%
            arrange(Well_Name, Logger_ID) ->
            aquatroll_bad_sensors

        aquatroll_filtered %>%
            summarise(flag_sensors(Temp, limits = AQUATROLL_TEMP_RANGE)) ->
            aquatroll_bdg

        battery %>%
            filter(Timestamp > latest_ts - FLAG_TIME_WINDOW * 60 * 60,
                   Timestamp < latest_ts) ->
            battery_filtered

        battery_filtered %>%
            summarise(flag_sensors(BattV_Avg, limits = VOLTAGE_RANGE)) ->
            battery_bdg

        # Return data and badge information
        list(sapflow = sapflow,
             sapflow_filtered = sapflow_filtered,
             sapflow_bdg = sapflow_bdg,
             sapflow_table_data = sapflow_table_data,
             sapflow_bad_sensors = sapflow_bad_sensors,

             teros = teros,
             teros_filtered = teros_filtered,
             teros_bad_sensors = teros_bad_sensors,
             teros_bdg = teros_bdg,

             aquatroll_600 = aquatroll$aquatroll_600,
             aquatroll_200 = aquatroll$aquatroll_200,
             aquatroll_filtered = aquatroll_filtered,
             aquatroll_bad_sensors = aquatroll_bad_sensors,
             aquatroll_bdg = aquatroll_bdg,

             battery = battery,
             battery_bdg = battery_bdg)

    })

    progress <- reactive({
        EVENT_START <- as_datetime(paste(input$event_date, input$event_start), tz = "EST")
        EVENT_STOP <- EVENT_START + hours(10)
        EVENT_HOURS <- as.numeric(difftime(EVENT_STOP, EVENT_START, units = "hours"))

        list(EVENT_START = EVENT_START,
             EVENT_STOP = EVENT_STOP,
             EVENT_HOURS = EVENT_HOURS)
    })

    # ------------------ Dashboard graphs -----------------------------

    observeEvent({
        input$prog_button
        autoInvalidate() # for actual app, we can have multiple triggers
    }, {
        circleval <- round(as.numeric(difftime(with_tz(Sys.time(), tzone = "EST"),
                                               progress()$EVENT_START,
                                               units = "hours")) / progress()$EVENT_HOURS, 2)

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

        battery[!between(battery$BattV_Avg, min(VOLTAGE_RANGE), max(VOLTAGE_RANGE)), ] %>% select(Logger) -> bounds

        battery[is.na(battery$BattV_Avg), ] %>% select(Logger) -> nas

        unique(bind_rows(nas, bounds)) -> vals

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
                geom_rect(aes(xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                              ymin = min(SAPFLOW_RANGE), ymax = max(SAPFLOW_RANGE)),
                          fill = "#BBE7E6",
                          alpha = 0.7,
                          col = "#BBE7E6") +
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
                # Certain versions of plotly seem to have a bug and produce
                # a tidyr::pivot error when there's a 'variable' column; rename
                rename(var = variable) %>%
                filter(Timestamp > latest_ts - GRAPH_TIME_WINDOW * 60 * 60,
                       Timestamp < latest_ts) %>%
                mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
                group_by(Plot, var, Logger, Timestamp_rounded) %>%
                summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
                left_join(TEROS_RANGE, by = c("var" = "variable")) -> tdat

            ggplot(tdat) +
                geom_rect(group = 1,
                          aes(xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                              ymin = low, ymax = high), fill = "#BBE7E6", alpha = 0.7, col = "#BBE7E6") +
                facet_wrap(~var, scales = "free", ncol = 2) +
                geom_line(aes(Timestamp_rounded, value, color = Plot, group = Logger)) +
                xlab("") +
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                geom_hline(aes(yintercept = low), color = "grey", linetype = 2) +
                geom_hline(aes(yintercept = high), color = "grey", linetype = 2) ->
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

        aquatroll <- reactive_df()$aquatroll_filtered

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
                coord_cartesian(xlim = c(latest_ts - GRAPH_TIME_WINDOW * 60 * 60, latest_ts)) +
                annotate("rect", fill = "#BBE7E6", alpha = 0.7,
                         xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                         ymin = min(AQUATROLL_TEMP_RANGE), ymax = max(AQUATROLL_TEMP_RANGE)) +
                geom_line() + facet_wrap(~variable, scales = "free") +
                xlab("") ->
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
                         xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                         ymin = min(VOLTAGE_RANGE), ymax = max(VOLTAGE_RANGE)) +
                geom_line() +
                labs(x = "", y = "Battery (V)", color = "Logger") +
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
                geom_rect(aes(xmin = progress()$EVENT_START, xmax = progress()$EVENT_STOP,
                              ymin = min(SAPFLOW_RANGE), ymax = max(SAPFLOW_RANGE)),
                          fill = "#BBE7E6",
                          alpha = 0.7,
                          col = "#BBE7E6") +
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
            select(Timestamp, ID, Plot, variable, value, Logger, Grid_Square) %>%
            arrange(Timestamp) %>%
            pivot_wider(id_cols = c("ID", "Plot", "variable", "Grid_Square"), names_from = "Timestamp", values_from = "value")
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
                labs(color = "Well Name") +
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

    # ------------------ Maps tab -----------------------------
    statusmap <- mapsServer("mapsTab", STATUS_MAP = TRUE, dd = reactive_df())
    output$status_map <- renderPlot(statusmap())
    datamap <- mapsServer("mapsTab", STATUS_MAP = FALSE, dd = reactive_df())
    output$data_map <- renderPlot(datamap())


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

    # initial_alert <- observeEvent(input$txt_alert,{
    #     #only sent to input number when user first inputs information
    #
    #     phone_number <- parse_number(input$phone_number, locale = locale(grouping_mark = "-"))
    #
    #     text_msg <- gm_mime() %>%
    #         gm_to(paste0(phone_number, carrier_email)) %>% #change to link with df
    #         gm_from("compassfme.tools@gmail.com") %>%
    #         gm_text_body("Thank you for signing up for hourly alerts!")
    #
    #     # need to add how often to send, right now only once
    #     gm_send_message(text_msg)
    #
    # })
    #
    observeEvent({

        #this will calculate values and send out messages to everyone in "new_user" df
        # could just have people not choose what they want alerts for?
        #initial_alert()
        alertInvalidate()
    }, {

        #     reactive_df()$teros_filtered %>%
        #         filter(!ID %in% reactive_df()$teros_bad_sensors$ID) %>%
        #         group_by(Plot, variable) %>%
        #         filter(Timestamp == dplyr::last(Timestamp)) %>%
        #         summarise(value = mean(value, na.rm = TRUE)) %>%
        #         mutate(dataset = "TEROS (avg)") -> teros
        #
        #     reactive_df()$aquatroll_200 %>%
        #         pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity"),
        #                      names_to = "variable", values_to = "value") -> aq200
        #
        #     reactive_df()$aquatroll_600 %>%
        #         pivot_longer(cols = c("Temp", "Pressure_psi", "Salinity", "DO_mgl"),
        #                      names_to = "variable", values_to = "value") %>%
        #         bind_rows(aq200) -> full_trolls_long
        #
        #     full_trolls_long %>%
        #         filter(Timestamp == dplyr::last(Timestamp)) %>%
        #         group_by(Plot, variable) %>%
        #         summarise(value = mean(value, na.rm = TRUE)) %>%
        #         mutate(dataset = "AquaTroll (last)") -> trolls
        #
        #     reactive_df()$battery %>%
        #         filter(Timestamp == dplyr::last(Timestamp)) %>%
        #         select(Plot, Logger, BattV_Avg) %>%
        #         rename(value = BattV_Avg) %>%
        #         mutate(dataset = "Battery (last)") -> battery
        #
        # write.csv(battery, file = textConnection("bat", "w"), quote = FALSE)

        msg <- paste(
            paste0("System status as of: ",
                   with_tz(Sys.time(), tzone = "America/New_York"), " EDT"),
            "",
            paste0("Sapflow: ", reactive_df()$sapflow_bdg$percent_in),
            paste0("TEROS: ", reactive_df()$teros_bdg$percent_in),
            paste0("Aquatroll: ", reactive_df()$aquatroll_bdg$percent_in),
            paste0("Battery: ", reactive_df()$battery_bdg$percent_in),
            # "",
            # "** Battery details **",
            # paste(bat, collapse = "\n"),
            sep = "\n")
        #
        # cat("Sapflow:", reactive()$sapflow_bdg$percent_in, "\n",
        #     "TEROS:", reactive()$teros_bdg$percent_in, "\n",
        #     "Aquatroll:", reactive()$aquatroll_bdg$percent_in, "\n",
        #     "Battery:", reactive()$battery_bdg$percent_in)

        for(i in seq_len(nrow(TEXT_MSG_USERS))) {
            phone_number <- TEXT_MSG_USERS$number[i]
            carrier <- TEXT_MSG_USERS$carrier[i]

            carrier_email <- if(carrier == "Verizon") {
                carrier_email <- "@vtext.com"
            } else if(carrier == "AT&T") {
                carrier_email <- "@txt.att.net"
            } else if(carrier == "Sprint") {
                carrier_email <- "@messaging.sprintpcs.com"
            } else if(carrier == "T-Mobile") {
                carrier_email <- "@tmomail.net"
            }

            email <- paste0(phone_number, carrier_email)

            # Wrap this in a try so that if not authorized the app doesn't stop
            try({
                text_msg <- gm_mime() %>%
                    gm_to(email) %>%
                    gm_from("compassfme.tools@gmail.com") %>%
                    gm_text_body(msg) # CHANGE THIS

                # need to add how often to send, right now only once

                gm_send_message(text_msg)
            })
        }


    })
    #
    #     new_user <- reactiveValues(data = data_frame(phone_number = numeric(), choices = character()))

    #     observeEvent(input$txt_alert, {
    # # this call will append a df with information of choices
    #
    #         # gm_auth_configure(path = "PATH TO JSON HERE")
    #         # gm_oauth_app()
    #
    #         carrier_email <- if(input$carrier == "Verizon") {
    #             carrier_email <- "@vtext.com"
    #         } else if(input$carrier == "AT&T") {
    #             carrier_email <- "@txt.att.net"
    #         } else if(input$carrier == "Sprint") {
    #             carrier_email <- "@messaging.sprintpcs.com"
    #         } else if(input$carrier == "T-Mobile") {
    #             carrier_email <- "@tmomail.net"
    #         }
    #
    #         p_number <- parse_number(input$phone_number, locale = locale(grouping_mark = "-"))
    #
    #         email <- paste0(phone_number, carrier_email)
    #
    #         new_user$data <- rbind(new_user$data, data_frame(phone_number = email, choices = input$number)) # need to figure out how to paste choices
    #
    #         shinyalert("Message sent", type = "success")
    #
    #     })
}
