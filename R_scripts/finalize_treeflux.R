# finalize_treeflux.R
# Script to process raw Licor files into
# ready-for-analysis data matched with metadata
# KAM/BBL 2025

# install.packages("remotes")
# remotes::install_github("COMPASS-DOE/fluxfinder")

library(fluxfinder)
options(fluxfinder.quiet = TRUE)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(readr)
library(arrow)

now <- function() format(Sys.time(), "%a %b %d %X %Y")

message(now(), " Welcome to finalize_treeflux.R")

DATA_DIR_ROOT <- "Data/tree_flux_licor/"
INPUT_DIR <- file.path(DATA_DIR_ROOT, "temporary_data")
OUTPUT_DIR <- file.path(DATA_DIR_ROOT, "processing_outputs")

# ---- Chamber metadata prep ----
CMD_DIR <- file.path(DATA_DIR_ROOT, "chamber_metadata")
cmd <- read_csv(file.path(CMD_DIR,
                          "Static chamber inventory_11272023.xlsx - Updated 11_27_2023.csv"),
                col_types = "cdddddddd") %>%
    select(size_class = `Size Class`,
           area_cm2 = `Surface Area of Tree Covered (cm2)`,
           volume_cm3 = `Volume (cm3)`) %>%
    # TEMPORARY -- collapse the two size 1 classes
    mutate(size_class = if_else(size_class %in% c("1a", "1b"), "1", size_class)) %>%
    group_by(size_class) %>%
    summarise(area = mean(area_cm2), volume_cm3 = mean(volume_cm3))

tree_assignments <- read_csv(file.path(CMD_DIR,
                                       "TEMPEST_TreeChamberInstallation_11272023.xlsx - Orginal.csv"),
                             col_types = "_c_ccc__") %>%
    filter(!is.na(Plot)) %>%
    select(Plot, Species, ID, size_class = `Chamber Size Class`)

chamber_metadata <- left_join(tree_assignments, cmd, by = "size_class")

# ---- Initialization ----

files <- list.files(INPUT_DIR, pattern = "\\.RDS$",
                    full.names = TRUE, recursive = TRUE)
message("I see ", length(files), " data files. Reading...")
results_list <- lapply(files, readRDS)


# OK, we've got the input files, now what?

# We use a "treeflux-processing-info" file to step through the data. This
# simplifies things and provides a documentary record of decisions, etc.
message("Reading processing info file...")
tfpi <- read_csv(file.path(DATA_DIR_ROOT, "treeflux-processing-info.csv"),
                 col_types = "cDcccdcc")

# ---- Processing ----
bind_rows(results_list) %>%
    rename(Plot = plot, Timepoint = timepoint) ->
    results

# TODO: print a warning or something if number of results tables ≠ tfpi
if(length(results_list) != nrow(tfpi)) {
    warning("The number of results is not equal to rows in processing list!")
}

message("Writing concentration data")
conc_fn <- file.path(OUTPUT_DIR, "tempest_tree_ghg_concentrations.csv")
message("\tWriting ", basename(conc_fn))
write_csv(results, conc_fn)
conc_fn_pqt <- gsub("csv", "parquet", conc_fn)
message("\tWriting ", basename(conc_fn_pqt))
arrow::write_parquet(results, conc_fn_pqt)

# ---- Flux calculation ----
library(fluxfinder)

# Bring in chamber metadata
results %>%
    left_join(chamber_metadata, by = c("Plot", "ID")) %>%
    mutate(Year = year(TIMESTAMP), Date = date(TIMESTAMP)) ->
    results

# CO2 fluxes
message("Computing CO2 fluxes...")
results %>%
    filter(!is.na(CO2)) %>%
    group_by(Year, Date, Plot, Timepoint, Species, ID) %>%
    filter(n() > 1) %>%
    group_modify(~ffi_fit_models(.x$secs,
                                 .x$CO2,
                                 area = .x$area[1],
                                 volume = .x$volume[1])) %>%
    select(Year, Date, Plot, Timepoint, Species, ID,
           CO2_lin_flux.estimate = lin_flux.estimate,
           CO2_lin_r.squared = lin_r.squared,
           CO2_rob_flux.estimate = rob_flux.estimate) ->
    fluxes_CO2

# CH4 fluxes
message("Computing CH4 fluxes...")
results %>%
    filter(!is.na(CH4)) %>%
    group_by(Year, Date, Plot, Timepoint, Species, ID) %>%
    filter(n() > 1) %>%
    group_modify(~ffi_fit_models(.x$secs,
                                 .x$CH4,
                                 area = .x$area[1],
                                 volume = .x$volume[1])) %>%
    select(Year, Date, Plot, Timepoint, Species, ID,
           CH4_lin_flux.estimate = lin_flux.estimate,
           CH4_lin_r.squared = lin_r.squared,
           CH4_rob_flux.estimate = rob_flux.estimate) ->
    fluxes_CH4

fluxes_CO2 %>%
    left_join(fluxes_CH4, by = c("Year", "Date", "Plot", "Timepoint", "Species", "ID")) %>%
    arrange(Year, Date, Plot, Timepoint, ID) ->
    fluxes

message("Writing flux data")
fluxes_fn <- file.path(OUTPUT_DIR, "tempest_tree_ghg_fluxes.csv")
message("\tWriting ", basename(fluxes_fn))
write_csv(fluxes, fluxes_fn)
fluxes_fn_pqt <- gsub("csv", "parquet", fluxes_fn)
message("\tWriting ", basename(fluxes_fn_pqt))
arrow::write_parquet(fluxes, fluxes_fn_pqt)

# ---- Summary plots ----
message("Writing summary plots")
fluxes %>%
    group_by(Year, Date, Plot, Timepoint) %>%
    # compute z-scores for
    mutate(z_CO2 = (CO2_rob_flux.estimate - mean(CO2_rob_flux.estimate, na.rm = TRUE)) /
               sd(CO2_rob_flux.estimate, na.rm = TRUE),
           z_CH4 = (CH4_rob_flux.estimate - mean(CH4_rob_flux.estimate, na.rm = TRUE)) /
               sd(CH4_rob_flux.estimate, na.rm = TRUE),
           lab_CO2 = if_else(abs(z_CO2) > 1.96, ID, ""),
           lab_CH4 = if_else(abs(z_CH4) > 1.95, ID, "")) ->
    fluxes_plot

# All data plots
fn <- file.path(OUTPUT_DIR, "tempest_CO2_fluxes_all.pdf")
ggplot(fluxes_plot, aes(yday(Date), CO2_rob_flux.estimate, color = Plot)) +
    geom_jitter() +
    facet_grid(Year ~ .) +
    xlab("Day of year") +
    ylab("Robust linear model flux (µmol/m2/s)") +
    ggtitle(fn)
message("\tSaving ", basename(fn), "...")
ggsave(fn, width = 12, height = 8)

fn <- file.path(OUTPUT_DIR, "tempest_CH4_fluxes_all.pdf")
ggplot(fluxes_plot, aes(yday(Date), CH4_rob_flux.estimate, color = Plot)) +
    geom_jitter() +
    facet_grid(Year ~ .) +
    xlab("Day of year") +
    ylab("Robust linear model flux (nmol/m2/s)") +
    ggtitle(fn)
message("\tSaving ", basename(fn), "...")
ggsave(fn, width = 12, height = 8)


# Annual plots
for(yr in unique(fluxes_plot$Year)) {
    fluxes_plot_yr <- filter(fluxes_plot, Year == yr)

    fn <- file.path(OUTPUT_DIR, paste0("tempest_CO2_fluxes_", yr, ".pdf"))
    ggplot(fluxes_plot_yr, aes(1, CO2_rob_flux.estimate, color = z_CO2)) +
        geom_jitter() +
        scale_color_distiller(type = "div") +
        geom_text(aes(label = lab_CO2), size = 2, na.rm = TRUE) +
        facet_grid(Timepoint ~ Plot) +
        ylab("Robust linear model flux (µmol/m2/s)") +
        coord_flip() +
        theme(axis.text.y = element_blank(), axis.title = element_blank()) +
        ggtitle(fn)
    message("\tSaving ", basename(fn), "...")
    ggsave(fn, width = 10, height = 6)

    fn <- file.path(OUTPUT_DIR, paste0("tempest_CH4_fluxes_", yr, ".pdf"))
    ggplot(fluxes_plot_yr, aes(1, CH4_rob_flux.estimate, color = z_CH4)) +
        geom_jitter() +
        scale_color_distiller(type = "div") +
        geom_text(aes(label = lab_CH4), size = 2, na.rm = TRUE) +
        facet_grid(Timepoint ~ Plot) +
        ylab("Robust linear model flux (nmol/m2/s)") +
        coord_flip() +
        theme(axis.text.y = element_blank(), axis.title = element_blank()) +
        ggtitle(fn)
    message("\tSaving ", basename(fn), "...")
    ggsave(fn, width = 10, height = 6)
}

# Species plots
fn <- file.path(OUTPUT_DIR, "tempest_CO2_species_fluxes.pdf")
ggplot(fluxes, aes(CO2_rob_flux.estimate, Date, color = Species)) +
    geom_point(na.rm = TRUE) +
    xlim(c(0,2)) +
    ggtitle(fn)
message("\tSaving ", basename(fn), "...")
ggsave(fn, width = 10, height = 8)

fn <- file.path(OUTPUT_DIR, "tempest_CH4_species_fluxes.pdf")
ggplot(fluxes, aes(CH4_rob_flux.estimate, Date, color = Species)) +
    geom_point(na.rm = TRUE) +
 #   xlim(c(0,2)) +
    ggtitle(fn)
message("\tSaving ", basename(fn), "...")
ggsave(fn, width = 10, height = 8)


message(now(), " All done")

