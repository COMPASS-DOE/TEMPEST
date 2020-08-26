# datahub.R

# Generate files for PNNL's DataHub repository
make_datahub <- function() {
  
  DIR <- "datahub"
  
  if(!dir.exists(DIR)) {
    message("Creating ", DIR)
    dir.create(DIR)
  }
  
  MDF <- file.path(DIR, "metadata.txt")
  if(file.exists(MDF)) {
    message("Removing old ", MDF)
    file.remove(MDF)
  }  
  
  cat("Metadata file - PREMIS-ghg data products",
      date(), sep = "\n", file = MDF)
  
  write_df <- function(x) {
    x_name <- deparse(substitute(x))
    x_fn <- file.path(DIR, paste0(x_name, ".csv"))
    message("Writing ", x_fn)
    write_csv(x, x_fn)
  }
  
  write_metadata <- function(x, description, doi, metadata, mdf = MDF) {
    x_name <- deparse(substitute(x))
    if(!identical(colnames(x), names(metadata))) {
      stop("Metadata doesn't match dataset for ", x_name)
    }
    
    message("Updating ", mdf, " with metadata for ", x_name)
    
    cat("\n===================================",
        paste("Metadata for object: ", x_name),
        paste("Description: ", description),
        paste("Paper or data reference(s):", paste(doi, collapse = " "), "\n"),
        file = mdf, sep = "\n", append = TRUE)
    cat(paste(names(metadata), metadata, sep = ": "),
        file = mdf, sep = "\n", append = TRUE)
  }
  
  con_licor_data <- readd("con_licor_data")
  write_df(con_licor_data)
  write_metadata(con_licor_data, 
                 description = "Continuous soil respiration fluxes measured at SERC 2018-2019",
                 doi = "In press (JGR)",
                 metadata = c("table" = "Internal reference; ignore",
                              "Timestamp" = "Timestamp in YYYY-MM-DDTHH:MM:SSZ (ISO 8601) format",
                              "Label" = "Internal reference; ignore",
                              "Port" = "Multiplexer port number, integer",
                              "Flux" = "Soil surface CO2 flux, µmol/m2/s",
                              "R2" = "IRGA model-fitting R2 (see LI-8100A manual)",
                              "Tcham" = "Temperature in chamber, degC",
                              "V1" = "Internal reference; ignore",
                              "SMoist" = "Soil moisture, volumetric (normally 0-1)",
                              "T5" = "Soil temperature at 5 cm, degC",
                              "V4" = "Internal reference; ignore",
                              "RH" = "Relative humidity in chamber (0-100)",
                              "Cdry" = "Atmospheric [CO2] concentration, ppmv",
                              "Comments" = "Comments"))
  
  readd("licor_daily_data") %>% 
    select(Timestamp, Collar, Origin_Plot, Destination_Plot, meanFlux, sdFlux, meanSM, meanT5, meanT20) ->
    licor_daily_data
  
  write_df(licor_daily_data)
  write_metadata(licor_daily_data,
                 description = "Survey soil respiration fluxes measured at SERC 2018-2019",
                 doi = c("In press (Biogeosciences)"),
                 metadata = c("Timestamp" = "Timestamp in YYYY-MM-DDTHH:MM:SSZ (ISO 8601) format",
                              "Collar" = "Transplant collar number",
                              "Origin_Plot" = "Origin plot of transplants",
                              "Destination_Plot" = "Destination plot of transplants",
                              "meanFlux" = "Mean of soil surface CO2 flux, µmol/m2/s",
                              "sdFlux" = "Standard deviation of  soil surface CO2 flux, µmol/m2/s",
                              "meanSM" = "Soil moisture, volumetric (normally 0-1)",
                              "meanT5" = "Soil temperature at 5 cm, degC",
                              "meanT20" = "Soil temperature at 20 cm, degC"
                 ))
  
  plot_data <- readd("plot_data")
  write_df(plot_data)
  write_metadata(plot_data,
                 description = "SERC plot metadata",
                 doi = c("In press (Biogeosciences)", "In press (JGR)"),
                 metadata = c("Site" = "Site name (SERC, Smithsonian Environmental Research Center)",
                              "Plot" = "Plot code (High/Medium/Low Salinity, High/Medium/Low Elevation",
                              "Salinity" = "Salinity level of creek",
                              "Elevation" = "High, medium, or low (creekside)",
                              "Longitude" = "Decimal longitude (WGS84)",
                              "Latitude" = "Decimal latitude (WGS84)",
                              "Plot_area_m2" = "Area of plot, m2"))
}
