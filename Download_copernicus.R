##### Step 1: load devtools and download CopernicusDownloadR 

library(devtools)
devtools::install_github("elifrag/CopernicusDownloadR")

library(CopernicusDownloadR) #does not work

##### Step 2: As library(CopernicusDownloadR) did notwork, chatGPT told me to use library(CopernicusDownloadeR) instead. It seems there was a typo somewhere in the creation of the package.

library(CopernicusDownloadeR) #WORKS!!!

##### Step 3: I had to download python 3.11 using the terminal

##### Step 4: Restart R

##### Step 5: Force R to use the python 3.11

library(reticulate)
use_python("/opt/homebrew/bin/python3.11", required = TRUE)
py_config()

##### Step 6: redefine function setupCopernicusEnvironment manually 

setupCopernicusEnvironment_fixed <- function(username, password) {
  # Use already-initialized Python
  message("Using Python: ", py_config()$python)
  
  # Try installing the package in the current environment
  py_install("copernicusmarine", pip = TRUE)
  
  # Import and authenticate the client
  cm <- import("copernicusmarine")
  
  # Login (if needed — adjust based on actual package usage)
  cm$login(username = username, password = password)
  
  return(cm)
}

##### Step 7: run redefined setupCopernicusEnvironment function

cm <- setupCopernicusEnvironment_fixed(
  username = "eduardoarle@tauex.tau.ac.il",
  password = "Telaviv2021.")

##### Step 8: load package with extra E again

library(CopernicusDownloadeR)

##### Step 9: install your modified function changing the name of the package to include the extra E (line 58)

#Modified function by Juan David with extra E 
extractData_mod <- function (cm, variables, lon, lat, startDate, endDate, depth,
                             frequency, csv_path = NULL, data_output = NULL)
{
  if (is.null(csv_path)) {
    csv_path <- system.file("extdata", "CopernicusVariables&Datasets.csv",
                            package = "CopernicusDownloadeR")
  }
  if (!file.exists(csv_path)) {
    stop("The CSV file containing variable and dataset information was not found.")
  }
  data <- read.csv(csv_path)
  if (length(lon) == 1) {
    minLon <- lon
    maxLon <- lon
  }
  else if (length(lon) == 2) {
    minLon <- lon[1]
    maxLon <- lon[2]
  }
  else {
    stop("Longitude must be either a single value or a range of length 2.")
  }
  if (length(lat) == 1) {
    minLat <- lat
    maxLat <- lat
  }
  else if (length(lat) == 2) {
    minLat <- lat[1]
    maxLat <- lat[2]
  }
  else {
    stop("Latitude must be either a single value or a range of length 2.")
  }
  if (length(depth) == 1) {
    minDepth <- depth
    maxDepth <- depth
  }
  else if (length(depth) == 2) {
    minDepth <- depth[1]
    maxDepth <- depth[2]
  }
  else {
    stop("Depth must be either a single value or a range of length 2.")
  }
  metadata_list <- list()
  for (variable in variables) {
    variable_info <- data[data$variable == variable, ]
    if (nrow(variable_info) == 0) {
      stop(paste("The variable", variable, "does not exist in the dataset."))
    }
    if (frequency == "daily") {
      dataset_id <- variable_info$dataset_id_daily
    }
    else if (frequency == "monthly") {
      dataset_id <- variable_info$dataset_id_monthly
    }
    else {
      stop("Invalid frequency. Choose 'daily' or 'monthly'.")
    }
    variable_name_copernicus <- variable_info$variableNameCopernicus
    cat("Downloading data for variable:", variable, "\n")
    result <- cm$subset(dataset_id = dataset_id, start_datetime = startDate,
                        end_datetime = endDate, variables = list(variable_name_copernicus),
                        minimum_longitude = minLon, maximum_longitude = maxLon,
                        minimum_latitude = minLat, maximum_latitude = maxLat,
                        minimum_depth = minDepth, maximum_depth = maxDepth,
                        output_directory = data_output)
    metadata_list[[variable]] <- list(variable = variable,
                                      dataset_id = dataset_id, variableNameCopernicus = variable_name_copernicus,
                                      startDate = startDate, endDate = endDate, minimum_longitude = minLon,
                                      maximum_longitude = maxLon, minimum_latitude = minLat,
                                      maximum_latitude = maxLat, minimum_depth = minDepth,
                                      maximum_depth = maxDepth, resolution = ifelse(!is.na(variable_info$resolution),
                                                                                    variable_info$resolution, "N/A"), doi = ifelse(!is.na(variable_info$doi),
                                                                                                                                   variable_info$doi, "N/A"), download_date = Sys.Date())
  }
  metadata_df <- do.call(rbind, lapply(metadata_list, as.data.frame))
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  metadataFileName <- paste0("copernicus_metadata_", timestamp,
                             ".csv")
  if (is.null(data_output)) {
    metadata_output <- file.path(getwd(), metadataFileName)
  }
  else {
    metadata_output <- file.path(data_output, metadataFileName)
  }
  write.csv(metadata_df, metadata_output, row.names = FALSE)
  cat("Data saved to:", metadata_output, "\n")
  return(result)
}

##### Step 9: reinstall function listVariables including the extra E

listVariables_mod <- function (csv_path = NULL, show = NULL) 
{
  if (is.null(csv_path)) {
    csv_path <- system.file("extdata", "CopernicusVariables&Datasets.csv", 
                            package = "CopernicusDownloadeR")
  }
  if (!file.exists(csv_path)) {
    stop("The CSV file containing variable and dataset information was not found.")
  }
  data <- read.csv(csv_path)
  if (is.null(show)) {
    cat("Available Variables:\n")
    print(unique(data$variable))
  }
  else if (show == "completeList") {
    cat("Available Data:\n")
    print(data[, c("variable", "metric", "datasetName", "startDate", 
                   "endDate", "resolution", "depths")])
  }
}


##### Step 10: list all variables with details
listVariables_mod()
listVariables_mod(show = "completeList")

##### Step 10: download

results <- extractData_mod(cm,
                variables = c("oceanTemperature", "salinity"),  #
                lon = c(-15, 37),
                lat = c(30, 46),
                startDate = "2000-01-01",
                endDate = "2020-01-01",
                depth = c(0, 1000),
                frequency = "monthly")


