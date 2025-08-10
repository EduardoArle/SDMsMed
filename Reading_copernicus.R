#load packages
library(terra)

#list WDs
wd_test <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Test_download'

#read in
setwd(wd_test)
v1 <- rast('cmems_mod_glo_phy_my_0.083deg_P1M-m_so_33.00W-35.00E_31.00N-33.00N_0.49-902.34m_2000-01-01-2020-01-01.nc')

v2 <- rast('cmems_mod_glo_phy_my_0.083deg_P1M-m_thetao_33.00W-35.00E_31.00N-33.00N_0.49-902.34m_2000-01-01-2020-01-01.nc')


nlyr(v1)

plot(v1[[1]])

function (csv_path = NULL, show = NULL) 
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