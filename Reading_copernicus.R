#load packages
library(terra)

#list WDs
wd_test <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Test_download'
wd_processed <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/Copernicus/Processed Layers'

#read in
setwd(wd_test)

so <- rast('cmems_mod_glo_phy_my_0.083deg_P1M-m_so_15.00W-37.00E_30.00N-46.00N_0.49-902.34m_2000-01-01-2020-01-01.nc')

thetao <- rast('cmems_mod_glo_phy_my_0.083deg_P1M-m_thetao_15.00W-37.00E_30.00N-46.00N_0.49-902.34m_2000-01-01-2020-01-01.nc')


##### Salinity #####

#split the stacks per month

#get the names of the layers
so_names <- names(so) 

#parse depth (numeric) and time index (integer) from the names
depths <- as.numeric(sub(".*depth=([0-9\\.]+)_.*", "\\1", so_names))
time_idx <- as.integer(sub(".*_([0-9]+)$", "\\1", so_names))

#unique sorted time indices (months)
uniq_idx <- sort(unique(time_idx))

#build the list: for each time index, subset all layers with that index,
#order them by depth (ascending), and return a SpatRaster of those layers
so_months <- lapply(uniq_idx, function(i) {
  layers <- which(time_idx == i)
  layers <- layers[order(depths[layers])]    # ensure surface -> deep order
  so[[layers]]})                         # returns a SpatRaster of these layers


#name the list elements using time metadata 
tr <- time(so)

names(so_months) <- sapply(uniq_idx, function(i) {
    first_layer_idx <- which(time_idx == i)[1]
    format(tr[first_layer_idx], "%Y-%m")})    # "YYYY-MM"

#exclude January 2020 to make a 20 year period
so_months_20y <- so_months[-241]


#calculate meanSal over 20 years (2000-01 to 2019-12)

#make a copy of one of the stacks to populate with the results of the calculations
meanSal_2000_2020 <- so_months[[1]]

#replace every value with an invalid number (9999)
for(i in 1:nlyr(meanSal_2000_2020))
{
  meanSal_2000_2020[[i]][!is.na(meanSal_2000_2020[[i]][])] <- 9999
  print(i)
}

#calculate mean salinity for each layer across 20 years
for(i in 1:nlyr(meanSal_2000_2020))
{
  #save info we will need
  varname <- varnames(meanSal_2000_2020[[i]])
  name <- names(meanSal_2000_2020[[i]])
  unit <- units(meanSal_2000_2020[[i]])
  
  #select each depth layer from all years
  layer <- lapply(so_months_20y, function(x){x[[i]]})
  
  #stack all rasters of same layer
  layer_stack <- rast(layer)
  
  #average the values for each layer
  meanSal_2000_2020[[i]] <- app(layer_stack, mean, na.rm = TRUE)
  
  #put information lost back
  varnames(meanSal_2000_2020[[i]]) <- varname
  names(meanSal_2000_2020[[i]]) <- name
  units(meanSal_2000_2020[[i]]) <- unit
  
  print(i)
}

#save
setwd(wd_processed)
writeRaster(meanSal_2000_2020, 'MeanSalinity_2000_2020.tif')


#calculate maxSal over 20 years (2000-01 to 2019-12)

#make a copy of one of the stacks to populate with the results of the calculations
maxSal_2000_2020 <- so_months[[1]]

#replace every value with an invalid number (9999)
for(i in 1:nlyr(maxSal_2000_2020))
{
  maxSal_2000_2020[[i]][!is.na(maxSal_2000_2020[[i]][])] <- 9999
  print(i)
}

#calculate mean salinity for each layer across 20 years
for(i in 1:nlyr(maxSal_2000_2020))
{
  #save info we will need
  varname <- varnames(maxSal_2000_2020[[i]])
  name <- names(maxSal_2000_2020[[i]])
  unit <- units(maxSal_2000_2020[[i]])
  
  #select each depth layer from all years
  layer <- lapply(so_months_20y, function(x){x[[i]]})
  
  #stack all rasters of same layer
  layer_stack <- rast(layer)
  
  #extract years from the time dimension
  years <- format(time(layer_stack), "%Y")
  
  #split layer indices by year
  year_groups <- split(seq_along(years), years)
  
  #calculate annual maxima
  annual_max_list <- lapply(year_groups, function(idxs) {
    yearly_stack <- layer_stack[[idxs]]
    app(yearly_stack, max, na.rm = TRUE)
  })
  
  #stack annual max rasters
  annual_max_stack <- rast(annual_max_list)
  
  # Calculate long-term average of annual maxima
  maxSal_2000_2020[[i]]  <- app(annual_max_stack, mean, na.rm = TRUE)
  
  #put information lost back
  varnames(maxSal_2000_2020[[i]]) <- varname
  names(maxSal_2000_2020[[i]]) <- name
  units(maxSal_2000_2020[[i]]) <- unit
  
  print(i)
}

#save
setwd(wd_processed)
writeRaster(maxSal_2000_2020, 'MaxSalinity_2000_2020.tif')


#calculate minSal over 20 years (2000-01 to 2019-12)

#make a copy of one of the stacks to populate with the results of the calculations
minSal_2000_2020 <- so_months[[1]]

#replace every value with an invalid number (9999)
for(i in 1:nlyr(minSal_2000_2020))
{
  minSal_2000_2020[[i]][!is.na(minSal_2000_2020[[i]][])] <- 9999
  print(i)
}

#calculate mean salinity for each layer across 20 years
for(i in 1:nlyr(minSal_2000_2020))
{
  #save info we will need
  varname <- varnames(minSal_2000_2020[[i]])
  name <- names(minSal_2000_2020[[i]])
  unit <- units(minSal_2000_2020[[i]])
  
  #select each depth layer from all years
  layer <- lapply(so_months_20y, function(x){x[[i]]})
  
  #stack all rasters of same layer
  layer_stack <- rast(layer)
  
  #extract years from the time dimension
  years <- format(time(layer_stack), "%Y")
  
  #split layer indices by year
  year_groups <- split(seq_along(years), years)
  
  #calculate annual maxima
  annual_min_list <- lapply(year_groups, function(idxs) {
    yearly_stack <- layer_stack[[idxs]]
    app(yearly_stack, min, na.rm = TRUE)
  })
  
  #stack annual max rasters
  annual_min_stack <- rast(annual_min_list)
  
  # Calculate long-term average of annual minima
  minSal_2000_2020[[i]]  <- app(annual_min_stack, mean, na.rm = TRUE)
  
  #put information lost back
  varnames(minSal_2000_2020[[i]]) <- varname
  names(minSal_2000_2020[[i]]) <- name
  units(minSal_2000_2020[[i]]) <- unit
  
  print(i)
}

#save
setwd(wd_processed)
writeRaster(minSal_2000_2020, 'MinSalinity_2000_2020.tif')



##### Temperature #####

#split the stacks per month

#get the names of the layers
thetao_names <- names(thetao) 

#parse depth (numeric) and time index (integer) from the names
depths <- as.numeric(sub(".*depth=([0-9\\.]+)_.*", "\\1", thetao_names))
time_idx <- as.integer(sub(".*_([0-9]+)$", "\\1", thetao_names))

#unique sorted time indices (months)
uniq_idx <- sort(unique(time_idx))

#build the list: for each time index, subset all layers with that index,
#order them by depth (ascending), and return a SpatRaster of those layers
thetao_months <- lapply(uniq_idx, function(i) {
  layers <- which(time_idx == i)
  layers <- layers[order(depths[layers])]    # ensure surface -> deep order
  thetao[[layers]]})                         # returns a SpatRaster of these layers


#name the list elements using time metadata 
tr <- time(thetao)

names(thetao_months) <- sapply(uniq_idx, function(i) {
  first_layer_idx <- which(time_idx == i)[1]
  format(tr[first_layer_idx], "%Y-%m")})    # "YYYY-MM"

#exclude January 2020 to make a 20 year period
thetao_months_20y <- thetao_months[-241]


#calculate meanT over 20 years (2000-01 to 2019-12)

#make a copy of one of the stacks to populate with the results of the calculations
meanTemp_2000_2020 <- thetao_months[[1]]

#replace every value with an invalid number (9999)
for(i in 1:nlyr(meanTemp_2000_2020))
{
  meanTemp_2000_2020[[i]][!is.na(meanTemp_2000_2020[[i]][])] <- 9999
  print(i)
}

#calculate mean salinity for each layer across 20 years
for(i in 1:nlyr(meanTemp_2000_2020))
{
  #save info we will need
  varname <- varnames(meanTemp_2000_2020[[i]])
  name <- names(meanTemp_2000_2020[[i]])
  unit <- units(meanTemp_2000_2020[[i]])
  
  #select each depth layer from all years
  layer <- lapply(thetao_months_20y, function(x){x[[i]]})
  
  #stack all rasters of same layer
  layer_stack <- rast(layer)
  
  #average the values for each layer
  meanTemp_2000_2020[[i]] <- app(layer_stack, mean, na.rm = TRUE)
  
  #put information lost back
  varnames(meanTemp_2000_2020[[i]]) <- varname
  names(meanTemp_2000_2020[[i]]) <- name
  units(meanTemp_2000_2020[[i]]) <- unit
  
  print(i)
}

#save
setwd(wd_processed)
writeRaster(meanTemp_2000_2020, 'MeanTemperature_2000_2020.tif', overwrite = TRUE)



#calculate maxSal over 20 years (2000-01 to 2019-12)

#make a copy of one of the stacks to populate with the results of the calculations
maxTemp_2000_2020 <- thetao_months[[1]]

#replace every value with an invalid number (9999)
for(i in 1:nlyr(maxTemp_2000_2020))
{
  maxTemp_2000_2020[[i]][!is.na(maxTemp_2000_2020[[i]][])] <- 9999
  print(i)
}

#calculate mean salinity for each layer across 20 years
for(i in 1:nlyr(maxTemp_2000_2020))
{
  #save info we will need
  varname <- varnames(maxTemp_2000_2020[[i]])
  name <- names(maxTemp_2000_2020[[i]])
  unit <- units(maxTemp_2000_2020[[i]])
  
  #select each depth layer from all years
  layer <- lapply(thetao_months_20y, function(x){x[[i]]})
  
  #stack all rasters of same layer
  layer_stack <- rast(layer)
  
  #extract years from the time dimension
  years <- format(time(layer_stack), "%Y")
  
  #split layer indices by year
  year_groups <- split(seq_along(years), years)
  
  #calculate annual maxima
  annual_max_list <- lapply(year_groups, function(idxs) {
    yearly_stack <- layer_stack[[idxs]]
    app(yearly_stack, max, na.rm = TRUE)
  })
  
  #stack annual max rasters
  annual_max_stack <- rast(annual_max_list)
  
  # Calculate long-term average of annual maxima
  maxTemp_2000_2020[[i]]  <- app(annual_max_stack, mean, na.rm = TRUE)
  
  #put information lost back
  varnames(maxTemp_2000_2020[[i]]) <- varname
  names(maxTemp_2000_2020[[i]]) <- name
  units(maxTemp_2000_2020[[i]]) <- unit
  
  print(i)
}

#save
setwd(wd_processed)
writeRaster(maxTemp_2000_2020, 'MaxTemperature_2000_2020.tif')


#calculate minSal over 20 years (2000-01 to 2019-12)

#make a copy of one of the stacks to populate with the results of the calculations
minTemp_2000_2020 <- thetao_months[[1]]

#replace every value with an invalid number (9999)
for(i in 1:nlyr(minTemp_2000_2020))
{
  minTemp_2000_2020[[i]][!is.na(minTemp_2000_2020[[i]][])] <- 9999
  print(i)
}

#calculate mean salinity for each layer across 20 years
for(i in 1:nlyr(minTemp_2000_2020))
{
  #save info we will need
  varname <- varnames(minTemp_2000_2020[[i]])
  name <- names(minTemp_2000_2020[[i]])
  unit <- units(minTemp_2000_2020[[i]])
  
  #select each depth layer from all years
  layer <- lapply(thetao_months_20y, function(x){x[[i]]})
  
  #stack all rasters of same layer
  layer_stack <- rast(layer)
  
  #extract years from the time dimension
  years <- format(time(layer_stack), "%Y")
  
  #split layer indices by year
  year_groups <- split(seq_along(years), years)
  
  #calculate annual minima
  annual_min_list <- lapply(year_groups, function(idxs) {
    yearly_stack <- layer_stack[[idxs]]
    app(yearly_stack, min, na.rm = TRUE)
  })
  
  #stack annual max rasters
  annual_min_stack <- rast(annual_min_list)
  
  # Calculate long-term average of annual minima
  minTemp_2000_2020[[i]]  <- app(annual_min_stack, mean, na.rm = TRUE)
  
  #put information lost back
  varnames(minTemp_2000_2020[[i]]) <- varname
  names(minTemp_2000_2020[[i]]) <- name
  units(minTemp_2000_2020[[i]]) <- unit
  
  print(i)
}

#save
setwd(wd_processed)
writeRaster(minTemp_2000_2020, 'MinTemperature_2000_2020.tif')
