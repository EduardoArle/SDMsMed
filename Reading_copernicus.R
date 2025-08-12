#load packages
library(terra)

#list WDs
wd_test <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Test_download'

#read in
setwd(wd_test)

so <- rast('cmems_mod_glo_phy_my_0.083deg_P1M-m_so_15.00W-37.00E_30.00N-46.00N_0.49-902.34m_2000-01-01-2020-01-01.nc')

thetao <- rast('cmems_mod_glo_phy_my_0.083deg_P1M-m_thetao_15.00W-37.00E_30.00N-46.00N_0.49-902.34m_2000-01-01-2020-01-01.nc')

#split the stacks per month

#get the names of the layers
so_names <- names(v1) 

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
  so[[layers]]                          # returns a SpatRaster of these layers
})

#name the list elements using time metadata 
tr <- time(so)
names(so_months) <- sapply(uniq_idx, function(i) {
    first_layer_idx <- which(time_idx == i)[1]
    format(tr[first_layer_idx], "%Y-%m")     # "YYYY-MM"
  })


#calculate meanT over 20 years (2000-01 to 2019-12)

meanT_2000_2020 <- 
