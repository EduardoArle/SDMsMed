################################################################################
#                                Run models                                    #
################################################################################

#load packages
library(sf); library(terra); library(data.table)

#list WDs
wd_pr_pa <- ''
wd_vars_pr <- '/Users/carloseduardoaribeiro/Documents/Collaborations/Uri Roll/Data/BioOracle/Present'
wd_vars_2090 <- ''


#load present variable layers
setwd(wd_vars_pr)
vars <- lapply(list.files(pattern = '.tif$'), rast)

#load 2090 variable layers
setwd(wd_vars_2090)
vars_2090 <- lapply(list.files(pattern = '.tif$'), rast)






