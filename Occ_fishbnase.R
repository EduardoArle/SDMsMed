################################################################################
#                  Obtain body size data from FishBase                         #
################################################################################

#load packages
library(rfishbase)


#get all data on species available in FishBase
sps_table <- species_names()

#select the target species (clade Selachimorpha)

#manually input the 8 orders in the clade
orders_selach <- c('Heterodontiformes', 'Orectolobiformes', 'Carcharhiniformes',
                   'Lamniformes', 'Hexanchiformes', 'Squatiniformes',
                   'Pristiophoriformes', 'Squaliformes')

#select only species in the aforementioned orders
selach_table <- sps_table[sps_table$Order %in% orders_selach,]

#make a species list of Selachimorpha
sps_list <- selach_table$Species

#get info length species
table_length <- popchar(species_list = sps_list,
                        fields = NULL,
                        server = c("fishbase", "sealifebase"),
                        version = "latest",
                        db = NULL)
