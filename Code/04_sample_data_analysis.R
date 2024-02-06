## Code to determine how well sample data sets conformed to task-independent
## and social-ecological specific standards

library(rgdal)
library(rgeos)
library(sf)
library(tidyverse)

## Read in the roads and dams shapefiles
roads <- st_read("./Data/SNV_201903A.shp")
dams <- st_read("./Data/Usinas_Hidrelétricas_UHE.shp")

## Change the shapefiles into data frames
rds.df <- as.data.frame(roads,xy=TRUE)
dams.df <- as.data.frame(dams,xy=TRUE)

## Count the number of empty cells in name/owner columns (dams) and name/status
## columns (roads)

(length(which(is.na(dams.df$NOME))) + length(which(is.na(dams.df$PROPRIETAR))))/(2*nrow(dams.df))


(length(which(is.na(rds.df$ds_legenda))) + length(which(is.na(rds.df$vl_br))))/(2*nrow(rds.df))

## Get the overall percent of empty cells within each data set
length(which(!is.na(dams.df)))/(nrow(dams.df)*ncol(dams.df))

length(which(!is.na(rds.df)))/(nrow(rds.df)*ncol(rds.df))
