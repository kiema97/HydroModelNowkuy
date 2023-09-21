###############################################################################
##############      CLIMATE DATA ANALYSIS               ###################
###############################################################################
## NETOYAGE
rm(list = ls())
## LIBRARIES
library(tidyverse)
library(DBI)
library(RSQLite)
## WORK SPACE
setwd("G:/PROJET/Article/CC_Eau/BV_Nowkuy_CC/")

## DATA BASE
DataBase <- RSQLite::dbConnect(RSQLite::SQLite(),"data/DataBase.sqlite")

## DATA IMPORTATION 
nasa_synop_corr <- dbReadTable(conn = DataBase,
                                name = "NASA_SYNOP_DATA_CORRECTED" )

col_names <- names(nasa_synop_corr)
nasa_fict_station_data_corr <- dbReadTable(conn = DataBase,
                                          name = "NASA_FICTIVES_STATION_DATA_CORRECTED")%>%
  select(ID, Variables,YYYYMMDD,LONG, LAT,BRUTE,CORR)%>%
  rename_all(~col_names)%>%
  mutate(STATION=paste0("station_",STATION))


nasa_corr_data <- rbind(nasa_synop_corr,nasa_fict_station_data_corr)%>%
  mutate(CORR=ifelse(VARIABLES=="PCP" & CORR<0, 0,CORR))

dbWriteTable(conn =DataBase ,
             name ="nasa_corr_data" ,
             value = nasa_corr_data,
             field.types=c(YYYYMMDD="DATE"))



