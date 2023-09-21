########################################################
######      ETP COMPUTATION WITH Hargreaves-Samani    ##
########################################################

## NEOTYAGE
rm(list = ls())

## LIBRARIES
library(tidyverse)
library(Evapotranspiration)

## WORK SPACE 
setwd("G:/PROJET/Article/CC_Eau/BV_Nowkuy_CC/")
## DATA BASE
DataBase <- RSQLite::dbConnect(RSQLite::SQLite(),"data/DataBase.sqlite")

## DATA IMPORTATION
ClimStations <- dbReadTable(conn = DataBase,name = "id_stations")

nasa_corr_data <- dbReadTable(conn =DataBase ,
                              name = "nasa_corr_data")%>%
  select(-CORR)%>%
  spread(key =VARIABLES ,value = BRUTE)
  
ClimaticData <- nasa_corr_data%>%
  left_join(ClimStations[,c("ID","ALT")],by=c("STATION"="ID"))%>%
  dplyr::select(LONG,LAT,ALT,everything())%>%
  mutate(MM=month(YYYYMMDD),
         YYYY=year(YYYYMMDD))

ClimStationID <- ClimStations$ID
rad_fct <- (pi/4)/45
ETP.DATA <- rbind()
for (ID in ClimStationID) {
  extract_clima_data <- ClimaticData%>%
    filter(STATION==ID)
  n_month <- length(unique(paste0(extract_clima_data$MM," ",
                                  extract_clima_data$YYYY)))
  tmp_data <- list(Date.daily=as.Date(extract_clima_data$YYYYMMDD),
                   Date.month= as.yearmon(min(extract_clima_data$YYYY)+
                                            seq(0,n_month)/12),
                   J=as.zoo(yday(extract_clima_data$YYYYMMDD)),
                   i=month(extract_clima_data$YYYYMMDD),
                   ndays=as.zoo(day(extract_clima_data$YYYYMMDD)),
                   Tmax=as.zoo(extract_clima_data$TMAX),
                   Tmin=as.zoo(extract_clima_data$TMIN))
  constants2 <- list(Elev=unique(extract_clima_data$LAT),
                     lambda=2.45,
                     lat_rad=unique(extract_clima_data$LAT)*rad_fct,
                     Gsc=0.082
                     )
  etp_data <- Evapotranspiration::ET.HargreavesSamani(data =tmp_data ,
                         constants=constants2,AdditionalStats="no")
  etp_data2 <- data.frame(ID=ID,
                          DATE=extract_clima_data$YYYYMMDD,
                          LON=extract_clima_data$LON,
                          LAT=extract_clima_data$LAT,
                          ALT=extract_clima_data$ALT,
                          ETP=round(etp_data$ET.Daily,2))
  ETP.DATA <- rbind(ETP.DATA,etp_data2)
}

ETP_DATA2 <- ETP.DATA%>%
  mutate(DATE=as.character(DATE))

dbWriteTable(conn =DataBase,
             name ="ET_HargreavesSamani" ,
             value = ETP_DATA2,
             field.types=c(DATE="DATE"),
             overwrite=TRUE)
ET_HargreavesSamani <- dbReadTable(conn = DataBase,
                                   name = "ET_HargreavesSamani")
