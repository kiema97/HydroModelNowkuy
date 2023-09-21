###############################################################################
##############                DATA CORRECTION               ###################
###############################################################################
## NETOYAGE
rm(list = ls())
## LIBRARIES
library(tidyverse)
library(lubridate)
library(CDFt)
library(DBI)
library(RSQLite)
library(plotrix)
## WORK SPACE
setwd("D:/Recherche/Article_KIEMA")

## DATA BASE
DataBase <- RSQLite::dbConnect(RSQLite::SQLite(),"data/DataBase.sqlite")

col_names <- c("YYYYMMDD","YYYY","MM","DD", "STATION","PCP","TMAX","TMIN")
observed_data <- dbReadTable(conn = DataBase,name = "synoptic_data")%>%
  mutate(DATE=as_date(DATE),
         YYYY= year(DATE),MM= month(DATE),DD=yday(DATE),
         STATION=ifelse(STATION=="Bobo_Dioulasso","Bobo",STATION))%>%
  select(DATE, YYYY, MM, DD, everything() )%>%
  rename_all(~col_names)%>%
  filter(YYYYMMDD>=as.Date("1983-01-01"),
         YYYYMMDD<=as.Date("2017-12-31"))%>%
  gather(key = "Variables",value = "Value",PCP,TMAX,TMIN)

nasa_data_synop_station <- dbReadTable(conn =DataBase ,name = "NASA_POWER_DATA_SYNOPTIC_STATION")%>%
  rename(PCP=PRECTOTCORR, STATION=NAMES,
         TMAX=T2M_MAX,TMIN=T2M_MIN)%>%
  mutate(PCP=ifelse(PCP<=0.1,0,PCP))%>%
  filter(YYYYMMDD>=as.Date("1983-01-01"),
         YYYYMMDD<=as.Date("2017-12-31"))%>%
  select(-WS2M,-RH2M,-LON,-LAT)%>%
  gather(key = "Variables",value = "Value",PCP,TMAX,TMIN)

stations <- c("Bobo","Dedougou")
variables <- c("PCP","TMAX","TMIN")
MM <- unique(observed_data$MM)
CORRECTED_DATAS <- rbind()
for (station in stations) {
  for(variable in variables){
    for(i in MM){
      # DONNEES OBSERVEES A LA STATION DE BOBO-DIOULASSO
      monthly_observed_data <- observed_data%>%
        filter(MM==i, STATION==station,Variables==variable)
      # DONNEES DE LA NASA A LA STATION DE BOBO-DIOULASSO
      monthly_nasa_data_synop_station <- nasa_data_synop_station%>%
        filter(MM==i, STATION==station,Variables==variable)
      
      corrected_data <- CDFt(ObsRp =monthly_observed_data$Value,
                             DataGp =monthly_nasa_data_synop_station$Value,
                             DataGf = monthly_nasa_data_synop_station$Value)
      corrected_data2 <- data.frame(STATION=station,
                                    VARIABLES=variable,
                                    YYYYMMDD=monthly_observed_data$YYYYMMDD,
                                    OBS=monthly_observed_data$Value,
                                    BRUTE=monthly_nasa_data_synop_station$Value,
                                    CORR=round(corrected_data$DS))
      CORRECTED_DATAS <- rbind(CORRECTED_DATAS,corrected_data2)
    }
  }
}

STATIONS <- unique(CORRECTED_DATAS$STATION)
VARIABLES <- unique(CORRECTED_DATAS$VARIABLES)
ECDF_DATAS <- rbind()
for (station in STATIONS) {
  for(variable in VARIABLES){
    extract_data <- CORRECTED_DATAS%>%
      filter(STATION==station,VARIABLES==variable)
    
    ecdf_observed <- ecdf(extract_data$OBS)
    ecdf_brute <- ecdf(extract_data$BRUTE)
    ecdf_corrected <- ecdf(extract_data$CORR)
    
    # EXTRACTION DES VALEURS
    extract_data$ecdf_obs <- round(ecdf_observed(extract_data$OBS),2)
    extract_data$ecdf_brute <- round(ecdf_brute(extract_data$BRUTE),2)
    extract_data$ecdf_corr <-round( ecdf_corrected(extract_data$CORR),2)
   ECDF_DATAS <-  rbind(ECDF_DATAS,extract_data)
  }
}

ECDF_DATAS1 <- ECDF_DATAS%>%
  select(-ecdf_obs,-ecdf_brute,-ecdf_corr)%>%
  gather(key = "var",value = "value_x",OBS,BRUTE,CORR)


ECDF_DATAS2 <- ECDF_DATAS%>%
  select(-OBS,-BRUTE,-CORR)%>%
  gather(key = "var",value = "value_Y",ecdf_obs,ecdf_brute,ecdf_corr)%>%
  mutate(var=recode(var,"ecdf_obs"="OBS","ecdf_brute"="BRUTE","ecdf_corr"="CORR"))

ECDF_DATAS3 <- ECDF_DATAS1%>%
  left_join(ECDF_DATAS2,by=c("STATION","VARIABLES","YYYYMMDD","var"))%>%
  mutate(YYYYMMDD=as.character(YYYYMMDD))

ECDF_DATAS3[ECDF_DATAS3$STATION=="Bobo","STATION"] <- "Bobo-Dioulasso"
ECDF_DATAS3[ECDF_DATAS3$VARIABLES=="PCP","VARIABLES"] <- "Rainfall (mm)"
ECDF_DATAS3[ECDF_DATAS3$VARIABLES=="TMAX","VARIABLES"] <- "Tmax (°C)"
ECDF_DATAS3[ECDF_DATAS3$VARIABLES=="TMIN","VARIABLES"] <- "Tmin (°C)"

#dbWriteTable(conn =DataBase ,name ="ECDF_DATA" ,value = ECDF_DATAS3,field.types=c(YYYYMMDD="DATE"))
bcplot <- ggplot(data = ECDF_DATAS3 ,aes(x =value_x ,y = value_Y,color=var))+
  geom_line(size = 0.2)+
  #geom_point() + geom_smooth(method = "loess") +
  facet_grid(facets = STATION~VARIABLES,scales = "free")+
  xlab("\nValues")+
  ylab("ecdf")+
  scale_color_manual(name="",values = c("red","green","black"),
                     labels=c("MERRA-2 (raw)","MERRA-2 (bias corrected)","Observations")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(colour="black"))

ggsave("graphs/bcplot.png", bcplot, width = 8, height = 5, units = "in", dpi = 400,
       scale=0.8)
