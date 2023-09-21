###############################################################
#########         EXPLORATORY DATA ANALASIS (EDA)         #####
################################################################
## NETOYAGE
rm(list = ls())
## LIBRARIES
library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(RColorBrewer)

## WORK SPACE
setwd("E:/PROJETS/BV_Nowkuy_CC")

## DATA BASE
DataBase <- RSQLite::dbConnect(RSQLite::SQLite(),"data/DataBase.sqlite")

debit_data <- dbReadTable(conn =DataBase,name = "Discharge")%>%
  mutate(YYYYMMDD=as_date(Date),
         YYYY=year(YYYYMMDD),
         MM=month(YYYYMMDD),
         DD=day(YYYYMMDD))%>%
  filter(YYYYMMDD%within%interval("1983-01-01","2018-12-31"))%>%
  dplyr::select(YYYYMMDD,YYYY,MM,DD, everything(),-Date)


debit_na <- debit_data%>%
  group_by(YYYY,MM)%>%
  summarise(pct_na=round(mean(is.na(Debit))*100,2))

## MISSING DATA GRAPH
ggplot(debit_na, aes(x=YYYY,y=MM)) +
  geom_raster(aes(fill = pct_na))+
  scale_fill_continuous(low = "#FFFFFF",high = "#CB181D")+
  #scale_fill_gradient(low = "#999999",high = "#000000")+
  scale_x_continuous(breaks=seq(1983,2018,4),expand = c(0,0))+
  scale_y_continuous(breaks=1:12,labels=month.abb,expand = c(0,0))+
  theme_bw()+
  xlab("Years")+
  ylab("Months")+
  theme(axis.text.x =element_text(angle = 90,face = "bold",colour = "black"),
        axis.text.y = element_text(face = "bold",colour = "black"),
        axis.title.x = element_text(face = "bold",colour = "black",size = 12),
        axis.title.y = element_text(face = "bold",colour = "black",size = 12),
        legend.text.align =0,
        legend.key.height = unit(0.8,"cm"),
        legend.title = element_text(face = "bold",colour = "black",hjust = 0.5,vjust = 0.5),
        legend.text = element_text(face = "bold",colour = "black"))+
  guides(fill = guide_legend(title="(%)",reverse = TRUE,title.hjust = 0))

graph_path <- "output/tendance"
ggsave(filename =file.path(graph_path,"na_discharge.png") ,
       device ="png" ,width =6 ,height =4 ,dpi = 800)
