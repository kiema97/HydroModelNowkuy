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
setwd("D:/Recherche/Article_KIEMA")

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
missplot <- ggplot(debit_na, aes(x=YYYY,y=MM)) +
  geom_raster(aes(fill = pct_na))+
  geom_vline(xintercept = 1984, linetype = 4, linewidth = 1, colour = "blue") +
  geom_vline(xintercept = 2008, linetype = 4, linewidth = 1, colour = "blue") +
  scale_fill_continuous(low = "#FFFFFF",high = "#CB181D")+
  #scale_fill_gradient(low = "#999999",high = "#000000")+
  scale_x_continuous(breaks=seq(1983,2018,4),expand = c(0,0))+
  scale_y_continuous(breaks=1:12,labels=month.abb,expand = c(0,0))+
  theme_bw()+
  xlab("\nYears")+
  ylab("Months\n")+
  theme(axis.text.x =element_text(angle = 0,colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text(colour = "black",size = 12),
        axis.title.y = element_text(colour = "black",size = 12),
        legend.text.align =0,
        legend.key.height = unit(0.8,"cm"),
        legend.title = element_text(colour = "black",hjust = 0.5,vjust = 0.5),
        legend.text = element_text(colour = "black"))+
  guides(fill = guide_legend(title="Percent. (%)",reverse = TRUE,title.hjust = 0))

missplot

graph_path <- "graphs"
ggsave(filename =file.path(graph_path,"na_discharge.png") ,
       missplot ,width =6 ,height =3 ,dpi = 400)
