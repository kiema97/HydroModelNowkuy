###############################################################################
#############        EMPIRICAL CUMULATIVE DISTRIBUTION               ###################
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
setwd("G:/PROJET/Article/CC_Eau/BV_Nowkuy_CC/")

## DATA BASE
DataBase <- RSQLite::dbConnect(RSQLite::SQLite(),"data/DataBase.sqlite")

ECDF_DATA <- dbReadTable(conn =DataBase ,name = "ECDF_DATA")
ggplot(data =ECDF_DATA ,aes(x =value_x ,y = value_Y,color=var))+
  geom_line()+
  facet_grid(facets = STATION~VARIABLES,scales = "free_x")+
  xlab("Value")+
  ylab("Empirical Cumulative Distribution")+
  scale_color_manual(name="",values = c("red","green","black"),
                     labels=c("raw","corrected","observed"))+
  theme_bw()+
  theme(legend.position = "top",
        strip.background = element_rect(fill = NA,colour = "black",linewidth =1 ),
        strip.text = element_text(face = "bold")
  )
graph_path <- "G:/PROJET/Article/CC_Eau/BV_Nowkuy_CC/output/BiasCorrection"
ggsave(filename ="ecdf_graph.png",path = graph_path ,device ="png" ,width =5 ,height =5 ,dpi = 800)
