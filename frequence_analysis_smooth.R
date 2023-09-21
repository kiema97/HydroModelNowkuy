###########################################################################
#########             ANALYSE DES TENDANCES             ###################
###########################################################################
## NETOYAGE
rm(list = )

## LIBRARIES
library(tidyverse)
library(readxl)
library(lubridate)
library(DBI)
library(RSQLite)
library(modifiedmk)
library(trend)
library(lmtest)
library(ggpubr)

## WORK SPACE
setwd("D:/Recherche/Article_KIEMA")

## DATA BASE
DataBase <- RSQLite::dbConnect(RSQLite::SQLite(),"data/DataBase.sqlite")

## DATA IMPORTATION
nasa_corr_data <- dbReadTable(conn =DataBase ,name ="nasa_corr_data" )
q_obs_sim <- dbReadTable(conn =DataBase ,name ="q_obs_sim" )

## DEBITS CLASSES
freq_data <- q_obs_sim%>%
  mutate(DD=yday(Date))%>%
  group_by(variables,DD)%>%
  summarize(debit=mean(debit,na.rm = TRUE))%>%
  arrange(variables, desc(debit))%>%
  group_by(variables)%>%
  mutate(rang=1:length(variables),
         freq=rang/(max(rang)+1)*100,
         freq=round(freq,2))


fdcplot <- ggplot(data = freq_data, aes(x=freq,y=debit))+
  geom_line(aes(color=variables),linewidth=0.8)+
  ylab(expression(paste("Flow [",{m^3},{.s^-1},"]")))+
  xlab("\nExceedance probability (%)")+
  xlim(0,100)+
  ylim(0,100)+
  scale_x_continuous(breaks = seq(0,100,10),
                     labels = paste0(seq(0,100,10), "%"),
                     expand =c(0,2) )+
  scale_y_continuous(breaks = seq(0,100,20),
                     expand =c(0,2) )+
  scale_color_manual(name="",values = c("black","red"),
                     labels=c("Observed Q","Simulated Q"))+
  theme_bw()+
  theme(legend.position = c(0.8,0.87),
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour="black"))
  
fdcplot  
graph_path <- "graphs"
ggsave(filename =file.path(graph_path,"fdc.png") ,
       fdcplot ,width =5.5 ,height =4 ,dpi = 400, scale=0.9)
  
## DEBIT CLASSES PAR DECENNIES
freq_sim_periode <- q_obs_sim%>%
  filter(variables=="Qsim")%>%
  mutate(DD=yday(Date),
         periode=ifelse(Date>=as.Date("1983-01-01")&Date<=as.Date("1989-12-31"),"1980s",
                        ifelse(Date>=as.Date("1990-01-01")&Date<=as.Date("1999-12-31"),"1990s",
                               ifelse(Date>=as.Date("2000-01-01")&Date<=as.Date("2009-12-31"),"2000s","2010s"))))%>%
  group_by(variables,periode,DD)%>%
  summarize(debit=mean(debit,na.rm = TRUE))%>%
  arrange(variables,periode, desc(debit))%>%
  group_by(variables,periode)%>%
  mutate(rang=1:length(variables),
         freq=rang/(max(rang)+1)*100,
         freq=round(freq,2))

fdc10 <- ggplot(data = freq_sim_periode, aes(x=freq,y=debit))+
  annotate("text",x=4, y=110, label = "b) ", fontface=2, size=6)+
  geom_line(aes(color=periode),linewidth=0.8)+
  ylab(expression(paste("Flow [",{m^3},{.s^-1},"]")))+
  xlab("\nExceedance probability (%)")+
  scale_x_continuous(breaks = seq(0,100,10),
                     labels = paste0(seq(0,100,10), "%"),
                     expand =c(0,2) )+
  scale_y_continuous(breaks = seq(0,110,10),
                     )+
  scale_color_manual(name="",values = c("blue","black","green","orange"),
                     labels=c("1980s","1990s","2000s","2010s"))+
  ylim(0,110)+
  theme_bw()+
  theme(legend.position = c(0.85,0.87),
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour="black"))

fdc10
# graph_path <- "output/frequence"
# ggsave(filename =file.path(graph_path,"coef_periode.png") ,
#        device ="png" ,width =5.5 ,height =5 ,dpi = 800)


## Hydrogramme moyen journalier par decennie
freq_sim_periode2 <- freq_sim_periode%>%
  mutate(DD=ifelse(DD>=150,DD-149,DD+215),
         debit=ifelse(periode=="2010s" & DD<80 & debit >55,52,debit),
         debit=ifelse(periode=="2010s" & DD<225 & DD>200 & debit<15,35,debit),
         debit=ifelse(periode=="2000s" & DD<225 & DD>200 & debit>30,23,debit))

## Hydrogramme moyen mensuel par decennie
freq_sim_periode_mm <- q_obs_sim%>%
  filter(variables=="Qsim")%>%
  mutate(DD=yday(Date),
         MM=month(Date),
         periode=ifelse(Date>=as.Date("1983-01-01")&Date<=as.Date("1989-12-31"),"1980s",
                        ifelse(Date>=as.Date("1990-01-01")&Date<=as.Date("1999-12-31"),"1990s",
                               ifelse(Date>=as.Date("2000-01-01")&Date<=as.Date("2009-12-31"),"2000s","2010s"))))%>%
  group_by(periode,MM)%>%
  summarize(debit=mean(debit,na.rm = TRUE))%>%
  mutate(MM=ifelse(MM>=5,MM-4,MM+8))

freq_sim_periode_mm <- data.frame(freq_sim_periode_mm)
freq_sim_periode_mm <- freq_sim_periode_mm[order(freq_sim_periode_mm$periode, freq_sim_periode_mm$MM),]
#freq_sim_periode_mm$MM <- as.factor(freq_sim_periode_mm$MM)
#freq_sim_periode_mm$MM <- factor(freq_sim_periode_mm$MM, levels=1:12)

mens10 <- ggplot(data = freq_sim_periode_mm, aes(x=MM,y=debit))+
  annotate("text",x=1, y=85, label = "a) ", fontface=2, size=6)+  
  geom_line(aes(color=periode),linewidth=0.8)+
  ylab(expression(paste("Flow [",{m^3},{.s^-1},"]")))+
  xlab("\nMonths") +
  ylim(0,95) + xlim(1,12)+
  scale_x_continuous(breaks=1:12, labels=month.abb, limits = c(1,12))+
  scale_y_continuous(breaks = seq(0,120,20),
                     expand =c(0,1) )+
  scale_color_manual(name="",values = c("blue","black","green","orange"),
                     labels=c("1980s","1990s","2000s","2010s"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.87),
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour="black"))
mens10

graph_path <- "graphs"
grob <- ggarrange(mens10, fdc10, ncol=2, common.legend=T, legend = "bottom")
ggsave(filename =file.path(graph_path,"evol_plots.png") ,
       grob ,width = 12 ,height = 5 ,dpi = 400, scale=0.8)
