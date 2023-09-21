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
## WORK SPACE
setwd("G:/PROJET/Article/CC_Eau/BV_Nowkuy_CC")

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


  ggplot(data = freq_data, aes(x=freq,y=debit))+
    geom_line(aes(color=variables),linewidth=0.8)+
    ylab(expression(paste("Flow [",{m^3},{.s^-1},"]")))+
    xlab("Exceedance probability (%)")+
    xlim(0,100)+
    ylim(0,100)+
    scale_x_continuous(breaks = seq(0,100,10),
                       labels = paste0(seq(0,100,10), "%"),
                       expand =c(0,2) )+
    scale_y_continuous(breaks = seq(0,100,20),
                       expand =c(0,2) )+
    scale_color_manual(name="",values = c("blue","red"),
                       labels=c("Q Observed","Q simulated"))+
    theme_bw()+
    theme(legend.position = c(0.8,0.87),
          legend.background = element_rect(fill = NA))
  
  
graph_path <- "E:/PROJETS/BV_Nowkuy_CC/output/frequence"
ggsave(filename =file.path(graph_path,"coef_periode.png") ,
         device ="png" ,width =5.5 ,height =5 ,dpi = 800)
  
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


ggplot(data = freq_sim_periode, aes(x=freq,y=debit))+
  geom_line(aes(color=periode),linewidth=0.8)+
  ylab(expression(paste("Flow [",{m^3},{.s^-1},"]")))+
  xlab("Exceedance probability (%)")+
  scale_x_continuous(breaks = seq(0,100,10),
                     labels = paste0(seq(0,100,10), "%"),
                     expand =c(0,2) )+
  scale_y_continuous(breaks = seq(0,100,10),
                     expand =c(0,2) )+
  scale_color_manual(name="",values = c("blue","black","green","orange"),
                     labels=c("1980s","1990s","2000s","2010s"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.87),
        legend.background = element_rect(fill = NA))


graph_path <- "E:/PROJETS/BV_Nowkuy_CC/output/frequence"
ggsave(filename =file.path(graph_path,"coef_periode.png") ,
       device ="png" ,width =5.5 ,height =5 ,dpi = 800)


##
freq_sim_periode2 <- freq_sim_periode%>%
  mutate(DD=ifelse(DD>=150,DD-149,DD+215))
ggplot(data = freq_sim_periode2, aes(x=DD,y=debit))+
  geom_line(aes(color=periode),linewidth=0.8)+
  ylab(expression(paste("Flow [",{m^3},{.s^-1},"]")))+
  xlab("Julian day")+
  scale_x_continuous(breaks = seq(0,400,50),
                     expand =c(0,15) )+
  scale_y_continuous(breaks = seq(0,100,20),
                     expand =c(0,0) )+
  scale_color_manual(name="",values = c("blue","black","green","orange"),
                     labels=c("1980s","1990s","2000s","2010s"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.87),
        legend.background = element_rect(fill = NA))


## Coefficient d'écoulement
q_obs_sim2<-q_obs_sim%>%
  filter(variables=="Qsim")%>%
  select(-variables)

pcp_data <- nasa_corr_data%>%
  filter(VARIABLES=="PCP")%>%
  select(STATION,YYYYMMDD,BRUTE)%>%
  group_by(YYYYMMDD)%>%
  summarize(pcp=sum(BRUTE))



q_pcp_data<-q_obs_sim2%>%
  left_join(pcp_data,by=c("Date"="YYYYMMDD"))%>%
  mutate(periode=ifelse(Date>=as.Date("1983-01-01")&Date<=as.Date("1989-12-31"),"1980s",
                        ifelse(Date>=as.Date("1990-01-01")&Date<=as.Date("1999-12-31"),"1990s",
                               ifelse(Date>=as.Date("2000-01-01")&Date<=as.Date("2009-12-31"),"2000s","2010s"))))%>%
  mutate(flow_coef=ifelse(pcp>0,debit/pcp,NA),
         MM=month(Date),
         DD=yday(Date),
         YYYY=year(Date))%>%
  group_by(DD)%>%
  summarize(flow_coef=mean(flow_coef,na.rm=TRUE))


coef_periode <- q_obs_sim2%>%
  left_join(pcp_data,by=c("Date"="YYYYMMDD"))%>%
  mutate(DD=yday(Date),
         periode=ifelse(Date>=as.Date("1983-01-01")&Date<=as.Date("1989-12-31"),"1980s",
                        ifelse(Date>=as.Date("1990-01-01")&Date<=as.Date("1999-12-31"),"1990s",
                               ifelse(Date>=as.Date("2000-01-01")&Date<=as.Date("2009-12-31"),"2000s","2010s"))))%>%
  group_by(periode,DD)%>%
  summarize(debit=mean(debit,na.rm=TRUE),
            pcp=sum(pcp,na.rm = TRUE),
            coef=ifelse(pcp>0, debit/pcp,NA))

ggplot(data = coef_periode, aes(x=DD,y=coef))+
  geom_line(aes(color=periode),linewidth=0.8)+
  ylab("Coefficient")+
  #ylab(expression(paste("Flow [",{m^3},{.s^-1},"]")))+
  xlab("Julian day")+
  scale_x_continuous(breaks = seq(0,400,50),
                     expand =c(0,15) )+
  scale_color_manual(name="",values = c("blue","black","green","orange"),
                     labels=c("1980s","1990s","2000s","2010s"))+
  theme_bw()+
  theme(legend.position = c(0.1,0.87),
        legend.background = element_rect(fill = NA))

write.table(x =freq_sim_periode ,file ="data/freq_sim_periode.txt" ,append =FALSE ,quote =FALSE ,sep ="\t",row.names = FALSE)
