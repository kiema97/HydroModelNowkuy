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
          legend.background = element_rect(fill = NA))
  
graph_path <- "graphs"
ggsave(filename =file.path(graph_path,"coef_periode.png") ,
       fdcplot ,width =5.5 ,height =4 ,dpi = 400)
  
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


fdc_period_plot <- ggplot(data = freq_sim_periode, aes(x=freq,y=debit))+
  geom_line(aes(color=periode),linewidth=0.8)+
  ylab(expression(paste("Flow [",{m^3},{.s^-1},"]")))+
  xlab("\nExceedance probability (%)")+
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


ggsave(filename =file.path(graph_path,"coef_by_periode.png") ,
       fdc_period_plot,width =5.5 ,height =5 ,dpi = 400)


##
freq_sim_periode2 <- freq_sim_periode%>%
  mutate(DD=ifelse(DD>=150,DD-149,DD+215))
freq_sim_periode2 <- data.frame(freq_sim_periode2)

for (p in freq_sim_periode2$periode) {
  #p <- "2010s"
  xx <-freq_sim_periode2[freq_sim_periode2$periode==p,"debit"]
  max((xx-diff(xx))/xx
  #xx2 <- na.spline(xx)
  #xx <- ifelse(is.na(xx),xx2,xx)
  freq_sim_periode2[freq_sim_periode2$periode==p,"debit"] <- xx
}
ggplot(data = freq_sim_periode2, aes(x=DD,y=debit))+
  #geom_line(aes(color=periode),linewidth=0.8)+
  geom_line(aes(color=periode), linewidth=0.7) + #geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) + 
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
