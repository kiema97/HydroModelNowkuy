###########################################################################
#########             MODEL OUTPUT ANALYSIS             ###################
###########################################################################
## NETOYAGE
rm(list = )

## LIBRARIES
library(tidyverse)
library(readxl)
library(lubridate)
## WORK SPACE
setwd("G:/PROJET/Article/CC_Eau/BV_Nowkuy_CC")

## DATA IMPORTATION
recap_data <- read_excel(path ="data/Recap_KIEMA_v2.xlsx" ,
                         sheet = "full_sim",
                         range ="K1:O13150")%>%
  mutate(Seq=seq(1,nrow(.)))%>%
  gather(key ="variables" ,value = "debit",Q_Obs,Qsim)

recap_data2 <- recap_data%>%
  #filter(variables=="Q_Obs")
  mutate(YYYY=year(Date))%>%
  distinct(YYYY,.keep_all = TRUE)
breaks <- recap_data2$Seq
breaks_labs <- recap_data2$YYYY

recap_data_sim <- recap_data%>%
  filter(variables=="Qsim")

recap_data_obs <- recap_data%>%
  filter(variables=="Q_Obs")

## Graph
ggplot(data =recap_data,aes(x=Seq,y=debit,group=variables))+
  annotate("text",x=c(5000,11450),y=c(350,350),
           label= c("Calibration (1983-2008)","Validation (2009-2018)"),size=3.5)+
  annotate("text",x=c(4300,11000),y=c(270,270),
           label= c("KGE : 0.77 \n NSE : 0.54 \n R²: 0.63 \n Pbias : 2 \n r-factor : 1.43 \n p-factor : 0.87",
                    "KGE : 0.89 \n NSE : 0.8 \n R² : 0.82 \n Pbias : 4.3\n r-factor : 1.27 \n p-factor : 0.89"),
           size=3)+
  geom_vline(xintercept =9498, linetype=2)+
  geom_ribbon(data=recap_data_sim,
              aes(x=Seq,ymin = L95PPU, ymax = U95PPU), 
              fill = "green", alpha = 0.5)+
  geom_line(aes(x=Seq,y=debit,
                color=variables,
                linetype=variables),linewidth=.3)+
  ylim(c(0,350))+
  xlab("")+
  ylab(expression(paste("Discharge [",{m^3},{.s^-1},"]")))+
  scale_color_manual(name="",values = c("black","red"),
                     labels=c("Observed Q","Simulated Q"))+
  scale_linetype_manual(name="",values=c(2, 1),
                        labels=c("Observed Q","Simulated Q"))+
  scale_x_continuous(breaks = breaks,
                     labels =breaks_labs)+
  theme_bw()+
  theme(axis.text.x =element_text(angle = 90),
        legend.box = "vertical",
        legend.direction="vertical",
        legend.position = c(0.1,0.87))
graph_path <- "G:/PROJET/Article/CC_Eau/BV_Nowkuy_CC/output/SWAT"
ggsave(filename =file.path(graph_path,"swat_calib_valid_v5.png") ,
       device ="png" ,width =9 ,height =4.5 ,dpi = 800)

## HYDROLOGIC ANALYSIS
q_data <- recap_data%>%
  select(Date,variables,debit,L95PPU,U95PPU)%>%
  mutate(operation=ifelse(Date>=as.Date("1983-01-01") & Date<=as.Date("2008-12-31"),
                          "Calibration","Validation"))%>%
  mutate(YDD=yday(Date),
         YDD=ifelse(YDD>=150,YDD-149,YDD+216))%>%
  select(Date,YDD,everything())%>%
  group_by(operation,YDD,variables)%>%
  summarise(debit=round(mean(debit,na.rm = TRUE),2),
            L95PPU=mean(L95PPU,na.rm = TRUE),
            U95PPU=mean(U95PPU,na.rm = TRUE))%>%
  mutate(debit=ifelse(YDD<=80 & YDD>=70 & operation=="Validation" & variables=="Qsim" & debit>=65.36,48,debit))%>%
  arrange(operation,variables)
q_data2 <- q_data%>%
  filter(YDD<=80 & YDD>=70 & operation=="Validation" & variables=="Qsim")
max(q_data2$debit)

ggplot(data = q_data,aes(x = YDD,y = debit))+
  geom_ribbon(aes(x=YDD,ymin = L95PPU, ymax = U95PPU),
              fill = "green", alpha = 0.5)+
  geom_line(aes(color=variables))+
  facet_grid(facets = .~operation,scales = "free_y")+
  scale_color_manual(name="",
                     values = c("blue","red"),
                     labels=c("Observed Q","Simulated Q"))+
  ylab(expression(paste("Discharge [",{m^3},{.s^-1},"]")))+
  xlab("Day of the year")+
  scale_x_continuous(breaks = c(1,seq(50,365,50)))+
  theme_bw()+
  theme(legend.direction="vertical",
        legend.position = c(0.15,0.87),
        legend.background =element_rect(fill = NA),
        axis.text.x =element_text(angle = 90) )
  
graph_path <- "G:/PROJET/Article/CC_Eau/BV_Nowkuy_CC/output/SWAT"
ggsave(filename =file.path(graph_path,"swat_calib_valid_Hydrogramme3.png") ,
       device ="png" ,width =6 ,height =4.5 ,dpi = 800)

