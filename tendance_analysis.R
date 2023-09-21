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
ET_HargreavesSamani <- dbReadTable(conn =DataBase ,name ="ET_HargreavesSamani" )


## DATA TREAMENT
tmp <- nasa_corr_data%>%
  filter(!VARIABLES=="PCP")%>%
  select(-CORR)%>%
  spread(key = VARIABLES,value =BRUTE )%>%
  mutate(TMEAN=(TMAX+TMIN)/2,
         DD=day(YYYYMMDD),
         MM=month(YYYYMMDD))%>%
  group_by(MM,DD)%>%
  summarize(TMAX=mean(TMAX),
            TMIN=mean(TMIN))
  
pcp_data <- nasa_corr_data%>%
  filter(VARIABLES=="PCP")%>%
  select(STATION,YYYYMMDD,BRUTE)%>%
  mutate(YYYY=year(YYYYMMDD))%>%
  group_by(STATION,YYYY)%>%
  summarise(BRUTE=sum(BRUTE))%>%
  rename(Value=BRUTE)%>%
  group_by(YYYY)%>%
  summarise(Value=round(mean(Value)))

etp_data <- ET_HargreavesSamani%>%
  select(ID,DATE,ETP)%>%
  mutate(YYYY=year(DATE))%>%
  group_by(ID,YYYY)%>%
  summarise(ETP=sum(ETP))%>%
  group_by(YYYY)%>%
  summarise(ETP=round(mean(ETP),2))

q_data <- q_obs_sim%>%
  filter(variables=="Qsim")%>%
  mutate(YYYY=year(Date))%>%
  group_by(YYYY)%>%
  summarise(debit=round(mean(debit),2))


## EVOLUTION TEMPORELLE
# DISCHARGE
ggplot(data =q_data ,aes(x=YYYY,y=debit))+
  geom_line(color="steelblue")+
  geom_point(color="black")+
  geom_smooth(method = "lm",color="black", linetype=2)+
  scale_x_continuous(breaks = seq(1983,2018,4))+
  ylab(expression(paste("Discharge [",{m^3},{.s^-1},"]")))+
  xlab("Years")+
  theme_bw()
graph_path <- "G:/PROJET/Article/CC_Eau/BV_Nowkuy_CC/output/tendance"
ggsave(filename =file.path(graph_path,"q_evolution.png") ,
       device ="png" ,width =6 ,height =4 ,dpi = 800)


## PCP
ggplot(data =pcp_data ,aes(x=YYYY,y=Value))+
  geom_line(color="steelblue")+
  geom_point(color="black")+
  geom_smooth(method = "lm",color="black", linetype=2)+
  scale_x_continuous(breaks = seq(1983,2018,4))+
  ylab("Rainfall [MM]")+
  xlab("Years")+
  theme_bw()
ggsave(filename =file.path(graph_path,"pcp_evolution.png") ,
       device ="png" ,width =6 ,height =4 ,dpi = 800)

## ETP
ggplot(data =etp_data ,aes(x=YYYY,y=ETP))+
  geom_line(color="steelblue")+
  geom_point(color="black")+
  geom_smooth(method = "lm",color="black", linetype=2)+
  scale_x_continuous(breaks = seq(1983,2018,4))+
  ylab("Evapotranspiration [MM]")+
  xlab("Years")+
  theme_bw()
ggsave(filename =file.path(graph_path,"etp_evolution.png") ,
       device ="png" ,width =6 ,height =4 ,dpi = 800)

## Test d'Hypothèse

## Test de Mann-Kendall Modifié

mk_etp <- modifiedmk::mmky(etp_data$ETP)
mk_pcp <-modifiedmk::mmky(pcp_data$Value)
mk_debit <- modifiedmk::mmky(q_data$debit)


## Test de Pettitt
pt_etp <- trend::pettitt.test(etp_data$ETP) 
pt_pcp <- trend::pettitt.test(pcp_data$Value)
pt_debit <- trend::pettitt.test(q_data$debit)

pt_etp$estimate #13
pt_pcp$estimate#25
pt_debit$estimate#24
  
## Granger-causality Test
nowkuy_data <- pcp_data%>%
  rename(PCP=Value)%>%
  left_join(etp_data,by="YYYY")%>%
  left_join(q_data,by="YYYY")

granger_test_Q_ETP <- rbind()
for(i in 0:5){
test_Q_PCP <- grangertest(PCP~debit,data = nowkuy_data,order=0)
test_Q_PCP2 <- data.frame(order=i,
                          F=round(test_Q_PCP$F[2],4),
                          P_value=round(test_Q_PCP$`Pr(>F)`[2],3))
granger_test_Q_ETP <- rbind(granger_test_Q_ETP,test_Q_PCP2)
}

granger_test_Q_ETP2 <- granger_test_Q_ETP%>%
  mutate(label="Granger-causality Test (PCP~Q)")
ggplot(data = granger_test_Q_ETP2,aes(x=order,y=P_value))+
  geom_bar(stat = "identity",fill="steelblue",color="black")+
  xlab("order")+
  ylab("p-value")+
  facet_grid(facets = .~label)+
  theme_bw()
ggsave(filename =file.path(graph_path,"granger_test_PCP_Q.png") ,
       device ="png" ,width =5.5 ,height =4 ,dpi = 800)


write.table(x =granger_test_Q_ETP ,
            file = "output/tendance/granger_test_Q_ETP.txt",
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)


pcp_q_cor <- cor.test(x=nowkuy_data$PCP,
                      y=nowkuy_data$debit,
                      method="spearman")

etp_q_cor <- cor.test(x=nowkuy_data$ETP,
         y=nowkuy_data$debit,
         method="spearman")








