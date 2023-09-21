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

## Test de Mann-Kendall Modifié

mk_etp <- modifiedmk::tfpwmk(etp_data$ETP)
mk_pcp <-modifiedmk::tfpwmk(pcp_data$Value)
mk_debit <- modifiedmk::tfpwmk(q_data$debit)

pcp.pval <- round(as.numeric(mk_pcp["P-value"]), 3)
pet.pval <- round(as.numeric(mk_etp["P-value"]),3)
sq.pval <- round(as.numeric(mk_debit["P-value"]),3)

pcp.slope <- round(as.numeric(mk_pcp["Sen's Slope"]),2)
pet.slope <- round(as.numeric(mk_etp["Sen's Slope"]),2)
sq.slope <- round(as.numeric(mk_debit["Sen's Slope"]),2)

## EVOLUTION TEMPORELLE
# DISCHARGE
fontface <- 3
sqplot <- ggplot(data =q_data ,aes(x=YYYY,y=debit))+
  annotate("text",x=1983,y=48,label="c) ", fontface=fontface, size=6) +
  annotate("text",x=2011,y=12,
           label=paste0("MM-K pvalue = ", sq.pval,"\nSen's slope = ", sq.slope), 
           size=4, fontface=3) +    
  geom_line(color="green")+
  geom_point(color="green")+
  geom_smooth(method = "lm",color="darkgreen", linetype=2, se=T, level = 0.95)+
  scale_x_continuous(breaks = seq(1983,2018,4))+
  ylim(5,50) +
  ylab(expression(paste("Annual discharge [",{m^3},{.s^-1},"]")))+
  xlab("\nYears")+
  theme_bw() + 
  theme(axis.text = element_text(colour="black", size=12))
sqplot

graph_path <- "graphs"
# ggsave(filename =file.path(graph_path,"q_evolution.png") ,
#        device ="png" ,width =6 ,height =4 ,dpi = 800)


## PCP
prplot <- ggplot(data =pcp_data ,aes(x=YYYY,y=Value))+
  annotate("text",x=1983,y=1250,label="a) ", fontface=fontface, size=6) +  
  annotate("text",x=2011,y=700,label=paste0("MM-K pvalue = ", pcp.pval,"\nSen's slope = ", pcp.slope,""), 
           size=4, fontface=3) +    
  geom_line(color="blue")+
  geom_point(color="blue")+
  geom_smooth(method = "lm",color="darkblue", linetype=2)+
  scale_x_continuous(breaks = seq(1983,2018,4))+
  ylab("Annual rainfall [mm]")+
  xlab("")+
  theme_bw()+
  theme(axis.text = element_text(colour="black", size=12))
  
prplot
# ggsave(filename =file.path(graph_path,"pcp_evolution.png") ,
#        device ="png" ,width =6 ,height =4 ,dpi = 800)

## ETP
petplot <- ggplot(data =etp_data ,aes(x=YYYY,y=ETP))+
  annotate("text",x=1983,y=2325,label="b) ", fontface=fontface, size=6) +   
  annotate("text",x=2011,y=2000,label=paste0("MM-K pvalue = ", pet.pval," (N.S.)\nSen's slope = ", pet.slope,""), 
           size=4, fontface=3) +     
  geom_line(color="orange")+
  geom_point(color="orange")+
  geom_smooth(method = "lm",color="darkorange", linetype=2)+
  scale_x_continuous(breaks = seq(1983,2018,4))+
  ylab("Annual PET [mm]")+
  xlab("")+
  theme_bw() +
  theme(axis.text = element_text(colour="black",size=12))

petplot

# ggsave(filename =file.path(graph_path,"etp_evolution.png") ,
#        device ="png" ,width =6 ,height =4 ,dpi = 800)

grob <- ggarrange(prplot, petplot, sqplot, ncol=1)
ggsave(filename="graphs/MK_plot.png", grob, scale=0.45,
       width = 15, height = 18, units="in", dpi=400)
## Test d'Hypothèse

# 
# 
# ## Test de Pettitt
# pt_etp <- trend::pettitt.test(etp_data$ETP) 
# pt_pcp <- trend::pettitt.test(pcp_data$Value)
# pt_debit <- trend::pettitt.test(q_data$debit)
# 
# 
# pt_etp$estimate #13
# pt_pcp$estimate#25
# pt_debit$estimate#24
#   
# ## Granger-causality Test
 nowkuy_data <- pcp_data%>%
   rename(PCP=Value)%>%
   left_join(etp_data,by="YYYY")%>%
   left_join(q_data,by="YYYY")
 
# granger_test_Q_ETP <- rbind()
# for(i in 0:5){
# test_Q_PCP <- grangertest(PCP~debit,data = nowkuy_data,order=0)
# test_Q_PCP2 <- data.frame(order=i,
#                           F=round(test_Q_PCP$F[2],4),
#                           P_value=round(test_Q_PCP$`Pr(>F)`[2],3))
# granger_test_Q_ETP <- rbind(granger_test_Q_ETP,test_Q_PCP2)
# }
# 
# granger_test_Q_ETP2 <- granger_test_Q_ETP%>%
#   mutate(label="Granger-causality Test (PCP~Q)")
# ggplot(data = granger_test_Q_ETP2,aes(x=order,y=P_value))+
#   geom_bar(stat = "identity",fill="steelblue",color="black")+
#   xlab("order")+
#   ylab("p-value")+
#   facet_grid(facets = .~label)+
#   theme_bw()
# ggsave(filename =file.path(graph_path,"granger_test_PCP_Q.png") ,
#        device ="png" ,width =5.5 ,height =4 ,dpi = 800)
# 
# 
# write.table(x =granger_test_Q_ETP ,
#             file = "output/tendance/granger_test_Q_ETP.txt",
#             quote = FALSE,
#             sep = "\t",
#             row.names = FALSE)
# 

pcp_q_cor <- cor.test(x=nowkuy_data$PCP,
                      y=nowkuy_data$debit,
                      method="spearman")
pcp_q_cor
etp_q_cor <- cor.test(x=nowkuy_data$ETP,
         y=nowkuy_data$debit,
         method="spearman")

etp_q_cor






