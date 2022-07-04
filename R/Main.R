library(SWTools)
library(tidyverse)
library(zoo)
library(hydroTSM)
library(cowplot)
library(lubridate)

source("R/CalibRRModel_Penrice.R")

#from map Penrice
stations<-c("23313","23302","23300","23317","23725","23705") #23705 is outside catchment, but similar elevation and good data

#Uncomment to download SILO and streamflow data
#SILODownload(stations,path="SILO",startdate="18890101",enddate="20210101")
##force 9am time to align with rainfall
#AQWPDownload("A5050517","Discharge.Best Available","ML/d","Discharge/A5050517.json",StartTime="1977-06-23 09:00")

X<-SILOLoad(stations,"SILO",startdate=as.Date("1891-01-01"),enddate=as.Date("2020-12-31"))

#calculate annual average for 23313
annual<-daily2annual(X[["23313"]]$tsd$Rain,FUN=sum)
print(paste("23313 total:",round(mean(annual),0),"post 1970:",round(mean(window(annual,start="1970-01-01",end="2020-01-01")),0)))
annual<-daily2annual(X[["23302"]]$tsd$Rain,FUN=sum)
print(paste("23302 total:",round(mean(annual),0),"post 1973:",round(mean(window(annual,start="1973-01-01",end="2020-01-01")),0)))

#output information about the SILO data
write.csv(SILOSiteSummary(X),"Outputs/summary.csv")
SILOQualityCodes(X,"Outputs/QualityCodes.tiff")
SILOReport(X,path="Outputs/",filename="Report.doc")

#calibrate RR model
OutputsModelBefore<-Calibrate_Model(X)

#run homogeneity tests
SILOCheckConsistency(X,"Checks/Before")

#correct non-homogeneous data identified from SILOCheckConsistency results
X<-SILOCorrectSite(X,"23313","23705",1970,after=FALSE,plot="Outputs/23313")
X<-SILOCorrectSite(X,"23302","23725",1973,year_start=1908,after=FALSE,plot="Outputs/23302")

#recalculate annual average for 23313
#calculate annual average for 23313
annual<-daily2annual(X[["23313"]]$tsd$Rain,FUN=sum)
print(paste("23313 total:",round(mean(annual),0),"post 1970:",round(mean(window(annual,start="1970-01-01",end="2020-01-01")),0)))
annual<-daily2annual(X[["23302"]]$tsd$Rain,FUN=sum)
print(paste("23302 total:",round(mean(annual),0),"post 1973:",round(mean(window(annual,start="1973-01-01",end="2020-01-01")),0)))

#rerun homogeneity tests on corrected data
SILOCheckConsistency(X,"Checks/After")

#calibrate RR model to corrected data
OutputsModelAfter<-Calibrate_Model(X)

#Output comparison of models

#NSE
stats<-array(0,c(2,4))
stats[1,1]<-OutputsModelBefore[['calib_KGE']]$CritValue
stats[1,3]<-OutputsModelBefore[['valid_KGE']]$CritValue
stats[2,1]<-OutputsModelAfter[['calib_KGE']]$CritValue
stats[2,3]<-OutputsModelAfter[['valid_KGE']]$CritValue
stats[1,2]<-OutputsModelBefore[['calib_NSE']]$CritValue
stats[1,4]<-OutputsModelBefore[['valid_NSE']]$CritValue
stats[2,2]<-OutputsModelAfter[['calib_NSE']]$CritValue
stats[2,4]<-OutputsModelAfter[['valid_NSE']]$CritValue
colnames(stats)<-c("Calib.KGE","Calib.NSE","Valid.KGE","Valid.NSE")
rownames(stats)<-c("Original","Corrected")
write.csv(round(stats,2),"Outputs/RRModelStats.csv")

#parameter values
param<-rbind(OutputsModelBefore[['calib_para']],OutputsModelAfter[['calib_para']])
colnames(param)<-paste0("X",1:6)
rownames(param)<-c("Original","Corrected")
write.csv(round(param,2),"Outputs/RRModelParam.csv")

#plot comparison of daily and annual flow duration curves 
d_models<-zoo(cbind(OutputsModelBefore[['application']]$Qsim,
                    OutputsModelAfter[['application']]$Qsim),OutputsModelAfter[['application']]$DatesR)
names(d_models)<-c("Original","Corrected")
a_models<-daily2annual(d_models,FUN=sum)

X1<-as_tibble(d_models) %>% 
  setNames(c("Original","Corrected")) %>% 
  gather("model","flow") %>% 
  mutate(period="Daily") %>% 
  bind_rows(
    as_tibble(a_models) %>% 
    setNames(c("Original","Corrected")) %>% 
    gather("model","flow") %>% 
    mutate(period="Annual")
  )

p1<-ggplot(X1 %>% filter(period=="Daily"))+
  geom_line(aes(y = 1 - ..y..,x=flow,colour=model),stat='ecdf')+
  scale_x_continuous(trans="log",breaks = c(0.01,0.1,1,10,100))+
  coord_flip(xlim=c(0.01,max(X1 %>% filter(period=="Daily") %>% pull(flow))),
                  ylim=c(0,0.5))+
  xlab("Daily discharge (mm/d)")+ylab("Proportion of time exceeded")+theme_bw()+
  theme(legend.position = "none")+
theme(text = element_text(size = 10))+
  scale_colour_manual(values=c("#1a9850","#fc8d59"))


p2<-ggplot(X1 %>% filter(period=="Annual"))+
  geom_line(aes(y = 1 - ..y..,x=flow,colour=model),stat='ecdf')+
   coord_flip(xlim=c(1,200),
              ylim=c(0,1))+
  xlab("Annual discharge (mm/year)")+ylab("Proportion of time exceeded")+theme_bw()+ 
  theme(text = element_text(size = 10),
        legend.position=c(0.8,0.85))  +
  guides(colour=guide_legend(title="Rainfall"))+
  scale_colour_manual(values=c("#1a9850","#fc8d59"))
  

plot_grid(p1,p2,rel_widths = c(1,1))
ggsave("Outputs/fdc.tiff",width=19,height=10,unit="cm",compression = "lzw",dpi=1000)

#annual averages
X1 %>% filter(period=="Annual") %>% group_by(model) %>% summarise(flow=mean(flow))
#no flow days
X1 %>% filter(period=="Daily") %>% group_by(model) %>% summarise(flow=sum(flow>0.01)/length(flow)*100)

#summary of P and Q in the different periods
ModelMeans<-function(model,name)
{
  length<-time_length(max(model$DatesR)-min(model$DatesR)+1,"years")
  P<-sum(model$Precip)/length
  Q<-sum(model$Qsim)/length
  return(tibble(name=name,P=P,Q=Q))
}

means<-ModelMeans(OutputsModelBefore[['calib_model']],"calib-orig") %>% 
  bind_rows(ModelMeans(OutputsModelAfter[['calib_model']],"calib-corrected")) %>% 
  bind_rows(ModelMeans(OutputsModelBefore[['valid_model']],"valid-orig")) %>% 
  bind_rows(ModelMeans(OutputsModelAfter[['valid_model']],"valid-corrected")) %>%
  bind_rows(ModelMeans(OutputsModelBefore[['application']],"application-orig")) %>% 
  bind_rows(ModelMeans(OutputsModelAfter[['application']],"application-corrected"))
write.csv(means,"Outputs/MeanQ.csv")
  
Q<-AQWPLoad("Discharge/A5050517.json")
Qa<-Q %>%  mutate(year=year(Time)) %>% 
  group_by(year) %>% 
  summarise(Q=sum(Value)/117.1291) #ML/yr to mm/yr

Obs<-c(mean(Qa %>% filter(year>=1978 & year<=2004) %>% pull(Q),na.rm=TRUE),
       mean(Qa %>% filter(year>=2005 & year<=2020) %>% pull(Q),na.rm=TRUE))