library(airGR)

weightedAverage<-function(X,weights,colName)
{
  rain<-sapply(X,function(x) x$tsd[,colName])
  
  rain<-as_tibble(rain) %>% 
    bind_cols(Date=index(X[[1]]$tsd)) %>% 
    gather("Station","Value",-Date) %>% 
    left_join(weights,by="Station") %>% 
    mutate(Value=Value*Area) %>% 
    select(-Area) %>% 
    group_by(Date) %>% 
    summarise(Value=sum(Value))
  
  return(rain)
}

Calibrate_Model<-function(X)
{
#Thiessen weightings
weights<-read_csv("GIS/A5050517Thiessen.csv")
TotalArea<-sum(weights$Area)/1e6 #convert m2 to km2
weights<-weights %>% mutate(Area=Area/sum(Area),
                            Station=as.character(Station))

Rain<-weightedAverage(X,weights,"Rain")
PET<-weightedAverage(X,weights,"Mwet")

Q<-AQWPLoad("Discharge/A5050517.json")
#remove poor quality data, set low flow to zero (at 0.1 ML/d) convert to mm and align rainfall data period
Q<-Q %>% mutate(Value=ifelse(Qual<0,NA,Value),
                Value=ifelse(Value<0.1,0,Value)) %>% 
  mutate(Value=Value/TotalArea,
         Time=as.Date(Time)) %>% 
  select(Time,Value) 

Q<-Rain %>% left_join(Q,by=c("Date"="Time")) %>% 
  select(Date,Value=Value.y)


#setup RR model
#based on https://cran.rstudio.com/web/packages/airGR/vignettes/V01_get_started.html

ErrorC<-ErrorCrit_KGE
RunModel<-RunModel_GR6J

startDate="1978-01-01"
endDate="2004-12-31"

InputsModel <- CreateInputsModel(FUN_MOD = RunModel, DatesR = as.POSIXct(Rain$Date,tz="GMT"),
                                 Precip = Rain$Value, PotEvap = PET$Value)

Ind_Run <- seq(which(format(Rain$Date, format = "%Y-%m-%d") ==  startDate), 
               which(format(Rain$Date, format = "%Y-%m-%d") ==  endDate))

RunOptions <- CreateRunOptions(FUN_MOD = RunModel,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
                               IniStates = NULL, IniResLevels = NULL, IndPeriod_WarmUp = NULL)

InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorC, InputsModel = InputsModel, 
                               RunOptions = RunOptions, VarObs = "Q", Obs = as.numeric(Q$Value)[Ind_Run],
                               transfo="sqrt")

CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel, FUN_CALIB = Calibration_Michel)

OutputsCalib <- Calibration(InputsModel = InputsModel, RunOptions = RunOptions,
                                   InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                   FUN_MOD = RunModel)

Param <- OutputsCalib$ParamFinalR

#calibration period
OutputsModel <- RunModel(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)
plot(OutputsModel, Qobs = Q$Value[Ind_Run])
OutputsCrit <- ErrorC(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

results<-list()
results[['calib_KGE']] <- OutputsCrit
results[['calib_NSE']] <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
results[['calib_para']] <- Param
results[['calib_model']]<- OutputsModel

#validation period

Ind_Run <- seq(which(format(Rain$Date, format = "%Y-%m-%d") ==  "2005-01-01"), 
               which(format(Rain$Date, format = "%Y-%m-%d") ==  "2020-12-31"))

RunOptions <- CreateRunOptions(FUN_MOD = RunModel,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
                               IniStates = NULL, IniResLevels = NULL, IndPeriod_WarmUp = NULL)

InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorC, InputsModel = InputsModel, 
                               RunOptions = RunOptions, VarObs = "Q", Obs = as.numeric(Q$Value)[Ind_Run],
                               transfo="sqrt")

OutputsModel <- RunModel(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)
plot(OutputsModel, Qobs = Q$Value[Ind_Run])

OutputsCrit <- ErrorC(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

results[['valid_KGE']] <- OutputsCrit
results[['valid_NSE']] <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
results[['valid_model']]<- OutputsModel

#comparison period
Ind_Run <- seq(which(format(Rain$Date, format = "%Y-%m-%d") ==  "1900-01-01"), 
               which(format(Rain$Date, format = "%Y-%m-%d") ==  "1977-12-31"))

RunOptions <- CreateRunOptions(FUN_MOD = RunModel,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
                               IniStates = NULL, IniResLevels = NULL, IndPeriod_WarmUp = NULL)

OutputsModel <- RunModel(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)

results[['application']] <- OutputsModel

  return(results)
}