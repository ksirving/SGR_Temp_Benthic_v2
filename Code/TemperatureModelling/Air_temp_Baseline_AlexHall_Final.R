library(h5)
library(sf)
library(raster)
library(rgdal)
library(foreach)
library(doParallel)
library(tidyverse)
library(lubridate)
install.packages("h5")
########################


getwd()
#read DAT file with lat and long
lat<-read.table("ignore/temp_input/Lat.dat")
lon<-read.table("ignore/temp_input//Lon.dat")


#read in comid points
com1 <- st_read("ignore/temp_input/COMID_to_Point.shp") %>% 
  dplyr::select(1) %>% 
  st_zm()

#set wd
setwd("E:/TA/Baseline")

#get list of files from all years but only May through September

# get h5 file names in extracted directory
h5fls <- list.files(getwd(), recursive = T, full.names = T)

# setup parallel backend
ncores <- detectCores() - 1  
cl<-makeCluster(ncores)
registerDoParallel(cl)
strt<-Sys.time()

#take month and day of files
trufls <- substr(h5fls, 48, 51)
trufls <- as.numeric(trufls) %>% data.frame()

#select may through september
trufls$keep <- trufls >= 0501 & trufls <= 0930
trufls$keep <- as.vector(trufls$keep)
trufls$h5fls<- list.files(getwd(), recursive = T, full.names = T)
trufls<- trufls %>% 
  filter(keep == TRUE)



res <- foreach(i = seq_along(trufls$h5fls), .packages = c('dplyr', 'sf', 'raster', 'h5', 'rgdal')) %dopar% {

  
  # log
  sink('log.txt')
  cat(i, 'of', length(trufls$h5fls), '\n')
  print(Sys.time()-strt)
  sink()
  
  # select one h5 file, open connection
  h5fl <- trufls$h5fls[i]
  h5flcon <- h5file(name = h5fl, mode = "a")
  
  # read TA dataset, 1600 x 2400
  temp <- readDataSet(h5flcon['TA'])
  
  #reverse row to correct layout
  temp <- temp[nrow(temp):1, ]
  
  #make raster file of TA
  temp<-raster(temp)
  extent(temp) <- matrix(c(min(lon),min(lat), max(lon), max(lat)), nrow=2)
  crs(temp) <-   "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


  #extract temp value at each point
  pts <- as(com1$geometry, "Spatial") 
  temp_K<-extract(x=temp, y=pts) %>% data.frame()
  temp_K$COMID <- com1$COMID
  

  # close the connection
  h5close(h5flcon)

  return(temp_K)
  
}

stopCluster(cl)

#names list items
names(res) <- trufls$h5fls


#make df  - to combine a list vertically and then unest it from tibble format
temperature_K <- res %>% 
  enframe %>% 
  unnest %>% 
  mutate(
    date = gsub('^.*\\_([0-9]+)\\.h5$', '\\1', name),
    date = lubridate::ymd(date)
  ) %>% 
  dplyr::select(2:4)
names(temperature_K)[1]<- "temp_K"




save(temperature_K, file = 'L:/Flow ecology and climate change_ES/Jenny/AirTemp/AirTemperature_Baseline/temperature_K.RData', compress = 'xz')



#remove NaN values


#load alex hall air temp
load(file = 'L:/Flow ecology and climate change_ES/Jenny/AirTemp/AirTemperature_Baseline/temperature_K.RData')
temperature_C<- temperature_K %>% 
  mutate(temp_C = temp_K-273.15) %>% 
  dplyr::select(-temp_K)

#read in the observed stream temp data
StreamTemp_RB4<- read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/StreamTem_RB4.csv")
StreamTemp_RB4$date<- ymd(StreamTemp_RB4$date)

#join observed stream temp with alex halls air temp
Temp_Stream_AirAlex_Join <- left_join(StreamTemp_RB4, temperature_C, by = c("COMID", "date"))
write.csv(Temp_Stream_AirAlex_Join, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Air_Alex.csv")






###############################################################




# stream temp model for RB4 region and dates

RB4_model <- read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Air_Alex.csv") %>% 
  dplyr::select(-X.1, -X)
RB4_model$date<- ymd(RB4_model$date)

unique(RB4_model$COMID) #17567207 22524629 17574527 17585800 20365115 17575785

#calculate date range of temp timeseries for each COMID
dates<-RB4_model %>% 
  group_by(COMID) %>% 
  summarise(firstDay = min(date), lastDay = max(date), NumDays = max(date)- min(date))

#plots

library(scales)#for the alpha parameter

#17567207  - Lockwood creek, trib to piru creek above all dams. high in watershed
#dates: includes April - Sept.  2014-05-14 - 2014-09-30
#Natural condition
#shading might be important here - its in the mountains 
with(RB4_model[RB4_model$COMID==17567207,], plot(temp_C, Max_Tw_C, 
                                                 xlim = c(0,30), ylim = c(0,30),
                                                 col = alpha(col = "black", 0.4), pch=16,
                                                 xlab = "Max Air Temperature (C)",
                                                 ylab = "Max Stream Temperature (C)",
                                                 main = "17567207", 
                                                 abline(lm(Max_Tw_C~temp_C))))
abline(a= 0, b =1, lty = "dashed", col = "grey", lwd = 2)
mdl = lm(Max_Tw_C~temp_C, data = RB4_model[RB4_model$COMID==17567207,])
mdl
cor(RB4_model[RB4_model$COMID==17567207,]$Max_Tw_C,
    RB4_model[RB4_model$COMID==17567207,]$temp_C, 
    use =  "complete.obs")
  #0.6710819

#22524629 Bear Creek, trib to west fork san gabriel, high in watershed
#dates: includes April - Sept.  2014-05-06-2014-09-30
#natural condition
#shading might be important here - its in the mountains 
with(RB4_model[RB4_model$COMID==22524629,], plot(temp_C, Max_Tw_C, 
                                                 xlim = c(10,30), ylim = c(10,30), 
                                                 col = alpha(col = "black", 0.4), pch=16,
                                                 main = "22524629", 
                                                 abline(lm(Max_Tw_C~temp_C))))
abline(a= 0, b =1, lty = "dashed", col = "grey", lwd = 2)
mdl = lm(Max_Tw_C~temp_C, data = RB4_model[RB4_model$COMID==22524629,])
mdl
cor(RB4_model[RB4_model$COMID==22524629,]$Max_Tw_C,
    RB4_model[RB4_model$COMID==22524629,]$temp_C, 
    use =  "complete.obs")
  #0.6182914

#17574527 trib (headwaters) to santa clara river
#dates: includes April - Sept.  2014-04-15 - 2014-05-19
#unclear if natural condition, adjacent to some operation - possible heated effluent? 
#very small 2 degree stream, adjacent to some operation - possible heated effluent? 
with(RB4_model[RB4_model$COMID==17574527,], plot(temp_C, Max_Tw_C, 
                                                 xlim = c(0,60), ylim = c(0,60),
                                                 col = alpha(col = "black", 0.4), pch=16,
                                                 main = "17574527", 
                                                 abline(lm(Max_Tw_C~temp_C))))
abline(a= 0, b =1, lty = "dashed", col = "grey", lwd = 2)
mdl = lm(Max_Tw_C~temp_C, data = RB4_model[RB4_model$COMID==17574527,])
mdl
cor(RB4_model[RB4_model$COMID==17574527,]$Max_Tw_C,
    RB4_model[RB4_model$COMID==17574527,]$temp_C, 
    use =  "complete.obs")
  #0.8305766

#17585800 Matilija creek above matilija dam
#dates: includes April - Sept.  2014-05-28 2014-07-29
#natural condition
#stream is unconfined so shading shouldnt be too important
with(RB4_model[RB4_model$COMID==17585800,], plot(temp_C, Max_Tw_C,  
                                                 xlim = c(15,35), ylim = c(15,35),
                                                 col = alpha(col = "black", 0.4), pch=16,
                                                 main = "17585800", 
                                                 abline(lm(Max_Tw_C~temp_C))))
abline(a= 0, b =1, lty = "dashed", col = "grey", lwd = 2)
mdl = lm(Max_Tw_C~temp_C, data = RB4_model[RB4_model$COMID==17585800,])
mdl
cor(RB4_model[RB4_model$COMID==17585800,]$Max_Tw_C,
    RB4_model[RB4_model$COMID==17585800,]$temp_C, 
    use =  "complete.obs")
#0.4535367

#20365115 Coastal creek in between palicades and santa monica, close to coast but still in santa monica mountains
#dates: includes April - Sept.  2014-05-20-2014-07-31
#natural condition
#shading may be important, but also likely a very small stream so it migth heat up a lot
with(RB4_model[RB4_model$COMID==20365115,], plot(temp_C, Max_Tw_C,  
                                                 xlim = c(0,50), ylim = c(10,50),
                                                 col = alpha(col = "black", 0.4), pch=16, 
                                                 main = "20365115", 
                                                 abline(lm(Max_Tw_C~temp_C))))
abline(a= 0, b =1, lty = "dashed", col = "grey", lwd = 2)
mdl = lm(Max_Tw_C~temp_C, data = RB4_model[RB4_model$COMID==20365115,])
mdl
cor(RB4_model[RB4_model$COMID==20365115,]$Max_Tw_C,
    RB4_model[RB4_model$COMID==20365115,]$temp_C, 
    use =  "complete.obs")
#0.3084889

#17575785
#santa clara mainstem by gage 
#dates: includes April - Sept.  2004-10-07 - 2007-09-24
#agriculture surrounding reach, downstream of castaic creek with large dam 
#looks unconfined - shading unlikely to be an issue
with(RB4_model[RB4_model$COMID==17575785,], plot(temp_C, Ins_Tw_C,  
                                                 xlim = c(10,40), ylim = c(10,40),
                                                 col = alpha(col = "black", 0.4), pch=16,
                                                 main = "17575785", 
                                                 abline(lm(Ins_Tw_C~temp_C))))
abline(a= 0, b =1, lty = "dashed", col = "grey", lwd = 2)
mdl = lm(Ins_Tw_C~temp_C, data = RB4_model[RB4_model$COMID==17575785,])
mdl
cor(RB4_model[RB4_model$COMID==17575785,]$Ins_Tw_C,
    RB4_model[RB4_model$COMID==17575785,]$temp_C, 
    use =  "complete.obs")
#0.3985866




#plots with Max Stream Temp
RB4_model_max <- RB4_model[!is.na(RB4_model$Max_Tw_C),]
reach<-unique(RB4_model_max$COMID)

#plots with instantaneous Stream Temp
RB4_model_inst <- RB4_model %>% 
  filter(!COMID %in% reach)
reach_inst<-unique(RB4_model_inst$COMID)


# 7-day period maximum weekly average and weekely maxiumn temperature, (MWAT and MWMT)

# need to caluclate mean temp by averageing max and min temp, except all the means were monitored
sum(is.na(RB4_model_max$Mean_Tw_C)) #0 

#rename dataframe to match the dataframe name in the Livneh script
RB4_model_mean <- RB4_model_max 

# need to remove 17574527 because only 19 days of data in the summer month
RB4_model_mean<-RB4_model_mean %>% 
  filter(COMID != 17574527)

#remove NAs
RB4_model_mean <- RB4_model_mean[!is.na(RB4_model_mean$Mean_Tw_C),]

com<- unique(RB4_model_mean$COMID)

for (i in 1:length(com)){
  assign(paste("meanTemp", com[i], sep = "_"), RB4_model_mean[RB4_model_mean$COMID == com[i],])
}


#make list of dataframes to appl same function to each
df.list<- list(meanTemp_17567207, meanTemp_17585800, meanTemp_20365115, meanTemp_22524629)


Air_temp_metric_mean_water<- function(df){
  c<-df$Mean_Tw_C
  n<- 1:(length(c)-6)
  h20<-NULL
  for (i in n){
    q <- mean(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6])) 
    h20 <- rbind(h20, q)
  }
  df$SevDayMeanWater<- c(h20,NA,NA,NA,NA,NA,NA)
  return(df)
}

Air_temp_metric_mean_air<- function(df){
  #calculate the 7 day averages of the air
  c<-df$temp_C
  n<- 1:(length(c)-6)
  air<-NULL
  for (i in n){
    q <- mean(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    air <- rbind(air, q)
  }
  df$SevDayMeanAir <- c(air,NA,NA,NA,NA,NA,NA)
  return(df)
}

Air_temp_metric_max_water<- function(df){  
  #calculate the 7 day max of the water
  c<-df$Max_Tw_C
  n<- 1:(length(c)-6)
  h20_max<-NULL
  for (i in n){
    q <- max(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    h20_max <- rbind(h20_max, q)
  }
  df$SevDayMaxWater<- c(h20_max,NA,NA,NA,NA,NA,NA)
  return(df)
}  

Air_temp_metric_max_air<- function(df){
  #calculate the 7 day max of the air
  c<-df$temp_C
  n<- 1:(length(c)-6)
  air_max<-NULL
  for (i in n){
    q <- max(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    air_max <- rbind(air_max, q)
  }
  df$SevDayMaxAir <- c(air_max,NA,NA,NA,NA,NA,NA)
  return(df)
}  

Air_temp_metric_min_water<- function(df){
  c<-df$Min_Tw_C
  n<- 1:(length(c)-6)
  h20<-NULL
  for (i in n){
    q <- min(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6])) 
    h20 <- rbind(h20, q)
  }
  df$SevDayMinWater<- c(h20,NA,NA,NA,NA,NA,NA)
  return(df)
}

Air_temp_metric_min_air<- function(df){

  c<-df$temp_C
  n<- 1:(length(c)-6)
  air<-NULL
  for (i in n){
    q <- min(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    air <- rbind(air, q)
  }
  df$SevDayMinAir <- c(air,NA,NA,NA,NA,NA,NA)
  return(df)
}

max_air <- lapply(df.list, function(x) Air_temp_metric_max_air(x))
max_water <- lapply(df.list, function(x) Air_temp_metric_max_water(x))
min_air <- lapply(df.list, function(x) Air_temp_metric_min_air(x))
min_water <- lapply(df.list, function(x) Air_temp_metric_min_water(x))
mean_air <- lapply(df.list, function(x) Air_temp_metric_mean_air(x))
mean_water <- lapply(df.list, function(x) Air_temp_metric_mean_water(x))


#binds list elements together into a dataframe
max_air_test<-do.call("rbind", max_air)
max_water_test<-do.call("rbind", max_water)
min_air_test<-do.call("rbind", min_air)
min_water_test<-do.call("rbind", min_water)
mean_air_test<-do.call("rbind", mean_air)
mean_water_test<-do.call("rbind", mean_water)

#join variables to one df
final_RB4<- cbind(max_air_test, max_water_test[,9], min_air_test[,9], min_water_test[,9], mean_air_test[,9], mean_water_test[,9])
names(final_RB4)[10]<- "SevDayMaxWat"
names(final_RB4)[11]<- "SevDayMinAir"
names(final_RB4)[12]<- "SevDayMinWat"
names(final_RB4)[13]<- "SevDayMeanAir"
names(final_RB4)[14]<- "SevDayMeanWat"
final_RB4<- final_RB4 %>% 
  dplyr::select(COMID, date, Max_Tw_C, Min_Tw_C, Mean_Tw_C, temp_C, SevDayMaxAir, SevDayMaxWat, SevDayMinAir, SevDayMinWat, SevDayMeanAir, SevDayMeanWat)

#rbind with 'final' dataframe from Livneh script
load(file = 'L:/Flow ecology and climate change_ES/Jenny/AirTemp/AirTemperature_Baseline/final.RData')
names(final_RB4)[5]<-"Mean_Tw_C_new"
names(final_RB4)[6]<- "TempAir_C"

final_RB4_Livneh <- rbind(final_RB4, final)

save(final_RB4_Livneh, file = 'L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/SevDayMeanMinMax.RData')





#load data
load(file = 'L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/SevDayMeanMinMax.RData')


#load watershed area/elevation streamcat data
area <- read.csv("C:/Users/JennyT/Documents/LitReview/RB4/StreamCat/Elevation_Region18.csv") %>% 
  select(COMID, WsAreaSqKm, ElevWs)
final_RB4_Livneh <- left_join(final_RB4_Livneh, area, by = "COMID")

#keep only summer months to build model
final_RB4_Livneh$month<- month(final_RB4_Livneh$date)
final_RB4_Livneh <- final_RB4_Livneh %>% 
  filter(month %in% c(5,6,7,8,9))

#remove 20% of each COMID
library(caret)
set.seed(34232)
inTrain<-createDataPartition(y=final_RB4_Livneh$COMID,
                             p=0.8, list=FALSE)
training<-final_RB4_Livneh[inTrain,]
testing<-final_RB4_Livneh[-inTrain,]


library(lattice)
library(lme4)
par(mar = c(5,5,5,5))
xyplot(SevDayMeanWat ~ SevDayMeanAir| factor(COMID), data=training, pch = 19,
       Main = "Weekly Means", xlab = "7-day Mean Air", ylab = "7-day Mean Water",
       layout = c(7,3), type = c("p", "g", "r"),
       col.line = "red", col = "black", alpha = .4,
       par.strip.text=list(cex=.75))

xyplot(SevDayMaxWat ~ SevDayMaxAir| factor(COMID), data=training, pch = 19,
       Main = "Weekly Maximums", xlab = "7-day Max Air", ylab = "7-day Max Water",
       layout = c(7,3), type = c("p", "g", "r"),
       col.line = "red", col = "black", alpha = .4,
       par.strip.text=list(cex=.75))
xyplot(SevDayMinWat ~ SevDayMinAir| factor(COMID), data=training, pch = 19,
       Main = "Weekly Minimums", xlab = "7-day Min Air", ylab = "7-day Min Water",
       layout = c(7,3), type = c("p", "g", "r"),
       col.line = "red", col = "black", alpha = .4,
       par.strip.text=list(cex=.75))

#linear model relating 7 day mean air to 7 day mean water
#fit_mean<- lmList(SevDayMeanWat~SevDayMeanAir | COMID, data = training)
  #averaged model from excel: y = 0.705084x + 5.844546  - Unaltered
  #averaged model from excel: y = 0.5392216x + 10.4349226  - Altered, Santa Ana below Prado

#linear model relating 7 day max air to 7 day max water 
#fit_max<- lmList(SevDayMaxWat~SevDayMaxAir | COMID, data = training)
  #averaged model from excel: y = 0.71806x + 10.61639  - Unaltered
  #averaged model from excel: y = 0.4750708x + 14.009253  - Altered, Santa Ana below Prado

#multiple linear model
mod_mean<- lm(SevDayMeanWat~SevDayMeanAir+WsAreaSqKm, data = training)
mod_max <- lm(SevDayMaxWat~SevDayMaxAir+ElevWs+WsAreaSqKm, data = training)
mod_min <- lm(SevDayMinWat~SevDayMinAir+ElevWs+WsAreaSqKm, data = training)
#save models for use in future predcitions
save(mod_mean, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/mod_mean.rda")
save(mod_max, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/mod_max.rda")
save(mod_min, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/mod_min.rda")

#### testing Model ####

#residual plots are normal
hist(mod_mean$residuals, 
     xlab = "Residual", ylab = "Frequency", 
     main = "Residual Plot for 7-day Mean Stream Temperature",
     col = "grey", 
     breaks = 20) 
hist(mod_max$residuals, 
     xlab = "Residual", ylab = "Frequency", 
     main = "Residual Plot for 7-day Maximum Stream Temperature",
     col = "grey", 
     breaks = 20)
hist(mod_min$residuals, 
     xlab = "Residual", ylab = "Frequency", 
     main = "Residual Plot for 7-day Minimum Stream Temperature",
     col = "grey", 
     breaks = 20)

#predict on testing data
pred_mean <- predict(mod_mean, newdata = testing)
pred_max <- predict(mod_max, newdata = testing)
pred_min <- predict(mod_min, newdata = testing)
testing$COMID<-as.factor(testing$COMID)
a<-ggplot(data = testing, aes(x = SevDayMeanWat, y = pred_mean))+ geom_point(alpha = 0.2)+
  labs(x = "Seven Day Mean Water Temp", y = "Predicted")+
  geom_smooth(method = "lm", col="red")
b<-ggplot(data = testing, aes(x = SevDayMaxWat, y = pred_max))+ geom_point(alpha = 0.2)+
  labs(x = "Seven Day Max Water Temp", y = "Predicted")+
  geom_smooth(method = "lm", col="red")
c<-ggplot(data = testing, aes(x = SevDayMinWat, y = pred_min))+ geom_point(alpha = 0.2)+
  labs(x = "Seven Day Min Water Temp", y = "Predicted")+
  geom_smooth(method = "lm", col="red")

library(gridExtra)
margin = theme(plot.margin = unit(c(10,10,10,10), "mm"))
g<-arrangeGrob(a + margin, b+ margin, c+margin, nrow = 1)
ggsave("L:/Flow ecology and climate change_ES/Jenny/AirTemp/WaterT_pred_vs_WaterT_obs.png", plot = g, dpi = 300, width = 15, height = 5)


#Root mean squared error and Nash-Sutcliffe Coef
testing$pred_mean<- pred_mean
testing$pred_max <- pred_max
testing$pred_min <- pred_min

testing <- testing %>% 
  mutate(resid_sq_mean = (pred_mean-SevDayMeanWat)^2) %>% 
  mutate(resid_sq_max = (pred_max-SevDayMaxWat)^2) %>% 
  mutate(resid_sq_min = (pred_min-SevDayMinWat)^2) %>% 
  mutate(NSC_demon_mean = SevDayMeanWat - mean(testing$SevDayMeanWat, na.rm = TRUE)) %>% 
  mutate(NSC_demon_max = SevDayMaxWat - mean(testing$SevDayMaxWat, na.rm = TRUE)) %>% 
  mutate(NSC_demon_min = SevDayMinWat - mean(testing$SevDayMinWat, na.rm = TRUE))

testing$RMSE_mean<- sqrt(mean(testing$resid_sq_mean, na.rm = TRUE))  #2.141971
testing$RMSE_max<- sqrt(mean(testing$resid_sq_max, na.rm = TRUE))  #3.325838
testing$RMSE_min<- sqrt(mean(testing$resid_sq_min, na.rm = TRUE))  #2.206111
testing$NSC_mean <- (1 - ((sum(testing$resid_sq_mean, na.rm = TRUE))/(sum(testing$NSC_demon_mean^2, na.rm = TRUE))))#0.6335249
testing$NSC_max <- (1 - ((sum(testing$resid_sq_max, na.rm = TRUE))/(sum(testing$NSC_demon_max^2, na.rm = TRUE)))) #0.46993
testing$NSC_min <- (1 - ((sum(testing$resid_sq_min, na.rm = TRUE))/(sum(testing$NSC_demon_min^2, na.rm = TRUE)))) #0.6935738
#caluculate the range of the training data so we know how important the RMSE is
range(training$SevDayMaxWat,na.rm = T)
range(training$SevDayMinWat,na.rm = T)
range(training$SevDayMeanWat,na.rm = T)

#test normality with Shapiro-Wilk normality test on minimum residutals
shpwlk<- sample(mod_min$residuals, 5000)
shapiro.test(shpwlk)
shpwlk<- sample(mod_mean$residuals, 5000)
shapiro.test(shpwlk)
shpwlk<- sample(mod_max$residuals, 5000)
shapiro.test(shpwlk)


#use linear models to calculated the 7day mean, min, and max water temp for baseline conditions

#load alex hall air temp
load(file = 'L:/Flow ecology and climate change_ES/Jenny/AirTemp/AirTemperature_Baseline/temperature_K.RData')
temperature_C<- temperature_K %>% 
  mutate(temp_C = temp_K-273.15, year = year(date)) %>% 
  dplyr::select(-temp_K)

#calculate the 7 day running mean and max air temp

Air_temp_metric_mean_air<- function(df){
  #calculate the 7 day averages of the air
  c<-df$temp_C
  n<- 1:(length(c)-6)
  air<-NULL
  for (i in n){
    q <- mean(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    air <- rbind(air, q)
  }
  df$SevDayMeanAir <- c(air,NA,NA,NA,NA,NA,NA)
  return(df)
}

Air_temp_metric_max_air<- function(df){
  #calculate the 7 day max of the air
  c<-df$temp_C
  n<- 1:(length(c)-6)
  air_max<-NULL
  for (i in n){
    q <- max(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    air_max <- rbind(air_max, q)
  }
  df$SevDayMaxAir <- c(air_max,NA,NA,NA,NA,NA,NA)
  return(df)
}  

Air_temp_metric_min_air<- function(df){
  #calculate the 7 day min of the air
  c<-df$temp_C
  n<- 1:(length(c)-6)
  air_min<-NULL
  for (i in n){
    q <- min(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    air_min <- rbind(air_min, q)
  }
  df$SevDayMinAir <- c(air_min,NA,NA,NA,NA,NA,NA)
  return(df)
}  


test<-temperature_C %>% 
  group_by(COMID, year) %>% 
    nest() %>% 
    mutate(
     mean_air = map(data, Air_temp_metric_mean_air)
      )
test <- test %>% 
    dplyr::select(- data) %>% 
    unnest()

test<-test %>% 
  group_by(COMID, year) %>% 
  nest() %>% 
  mutate(
    max_air = map(data, Air_temp_metric_max_air)
  )
test <- test %>% 
  dplyr::select(- data) %>% 
  unnest()

test<-test %>% 
  group_by(COMID, year) %>% 
  nest() %>% 
  mutate(
    min_air = map(data, Air_temp_metric_min_air)
  )
test <- test %>% 
  dplyr::select(- data) %>% 
  unnest()


#calculate 7 day mean and max water based on models: mod_max and mod_mean 
baseline_stream_temp <- test
baseline_stream_temp <- left_join(baseline_stream_temp, area, by = "COMID")

baseline_stream_temp$SevDayMeanWat <- predict(mod_mean, newdata = baseline_stream_temp)
baseline_stream_temp$SevDayMaxWat <- predict(mod_max, newdata = baseline_stream_temp)
baseline_stream_temp$SevDayMinWat <- predict(mod_min, newdata = baseline_stream_temp)
#calculate seven day max - seven day min
baseline_stream_temp$SevDayRngWat <- (baseline_stream_temp$SevDayMaxWat-baseline_stream_temp$SevDayMinWat)

#get maximum annual weekly max and mean; minimum weekly minimum,  
baseline_stream_temp <- baseline_stream_temp %>% 
  group_by(COMID, year) %>% 
  summarise(Max_Wkly_Mean_StreamT = max(SevDayMeanWat, na.rm = TRUE), 
            Max_Wkl_Max_StreamT = max(SevDayMaxWat, na.rm = TRUE),
            Min_Wkl_Min_StreamT = min(SevDayMinWat, na.rm = TRUE),
            Max_Wkl_Rng_StreamT = max(SevDayRngWat, na.rm = TRUE),
            Mean_Wkl_Rng_StreamT = mean(SevDayRngWat, na.rm = TRUE),
            Max_Wkl_Max_StreamT_grt_30_ = sum(SevDayMaxWat>30, na.rm = TRUE)) 


save(baseline_stream_temp, file = 'L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/baseline_stream_temp.RData')
load('L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/baseline_stream_temp.RData')

#species model based on stream temperature

library(ggplot2)
#read data
species <- read.csv("C:/Users/JennyT/Documents/LitReview/RB4/WorkingData_3-16-18/FlowMetrics_VariableSelection.csv") %>% 
  dplyr::select(name, COMID, year, occurrence) 

#join to stream temp metrics
species <- left_join(species, baseline_stream_temp, by= c("COMID", "year"))

#remove NA ie years when we dont have air temp and therefore no stream temp
species <- species[!is.na(species$Max_Wkly_Mean_StreamT),]

setwd("L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/Figures/boxplots")

save(species, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/spp_tmp_train_dat.RData")

#trout
trout <- species %>% 
  filter(name == "rainbow trout")
#boxplots of variables
ggplot(trout, aes(x = as.factor(occurrence), y = Max_Wkly_Mean_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly mean stream temperature (C)")+
  ggtitle("Rainbow Trout")+
  theme(text = element_text(size=20))
ggsave("trout_weekly_mean_+jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(trout, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly maximum stream temperature (C)")+
  ggtitle("Rainbow Trout")+
  theme(text = element_text(size=20))
ggsave("trout_weekly_max_jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(trout, aes(x = as.factor(occurrence), y = Min_Wkl_Min_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Minimum weekly minimum stream temperature (C)")+
  ggtitle("Rainbow Trout")+
  theme(text = element_text(size=20))
ggsave("trout_Min_Wkl_Min_StreamT_jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(trout, aes(x = as.factor(occurrence), y = Max_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum Weekly Range (C)")+
  ggtitle("Rainbow Trout")+
  theme(text = element_text(size=20))
ggsave("trout_Max_Wkl_Rng_StreamT_jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(trout, aes(x = as.factor(occurrence), y = Mean_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Mean Weekly Range (C)")+
  ggtitle("Rainbow Trout")+
  theme(text = element_text(size=20))
ggsave("trout_Mean_Wkl_Rng_StreamT_jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(trout, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT_grt_30_))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Number of 7 day Max > 30C (days)")+
  ggtitle("Rainbow Trout")+
  theme(text = element_text(size=20))
ggsave("trout_Max_Wkl_Max_StreamT_grt_30__jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

#cor(species$Min_Wkl_Min_StreamT, species$Max_Wkl_Rng_StreamT)

#trout model
trout_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT+Min_Wkl_Min_StreamT+Max_Wkl_Rng_StreamT, family = "binomial",data = trout)
summary(trout_mdl)

save(trout_mdl, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/trout_mdl.rda")

logodd_trout <- predict(trout_mdl, newdata = baseline_stream_temp) # log odds
probs_trout <- exp(logodd_trout)/(1+exp(logodd_trout))             # probability
head(probs_trout)

# set.seed(3193)
# N<-length(probs_trout)
# uni<-runif(N,0,1)
# fake_dat_trout<-(uni<probs_trout)*1
# head(fake_dat_trout)

mydt<- data.frame("values" = baseline_stream_temp$Max_Wkl_Max_StreamT, "prob"= probs_trout)
plot1<- ggplot(mydt, aes(x=baseline_stream_temp$Max_Wkl_Max_StreamT, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Max 7-day Max")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(10,50)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(10,50), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("trout_log_reg_maxMax.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

mydt<- data.frame("values" = baseline_stream_temp$Min_Wkl_Min_StreamT, "prob"= probs_trout)
plot2 <- ggplot(mydt, aes(x=baseline_stream_temp$Min_Wkl_Min_StreamT, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Min 7-day Min")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(5,20)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(5,20), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("trout_log_reg_minMin.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

mydt<- data.frame("values" = baseline_stream_temp$Max_Wkl_Rng_StreamT, "prob"= probs_trout)
plot3 <- ggplot(mydt, aes(x=baseline_stream_temp$Max_Wkl_Rng_StreamT, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Max 7-day Range")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(0,50)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(0,50), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("trout_log_reg_MaxRng.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

#making plot for manuscript

library(gridExtra)
margin = theme(plot.margin = unit(c(20,20,20,20), "mm"))
g<-arrangeGrob(plot1 + margin, plot2+ margin, plot3+ margin, nrow = 1, top = "(a) O. mykiss")
ggsave("C:/Users/JennyT/Documents/Prospectus/Omykiss_temp.png", plot = g, dpi = 300, width = 20, height = 5)



#chub
chub <- species %>% 
  filter(name == "arroyo chub")

ggplot(chub, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly maximum stream temp (C)")+
  ggtitle("Arroyo Chub")+
  theme(text = element_text(size=20))
ggsave("chub_weekly_max_jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(chub, aes(x = as.factor(occurrence), y = Max_Wkly_Mean_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly mean stream temp (C)")+
  ggtitle("Arroyo Chub")+
  theme(text = element_text(size=20))
ggsave("chub_weekly_mean_jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(chub, aes(x = as.factor(occurrence), y = Min_Wkl_Min_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Minimum weekly min stream temp (C)")+
  ggtitle("Arroyo Chub")+
  theme(text = element_text(size=20))
ggsave("chub_Min_Wkl_Min_StreamT_jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(chub, aes(x = as.factor(occurrence), y = Max_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly stream temp range (C)")+
  ggtitle("Arroyo Chub")+
  theme(text = element_text(size=20))
ggsave("chub_Max_Wkl_Rng_StreamT_jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(chub, aes(x = as.factor(occurrence), y = Mean_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Mean weekly stream temp range (C)")+
  ggtitle("Arroyo Chub")+
  theme(text = element_text(size=20))
ggsave("chub_Mean_Wkl_Rng_StreamT_jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(chub, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT_grt_30_))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Max Weekly Max greater than 30 (days)")+
  ggtitle("Arroyo Chub")+
  theme(text = element_text(size=20))
ggsave("chub_Max_Wkl_Max_StreamT_grt_30__jitter.png", plot = last_plot(), width = 10, height = 10, dpi = 300)


#chub mdl

chub_mdl <- glm(occurrence ~ Max_Wkly_Mean_StreamT, family = "binomial", data = chub)
summary(chub_mdl)

chub_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT_grt_30_, family = "binomial", data = chub)
summary(chub_mdl)

chub_mdl <- glm(occurrence ~ Min_Wkl_Min_StreamT, family = "binomial", data = chub)
summary(chub_mdl)

save(chub_mdl, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/chub_mdl.rda")

logodd_chub <- predict(chub_mdl, newdata = baseline_stream_temp)# log odds
probs_chub <- exp(logodd_chub)/(1+exp(logodd_chub))             # probability
head(probs_chub)
# 
# set.seed(3193)
# N<-length(probs_chub)
# uni<-runif(N,0,1)
# fake_dat_chub<-(uni<probs_chub)*1
# head(fake_dat_chub)

mydt<- data.frame("values" = baseline_stream_temp$Min_Wkl_Min_StreamT, "prob"= probs_chub)
ggplot(mydt, aes(x=baseline_stream_temp$Min_Wkl_Min_StreamT, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Min 7-day Min")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(5,20)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(5,20), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("chub_log_reg.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

#make figure to save for manuscritp
ggsave("C:/Users/JennyT/Documents/Prospectus/Gorcuttii_temp.png", plot = last_plot(), dpi = 300, width = 7, height = 5)

#sucker
suc <- species %>% 
  filter(name == "santa ana sucker")

ggplot(suc, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly maximum stream temp (C)")+
  ggtitle("Santa Ana Sucker")+
  theme(text = element_text(size=20))
ggsave("sucker_weekly_max.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(suc, aes(x = as.factor(occurrence), y = Max_Wkly_Mean_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly mean stream temp (C)")+
  ggtitle("Santa Ana Sucker")+
  theme(text = element_text(size=20))
ggsave("sucker_weekly_mean.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(suc, aes(x = as.factor(occurrence), y = Min_Wkl_Min_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Minimum weekly minimum stream temp (C)")+
  ggtitle("Santa Ana Sucker")+
  theme(text = element_text(size=20))
ggsave("sucker_Min_Wkl_Min_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(suc, aes(x = as.factor(occurrence), y = Max_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly range (C)")+
  ggtitle("Santa Ana Sucker")+
  theme(text = element_text(size=20))
ggsave("sucker_Max_Wkl_Rng_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(suc, aes(x = as.factor(occurrence), y = Mean_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Mean weekly range (C)")+
  ggtitle("Santa Ana Sucker")+
  theme(text = element_text(size=20))
ggsave("sucker_Mean_Wkl_Rng_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(suc, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT_grt_30_))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Max Weekly max greater than 30 (days)")+
  ggtitle("Santa Ana Sucker")+
  theme(text = element_text(size=20))
ggsave("sucker_Max_Wkl_Max_StreamT_grt_30_.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

#suc model

suc_mdl <- glm(occurrence ~ Max_Wkly_Mean_StreamT, family = "binomial",data = suc)
summary(suc_mdl)
# suc_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT, family = "binomial",data = suc)
# summary(suc_mdl)
# suc_mdl <- glm(occurrence ~ Min_Wkl_Min_StreamT, family = "binomial",data = suc)
# summary(suc_mdl)
# suc_mdl <- glm(occurrence ~ Max_Wkl_Rng_StreamT, family = "binomial",data = suc)
# summary(suc_mdl)
# suc_mdl <- glm(occurrence ~ Mean_Wkl_Rng_StreamT, family = "binomial",data = suc)
# summary(suc_mdl)
# suc_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT_grt_30_, family = "binomial",data = suc)
# summary(suc_mdl)



save(suc_mdl, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/suc_mdl.rda")

logodd_suc <- predict(suc_mdl, newdata = baseline_stream_temp)# log odds
probs_suc <- exp(logodd_suc)/(1+exp(logodd_suc))              # probability
head(probs_suc)

# set.seed(3193)
# N<-length(probs_suc)
# uni<-runif(N,0,1)
# fake_dat_suc<-(uni<probs_suc)*1
# head(fake_dat_suc)

mydt<- data.frame("values" = baseline_stream_temp$Max_Wkly_Mean_StreamT, "prob"= probs_suc)
ggplot(mydt, aes(x=baseline_stream_temp$Max_Wkly_Mean_StreamT, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Max 7-day Mean")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(10,50)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(10,50), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("suc_log_reg.png", plot = last_plot(), width = 10, height = 10, dpi = 300)
#make figure to save for manuscritp
ggsave("C:/Users/JennyT/Documents/Prospectus/Csantaanne_temp.png", plot = last_plot(), dpi = 300, width = 7, height = 5)


#toad
toad <- species %>% 
  filter(name == "arroyo toad")

ggplot(toad, aes(x = as.factor(occurrence), y = Max_Wkly_Mean_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly mean stream temp (C)")+
  ggtitle("Arroyo Toad")+
  theme(text = element_text(size=20))
ggsave("toad_weekly_mean.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(toad, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT))+
  geom_boxplot()+
  labs(x="occurrence")+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(y="Maximum weekly maximum stream temp (C)")+
  ggtitle("Arroyo Toad")+
  theme(text = element_text(size=20))
ggsave("toad_weekly_max.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(toad, aes(x = as.factor(occurrence), y = Min_Wkl_Min_StreamT))+
  geom_boxplot()+
  labs(x="occurrence")+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(y="Minimum weekly minimum stream temp (C)")+
  ggtitle("Arroyo Toad")+
  theme(text = element_text(size=20))
ggsave("toad_Min_Wkl_Min_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(toad, aes(x = as.factor(occurrence), y = Max_Wkl_Rng_StreamT))+
  geom_boxplot()+
  labs(x="occurrence")+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(y="Max weekly range (C)")+
  ggtitle("Arroyo Toad")+
  theme(text = element_text(size=20))
ggsave("toad_Max_Wkl_Rng_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(toad, aes(x = as.factor(occurrence), y = Mean_Wkl_Rng_StreamT))+
  geom_boxplot()+
  labs(x="occurrence")+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(y="Mean weekly range (C)")+
  ggtitle("Arroyo Toad")+
  theme(text = element_text(size=20))
ggsave("toad_Mean_Wkl_Rng_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(toad, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT_grt_30_))+
  geom_boxplot()+
  labs(x="occurrence")+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(y="Max weekly max greater than 30 (days)")+
  ggtitle("Arroyo Toad")+
  theme(text = element_text(size=20))
ggsave("toad_Max_Wkl_Max_StreamT_grt_30_.png", plot = last_plot(), width = 10, height = 10, dpi = 300)


#toad model

toad_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT, family = "binomial", data = toad)
summary(toad_mdl)
# toad_mdl <- glm(occurrence ~ Max_Wkly_Mean_StreamT, family = "binomial", data = toad)
# summary(toad_mdl)
# toad_mdl <- glm(occurrence ~ Min_Wkl_Min_StreamT, family = "binomial", data = toad)
# summary(toad_mdl)
# toad_mdl <- glm(occurrence ~ Max_Wkl_Rng_StreamT, family = "binomial", data = toad)
# summary(toad_mdl)
# toad_mdl <- glm(occurrence ~ Mean_Wkl_Rng_StreamT, family = "binomial", data = toad)
# summary(toad_mdl)
# toad_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT_grt_30_, family = "binomial", data = toad)
# summary(toad_mdl)

save(toad_mdl, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/toad_mdl.rda")


logodd_toad <- predict(toad_mdl, newdata = baseline_stream_temp) # log odds
probs_toad <- exp(logodd_toad)/(1+exp(logodd_toad))              # probability
head(probs_toad)

# set.seed(3193)
# N<-length(probs_toad)
# uni<-runif(N,0,1)
# fake_dat_toad<-(uni<probs_toad)*1
# head(fake_dat_toad)

mydt<- data.frame("values" = baseline_stream_temp$Max_Wkl_Max_StreamT, "prob"= probs_toad)
ggplot(mydt, aes(x=baseline_stream_temp$Max_Wkl_Max_StreamT, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Max 7-day Max")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(10,50)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(10,50), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("toad_log_reg.png", plot = last_plot(), width = 10, height = 10, dpi = 300)
#make figure to save for manuscritp
ggsave("C:/Users/JennyT/Documents/Prospectus/Acalifornicus_temp.png", plot = last_plot(), dpi = 300, width = 7, height = 5)


#vireo
vireo <- species %>% 
  filter(name == "least bell's vireo")

ggplot(vireo, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly maximum stream temp (C)")+
  ggtitle("Least Bell's Vireo")+
  theme(text = element_text(size=20))
ggsave("vireo_weekly_max.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(vireo, aes(x = as.factor(occurrence), y = Max_Wkly_Mean_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly mean stream temp (C)")+
  ggtitle("Least Bell's Vireo")+
  theme(text = element_text(size=20))
ggsave("vireo_weekly_mean.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(vireo, aes(x = as.factor(occurrence), y = Min_Wkl_Min_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Min weekly min stream temp (C)")+
  ggtitle("Least Bell's Vireo")+
  theme(text = element_text(size=20))
ggsave("vireo_Min_Wkl_Min_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(vireo, aes(x = as.factor(occurrence), y = Max_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Max weekly range (C)")+
  ggtitle("Least Bell's Vireo")+
  theme(text = element_text(size=20))
ggsave("vireo_Max_Wkl_Rng_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(vireo, aes(x = as.factor(occurrence), y = Mean_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Mean weekly range (C)")+
  ggtitle("Least Bell's Vireo")+
  theme(text = element_text(size=20))
ggsave("vireo_Mean_Wkl_Rng_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(vireo, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT_grt_30_))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Max weekly max greater than 30 (days)")+
  ggtitle("Least Bell's Vireo")+
  theme(text = element_text(size=20))
ggsave("vireo_Max_Wkl_Max_StreamT_grt_30_.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

#vireo model

# vireo_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT, family = "binomial", data = vireo)
# summary(vireo_mdl)
# vireo_mdl <- glm(occurrence ~ Max_Wkly_Mean_StreamT, family = "binomial", data = vireo)
# summary(vireo_mdl)
# vireo_mdl <- glm(occurrence ~ Min_Wkl_Min_StreamT, family = "binomial", data = vireo)
# summary(vireo_mdl)
# vireo_mdl <- glm(occurrence ~ Max_Wkl_Rng_StreamT, family = "binomial", data = vireo)
# summary(vireo_mdl)
# vireo_mdl <- glm(occurrence ~ Mean_Wkl_Rng_StreamT, family = "binomial", data = vireo)
# summary(vireo_mdl)
# vireo_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT_grt_30_, family = "binomial", data = vireo)
# summary(vireo_mdl)
#final:
vireo_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT + Min_Wkl_Min_StreamT, family = "binomial", data = vireo)
summary(vireo_mdl)

save(vireo_mdl, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/vireo_mdl.rda")

logodd_vir <- predict(vireo_mdl, newdata = baseline_stream_temp)  # log odds
probs_vir <- exp(logodd_vir)/(1+exp(logodd_vir))                  # probability
head(probs_vir)

# set.seed(3193)
# N<-length(probs_vir)
# uni<-runif(N,0,1)
# fake_dat_vir<-(uni<probs_vir)*1
# head(fake_dat_vir)

mydt<- data.frame("values" = baseline_stream_temp$Max_Wkl_Max_StreamT, "prob"= probs_vir)
plot1 <- ggplot(mydt, aes(x=baseline_stream_temp$Max_Wkl_Max_StreamT, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Max 7-day Max")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(10,50)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(10,50), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("vireo_log_reg_max_wkl_max.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

mydt<- data.frame("values" = baseline_stream_temp$Min_Wkl_Min_StreamT, "prob"= probs_vir)
plot2 <- ggplot(mydt, aes(x=baseline_stream_temp$Min_Wkl_Min_StreamT, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Min 7-day Min")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(5,20)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(5,20), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("vireo_log_Min_Wkl_Min_reg.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

#making plot for manuscript
library(gridExtra)
margin = theme(plot.margin = unit(c(20,20,20,20), "mm"))
g<-arrangeGrob(plot1 + margin, plot2+ margin, nrow = 1, top = "V. bellii pusillus")
ggsave("C:/Users/JennyT/Documents/Prospectus/Vbelliipusillus_temp.png", plot = g, dpi = 300, width = 14, height = 5)


# pond turtle
turtle <- species %>% 
  filter(name == "southwestern pond turtle")

ggplot(turtle, aes(x = as.factor(occurrence), y = Max_Wkly_Mean_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly mean stream temp (C)")+
  ggtitle("Southwestern Pond Turtle")+
  theme(text = element_text(size=20))
ggsave("turtle_weekly_mean.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(turtle, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Maximum weekly maximum stream temp (C)")+
  ggtitle("Southwestern Pond Turtle")+
  theme(text = element_text(size=20))
ggsave("turtle_weekly_max.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(turtle, aes(x = as.factor(occurrence), y = Min_Wkl_Min_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Min weekly min stream temp (C)")+
  ggtitle("Southwestern Pond Turtle")+
  theme(text = element_text(size=20))
ggsave("turtle_Min_Wkl_Min_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(turtle, aes(x = as.factor(occurrence), y = Max_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Max weekly range (C)")+
  ggtitle("Southwestern Pond Turtle")+
  theme(text = element_text(size=20))
ggsave("turtle_Max_Wkl_Rng_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(turtle, aes(x = as.factor(occurrence), y = Mean_Wkl_Rng_StreamT))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Mean weekly range (C)")+
  ggtitle("Southwestern Pond Turtle")+
  theme(text = element_text(size=20))
ggsave("turtle_Mean_Wkl_Rng_StreamT.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

ggplot(turtle, aes(x = as.factor(occurrence), y = Max_Wkl_Max_StreamT_grt_30_))+
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "tomato")+
  labs(x="occurrence")+
  labs(y="Max weekly max greater than 30 (C)")+
  ggtitle("Southwestern Pond Turtle")+
  theme(text = element_text(size=20))
ggsave("turtle_Max_Wkl_Max_StreamT_grt_30_.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

#turtle model

# turtle_mdl <- glm(occurrence ~ Max_Wkly_Mean_StreamT, family = "binomial", data = turtle)
# summary(turtle_mdl)
# turtle_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT, family = "binomial", data = turtle)
# summary(turtle_mdl)
# turtle_mdl <- glm(occurrence ~ Min_Wkl_Min_StreamT, family = "binomial", data = turtle)
# summary(turtle_mdl)
# turtle_mdl <- glm(occurrence ~ Max_Wkl_Rng_StreamT, family = "binomial", data = turtle)
# summary(turtle_mdl)
# turtle_mdl <- glm(occurrence ~ Mean_Wkl_Rng_StreamT, family = "binomial", data = turtle)
# summary(turtle_mdl)
# turtle_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT_grt_30_, family = "binomial", data = turtle)
# summary(turtle_mdl)
#final model
turtle_mdl <- glm(occurrence ~ Max_Wkl_Max_StreamT_grt_30_+Min_Wkl_Min_StreamT, family = "binomial", data = turtle)
summary(turtle_mdl)
#confint(turtle_mdl, c("Max_Wkl_Max_StreamT_grt_30_", "Min_Wkl_Min_StreamT"), level = 0.95)

save(turtle_mdl, file = "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/turtle_mdl.rda")

logodd_turt <- predict(turtle_mdl, newdata = baseline_stream_temp)  # log odds
probs_turt <- exp(logodd_turt)/(1+exp(logodd_turt))                 # probability
head(probs_turt)

# set.seed(3193)
# N<-length(probs_turt)
# uni<-runif(N,0,1)
# fake_dat_turt<-(uni<probs_turt)*1
# head(fake_dat_turt)

mydt<- data.frame("values" = baseline_stream_temp$Max_Wkl_Max_StreamT_grt_30_, "prob"= probs_turt)
plot1 <- ggplot(mydt, aes(x=baseline_stream_temp$Max_Wkl_Max_StreamT_grt_30_, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Max 7-day max > 30C")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(0, 100)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(0, 100), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("turtle_log_reg_maxGreat30.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

mydt<- data.frame("values" = baseline_stream_temp$Min_Wkl_Min_StreamT, "prob"= probs_turt)
plot2 <- ggplot(mydt, aes(x=baseline_stream_temp$Min_Wkl_Min_StreamT, y=prob))+
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se=FALSE, fullrange=TRUE)+
  xlab("Min 7-day Min")+
  ylab("Probability of Occurrence") +
  scale_x_continuous(expand=c(0,0), limits=c(0,20)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  coord_cartesian(xlim=c(0,20), ylim=c(0,1))+
  theme(text = element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
ggsave("turtle_log_reg_Min_Wkl_Min.png", plot = last_plot(), width = 10, height = 10, dpi = 300)

#making plot for manuscript
library(gridExtra)
margin = theme(plot.margin = unit(c(20,20,20,20), "mm"))
g<-arrangeGrob(plot1 + margin, plot2+ margin, nrow = 1, top = "A. marmorata")
ggsave("C:/Users/JennyT/Documents/Prospectus/Amarmorata_temp.png", plot = g, dpi = 300, width = 14, height = 5)




#mapping baseline predictions
NHD <- st_read("C:/Users/JennyT/Documents/LitReview/RB4/WorkingData_3-16-18/NHDFlowline_Clip_NAD1983_UTMzone11.shp") %>% 
  dplyr::select(COMID) 

baseline_stream_temp_predictions <- baseline_stream_temp 
baseline_stream_temp_predictions$sucker <- probs_suc
baseline_stream_temp_predictions <- baseline_stream_temp_predictions %>% 
  filter(year %in% c(1993, 2010, 2014)) 
baseline_stm_tmp_pred_sucker <- merge(NHD, baseline_stream_temp_predictions, by = "COMID")
st_write(baseline_stm_tmp_pred_sucker, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/sucker.shp")


baseline_stream_temp_predictions <- baseline_stream_temp 
baseline_stream_temp_predictions$chub <- probs_chub
baseline_stream_temp_predictions <- baseline_stream_temp_predictions %>% 
  filter(year %in% c(1993, 2010, 2014)) 
baseline_stm_tmp_pred_chub <- merge(NHD, baseline_stream_temp_predictions, by = "COMID")
st_write(baseline_stm_tmp_pred_chub, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/chub.shp")


baseline_stream_temp_predictions <- baseline_stream_temp 
baseline_stream_temp_predictions$trout <- probs_trout
baseline_stream_temp_predictions <- baseline_stream_temp_predictions %>% 
  filter(year %in% c(1993, 2010, 2014)) 
baseline_stm_tmp_pred_trout <- merge(NHD, baseline_stream_temp_predictions, by = "COMID")
st_write(baseline_stm_tmp_pred_trout, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/trout.shp")


baseline_stream_temp_predictions <- baseline_stream_temp 
baseline_stream_temp_predictions$turtle <- probs_turt
baseline_stream_temp_predictions <- baseline_stream_temp_predictions %>% 
  filter(year %in% c(1993, 2010, 2014))
baseline_stm_tmp_pred_turtle <- merge(NHD, baseline_stream_temp_predictions, by = "COMID")
st_write(baseline_stm_tmp_pred_turtle, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/turtle.shp")


baseline_stream_temp_predictions <- baseline_stream_temp 
baseline_stream_temp_predictions$toad <- probs_toad
baseline_stream_temp_predictions <- baseline_stream_temp_predictions %>% 
  filter(year %in% c(1993, 2010, 2014))
baseline_stm_tmp_pred_toad <- merge(NHD, baseline_stream_temp_predictions, by = "COMID")
st_write(baseline_stm_tmp_pred_toad, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/toad.shp")


baseline_stream_temp_predictions <- baseline_stream_temp 
baseline_stream_temp_predictions$vireo <- probs_vir
baseline_stream_temp_predictions <- baseline_stream_temp_predictions %>% 
  filter(year %in% c(1993, 2010, 2014))
baseline_stm_tmp_pred_vireo <- merge(NHD, baseline_stream_temp_predictions, by = "COMID")
st_write(baseline_stm_tmp_pred_vireo, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/prob maps/vireo.shp")


baseline_stream_temp_species_predictions<- baseline_stream_temp
baseline_stream_temp_species_predictions$vireo <- probs_vir
baseline_stream_temp_species_predictions$trout <- probs_trout
baseline_stream_temp_species_predictions$chub <- probs_chub
baseline_stream_temp_species_predictions$toad <- probs_toad
baseline_stream_temp_species_predictions$sucker <- probs_suc
baseline_stream_temp_species_predictions$turtle <- probs_turt
save(baseline_stream_temp_species_predictions, file = 'L:/Flow ecology and climate change_ES/Jenny/AirTemp/Modeling/baseline_stream_temp_species_predictions.RData')
