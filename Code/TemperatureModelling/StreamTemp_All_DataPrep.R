#prepping stream temperature data - want stream temp, COMID, and date to join with air temp data

library(tidyr)
library(lubridate)
library(dplyr)
library(sf)



###### LA/Orange County Hobo Files #####



Lockwood_1415<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/10405314_Lockwood_wet_1415.csv", skip = 1, col.names = c("ID", "Date", "Temp_C")) %>% 
  dplyr::select(-1)
Lockwood_1415 <- Lockwood_1415[-c(1:12),]# remove first 12 rows due to errors in HOBO sensor
Lockwood_1516<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/10405314_Lockwood_wet_1516.csv", skip = 1, col.names = c("ID", "Date", "Temp_C")) %>% 
  dplyr::select(-1)
Lockwood_1718<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/10766912_Lockwood_wet_1718.csv", skip = 1, col.names = c("ID", "Date", "Temp_C")) %>% 
  dplyr::select(-1)
Lockwood_1718 <- Lockwood_1718[-c(1:4),]# remove first 4 rows due to errors in HOBO sensor

Lockwood<- rbind(Lockwood_1415, Lockwood_1516, Lockwood_1718)
Lockwood$site<-"lockwood"
Lockwood$COMID <- 17567207

Bear_1415<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/2316005_Bear_wet_1415.csv", skip = 1, col.names = c("ID", "Date", "Temp_C")) %>% 
  dplyr::select(-1)
Bear_1415 <- Bear_1415[-c(1:19),]# remove first 19 rows due to errors in HOBO sensor
Bear_1516<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/2316005_Bear_wet_1516.csv", skip = 1, col.names = c("ID", "Date", "Temp_C")) %>% 
  dplyr::select(-1)
Bear_1617<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/2316005_Bear_wet_1617.csv", skip = 1, col.names = c("ID", "Date", "Temp_C")) %>% 
  dplyr::select(-1)
Bear_1718<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/2316005_Bear_wet_1718.csv", skip = 1, col.names = c("ID", "Date", "Temp_C")) %>% 
  dplyr::select(-1)

bear<- rbind(Bear_1415, Bear_1516,  Bear_1617, Bear_1718)
bear$site<- "bear"
bear$COMID <- 22524629

SantaAna<- read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/10672118_SantaAna_wet_1518.csv", skip = 1, col.names = c("ID", "Date", "Temp_C")) %>% 
  dplyr::select(-1)
SantaAna <- SantaAna[-c(1:28),]# remove first 28 rows due to errors in HOBO sensor
SantaAna$site<- "santaana"
SantaAna$COMID <- 22559698

Hobo<-rbind(Lockwood, bear, SantaAna)

#get year, month, day columns
Hobo <- Hobo %>% 
  separate( col = Date, into = c("date", "time", "am/pm"), sep = " ") %>% 
  dplyr::select(date, Temp_C, COMID)
Hobo$date<-mdy(Hobo$date)
Hobo$year<-year(Hobo$date)
Hobo$month<- month(Hobo$date)
Hobo$day<- day(Hobo$date)

#group_by site, year, month, day, and take the max, min, and mean value 
metrics<- Hobo %>% 
  group_by(COMID, year, month, day) %>% 
  summarise(Max_Tw_C = max(Temp_C), Min_Tw_C = min(Temp_C), Mean_Tw_C = mean(Temp_C), Med_Tw_C = median(Temp_C))
metrics$date<- ymd(paste(metrics$year, metrics$month, metrics$day, sep = "-"))
#metrics <- metrics %>% 
#  select(COMID, date, Max_Tw_C, Min_Tw_C, Mean_Tw_C, Med_Tw_C)


##### temp_timeseriestemp data from Rafi ######



tmseries<- st_read("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/temp_timeseriestemp_NHD_JOIN_GCS1983.shp") 
tmseries<- data.frame(tmseries) %>% 
  dplyr::select(Latitude, Longitude, COMID, SampleDate,  Result)
names(tmseries)[5] <- "Temp_C"
tmseries$year <- year(tmseries$SampleDate)
tmseries$month <- month(tmseries$SampleDate)
tmseries$day <- day(tmseries$SampleDate)


metrics2<- tmseries %>% 
  group_by(COMID, year, month, day) %>% 
  summarise(Max_Tw_C = max(Temp_C), Min_Tw_C = min(Temp_C), Mean_Tw_C = mean(Temp_C), Med_Tw_C = median(Temp_C))
metrics2$date<- ymd(paste(metrics2$year, metrics2$month, metrics2$day, sep = "-"))


###### USGS Stream Temp Files ######



usgs1<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Temp_COMID_17573647.csv") %>% 
  dplyr::select(X,date ,Max_Tw_C,SiteNum) #these col names are incorret bc excel reformatted need to change back
names(usgs1)[1:4]<- c("date",  "Max_Tw_C",  "Min_Tw_C", "COMID")
usgs1$date<- ymd(usgs1$date)

usgs2<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Temp_COMID_17574397.csv") %>% 
  dplyr::select(X,date ,Max_Tw_C,Min_Tw_C, SiteNum) #these col names are incorret bc excel reformatted need to change back
names(usgs2)[1:5]<- c("date",  "Max_Tw_C",  "Min_Tw_C", "Ins_Tw_C", "COMID")
usgs2$date<- ymd(usgs2$date)

usgs3<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Temp_COMID_17575785.csv") %>% 
  dplyr::select(X, date , SiteNum) #these col names are incorret bc excel reformatted need to change back
names(usgs3)[1:3]<- c("date",  "Ins_Tw_C", "COMID")
usgs3$date<- ymd(usgs3$date)

usgs4<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Temp_COMID_20350681.csv") %>% 
  dplyr::select(X, date , SiteNum) #these col names are incorret bc excel reformatted need to change back
names(usgs4)[1:3]<- c("date",  "Ins_Tw_C", "COMID")
usgs4$date<- ymd(usgs4$date)

usgs5<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Temp_COMID_22563116.csv") %>% 
  dplyr::select(X,date ,Max_Tw_C,Min_Tw_C, SiteNum) #these col names are incorret bc excel reformatted need to change back
names(usgs5)[1:5]<- c("date",  "Max_Tw_C",  "Min_Tw_C", "Med_Tw_C", "COMID")
usgs5$date<- ymd(usgs5$date)

usgs6<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Temp_COMID_22658309.csv") %>% 
  dplyr::select(X,date, Mean_Tw_C ,Max_Tw_C,Min_Tw_C, SiteNum) #these col names are incorret bc excel reformatted need to change back
names(usgs6)[1:6]<- c("date", "Mean_Tw_C", "Max_Tw_C",  "Min_Tw_C", "Med_Tw_C", "COMID")
usgs6$date<- ymd(usgs6$date)

usgs7<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Temp_COMID_22660257.csv") %>% 
  dplyr::select(X,date ,Max_Tw_C,Min_Tw_C, SiteNum) #these col names are incorret bc excel reformatted need to change back
names(usgs7)[1:5]<- c("date",  "Max_Tw_C",  "Min_Tw_C", "Med_Tw_C", "COMID")
usgs7$date<- ymd(usgs7$date)

usgs8<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Temp_COMID_22684930.csv") %>% 
  dplyr::select(X,date ,Max_Tw_C,Min_Tw_C, SiteNum) #these col names are incorret bc excel reformatted need to change back
names(usgs8)[1:5]<- c("date",  "Max_Tw_C",  "Min_Tw_C", "Mean_Tw_C", "COMID")
usgs8$date<- ymd(usgs8$date)

usgs9<-read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/Stream_Temp_COMID_948070372.csv") %>% 
  dplyr::select(X, date, SiteNum) #these col names are incorret bc excel reformatted need to change back
names(usgs9)[1:3]<- c("date", "Ins_Tw_C", "COMID")
usgs9$date<- ymd(usgs9$date)

usgs_all<-bind_rows(usgs1, usgs2, usgs3, usgs4, usgs5, usgs6, usgs7, usgs8, usgs9) %>% 
  dplyr::select(date, COMID, Max_Tw_C,  Min_Tw_C,  Ins_Tw_C,  Med_Tw_C, Mean_Tw_C)


######  Final Stream Temp Dataset Join ######

#write csv
StreamTemp<- bind_rows(metrics, metrics2, usgs_all)
write.csv(StreamTemp, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/StreamTemp_ALL.csv")

#read csv
StreamTemp <- read.csv("L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/StreamTemp_ALL.csv") %>% 
  dplyr::select(-X, - year, -month, -day)
StreamTemp$date<- ymd(StreamTemp$date)



#select COMIDs and Dates that are consistent with the RB4 project: 1981, Oct 1-2014, Sept 30
NHD<- st_read("C:/Users/JennyT/Documents/LitReview/RB4/WorkingData_3-16-18/NHDFLowline_Clip.shp") %>% 
  dplyr::select(COMID)

#RB4 subset based on correct comids and correct date range
RB4<- StreamTemp[StreamTemp$COMID %in% NHD$COMID & StreamTemp$date >= ymd(19811001) & StreamTemp$date <= ymd(20140930),]
write.csv(RB4, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/StreamTem_RB4.csv")

#Livneh subset based on those not include in RB4, and those that are before dec 31 2013 
Livneh<- anti_join(StreamTemp, RB4, by = c("COMID", "date"))
Livneh <- Livneh %>% 
  filter(date <= ymd(20131231))
write.csv(Livneh, "L:/Flow ecology and climate change_ES/Jenny/AirTemp/StreamTempObservation/StreamTem_Livneh.csv")

