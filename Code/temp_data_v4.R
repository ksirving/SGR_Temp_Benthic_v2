# Load packages
library(tidyverse)
# library(dplyr)
library(readxl)
library(lubridate)
library(zoo)
library(tidyr)
library(stringr)
library(tidylog)




setwd("input_data/Temperature/temp")

temp.files <- list.files(pattern = ".xlsx") ## list all files
# View(temp.files) #name of all files folder
temp.files[c]

temp_datax <- NULL ## create empty dataframe to cumulate into

c
c=11
for(c in 1:length(temp.files)) { ## will loop through list of files
  
  temp_data <- read_excel(paste(temp.files[c])) ##upload each file one by one
  # head(temp_data)
  names(temp_data)[2:3] <- c("DateTime", "Temperature")
  # names(temp_data)
  
  ## format date and add filename
  temp_data <- temp_data %>%
    separate(DateTime, into  = c('Date', 'Time'), sep = ' ', remove = F) %>% # Split Date-Time (PST) column into date and time
    separate(Date, into = c("Year", "Month", "Day"), sep = '-', remove = T)# %>% ## split the date column into date, year, month, day
    # rename(Temperature = c(`Ch: 1 - Temperature   (°C)` | `Ch: 1 - Temperature   (Â°C)`)) %>%
    

  temp_data <- temp_data[1:7] ## remove columns from weird dfs
  
  temp_data <- temp_data %>%
    mutate(FileName = temp.files[c]) ## add filename

  temp_datax <- bind_rows(temp_datax, temp_data)  ## combine all - cumulative dataframe

  # View(temp_data)

  
}

save(temp_datax, file =  "T1_temp_data_LAR.Rdata")

head(temp_datax)



# Replicates ------------------------------------------------------------

load( file =  "T1_temp_data_LAR.Rdata")

dim(temp_datax)

## check replicates
temp_dat_red <- temp_datax %>%
  select(-FileName) %>%
  distinct()

### some data replicated in the different files - change filename and remove duplicates

unique(temp_datax$FileName)

str_split(unique(temp_datax$FileName), pattern = " ")

temp_names <- temp_datax %>%
  separate(FileName, into = c("SiteID", "Date_Name"), sep = c(" ", "_")) %>%
  separate(SiteID, into = c("SiteID", "test"), sep = c("_", " ")) %>%
  select(-test, -Date_Name) %>%
  mutate(SiteName = case_when(SiteID == 21150073 ~ "Benedict", 
                              SiteID == 21150064 ~ "Willow",
                              SiteID == 21150065 ~ "Riverfront", 
                              SiteID == 21150068 ~ "Burbank",
                              SiteID == 21150070 ~ "Compton", 
                              SiteID == 21150074 ~ "Steelhead",
                              SiteID == 21150075 ~ "Compton", ## 2 x compton creek - same site, logger was lost
                              SiteID == 21150077 ~ "Rattlesnake")) 
         


temp_names <- temp_names %>% distinct()
dim(temp_names)

unique(temp_names$SiteID)

head(temp_names)


# data info ---------------------------------------------------------------

range(temp_names$DateTime)

ranges <- temp_names %>%
  group_by(SiteName) %>%
  na.omit() %>%
  summarise(mintemp = min(na.omit(Temperature)), maxtemp = max(na.omit(Temperature)), meantemp = mean(na.omit(Temperature)), ### summary stats
           StartDate = range(DateTime)[1], EndDate = range(DateTime)[2]) ## overall date range
  
## difference in time intervals, continuity and data gaps

temp_names <- temp_names[order(temp_names$DateTime),]


time_diffs <- temp_names %>%
  na.omit() %>%
  group_by(SiteName) %>%
  # order(DateTime) %>%
  mutate(LagDates = c(DateTime[-1], NA)) %>%## lag dates to get continuity
  mutate(DiffMins = difftime(DateTime, LagDates, units = "mins"), ## difference between dates mins
         DiffDays = difftime(DateTime, LagDates, units = "days")) ## difference between dates days


time_diffs_wide <- time_diffs %>%
  pivot_wider(names_from = SiteName, values_from =  LagDates)

head(time_diffs_wide)

write.csv(time_diffs_wide, "Temp_data_dates_wide.csv")

## 30 min interval

# Metrics -----------------------------------------------------------------

## look in SMR repo
## frist calculate daily means etc
## then do metrics

head(temp_names)

sum(is.na(temp_names))

df <- temp_names %>%
  group_by(SiteName, Year, Month, Day) %>%
  summarise(MeanTemp = mean(Temperature),
            MinTemp = min(Temperature),
            MaxTemp = max(Temperature))

head(df)

df <- df %>% 
  ungroup() %>%
  group_by(SiteName) %>%
  mutate(max_07da = zoo::rollmean(MaxTemp, k=7, fill = NA)) %>% ## rolling daily max
  mutate(mn_07da = zoo::rollmean(MeanTemp, k=7, fill = NA)) %>% ## rolling daily max
  mutate(DTR = MaxTemp - MinTemp) ## diurnal temp rate

df_sums <- df %>%
  mutate(SiteName = factor(SiteName, levels = c("Burbank", "Rattlesnake", "Benedict", "Steelhead", "Riverfront",
                                                "Compton", "Willow"))) %>%
  pivot_longer(MeanTemp:DTR, names_to = "Metric", values_to= "Values")%>%
  group_by(SiteName, Metric) %>%
  summarise(meanvals = mean(na.omit(Values))) %>%
  pivot_wider(names_from = Metric, values_from = meanvals)
  
df_sums

write.csv(df_sums, "means_per_sites.csv")



# Air temperature ---------------------------------------------------------

## uplosd air temp from NOAA - LA downtown 
airtemp <- read.csv("3028942.csv")

head(airtemp)

## make full time series 
dates <-  seq(from = as.Date("2021-08-20"), to = as.Date("2022-05-31"), by = 1)
dates

airtempx <- airtemp %>%
  ## format date 
  separate(DATE, into = c("Month", "Day", "Year"), sep = '/', remove = F)  %>% ## split the date column into date, year, month, day
  mutate(Year = paste0(20, Year)) %>%
  unite(col = "Date", c(Year, Month, Day), sep = "-") %>%
  mutate(DATE = as.Date(Date)) %>%
  filter(DATE %in% dates) %>% ## filter to water tep dates
  mutate( Metric = "Max Air Temp") %>% ## get mean from min max
  select(DATE, TMAX) %>% rename( Date = DATE, AirTemp = TMAX) %>%
  mutate(AirTemp = (AirTemp -32)*5/9) #%>% ## convert to celcius
  # group_by( Year, Month, Day) #%>%
  # summarise(MeanTemp = mean(AirTemp),
  #           MinTemp = min(AirTemp),
  #           MaxTemp = max(AirTemp)) %>%
  # ungroup() %>%
  # # group_by(SiteName) %>%
  # mutate(max_07da = zoo::rollmean(MaxTemp, k=7, fill = NA)) %>% ## rolling daily max
  # mutate(mn_07da = zoo::rollmean(MeanTemp, k=7, fill = NA)) %>% ## rolling daily max
  # mutate(DTR = MaxTemp - MinTemp)


  

head(airtempx)
range(airtempx$Date)

ggplot(airtempx, aes(y = AirTemp, x = Date)) +
  geom_line() 


# Figures -----------------------------------------------------------------
getwd()
## directory for figures
out.dir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SGR_Temp_Benthic/Figures/"

library("RColorBrewer")
install.packages("wesanderson")
# Load
library(wesanderson)
## map

## format date and make long

unique(df_date$SiteName)
unique(df_date$Metric)
head(df_date)
range(na.omit(df_date$Date))
names(df_date)


## alot going on here. join dates to airtemp preserving the na values, need the missing values to plot. make sites and metrics factors


df_datex <- df %>%
  unite(col = "Date", c(Year, Month, Day), sep = "-") %>%
  mutate(Date = as.Date(Date)) %>% rename(max07da = max_07da, mn07da = mn_07da) %>%
  pivot_longer(c(MeanTemp:DTR), names_to = "Metric", values_to = "Temp", values_drop_na = F) %>%
  unite("SiteMetric", c(SiteName, Metric), sep ="_", remove = T) %>%
  pivot_wider(names_from =  SiteMetric, values_from = Temp)%>%
  right_join(airtempx, by = "Date", keep = T)%>% 
  rename(Date = Date.y) %>% select(-Date.x) %>%
  pivot_longer(c(Benedict_MeanTemp:Willow_DTR), names_to = "SiteMetric", values_to = "Temp", values_drop_na = F) %>%
  separate(SiteMetric, into = c("SiteName", "Metric"), sep = "_") %>%
  pivot_wider(names_from = Metric, values_from = Temp) %>% 
  pivot_longer(c(AirTemp, MeanTemp:DTR), names_to="Metric", values_to = "Temp") %>%
  mutate(Metric = factor(Metric, levels = c("max07da", "mn07da", "MeanTemp", "MinTemp", "MaxTemp", "DTR", "AirTemp"))) %>%
  mutate(SiteName = factor(SiteName, levels = c("Burbank", "Rattlesnake", "Benedict", "Steelhead", "Riverfront",
                                                "Compton", "Willow")))
head(df_datex)
# test <- df_date %>% filter(SiteName == "Compton")

library(ggthemes)

## plot
T1 <- ggplot(df_datex, aes(y = Temp, x = Date, group = Metric, color = Metric)) +
  # geom_line(aes(y=AirTemp, col = "gray")) +
  geom_line() +
  # geom_line(aes(x=Date, y=AirTemp, color = "Max Daily Air Temp")) +
  facet_wrap(~SiteName) + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Water Temp (°C)") +
  scale_color_brewer(palette= "Set2",
                       labels = c("7 Day Max", "7 Day Mean","Mean Daily Temp", "Min Daily Temp",
                                  "Max Daily Temp", "Diurnal Temp Rate", "Max Daily Air Temp")) +
  scale_x_date(date_labels = "%b %Y") +
  theme_linedraw() 


T1

file.name1 <- paste0(out.dir, "Temp_by_Site.jpg")
ggsave(T1, filename=file.name1, dpi=300, height=5, width=6)


## metric labels
levels(df_datex$Metric)
levels(df_datex$Metric) <- c("7 Day Max", "7 Day Mean","Mean Daily Temp", "Min Daily Temp",
                            "Max Daily Temp", "Diurnal Temp Rate", "AirTemp")

## plot
T2 <- ggplot(subset(df_datex, !Metric == "AirTemp"), aes(y = Temp, x = Date, group = SiteName, color = SiteName)) +
  geom_line() +
  # geom_line(data = airtemp, aes(x=Date, y=Temp)) +
  facet_wrap(~Metric, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(name="Water Temp (°C)") +
  # scale_color_manual(breaks = c("Burbank", "Rattlesnake", "Benedict", "Steelhead", "Riverfront",
  #                               "Compton","Willow"),
  #                 values=c("red", "blue", "green")) 
  scale_color_brewer(palette= 'RdYlBu', direction=-1)+
  theme_dark()  

  # theme_dark()

T2

file.name1 <- paste0(out.dir, "Temp_by_Metric.jpg")
ggsave(T2, filename=file.name1, dpi=300, height=5, width=6)


# Map of sites ------------------------------------------------------------

setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SGR_Temp_Benthic/input_data/Temperature/temp")
getwd()

library(mapview)
library(sf)
library(leaflet)
library(leafem)

sites <- read.csv("Heal the Bay Temperature Logging Coordinates.csv")
sitesWater <- sites %>%
  na.omit() %>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F) %>%
  filter(!Site.Name %in% c("Rio Hondo Confluence", "Compton Creek Confluence"))

# sitesAir <- sites %>%
#   na.omit() %>%
#   st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F) %>%
#   filter(Site.Name %in% c("LA Downtown"))
# 
# str(sitesWater)
# sites
# set background basemaps:
basemapsList <- c( "Esri.WorldTopoMap" , "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap",
                  "CartoDB.Positron", "Stamen.TopOSMFeatures") 

mapviewOptions(basemaps=basemapsList, fgb = FALSE)
# 
# this map of all sites 
m1 <- mapview(sitesWater, col.regions = "orange",
                layer.name="Water Temperature Loggers") + #%>%
 mapview(sitesAir, col.regions = "blue",
          layer.name="Air Temperature Loggers")
  

m1 <- m1 %>% addStaticLabels(label = sitesWater$Site.Name,
                  # noHide = TRUE,
                  direction = 'top',
                  # textOnly = TRUE,
                  textsize = "20px") 

m1

# m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

mapshot(m1, url = paste0(getwd(), "/temp_logger_sites_mapview.html"),
        file = paste0(getwd(), "/temp_logger_sites_mapview.png"))


# Analysis ----------------------------------------------------------------

## divide into concrete/trees - based on google earth

head(df_date_type)

## remove air temp from metric list and add as sep clumn. Make names compitable with col names
newnames <- make.names(names(df_date_type), unique = FALSE, allow_ = TRUE)

df_date_type <- df_datex %>%
  mutate(StreamType = case_when(SiteName %in% c("Burbank", "Benedict", "Steelhead", "Rattlesnake") ~ "Vegetated",
                   SiteName %in% c("Compton", "Riverfront", "Willow") ~ "Concrete")) %>%
  pivot_wider(names_from = Metric, values_from = Temp) 

colnames(df_date_type) <- newnames

df_date_type <- df_date_type %>%
  pivot_longer(Mean.Daily.Temp:Diurnal.Temp.Rate, names_to = "Metric", values_to = "Temp") 

levels(df_date_type$Metric) <- c("7 Day Max", "7 Day Mean","Mean Daily Temp", "Min Daily Temp",
                               "Max Daily Temp", "Diurnal Temp Rate")

head(df_date_type)

## plot
T3 <- ggplot(df_date_type, aes(y = Temp, x = AirTemp, group = StreamType, color = StreamType)) +
  stat_smooth(method = "lm") +
  facet_wrap(~Metric) + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
 

T3

file.name1 <- paste0(out.dir, "Temp_vs_airTemp.jpg")
ggsave(T3, filename=file.name1, dpi=300, height=5, width=6)

T4 <- ggplot(df_date_type, aes(y = Temp, x = AirTemp, group = SiteName, color = SiteName)) +
  stat_smooth(method = "lm") +
  facet_wrap(~Metric) + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette= 'RdYlBu', direction=-1) 



T4

file.name1 <- paste0(out.dir, "Temp_vs_air_per_metric.jpg")
ggsave(T4, filename=file.name1, dpi=300, height=5, width=6)

# mixed model
head(df_date_type)

df_mod <- df_date_type %>%
  filter(Metric == "Max.Daily.Temp")

sites <- unique(df_mod$SiteName)
sites

fitted_data <- NULL

s = 1

for(s in 1:length(sites)) {
  
  ## extract site
  df_modx <- df_mod %>%
    filter(SiteName == sites[s])
  
  ## model
  mod_lm <- lm(Temp ~ AirTemp, data = df_modx)
  mod_sum <- summary(mod_lm)
  
  fitted_datax <- NULL
  
  fitted_datax$WaterTempFit <-  mod_lm$fitted.values ## water temp fit
  fitted_datax$Temp <- mod_lm$model$Temp ## water temp in model
  fitted_datax$AirTemp <- mod_lm$model$AirTemp ## air temp
  fitted_datax$SiteName <- sites[s] ## sites
  fitted_datax$R2 <- mod_sum$r.squared ## r 2
  fitted_datax$PVal <-  mod_sum$coefficients[2,4] ## p value
  fitted_datax$n <- length(mod_lm$model$Temp)
  
  fitted_datax <- as.data.frame(fitted_datax)
  
  fitted_data <- bind_rows(fitted_data, fitted_datax)
  
}

head(fitted_data)

### add streamtype,  get positions for labels
fitted_data <- fitted_data %>%
  mutate(StreamType = case_when(SiteName %in% c("Burbank", "Benedict", "Steelhead", "Rattlesnake") ~ "Vegetated",
                                SiteName %in% c("Compton", "Riverfront", "Willow") ~ "Concrete")) %>%
  group_by(SiteName) %>%
  mutate(Label = ifelse(Temp == max(Temp), paste(SiteName), NA))# %>%## add r2 for labels
  # mutate(Label = ifelse(length(na.omit(Label)) >= 2 , NA, Label))
  
  fitted_data[508, "Label"]  <- NA ## remove one na

names(fitted_data)

## plot
T5a <- ggplot(fitted_data, aes(y = WaterTempFit, x = AirTemp, group = SiteName, color = SiteName)) +
  stat_smooth(method = "lm") +
  facet_wrap(~StreamType) + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette= 'Dark2', direction=-1) +
  scale_y_continuous(name="Fitted Max Daily Water Temp (°C)") +
  scale_x_continuous(name="Max Daily Air Temp (°C)") +
  geom_label_repel(aes(label = Label),
                   nudge_x = .75,
                   na.rm = TRUE) +
  theme(legend.position = "none")
# theme_dark()



T5a

file.name1 <- paste0(out.dir, "Temp_vs_air_per_metric_fitted_site.jpg")
ggsave(T5a, filename=file.name1, dpi=300, height=5, width=6)

coefs <- fitted_data %>%
  mutate(SiteName = factor(SiteName, levels = c("Burbank", "Rattlesnake", "Benedict", "Steelhead", "Riverfront",
                                                "Compton", "Willow"))) %>%
  dplyr::select(SiteName, StreamType, R2, PVal, n) %>%
  distinct()

coefs

write.csv(coefs, "model_coefs_lm.csv")



#### stream type
sites <- unique(df_mod$StreamType)
sites

STfitted_data <- NULL

s = 1

for(s in 1:length(sites)) {
  
  ## extract site
  df_modx <- df_mod %>%
    filter(StreamType == sites[s])
  df_modx
  ## model
  mod_lm <- lm(Temp ~ AirTemp, data = df_modx)
  mod_sum <- summary(mod_lm)

  STfitted_datax <- NULL
  
  mod_sum$coefficients[2,4]

  STfitted_datax$WaterTempFit <-  mod_lm$fitted.values ## water temp fit
  STfitted_datax$Temp <- mod_lm$model$Temp ## water temp in model
  STfitted_datax$AirTemp <- mod_lm$model$AirTemp ## air temp
  STfitted_datax$SiteName <- sites[s] ## sites
  STfitted_datax$R2 <- mod_sum$r.squared ## r 2
  STfitted_datax$PVal <- mod_sum$coefficients[2,4] ## p value
  STfitted_datax$n <- length(mod_lm$model$Temp)
  
  STfitted_datax <- as.data.frame(STfitted_datax)
  
  STfitted_data <- bind_rows(STfitted_data, STfitted_datax)
  
}

head(STfitted_datax)

STfitted_data <- STfitted_data %>%
  # mutate(StreamType = case_when(SiteName %in% c("Burbank", "Benedict", "Steelhead") ~ "Vegetated",
  #                               SiteName %in% c("Rattlesnake", "Compton", "Riverfront", "Willow") ~ "Concrete")) %>%
  group_by(SiteName) %>%
  mutate(Label = ifelse(Temp == max(Temp), paste(SiteName), NA))# %>%## add r2 for labels
# mutate(Label = ifelse(length(na.omit(Label)) >= 2 , NA, Label))

STfitted_data[336, "Label"]  <- NA ## add labesl
# STfitted_data[460, "Label"]  <- "Concrete"

### plot fitted values

library(ggrepel)
# install.packages("ggrepel")

# labels <- fitted_data %>%
#   dplyr::select(SiteName, PVal, R2) %>% distinct()


T5 <- ggplot(STfitted_data, aes(y = WaterTempFit, x = AirTemp, group = SiteName, color = SiteName)) +
  stat_smooth(method = "lm") +
  # facet_wrap(~StreamType) + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette= 'Dark2', direction=-1) +
  scale_y_continuous(name="Fitted Max Daily Water Temp (°C)") +
  scale_x_continuous(name="Max Daily Air Temp (°C)") +
  geom_label_repel(aes(label = Label),
                   nudge_x = .75,
                   na.rm = TRUE) +
  theme(legend.position = "none")
# theme_dark()



T5

file.name1 <- paste0(out.dir, "Temp_vs_air_per_metric_fitted_streamtype.jpg")
ggsave(T5, filename=file.name1, dpi=300, height=5, width=6)

coefs <- STfitted_data %>%
  ungroup() %>%
  # mutate(SiteName = factor(SiteName, levels = c("Burbank", "Rattlesnake", "Benedict", "Steelhead", "Riverfront",
  #                                               "Compton", "Willow"))) %>%
  dplyr::select(SiteName, R2, PVal, n) %>%
  distinct()

coefs

write.csv(coefs, "model_coefs_lm_streamType.csv")




# fitted_data <- fitted_data %>%
#   mutate(StreamType = case_when(SiteName %in% c("Burbank", "Benedict", "Steelhead") ~ "Natural",
#                                 SiteName %in% c("Rattlesnake", "Compton", "Riverfront", "Willow") ~ "Concrete")) %>%
#   group_by(StreamType) %>%
#   mutate(Label = ifelse(Temp == max(Temp), paste(StreamType), NA))# %>%## add r2 for labels
# # mutate(Label = ifelse(length(na.omit(Label)) >= 2 , NA, Label))
# 
# fitted_data[508, "Label"]  <- NA ## remove one na
# 
# 
# T6 <- ggplot(fitted_data, aes(y = WaterTempFit, x = AirTemp, group = StreamType, color = StreamType)) +
#   stat_smooth(method = "lm") +
#   # facet_wrap(~Metric) + ## , scales = "free_x"
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_color_brewer(palette= 'Dark2', direction=-1) +
#   scale_y_continuous(name="Fitted Max Daily Water Temp (°C)") +
#   scale_x_continuous(name="Max Daily Air Temp (°C)") +
#   geom_label_repel(aes(label = Label),
#                    nudge_x = .75,
#                    na.rm = TRUE) +
#   theme(legend.position = "none")
# # theme_dark()
# 
# 
# 
# T6
# 
# file.name1 <- paste0(out.dir, "Temp_vs_air_per_metric_fitted.jpg")
# ggsave(T4, filename=file.name1, dpi=300, height=5, width=6)


## diurnul pattern

head(df_date_type)

T7 <- ggplot(data = subset(df_date_type, Metric == "Diurnal.Temp.Rate") , aes(x = SiteName, y = Temp, fill = StreamType)) +
  geom_boxplot() +
  scale_x_discrete(name = "Site") +
  scale_y_continuous(name = "Diurnal Temp Rate (°C)")

T7

file.name1 <- paste0(out.dir, "Temp_diurnal_rate.jpg")
ggsave(T7, filename=file.name1, dpi=300, height=5, width=6)

T7a <- ggplot(data = subset(df_date_type, Metric == "Diurnal.Temp.Rate") , aes(x = StreamType, y = Temp, fill = StreamType)) +
  geom_boxplot() +
  scale_x_discrete(name = "Site") +
  scale_y_continuous(name = "Diurnal Temp Rate (°C)")

T7a

file.name1 <- paste0(out.dir, "Temp_diurnal_rate_streamType.jpg")
ggsave(T7a, filename=file.name1, dpi=300, height=5, width=6)

## seasonal

df_date_seas <- df_date_type %>%
  separate(Date, into = c("Year", "Month", "Day")) %>%
  mutate(Season = case_when(Month %in% c("06", "07","08") ~ "Summer",
                            Month %in% c("09","10","11") ~ "Fall",
                            Month %in% c("12","01","02") ~ "Winter",
                            Month %in% c("03", "04", "05") ~ "Spring")) %>% na.omit() ## fix NAs

which(is.na(df_date_seas))

df_date_seas[9871:9876,]

T7b <- ggplot(data = subset(df_date_seas, Metric == "Diurnal.Temp.Rate") , aes(x = Season, y = Temp, fill = StreamType)) +
  geom_boxplot() +
  # facet_wrap(~StreamType) +
  scale_x_discrete(name = "Season") +
  scale_y_continuous(name = "Diurnal Temp Rate (°C)")

T7b

file.name1 <- paste0(out.dir, "Temp_diurnal_rate_season_streamtype.jpg")
ggsave(T7b, filename=file.name1, dpi=300, height=5, width=6)

T7c <- ggplot(data = subset(df_date_seas, Metric == "Diurnal.Temp.Rate") , aes(x = Season, y = Temp, fill = Season)) +
  geom_boxplot() +
  # facet_wrap(~StreamType) +
  scale_x_discrete(name = "Season") +
  scale_y_continuous(name = "Diurnal Temp Rate (°C)")

T7c

file.name1 <- paste0(out.dir, "Temp_diurnal_rate_season.jpg")
ggsave(T7c, filename=file.name1, dpi=300, height=5, width=6)

library(lme4)
library(merTools)

mod_mixed = lmer(Temp ~ AirTemp + (1 | SiteName), data = df_mod)
mod_mixed
summary(mod_mixed)
dim(mod_mixed@frame)
dim(df_mod)
coef(mod_mixed)
ranef(mod_mixed)$SiteName %>% head(5)
plotREsim(REsim(mod_mixed))

plot(ranef(mod_mixed, level = 1))

qqnorm(resid(mod_mixed, type = "normalized"),
       xlim = lims, ylim = lims, main = "lmm6.2")
abline(0,1, col = "red", lty = 2)

predict_with_re = predict(mod_mixed)

length(predict_with_re)
