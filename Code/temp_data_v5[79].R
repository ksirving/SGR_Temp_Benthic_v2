# Load packages

library(tidyverse)
library(purrr)
library(readxl)
library(lubridate)
library(readxl)
library(zoo)
library(dplyr)
library(tidyr)
library(stringr)
#install.packages("rio")
#library(rio)
library(readxl)
setwd("C:/Users/GisUser/Downloads/Biology Department/Temp/data_files")
getwd()
temp.files <- list.files() ## list all files
#View(temp.files) #name of all files folder


temp_datax <- NULL ## create empty dataframe to cumulate into

c
c=1
for(c in 1:length(temp.files)) { ## will loop through list of files
  
  temp_data <- read_excel(paste(temp.files[c])) ##upload each file one by one
  head(temp_data)
  #manipulate data here:
  #temp_data <- temp_data[c(1:4)] #Delete extra columns 5-8
  temp_data$date <- as.Date(temp_data$`Date-Time (PST)`)
  #temp_data$time <- format(as.POSIXct(temp_data$`Date-Time (PST)`),
                               # format = "%H:%M:%S") # Split Date-Time (PST) column into date and time
  temp_data = temp_data %>% 
    mutate(date = ymd(date)) %>% 
    mutate_at(vars(date), funs(year, month, day)) # now lets split the date column into date, year, month, day
  temp_data$date<-as.POSIXct(temp_data$date,
                              
                              format = "%Y-%m-%d",
                              
                              tz = "GMT")
  
  #str(temp_datax)
  names(temp_data)[3] <- "Temperature"
  View(temp_data)
  #temp_data <- temp_data[c(1:3,6:9)] #select columns to keep
  
  # add column to data frame and use: paste(temp.files[c]) for column name:
  # temp_datax <- temp_datax[c(1:8)]
  temp_data <- cbind(temp_data, paste(temp.files[c]))
  
  names(temp_data)[8] <- "File" # rename column 8 to "File"
  temp_datax <- bind_rows(temp_datax, temp_data)  ## combine all - cumulative dataframe
#temp_data  
  
  View(temp_datax)
  
}
View(temp_datax)

# merge two temp columns:

names(temp_datax)[10] <- "Temperature2"
temp_datax$temp = temp_datax$Temperature  # temp your new merged column, start with Temperature
temp_datax$temp[!is.na(temp_datax$Temperature2)] = temp_datax$Temperature2[!is.na(temp_datax$Temperature2)]  # merge with Temperature2
temp_datax = select(temp_datax, -3,-10)
names(temp_datax)[9] <- "Temperature °C"
View(temp_datax)

# temp_datax <- temp_datax[c(-2)] # remove column 2 Date-time

# fix column types for date (currently unknown), time (currently a character),

#temp_data$time <- format(as.POSIXct(temp_data$time))
str(temp_datax)
## format date time

## format date time

#change date to character:
temp_datax$date<-as.character(temp_datax$date)



