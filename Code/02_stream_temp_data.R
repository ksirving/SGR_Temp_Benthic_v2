## stream temp la county
## from Jenny's model

library(tidylog)
library(tidyverse)
library(sf)

# upload data -------------------------------------------------------------

load("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/Projects/San_Gabriel_Temp/Data/AirTemp/Modeling/baseline_stream_temp.RData")

baseline_stream_temp <- baseline_stream_temp %>% ungroup()

head(baseline_stream_temp) 
range(baseline_stream_temp$year)
names(baseline_stream_temp)
