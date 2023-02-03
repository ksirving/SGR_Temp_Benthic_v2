### temperature alteration for modelled area

## workflow
## match streamcat data to Jenny data
## calculate alteration
## eventually compare ref streamcat to temp prefs of historical distribution
## model with o/e csci metrics

## packages

library(tidylog)
library(tidyverse)
library(sf)

## upload ref temp data

refTemps <- read.csv("ignore/RefStreamTempPred_CA.csv")
head(refTemps)

## make long and separate metric and year
refTempsLong <- refTemps %>%
  pivot_longer(MAST_2008:MWST_2014, names_to = "MetricYear", values_to = "Values") %>%
  separate(MetricYear, into = c("Metric", "Year"))

head(refTempsLong)

## upload jenny temp data

load("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/Projects/San_Gabriel_Temp/Data/AirTemp/Modeling/baseline_stream_temp.RData")

baseline_stream_temp <- baseline_stream_temp %>% ungroup()
head(baseline_stream_temp)

length(unique(baseline_stream_temp$COMID)) ## 5428
## how many comids match?

sum(unique(baseline_stream_temp$COMID)  %in%  unique(refTempsLong$COMID)) ## 4885, where are the others?


