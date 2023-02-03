## model exploration

library(tidylog)
library(tidyverse)
library(sf)

out.dir <- "/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/SGR_Temp_Benthic/Figures/"
## workflow
## join csci with temp data, match years
## models with tolerant/senstivie taxa
## csci scores
## functional traits
## component metrics

## LA county temp data

load("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/Projects/San_Gabriel_Temp/Data/AirTemp/Modeling/baseline_stream_temp.RData")
str(baseline_stream_temp)

## ungroup due to old r version, make all columns numbers
baseline_stream_temp <- baseline_stream_temp %>% ungroup() %>%
  mutate(Max_Wkl_Max_StreamT_grt_30_ = as.numeric(Max_Wkl_Max_StreamT_grt_30_))

## get comids
TempSites <- unique(baseline_stream_temp$COMID)


## bugs - sites only

bugSites <- st_read("output_data/01_bio_sites_surrounding_counties.shp")
head(bugSites)

## bugs data

csciScores <- read.csv("ignore/01_csci_comp_mets_comid_socal.csv")
head(csciScores)


# Filter sites to LA temp region --------------------------------------------------

## how many temp in bug sites
sum(TempSites %in% bugSites$COMID) ## 476

## filter bug sites to temp sites
bugTempSites <- bugSites %>%
  filter(COMID %in% TempSites)

## how many sites with scores?
sum(unique(csciScores$stationcode) %in% unique(bugTempSites$masterid)) ## 702

## format scores
# csciScores <- csciScores %>%
#   select(-X) %>%
#   rename(masterid = stationcode)

head(csciScores)

## filter bug data using masterid ### remove reps - remove 2nd rep for now, change later!!!!
csciScoresLA <- csciScores %>%
  filter(masterid %in% bugTempSites$masterid, fieldreplicate == 1 ) %>%
  separate(sampledate, into = c("year", "Month", "DayTime"), sep= "-", remove = F) %>%
  mutate(year = as.numeric(year))
?separate
length(unique(csciScoresLA$masterid)) ## 705 sites in LA region with temp

## join comids
# scoresTempSites <- full_join(csciScoresLA, bugTempSites, by = "masterid")
# head(scoresTempSites)

# Join bug sites to temp data ---------------------------------------------

## join by comid and year

scoresTempSites <- csciScoresLA 

head(baseline_stream_temp)
str(baseline_stream_temp)

AllData <- left_join(scoresTempSites, baseline_stream_temp, by = c("COMID", "year")) %>% drop_na()
head(AllData)
dim(AllData) ## 6894

## save out
write.csv(AllData, "output_data/03_bugs_temp_joined_by_year.csv")


# Figures -----------------------------------------------------------------

## format for figures, make temp vars long, remove Max_Wkl_Max_StreamT_grt_30_

AllDataLong <- AllData %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Max_Wkl_Max_StreamT_grt_30_, names_to = "Variable", values_to = "Value") %>%
  filter(!Variable == "Max_Wkl_Max_StreamT_grt_30_") %>%
  filter(!Metric == "count")

supp.labs <- unique(AllDataLong$Variable)
names(supp.labs) <- c("Max Weekly Mean","Max Weekly Max", "Max Weekly Min", "Max Weekly Range", "Av Weekly Range")

head(AllDataLong)
names(AllDataLong)
m=1
mets <- unique(AllDataLong$Metric)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(AllDataLong, Metric == mets[m]), aes(y=MetricValue, x=Value, group = Variable, color = Variable)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
    geom_vline(xintercept = 30, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 26.667, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~Variable, labeller = labeller(supp.labs),
               scales = "free_x") +
    scale_x_continuous(name="Water Temp (°C)") +
    scale_y_continuous(name = paste(mets[m])) +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_GAMs_modelled.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

## figures of component metric by temp metric

mets <- unique(AllDataLong$Variable)
mets
m=1
names(AllDataLong)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(AllDataLong, Variable == mets[m]), aes(y=MetricValue, x=Value, group = Metric, color = Metric)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
    geom_vline(xintercept = 30, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 26.667, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~Metric,
               scales = "free") +
    scale_x_continuous(name=paste(mets[m])) +
    scale_y_continuous(name = "Metric Value") +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_GAMs_modelled.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

### issues - 
## only one temp value per year, need seasonal
## can we get other metrics?


# Bioassessment data ------------------------------------------------------

tempObs <- read.csv("ignore/smc_temp_loggers.csv")
head(tempObs)

## summary stats per site
tempObsAv <- tempObs %>%
  rename(sampledate = sampledatetime) %>%
  separate(sampledate, into = c("year", "Month", "DayTime"), sep= "-", remove = F) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(masterid, year) %>%
  summarise(MedTemp = median(temperature_degc),
            MinTemp = min(temperature_degc),
            MaxTemp = max(temperature_degc),
            MeanTemp = mean(temperature_degc)) %>%
  mutate(RgTemp = MaxTemp-MinTemp)
          

## join with bug data, join by year and masterid. only one site matches with year. join with just masterid for now, ask Rafi

obsData <- full_join(tempObsAv, AllDataLong, by = c("masterid"))
## join only working for one site

## make temp metrics long for figures
obsDataLong <- obsData %>%
  pivot_longer(MedTemp:RgTemp, names_to = "VariableObs", values_to = "ValueObs") %>%
  filter(!Metric == "count")

mets <- unique(obsDataLong$Metric)
mets
names(obsDataLong)

## figures of temp metric by component metric

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(obsDataLong, Metric == mets[m]), aes(y=MetricValue, x=ValueObs, group = VariableObs, color = VariableObs)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
    geom_vline(xintercept = 30, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 26.667, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~VariableObs,
               scales = "free_x") +
    scale_x_continuous(name="Water Temp (°C)") +
    scale_y_continuous(name = paste(mets[m])) +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_GAMs_observed.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}


## figures of component metric by temp metric

mets <- unique(obsDataLong$VariableObs)
mets
m=1
names(obsDataLong)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(obsDataLong, VariableObs == mets[m]), aes(y=MetricValue, x=ValueObs, group = Metric, color = Metric)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
    geom_vline(xintercept = 30, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 26.667, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~Metric,
               scales = "free") +
    scale_x_continuous(name=paste(mets[m])) +
    scale_y_continuous(name = "Metric Value") +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_GAMs_observed.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

