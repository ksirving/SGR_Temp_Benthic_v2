## model exploration

library(tidylog)
library(tidyverse)
library(sf)


# Capture probabilities ---------------------------------------------------

## capture probs
load(file="ignore/SMC_cap_prob_cali.csv")
head(oe_ca)
dim(oe_ca)
## bug data

bugs2 <- st_read("output_data/01_bio_sites_surrounding_counties.shp")
head(bugs2)
dim(bugs2)

## format capture probability

capLAC <- oe_ca %>%
  # filter(masterid %in% bugs2$masterid) %>%
  rename(TAXON = otu) %>%
  drop_na(captureprob)
dim(capLAC)

# Traits ------------------------------------------------------------------

## uplaod traits EPA

traitsUSA <- read.csv("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/SGR_Temp_Benthic/ignore/FreshwaterBioTraits_20100927.csv")
head(traitsUSA)
length(unique(traitsUSA$TAXON))
## filter to species in capture prob df
length(unique(capLAC$otu)) ## 335

TraitsCA <- traitsUSA %>%
  filter(TAXON %in% capLAC$TAXON )
length(unique(TraitsCA$TAXON)) ## 266

## get tolerance values to start with
names(TraitsCA)
unique(TraitsCA$TRAITS_NAME)

tols <- TraitsCA %>%
  filter(CATEGORY_NAME == "Tolerance") %>%
  dplyr::select(TAXON:TAXON_ORDER,STUDY_LOCATION_REGION, CATEGORY_NAME:VALUE_TEXT) %>%
  filter(TRAITS_NAME == "Thermal optima value") %>%
  group_by(TAXON) %>% 
  summarise(MeanValue = mean(VALUE_NUMBER)) ## quick fix, change later


length(unique(tols$TAXON)) ## 207 - find more!!!!

### join with capture probability

capTols <- inner_join(capLAC, tols, by = "TAXON") #%>%
# dplyr::select(-c(sampleid)) %>% distinct()
names(capTols)

head(capTols)

sum(is.na(capTols$MeanValue))
sum(is.na(capTols$captureprob))

## calculted means of trait per sites, weighrd on capture probability. 
## o/e calcuated by weightmean oberseved/weighted mean expected

oe <- capTols %>% group_by(masterid,latitude,longitude,county, huc, sampleid) %>%
  summarise(wgtValueEx = weighted.mean(MeanValue, captureprob),
            wgtValueEx_obs = weighted.mean(MeanValue[meanobserved>0], captureprob[meanobserved>0])) %>%
  ungroup() %>%
  mutate(oeTemp = wgtValueEx_obs/wgtValueEx)

head(oe)

write.csv(oe, "output_data/01_ObsExp_all_CA.csv")


# Join with Temp ----------------------------------------------------------


## join with bugsites to get comid

## temp alteration
## statewide


ggplot(oe, aes(x=longitude, y = latitude, color = oeTemp)) +
  geom_point() + 
  scale_color_viridis_c()


T1 <- ggplot(test, aes(y=oeTemp, x=Value, group = Variable, color = Variable)) +
  geom_smooth(method = "glm") +
  geom_vline(xintercept = 30, linetype="dashed", 
             color = "red", linewidth=0.5, show.legend = T) +
  geom_vline(xintercept = 26.667, linetype="dashed",
             color = "blue", linewidth=0.5, show.legend = T) +
  # geom_hline(yintercept = 0.79) +
  facet_wrap(~Variable, labeller =labeller(supp.labs),
             scales = "free_x") +
  scale_x_continuous(name="Water Temp (°C)") +
  scale_y_continuous(name = "Temp Preference (o/e)")

T1


## add 

head(expTax)
# Taxa occurrences and observed trait value--------------------------------------------------------

##  upload data - needs to be updated
Taxa <- read.csv("ignore/all_tax_data.csv")
head(Taxa)

## how many sites do we have in capture probability and in region
sum(unique(Taxa$stationcode) %in% unique(capTols$masterid)) ## 1184
names(Taxa)
## filter to regional sites ## get year by separating sampledate & remove replicates
?separate
taxaSub <- Taxa %>%
  filter(stationcode %in% capTols$masterid) %>%
  dplyr::select(stationcode, sampledate, replicate, finalid) %>%
  separate(sampledate, into= c("Date", "Time"), sep =c(" ")) %>%
  separate(Date, into = c("Month", "Day", "Year")) %>% dplyr::select(-Time, -Day) %>% ## get sample year
  filter(replicate == 1) %>% ## take first replicate, can change to random later
  rename(TAXON = finalid, masterid = stationcode)  %>% 
  inner_join(capTols, by = c("TAXON", "masterid")) ## join with trait and cap prob data

head(taxaSub)


## observed trait value

obsTax <- taxaSub %>%
  mutate(weightedValues = MeanValue*captureprob) %>%
  group_by(masterid,latitude,longitude,county, huc, Year) %>%
  mutate(wgtValueObs = mean(weightedValues)/mean(captureprob)) %>%
  ungroup() %>%
  dplyr::select(masterid, Year, wgtValueObs) %>%
  distinct() 


head(obsTax)
names(obsTax)


# Obs/Expected  -----------------------------------------------------------

### join obs and expected
## calculate o/e using (O-E)2/E - not sure about this calculation, check this!!!!

oe <- full_join(obsTax, expTax, by = "masterid") %>%
  ungroup() #%>%
mutate(ObsExp = ((wgtValueObs-wgtValueEx)^2)/wgtValueEx)

head(oe)



# Format data for figures  -------------------------------------------------------------------

## upload la temp
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

## filter bug sites to temp sites
bugTempSites <- bugSites %>%
  filter(COMID %in% TempSites)

scoresTempSites <- full_join(oe, bugTempSites, by = "masterid") %>%
  rename(year = Year) %>% mutate(year = as.numeric(year))
head(scoresTempSites)

## join data
AllData <- left_join(scoresTempSites, baseline_stream_temp, by = c("COMID", "year")) %>% drop_na()
head(AllData)
dim(AllData) ## 734


# Figures -----------------------------------------------------------------


## format for figures, make temp vars long, remove Max_Wkl_Max_StreamT_grt_30_

AllDataLong <- AllData %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Max_Wkl_Max_StreamT_grt_30_, names_to = "Variable", values_to = "Value") %>%
  filter(!Variable == "Max_Wkl_Max_StreamT_grt_30_") 

unique(AllDataLong$Variable)
supp.labs
supp.labs <- unique(AllDataLong$Variable)
names(supp.labs) <- c("Max Weekly Mean","Max Weekly Max", "Max Weekly Min", "Max Weekly Range", "Av Weekly Range")

head(AllDataLong)
# ?geom_vline

T1 <- ggplot(AllDataLong, aes(y=ObsExp, x=Value, group = Variable, color = Variable)) +
  geom_smooth(method = "glm") +
  geom_vline(xintercept = 30, linetype="dashed", 
             color = "red", linewidth=0.5, show.legend = T) +
  geom_vline(xintercept = 26.667, linetype="dashed",
             color = "blue", linewidth=0.5, show.legend = T) +
  # geom_hline(yintercept = 0.79) +
  facet_wrap(~Variable, labeller =labeller(supp.labs),
             scales = "free_x") +
  scale_x_continuous(name="Water Temp (°C)") +
  scale_y_continuous(name = "Temp Preference (o/e)")

T1

file.name1 <- paste0(out.dir, "03_temp_pref_temp_response_GAMs.jpg")
ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)




# check San Juan sites ----------------------------------------------------

soc_sites <- read.csv("input_data/CSCI_ASCI_SitesAll_for_SOC_FlowEcologyStudy.csv")

head(soc_sites)
dim(soc_sites)

soc_sp <- soc_sites %>%
  select(BugID, Lat, Long) %>%
  st_as_sf(coords=c("Long", "Lat"), crs=4326, remove=F)

soc_sp

# this map of all sites in same HUC 12
m1 <- mapview(soc_sp, cex=2, col.regions="green",
              layer.name="Bioassessment Stations") 


m1
m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")


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

csciScores <- read.csv("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/Cannabis_Eflows/ignore/csci_ca_scores.csv")
head(csciScores)


# Filter sites to LA temp region --------------------------------------------------

## how many temp in bug sites
sum(TempSites %in% bugSites$COMID) ## 476

## filter bug sites to temp sites
bugTempSites <- bugSites %>%
  filter(COMID %in% TempSites)

## how many sites with scores?
sum(csciScores$stationcode %in% bugTempSites$masterid) ## 1192

## format scores
csciScores <- csciScores %>%
  select(sampleid, stationcode, sampledate, sampleyear, fieldreplicate, csci) %>%
  rename(masterid = stationcode)

head(csciScores)

## filter bug data using masterid ### remove reps - remove 2nd rep for now, change later!!!!
csciScoresLA <- csciScores %>%
  filter(masterid %in% bugTempSites$masterid, fieldreplicate == 1 ) 
  
length(unique(csciScoresLA$masterid)) ## 707 sites in LA region with temp

## join comids
scoresTempSites <- full_join(csciScoresLA, bugTempSites, by = "masterid")
head(scoresTempSites)

# Join bug sites to temp data ---------------------------------------------

## join by comid and year

scoresTempSites <- scoresTempSites %>%
  rename(year = sampleyear)
head(scoresTempSites)

head(baseline_stream_temp)

AllData <- left_join(scoresTempSites, baseline_stream_temp, by = c("COMID", "year")) %>% drop_na()
head(AllData)
dim(AllData) ## 734

## save out
write.csv(AllData, "output_data/03_bugs_temp_joined_by_year.csv")


# Figures -----------------------------------------------------------------

## format for figures, make temp vars long, remove Max_Wkl_Max_StreamT_grt_30_

AllDataLong <- AllData %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Max_Wkl_Max_StreamT_grt_30_, names_to = "Variable", values_to = "Value") %>%
  filter(!Variable == "Max_Wkl_Max_StreamT_grt_30_") 

unique(AllDataLong$Variable)
supp.labs
supp.labs <- unique(AllDataLong$Variable)
names(supp.labs) <- c("Max Weekly Mean","Max Weekly Max", "Max Weekly Min", "Max Weekly Range", "Av Weekly Range")

head(AllDataLong)
?geom_vline

T1 <- ggplot(AllDataLong, aes(y=csci, x=Value, group = Variable, color = Variable)) +
  geom_smooth(method = "gam") +
  geom_vline(xintercept = 30, linetype="dashed", 
             color = "red", size=0.5, show.legend = T) +
  geom_vline(xintercept = 26.667, linetype="dashed",
             color = "blue", size=0.5, show.legend = T) +
  geom_hline(yintercept = 0.79) +
  facet_wrap(~Variable, labeller =labeller(supp.labs),
             scales = "free_x") +
  scale_x_continuous(name="Water Temp (°C)") +
  scale_y_continuous(name = "CSCI")

T1

file.name1 <- paste0(out.dir, "03_csci_temp_response_GAMs.jpg")
ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)

### issues - 
## only one temp value per year, need seasonal
## can we get other metrics?


# Bioassessment data ------------------------------------------------------

load("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/Cannabis_Eflows/input_data/SMC_phab_cali.csv")
head(phab_ca)

unique(phab_ca$analytename)
