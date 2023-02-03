### bioassessment sites

##workflow
## 1 - upload csci and asci
## subset to region of interest - RB4?
## plot
## explore data in region
## format alternative response metrics
## - component metrics
## - traits and capture probability

library(tidylog)
library(tidyverse)
library(sf)
library(mapview)
library(nhdplusTools)

# install_github("SCCWRP/CSCI", force=T)
library(CSCI)     
library(lubridate)


# CSCI --------------------------------------------------------------------

load("ignore/SMC_bmi_cali.csv") ## update
head(bug_tax_ca)
class(bug_tax_ca)

unique(bug_tax_ca$smcshed)

# sgr_sites <- bug_tax_ca %>% filter(smcshed == "San Gabriel") 

head(sgr_sites)
length(unique(sgr_sites$masterid)) ## 113

## make spatial
# Create dataframe for looking up COMIDS (here use all stations)
bug_segs <- bug_tax_ca %>%
  dplyr::select(masterid, longitude, latitude, comid) %>%
  distinct(masterid, .keep_all = TRUE) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  rename(COMID1 = comid)  %>%
  arrange(masterid)

# use nhdtools to get comids
bug_all_coms <- bug_segs %>%
  group_split(masterid) %>%
  set_names(., bug_segs$masterid) %>%
  map(~discover_nhdplus_id(.x$geometry))

bug_all_coms

# flatten into single dataframe instead of list
bug_segs_df <-bug_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "masterid")

bug_segs_df

bugs <- full_join(bug_segs, bug_segs_df, by = "masterid")
head(bugs)

## add county to filter
county <- bug_tax_ca %>%
  select(masterid, county) %>%
  distinct()

bugs2 <- left_join(bugs, county, by = "masterid")

class(bugs2)
dim(bugs2)

st_write(bugs2, "output_data/01_bio_sites_all.shp", append=F)

bugs2 <- st_read("output_data/01_bio_sites_all.shp")
head(bugs2)

## subset to surrounding counties - update this!!! do by HUC
sort(unique(bug_tax_ca$county))
names(bugs2)

## map of all sites in state
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)



# this map of all sites in same HUC 12
m1 <- mapview(bugs2, cex=2, col.regions="orange",
              layer.name="Bugs Stations") 


m1
# m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

mapshot(m1, url = paste0(getwd(), "/output_data/01_bio_sites_all_counties_mapview.html"),
        file = paste0(getwd(), "/ignore/01_bio_sites_all_counties_mapview.png"))


## define counties to select
counties <- c("Los Angeles", "Ventura", "Orange", "Riverside", "San Bernardino")

## filter to selelted counties
bug_sp_sub <- bugs2 %>%
  filter(county %in% counties) %>%
  select(masterid, latitude, longitude, county, COMID)

dim(bug_sp_sub)

st_write(bug_sp_sub, "output_data/01_bio_sites_surrounding_counties.shp", append=F)

length(unique(bug_sp_sub$county)) ## 6
length(unique(bug_sp_sub$COMID)) ## 923
length(unique(bug_sp_sub$masterid)) ## 1310

### plot

## select only soatial columns

# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)



# this map of all sites in same HUC 12
m1 <- mapview(bug_sp_sub, cex=2, col.regions="orange",
              layer.name="Bugs Stations") 
  

m1
# m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")

mapshot(m1, url = paste0(getwd(), "/output_data/01_bio_sites_surrounding_counties_mapview.html"),
        file = paste0(getwd(), "/ignore/01_bio_sites_surrounding_counties_mapview.png"))
getwd()

mapview()


# CSCI component metrics --------------------------------------------------

compMets <- read.csv("ignore/csci_comps.csv")
head(compMets)
names(compMets)

## make long 
## keep only metrics with "score"
compMets <- compMets %>%
  dplyr::select(masterid:mmi_score, ends_with("_score")) %>%
  pivot_longer(csci:intolerant_percent_score, names_to="Metric", values_to = "MetricValue")

unique(compMets$Metric) 

## add comid

bugs2 <- st_read("output_data/01_bio_sites_all.shp")
head(bugs2)

bugs2 <- as.data.frame(bugs2) %>%
  select(-geometry)


compMetsComs <- inner_join(compMets, bugs2, by="masterid")
head(compMetsComs)

## save out

write.csv(compMetsComs, "ignore/01_csci_comp_mets_comid_socal.csv")
