library(here)
library(tidyverse)
library(googleway)
library(sf)
library(leaflet)

# data from https://discover.data.vic.gov.au/dataset/all-victorian-sars-cov-2-covid-19-current-exposure-sites
# saved in the Data directory.
source("google_keys.R")
dset <- here("Data", "High Risk Exposure Sites Pubic - data-vic-gov-au.csv")

cc <- cols(
  Suburb = col_character(),
  Site_title = col_character(),
  Site_streetaddress = col_character(),
  Site_state = col_character(),
  Site_postcode = col_integer(),
  Exposure_date_dtm = col_date(format = ""),
  Exposure_date = col_character(),
  Exposure_time = col_character(),
  Notes = col_character(),
  Added_date_dtm = col_date(format = ""),
  Added_date = col_character(),
  Added_time = col_time(format = ""),
  Advice_title = col_character(),
  Advice_instruction = col_character(),
  Exposure_time_start_24 = col_time(format = ""),
  Exposure_time_end_24 = col_time(format = "")
)


# Flinders St Station as a position to measure distance from

sites <- read_csv(dset, col_types = cc)
sites <- mutate(sites,
                address_for_google = paste(Site_title, Site_streetaddress, Site_postcode, Site_state, "Australia", sep=" , "),
                # an & seems to mess up the query. Probably an error in url coding somewhere
                address_for_google = str_replace_all(address_for_google, "&", "and")
                )

# remove the bus/train stuff that will mess up the map.
# They need to be dealt with some other way.

sites <- filter(sites, !str_detect(Site_title, "^PTV"))

fl <- "unique_sites.Rda"
if (file.exists(fl)) {
  load(fl)
} else {
  
  sites.unique <- unique(select(sites, Suburb, Site_title, Site_state, Site_streetaddress, Site_postcode, address_for_google))
  sites.unique <- mutate(sites.unique, googleresults = map(address_for_google, google_geocode))
  FlindersSt <- tibble(address_for_google = "Flinders Street Railway Station, Flinders Street, Melbourne VIC, Australia")
  FlindersSt <- mutate(FlinderSt, googleresults = map(address_for_google, google_geocode))
    
  save(sites.unique, FlindersSt, file=fl)
}

# check for problems
sites.unique <- mutate(sites.unique, problems = map_chr(googleresults, "status") != "OK")
filter(sites.unique, problems)
# Suspect the errors are the & in the first and incorrect address in the second - "Cohuna Island Rd", not "Island Rd"
# Fix these in the original data, then rerun

# Another sanity check - distance from Melbourne CBD

sites.unique <- filter(sites.unique, !problems)

myToSF <- function(df) {
  locations <- map_df(df$googleresults, ~slice_head(geocode_coordinates(.x)))
  df <- bind_cols(df, locations)
  df <- st_as_sf(df, coords=c("lng", "lat"), crs="WGS84")
  return(df)
}
# create an SF object
sites.unique.sf <- myToSF(sites.unique)

FlindersSt.sf <- myToSF(FlindersSt)

leaflet(sites.unique.sf) %>% addTiles() %>% addCircleMarkers(clusterOptions = markerClusterOptions())

alldat <- right_join(sites.unique.sf, sites, by=c("Suburb", "Site_title", "Site_state", "Site_streetaddress", "Site_postcode"))

alldat <- mutate(alldat, 
                 popuptext = paste(Site_title, Site_streetaddress, Site_postcode, Advice_title, 
                                   paste("Exposure Date: ", Exposure_date_dtm, Exposure_time_start_24, Exposure_time_end_24), sep="</br>"),
                 Tier = as.factor(str_replace(Advice_title, "^Tier ([[:digit:]]+) -.+", "\\1")),
                 HowRecent =  difftime(Sys.Date(), alldat$Added_date_dtm, units = "days"),
                 NewSites = ifelse(HowRecent <= 2, "New", "Old"))

tiercolour <- colorFactor(c("red", "cyan", "blue"), ordered = TRUE, domain=alldat$Tier)

olddat <- filter(alldat, HowRecent > 2)
newdat <- filter(alldat, HowRecent <= 2)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=olddat, group = "old", clusterOptions = markerClusterOptions(), opacity=1, popup = ~popuptext, color = ~tiercolour(Tier)) %>% 
  addCircleMarkers(data=newdat, group = "new", clusterOptions = markerClusterOptions(), opacity=1, popup = ~popuptext, color = ~tiercolour(Tier)) %>% 
addLayersControl(
  #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
  overlayGroups = c("new", "old"),
  options = layersControlOptions(collapsed = FALSE)
)

# some in the US??
sites.check <- mutate(sites.unique.sf, fromFlindersSt = st_distance(sites.unique.sf, FlindersSt.sf))
arrange(sites.check, desc(fromFlindersSt))
