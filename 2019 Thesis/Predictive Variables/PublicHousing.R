library(pacman)
p_load(tidyverse, ggplot2, tigris, tmap, raster, httr, jsonlite, lubridate)

url <- "https://services.arcgis.com/VTyQ9soqVukalItT/arcgis/rest/services/Public_Housing_Buildings/FeatureServer/0/query?outFields=*&where=1%3D1"
APIdata<- GET(url)

pub_housing <- shapefile("Nash_Pub_Housing.shp")
pub_housing_df <- pub_housing@data 

pub_housing_df$date <- as.Date(substr(pub_housing_df$DOFA_ACTUA,0, 10))

pub_housing_df <- pub_housing_df %>% dplyr::select(TOTAL_DWEL, STATE2KX, CNTY2KX, TRACT2KX, BLOCK2KX, date) %>% mutate(CensusTractID = paste0(STATE2KX,CNTY2KX, TRACT2KX), units = as.integer(TOTAL_DWEL)) %>% filter(date < "2000-01-01")

pub_housing_sum <- pub_housing_df %>%  group_by(CensusTractID) %>% summarize(value = sum(units))

#quick Map
tracts_sp <- tigris::tracts("TN", "Davidson") 
joined <- geo_join(tracts_sp, pub_housing_sum, "GEOID", "CensusTractID", how = "left")

pub_housing_all <- joined@data %>% dplyr::select(CensusTractID = GEOID, value) %>% mutate(variable = "count of public housing units")
pub_housing_all[is.na(pub_housing_all)] <- 0

saveRDS(pub_housing_all, "C://School//Research//Predictive Variables//Master//PublicHousing.rds")

map <- tm_shape(pub_housing_all) + tm_fill("value", style = "quantile", palette = "Reds",
                                  title = "Public Housing Locations") +
  tm_borders()+
  tm_layout(
    inner.margins = c(.06, .06, .06, .06),
    legend.title.size = .75,
    legend.text.size = .5,
    legend.bg.color = "grey",
    legend.bg.alpha = 0,
    legend.width = 0.235)