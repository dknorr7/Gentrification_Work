library(pacman)
p_load(tidyverse, ggplot2, censusapi, tidycensus, factoextra, sf, basemapR,
       viridis, httr, lubridate,tigris, maps, mapdata, ggmap, stringr, osmdata, leaflet)

options(scipen=999)

#Set Census API key
Sys.setenv(CENSUS_KEY="573560da4d086b5e0160f76ff70202236510b048", overwrite = TRUE)

census_api_key("573560da4d086b5e0160f76ff70202236510b048")

# Read Inflation Adjustment Table -----------------------------------------
monthly_cpi <-
  read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)


monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))

yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2021]


# County Selection --------------------------------------------------------

Full_counties <- tidycensus::fips_codes

county_names <- c("Fulton County", "Dekalb County", "Davidson County", "King County", "San Diego County",
                  "Miami-Dade County", "Los Angeles County", "Cook County", "Dupage County", "Shelby County",
                  "Wake County", "Durham County", "Cuyahoga County", "Harris County", "Fort Bend County", "Montgomery County", "LosAngeles County", "Hamilton County")


my_counties_original <- Full_counties %>% filter(county %in% county_names)

#write.csv(my_counties, "county_picklist.csv")

my_counties <- read.csv("county_picklist1.csv")
# county_picklist1.csv adds county info to city name. Needed because census
# api pulls from county names not city

geo <- "tract"

# Variable Selection ------------------------------------------------------

my_vars <- c(
  total_pop = "B01003_001",
  median_income = "B19013_001",
  median_rent = "B25064_001",
  median_home_val = "B25077_001",
  grad_degree ="B06009_005" ,
  bach_degree ="B06009_006", 
  total_college1 = "B06009_001",
  total_race = "B02001_001",
  white = "B02001_002",
  multi_unit_total = "B25024_001", 
  multi_unit_single1 = "B25024_002",
  multi_unit_single2 = "B25024_003"
)


bach_degree <- get_acs(
  geography = "tract",
  variables = "B06009_006",
  state = "Florida",
  county = "Miami-Dade County",
  year = 2010,
  survey = "acs5",
  geometry = FALSE
)

x_2010 <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = geo,
    variables = my_vars,
    state = .x,
    county = .y,
    year = 2010,
    survey = "acs5",
    geometry = FALSE
  )
) 

raw_2010 <- x_2010 %>% mutate(year = 2010) %>% inner_join(., yearly_cpi, by = c("year" = "cpi_year"))

x_2019 <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = geo,
    variables = my_vars,
    state = .x,
    county = .y,
    year = 2019,
    survey = "acs5",
    geometry = FALSE
  )
) 

raw_2019 <- x_2019 %>% mutate(year = 2019) %>% inner_join(., yearly_cpi, by = c("year" = "cpi_year"))

all_raw <- rbind(raw_2010, raw_2019) %>% mutate(Value = ifelse(variable == "median_income"|variable == "median_rent"|variable == "median_home_val", estimate*(2-adj_factor), estimate)) %>%
  select(-estimate, -moe, -adj_factor, -cpi) %>%
  spread(variable, Value) %>%
  mutate(PCT_COLLEGE = ((grad_degree+bach_degree)/total_college1)*100,
         PCT_WHITE = (white/total_race)*100,
         PCT_MULTIUNIT = ((multi_unit_single1+multi_unit_single2)/multi_unit_total)*100,
         CountyID = substring(GEOID, 1,5)) %>%
  select(-grad_degree, -bach_degree, -total_college1, -total_pop,  -white, -total_race, -multi_unit_single1, -multi_unit_single2, -multi_unit_total )

cntylookup <- all_raw %>% group_by(CountyID) %>% summarize(i = min(NAME))
#write.csv(cntylookup, "countyID_Lookup.csv")

#Lazy way of adding colloquial city name to the 
city_match <- read.csv("countyID_Lookup.csv", colClasses = "character")

City_Data <- all_raw %>% left_join(., city_match, by = "CountyID") %>% gather("Var", "Val", median_home_val:PCT_MULTIUNIT)

City_pct_change <- City_Data %>% spread(year, Val) %>%
  mutate(PCT_CHANGE = 100*(`2019`-`2010`)/`2010`) %>%
  select(-`2010`, -`2019`) %>%
  filter(!is.infinite(PCT_CHANGE))

City_pct_change_wide <- City_pct_change %>% spread(Var, PCT_CHANGE)

City_pct_change_summary <- City_Data %>% 
  select(GEOID, city, year, Var, Val) %>%
  spread(year, Val) %>%
  mutate(PCT_CHANGE = ifelse(Var == "PCT_COLLEGE"|Var == "PCT_WHITE"|Var == "PCT_MULTIUNIT",`2019`-`2010`, 100*(`2019`-`2010`)/`2010`)) %>% 
  filter(!is.infinite(PCT_CHANGE))


# Atlanta -----------------------------------------------------------------


Atl_50_income_setup <- City_Data %>% filter(city == "Atlanta" & year == 2010 & Var == "median_income")
Atl_med_income <- median(Atl_50_income_setup$Val, na.rm=TRUE)
  
ATL_wide <- City_pct_change_wide %>% filter(city == "Atlanta", complete.cases(.), )
atl_labels <- ATL_wide$GEOID
ATL_DATA <- ATL_wide[,6:11]

scaled_atl <- scale(ATL_DATA)

fviz_nbclust(scaled_atl, kmeans, method = "silhouette", print.summary = TRUE)

km_atl <- kmeans(scaled_atl, centers = 5, nstart = 50)

ATL_results <- data.frame(km_atl$cluster, ATL_wide$GEOID)

atl_raw_comparison <- cbind(ATL_DATA,ATL_results)
ATL_sp <- geo_join(spatial_data = ATL, data_frame = ATL_results, by_sp = "GEOID", by_df = "ATL_wide.GEOID", how = "left")

atl_SF <- st_as_sf(ATL_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4")

ggplot(atl_SF) + geom_sf(aes(fill = factor(km_atl.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Atlanta Neighborhood Change Clusters", subtitle = "2010 - 2018")

ATL_K_summary <- atl_raw_comparison %>% group_by(km_atl.cluster) %>% summarize_if(is.numeric, mean)

#Get spatial data
ATL <- tracts("GA", "Fulton")



# Nashville -----------------------------------------------------------------

Nash_wide <- City_pct_change_wide %>% filter(city == "Nashville", complete.cases(.))

Nash_labels <- Nash_wide$GEOID
Nash_DATA <- Nash_wide[,6:11]

scaled_Nash <- scale(Nash_DATA)

fviz_nbclust(scaled_Nash, kmeans, method = "silhouette", print.summary = TRUE)

km_Nash <- kmeans(scaled_Nash, centers = 2, nstart = 50)
Nash_results <- data.frame(km_Nash$cluster, Nash_wide$GEOID)

Nash_raw_comparison <- cbind(Nash_DATA, Nash_results)

Nash_K_summary <- Nash_raw_comparison %>% group_by(km_Nash.cluster) %>% summarize_if(is.numeric, mean)

#Get spatial data
Nash <- tracts("TN", "Davidson", cb = TRUE )

Nash_sp <- geo_join(spatial_data = Nash, data_frame = Nash_results, by_sp = "GEOID", by_df = "Nash_wide.GEOID", how = "left")

Nash_SF <- st_as_sf(Nash_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4")

ggplot(Nash_SF) + geom_sf(aes(fill = factor(km_Nash.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Nashville Neighborhood Change Clusters", subtitle = "2010 - 2018")


# Seattle -----------------------------------------------------------------

Seattle_wide <- City_pct_change_wide %>% filter(city == "Seattle", complete.cases(.))

Seattle_labels <- Seattle_wide$GEOID
Seattle_DATA <- Seattle_wide[,6:11]

scaled_Seattle <- scale(Seattle_DATA)

fviz_nbclust(scaled_Seattle, kmeans, method = "silhouette", print.summary = TRUE)

km_Seattle <- kmeans(scaled_Seattle, centers = 5, nstart = 50)
Seattle_results <- data.frame(km_Seattle$cluster, Seattle_wide$GEOID)

Seattle_raw_comparison <- cbind(Seattle_DATA, Seattle_results)

Seattle_K_summary <- Seattle_raw_comparison %>% group_by(km_Seattle.cluster) %>% summarize_if(is.numeric, mean) 
Seattle_K_counts <- Seattle_raw_comparison %>% group_by(km_Seattle.cluster) %>% summarize(count = n())
#Get spatial data
Seattle <- tracts("WA", "King", cb = TRUE )

Seattle_sp <- geo_join(spatial_data = Seattle, data_frame = Seattle_results, by_sp = "GEOID", by_df = "Seattle_wide.GEOID", how = "left")

Seattle_SF <- st_as_sf(Seattle_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(Seattle_SF) + geom_sf(aes(fill = factor(km_Seattle.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Seattle Neighborhood Change Clusters", subtitle = "2010 - 2018")



# Miami -----------------------------------------------------------------
geo_join(spatial_data = Miami, data_frame = Miami_results, by_sp = "GEOID", by_df = "Miami_wide.GEOID", how = "left")

Miami_SF <- st_as_sf(Miami_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(Miami_SF) + geom_sf(aes(fill = factor(km_Miami.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Miami Neighborhood Change Clusters", subtitle = "2010 - 2018")




Miami_wide <- City_pct_change_wide %>% filter(city == "Miami", complete.cases(.))

MiamiSeattle_labels <- Miami_wide$GEOID
Miami_DATA <- Miami_wide[,6:11]

scaled_Miami <- scale(Miami_DATA)

fviz_nbclust(scaled_Miami, kmeans, method = "silhouette", print.summary = TRUE)

km_Miami <- kmeans(scaled_Miami, centers = 3, nstart = 50)
Miami_results <- data.frame(km_Miami$cluster, Miami_wide$GEOID)

Miami_raw_comparison <- cbind(Miami_DATA, Miami_results)

Miami_K_summary <- Miami_raw_comparison %>% group_by(km_Miami.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data
Miami <- tigris::tracts("Florida","Miami-Dade",year = 2014, refresh = TRUE)

Miami_sp <- 


Denver <- tigris::tracts(state = "Colorado", county = "Denver County")

# Denver -----------------------------------------------------------------

Denver_wide <- City_pct_change_wide %>% filter(city == "Denver", complete.cases(.))

Denver_labels <- Denver_wide$GEOID
Denver_DATA <- Denver_wide[,6:11]

scaled_Denver <- scale(Denver_DATA)

fviz_nbclust(scaled_Denver, kmeans, method = "silhouette", print.summary = TRUE)

km_Denver <- kmeans(scaled_Denver, centers = 3, nstart = 50)
Denver_results <- data.frame(km_Denver$cluster, Denver_wide$GEOID)

Denver_raw_comparison <- cbind(Denver_DATA, Denver_results)

Denver_K_summary <- Denver_raw_comparison %>% group_by(km_Denver.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data

Denver <- tigris::tracts(state = "Colorado", county = "Denver County")

Denver_sp <- geo_join(spatial_data = Denver, data_frame = Denver_results, by_sp = "GEOID", by_df = "Denver_wide.GEOID", how = "left")

Denver_SF <- st_as_sf(Denver_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(Denver_SF) + geom_sf(aes(fill = factor(km_Denver.cluster))) + 
    coord_sf() +
    theme(legend.position="none") +
    scale_fill_manual(values = colors, name = "Cluster", na.value="black") +
    theme(legend.position = "right") + 
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    labs(title = "Denver Neighborhood Change Clusters", subtitle = "2010 - 2018") + 
    addTiles()

#tmap_mode('view')
tmap_mode('plot')
tm_shape(Denver_sp)+ tm_fill("km_Denver.cluster") + tm_tiles(server = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}')


+  tm_layout(basemaps = leaflet::providers$OpenStreetMap)
# San Diego -----------------------------------------------------------------

SanDiego_wide <- City_pct_change_wide %>% filter(city == "San Diego", complete.cases(.))

SanDiego_labels <- SanDiego_wide$GEOID
SanDiego_DATA <- SanDiego_wide[,6:11]

scaled_SanDiego <- scale(SanDiego_DATA)

fviz_nbclust(scaled_SanDiego, kmeans, method = "silhouette", print.summary = TRUE)

km_SanDiego <- kmeans(scaled_SanDiego, centers = 3, nstart = 50)
SanDiego_results <- data.frame(km_SanDiego$cluster, SanDiego_wide$GEOID)

SanDiego_raw_comparison <- cbind(SanDiego_DATA, SanDiego_results)

SanDiego_K_summary <- SanDiego_raw_comparison %>% group_by(km_SanDiego.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data

SanDiego <- tigris::tracts(state = "California", county = "San Diego County")

SanDiego_sp <- geo_join(spatial_data = SanDiego, data_frame = SanDiego_results, by_sp = "GEOID", by_df = "SanDiego_wide.GEOID", how = "left")

SanDiego_SF <- st_as_sf(SanDiego_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(SanDiego_SF) + geom_sf(aes(fill = factor(km_SanDiego.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "San Diego Neighborhood Change Clusters", subtitle = "2010 - 2018")




# Chicago -----------------------------------------------------------------

Chicago_wide <- City_pct_change_wide %>% filter(city == "Chicago", complete.cases(.))

Chicago_labels <- Chicago_wide$GEOID
Chicago_DATA <- Chicago_wide[,6:11]

scaled_Chicago <- scale(Chicago_DATA)

fviz_nbclust(scaled_Chicago, kmeans, method = "silhouette", print.summary = TRUE)

km_Chicago <- kmeans(scaled_Chicago, centers = 3, nstart = 50)
Chicago_results <- data.frame(km_Chicago$cluster, Chicago_wide$GEOID)

Chicago_raw_comparison <- cbind(Chicago_DATA, Chicago_results)

Chicago_K_summary <- Chicago_raw_comparison %>% group_by(km_Chicago.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data

Chicago <- tigris::tracts(state = "Illinois", county = "Cook County")

Chicago_sp <- geo_join(spatial_data = Chicago, data_frame = Chicago_results, by_sp = "GEOID", by_df = "Chicago_wide.GEOID", how = "left")

Chicago_SF <- st_as_sf(Chicago_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(Chicago_SF) + geom_sf(aes(fill = factor(km_Chicago.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Chicago Neighborhood Change Clusters", subtitle = "2010 - 2018")




# Raleigh -----------------------------------------------------------------

Raleigh_wide <- City_pct_change_wide %>% filter(city == "Raleigh", complete.cases(.))

Raleigh_labels <- Raleigh_wide$GEOID
Raleigh_DATA <- Raleigh_wide[,6:11]

scaled_Raleigh <- scale(Raleigh_DATA)

fviz_nbclust(scaled_Raleigh, kmeans, method = "silhouette", print.summary = TRUE)

km_Raleigh <- kmeans(scaled_Raleigh, centers = 3, nstart = 50)
Raleigh_results <- data.frame(km_Raleigh$cluster, Raleigh_wide$GEOID)

Raleigh_raw_comparison <- cbind(Raleigh_DATA, Raleigh_results)

Raleigh_K_summary <- Raleigh_raw_comparison %>% group_by(km_Raleigh.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data

Raleigh <- tigris::tracts(state = "North Carolina", county = c("Durham County", "Wake County"))

Raleigh_sp <- geo_join(spatial_data = Raleigh, data_frame = Raleigh_results, by_sp = "GEOID", by_df = "Raleigh_wide.GEOID", how = "left")

Raleigh_SF <- st_as_sf(Raleigh_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(Raleigh_SF) + geom_sf(aes(fill = factor(km_Raleigh.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Raleigh Neighborhood Change Clusters", subtitle = "2010 - 2018")




# Cleveland -----------------------------------------------------------------

Cleveland_wide <- City_pct_change_wide %>% filter(city == "Cleveland", complete.cases(.))

Cleveland_labels <- Cleveland_wide$GEOID
Cleveland_DATA <- Cleveland_wide[,6:11]

scaled_Cleveland <- scale(Cleveland_DATA)

fviz_nbclust(scaled_Cleveland, kmeans, method = "silhouette", print.summary = TRUE)

km_Cleveland <- kmeans(scaled_Cleveland, centers = 3, nstart = 50)
Cleveland_results <- data.frame(km_Cleveland$cluster, Cleveland_wide$GEOID)

Cleveland_raw_comparison <- cbind(Cleveland_DATA, Cleveland_results)

Cleveland_K_summary <- Cleveland_raw_comparison %>% group_by(km_Cleveland.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data

Cleveland <- tigris::tracts(state = "Ohio", county = "Cuyahoga County")

Cleveland_sp <- geo_join(spatial_data = Cleveland, data_frame = Cleveland_results, by_sp = "GEOID", by_df = "Cleveland_wide.GEOID", how = "left")

Cleveland_SF <- st_as_sf(Cleveland_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(Cleveland_SF) + geom_sf(aes(fill = factor(km_Cleveland.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Cleveland Neighborhood Change Clusters", subtitle = "2010 - 2018")




# Memphis -----------------------------------------------------------------

Memphis_wide <- City_pct_change_wide %>% filter(city == "Memphis", complete.cases(.))

Memphis_labels <- Memphis_wide$GEOID
Memphis_DATA <- Memphis_wide[,6:11]

scaled_Memphis <- scale(Memphis_DATA)

fviz_nbclust(scaled_Memphis, kmeans, method = "silhouette", print.summary = TRUE)

km_Memphis <- kmeans(scaled_Memphis, centers = 3, nstart = 50)
Memphis_results <- data.frame(km_Memphis$cluster, Memphis_wide$GEOID)

Memphis_raw_comparison <- cbind(Memphis_DATA, Memphis_results)

Memphis_K_summary <- Denver_raw_comparison %>% group_by(km_Denver.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data

Memphis <- tigris::tracts(state = "Tennessee", county = "Shelby County")

Memphis_sp <- geo_join(spatial_data = Memphis, data_frame = Memphis_results, by_sp = "GEOID", by_df = "Memphis_wide.GEOID", how = "left")

Memphis_SF <- st_as_sf(Memphis_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(Memphis_SF) + geom_sf(aes(fill = factor(km_Memphis.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Memphis Neighborhood Change Clusters", subtitle = "2010 - 2018")




# Houston -----------------------------------------------------------------

Houston_wide <- City_pct_change_wide %>% filter(city == "Houston", complete.cases(.))

Houston_labels <- Houston_wide$GEOID
Houston_DATA <- Houston_wide[,6:11]

scaled_Houston <- scale(Houston_DATA)

fviz_nbclust(scaled_Houston, kmeans, method = "silhouette", print.summary = TRUE)

km_Houston <- kmeans(scaled_Houston, centers = 3, nstart = 50)
Houston_results <- data.frame(km_Houston$cluster, Houston_wide$GEOID)

Houston_raw_comparison <- cbind(Houston_DATA, Houston_results)

Houston_K_summary <- Houston_raw_comparison %>% group_by(km_Houston.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data

Houston <- tigris::tracts(state = "Texas", county = c("Fort Bend County", "Montgomery County","Harris County"))

Houston_sp <- geo_join(spatial_data = Houston, data_frame = Houston_results, by_sp = "GEOID", by_df = "Houston_wide.GEOID", how = "left")

Houston_SF <- st_as_sf(Houston_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(Houston_SF) + geom_sf(aes(fill = factor(km_Houston.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Houston Neighborhood Change Clusters", subtitle = "2010 - 2018")




# Los Angeles -----------------------------------------------------------------

LosAngeles_wide <- City_pct_change_wide %>% filter(city == "Los Angeles", complete.cases(.))

LosAngeles_labels <- LosAngeles_wide$GEOID
LosAngeles_DATA <- LosAngeles_wide[,6:11]

scaled_LosAngeles <- scale(LosAngeles_DATA)

fviz_nbclust(scaled_LosAngeles, kmeans, method = "silhouette", print.summary = TRUE)

km_LosAngeles <- kmeans(scaled_LosAngeles, centers = 3, nstart = 50)
LosAngeles_results <- data.frame(km_LosAngeles$cluster, LosAngeles_wide$GEOID)

LosAngeles_raw_comparison <- cbind(LosAngeles_DATA, LosAngeles_results)

LosAngeles_K_summary <- LosAngeles_raw_comparison %>% group_by(km_LosAngeles.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data

LosAngeles <- tigris::tracts(state = "California", county = "Los Angeles County")

LosAngeles_sp <- geo_join(spatial_data = LosAngeles, data_frame = LosAngeles_results, by_sp = "GEOID", by_df = "LosAngeles_wide.GEOID", how = "left")

LosAngeles_SF <- st_as_sf(LosAngeles_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(LosAngeles_SF) + geom_sf(aes(fill = factor(km_LosAngeles.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "LosAngeles Neighborhood Change Clusters", subtitle = "2010 - 2018")




# Chattanooga -----------------------------------------------------------------

Chat_wide <- City_pct_change_wide %>% filter(city == "Chattanooga", complete.cases(.))

Chat_labels <- Chat_wide$GEOID
Chat_DATA <- Chat_wide[,7:12]

scaled_Chat <- scale(Chat_DATA)

fviz_nbclust(scaled_Chat, kmeans, method = "silhouette", print.summary = TRUE)
fviz_nbclust(scaled_Chat, kmeans, method = "wss", print.summary = TRUE)


km_Chat <- kmeans(scaled_Chat, centers = 4, nstart = 50)
Chat_results <- data.frame(km_Chat$cluster, Chat_wide$GEOID)

fviz_cluster(km_Chat, data = scaled_Chat) + theme(legend.title = element_text("ff"))


Chat_raw_comparison <- cbind(Chat_DATA, Chat_results)

Chat_K_summary <- Chat_raw_comparison %>% group_by(km_Chat.cluster) %>% summarize_if(is.numeric, mean) 
#Get spatial data

Chat <- tigris::tracts(state = "Tennessee", county = "Hamilton County")

Chat_sp <- geo_join(spatial_data = Chat, data_frame = Chat_results, by_sp = "GEOID", by_df = "Chat_wide.GEOID", how = "left")

Chat_SF <- st_as_sf(Chat_sp)

colors <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")
cols <- c("cornflowerblue", "tomato", "seagreen", "darkgoldenrod4","purple4", "yellow")

ggplot(Chat_SF) + geom_sf(aes(fill = factor(km_Chat.cluster))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = colors, name = "Cluster", na.value="black") + theme(legend.position = "right")  + theme(axis.text = element_blank(), axis.ticks = element_blank()) + labs(title = "Hamilton County, Chattanooga \nNeighborhood Change Clusters", subtitle = "2010 - 2019")

write_sf(Chat_sp, "Chat_k5.shp")


DavidsonSummaryTable <- K_Description %>% group_by(Var) %>% summarize(Mean = mean(PCT_CHANGE, na.rm = TRUE),
                                                                      Median = median(PCT_CHANGE, na.rm = TRUE),
                                                                      Min = min(PCT_CHANGE, na.rm=TRUE),
                                                                      Max = max(PCT_CHANGE, na.rm=TRUE),
                                                                      SD = sd(PCT_CHANGE, na.rm=TRUE),
                                                                      count = n())


Chat_Description <- City_pct_change_summary %>%
  inner_join(., Chat_results, by = c("GEOID"="Chat_wide.GEOID")) %>%
  select(-km_Chat.cluster)



Chat_Description_grp <- Chat_Description %>% group_by(Var) %>%
  summarize(cnt = n(),
            med2010 = median(`2010`),
            med2019=median(`2019`),
            mean2010 = mean(`2010`, na.rm=TRUE),
            mean2019=mean(`2019`, na.rm = TRUE),
            min2010 = min(`2010`),
            min2019 = min(`2019`),
            max2010 = max(`2010`),
            max2019 = max(`2019`))
