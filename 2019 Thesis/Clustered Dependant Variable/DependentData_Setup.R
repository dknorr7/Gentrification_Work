library(pacman)
p_load(tidyverse, ggplot2, censusapi, tidycensus, sf, viridis, httr, lubridate,tigris, maps, mapdata, ggmap, stringr)

options(scipen=999)

Sys.setenv(CENSUS_KEY="802147089d1abcbfe2ea820bca13be641fcc9a28", overwrite = TRUE)


#Inflation Adjustment Table
monthly_cpi <-
  read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)

monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))

yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2017]


#Get 1990 2000 Tract Adjusted boundaries from Geolytics downloaded file (not dynamic) and adjust for inflation
geolyticsINC00 <- read.csv("Geolytics_ALL.csv") %>% 
  select(CensusTractID = GEO2010, INC = MDHHY0) %>%
  mutate(year = 2000) 

geolyticsINC90 <- read.csv("Geolytics_ALL.csv") %>% 
  select(CensusTractID = GEO2010,  INC = MDHHY9) %>%
  mutate(year = 1990) 

#Bind Geolytics Together and make inflation adjustments
geolytics1 <- rbind(geolyticsINC00, geolyticsINC90)
geolytics1$CensusTractID <- as.character(geolytics1$CensusTractID)

Inf_adjustedInc0090 <- inner_join(x = geolytics1, y = yearly_cpi, by = c("year" = "cpi_year")) %>%
  mutate(infAdjINC = INC/adj_factor) %>%
  select(CensusTractID, year, infAdjINC)

Spread_inc <- Inf_adjustedInc0090 %>% spread(key = year, value = infAdjINC) %>% select(CensusTractID, estimate_1990 = `1990`, estimate_2000 = `2000`)

Spread_inc$CensusTractID <- as.character(Spread_inc$CensusTractID)



#set constants for census data loop
#####################################################
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016)
geo <- "tract"
State <- "TN"
County <- "Davidson"
#####################################################

#Get HH Income Data from Census API
foo1 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data1 <- get_acs(geography = geo, table = "B19013", state = State, county = County,
                       year = loop_year, cache_table = TRUE) %>%
    mutate(year = loop_year, variable = "Median HH Income") %>%
    select(GEOID, year, variable, value = estimate)
  foo1 <- bind_rows(foo1, acs_data1)
}

##Infaltion Adjustment to 2017 dollars
MedHHInc_LT  <- left_join(x= foo1, y = yearly_cpi, by = c("year" = "cpi_year")) %>% 
  mutate(inf_adj_value = value/adj_factor) %>%
  select(GEOID, variable, year, value = inf_adj_value)

#Make Wide Format
MedHHInc <- MedHHInc_LT  %>% mutate(year = str_c("estimate_", year)) %>% select(GEOID, Varx = variable, value, year) %>%
  spread(key = year, value = value)

#Join to GeoLytics
Income00_16 <- inner_join(x = MedHHInc, y = Spread_inc, by = c("GEOID" = "CensusTractID"))

#1990 - 2016 Income Long Table
melted_Income <- reshape2::melt(Income00_16, id.vars = c("GEOID","Varx")) %>% mutate(year = str_sub(variable, -4)) %>%
  select(-variable)

#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^#^


#Get Educational Profiles from 1990 and 2000 from Geolytics downloaded file
raw_geolytics <- read.csv("GeolyticsAll.csv")
chk <- read.csv("Geolytics_ALL.csv")

Education00_raw <- raw_geolytics %>% select(CensusTractID = GEO2010, bachelorsorhigher = EDUC160, Total25older = EDUCPP0, bachelorsorhigher90 = EDUC169, Total25older90 = EDUCPP9) %>%
  mutate(pctCollege00 = (bachelorsorhigher/Total25older)*100, PctCollege90 = (bachelorsorhigher90/Total25older90)*100) %>% 
  select(CensusTractID, Education_2000 = pctCollege00, Education_1990 = PctCollege90)

Education00_raw$CensusTractID = as.character(Education00_raw$CensusTractID)


#Download Educational Profiles 2010 - 2016 from Census API

  
  foo16 <- tibble()
  for (loop_year in years) {
    acs_data16 <- get_acs(geography = geo, table = "B06009", state = State, county = County,
                          year = loop_year, cache_table = TRUE, output = "wide") %>%
      mutate(year = loop_year, PctCollegeHigher = 100*((B06009_005E + B06009_006E)/B06009_001E), variable = paste("Percent College Grad or higher-", geo)) %>%
      select(GEOID, year, variable, value = PctCollegeHigher)
    foo16 <- bind_rows(foo16, acs_data16) 
  }
  masterPath <- "C://School//Research//Predictive Variables//Master//"
  saveRDS(foo16, paste0(masterPath, "Pct_CollegeEd_", geo, ".rds"))
  
  foo16wide <- foo16 %>%  mutate(year = str_c("Education_", year)) %>% select(GEOID, value, year) %>%
    spread(key = year, value = value)
  
  f_EdChange <- foo16wide[complete.cases(foo16wide), ]
  Education00_raw$CensusTractID <- as.character(Education00_raw$CensusTractID)
  
  #merge 2000 geolytics data
  Education00_16 <- inner_join(f_EdChange, Education00_raw, by = c("GEOID" = "CensusTractID"))
  
  melted_Education <- reshape2::melt(Education00_16, id.vars = "GEOID") %>% mutate(year = str_sub(variable, -4)) %>% 
    mutate(Varx= "PctCollege") %>% select(-variable)

  # EducationChange <- Education00_16 %>% mutate(EDchange10to16 = 100*(estimate_2016 - estimate_2010)/estimate_2010, EDchange00to16 = 100*(estimate_2016 - ED2000)/ED2000) %>%
  #   select(GEOID, EDchange10to16,EDchange00to16, ED2000, ED2010 = estimate_2010, ED2016 = estimate_2016)
  
# } else {
#   
#   ED_BG <- tibble()
#   for (loop_year in years) {
#     acs_data16_BG <- get_acs(geography = geo, table = "B15003", state = State, county = County,
#                              year = loop_year, cache_table = TRUE, output = "wide") %>%
#       mutate(year = loop_year, PctCollegeHigher = 100*((B15003_022E + B15003_022E + B15003_023E + B15003_024E + B15003_025E)/B15003_001E), variable = paste("Percent College Grad or higher-", geo)) %>%
#       select(GEOID, year, variable, value = PctCollegeHigher)
#     ED_BG <- bind_rows(ED_BG, acs_data16_BG) }
#   
# }


#Home Value
foo12 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data12 <- get_acs(geography = geo, table = "B25077", state = State, county = County,
                        year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year, variable = paste("Median Home Value (2017 Inflation adjusted)-", geo)) %>%
    select(GEOID, MedianHomeVal = B25077_001E, variable, year)
  foo12 <- bind_rows(foo12, acs_data12)
}

MedHomeVal <- merge(x = foo12, y = yearly_cpi, by.x = "year", by.y = "cpi_year")

MedHomeValue_f <- MedHomeVal %>% mutate(inf_adj_value = MedianHomeVal/adj_factor, Var_ID = paste0("MedHomeVal", year), Varx = "Median Home Value", Year = str_sub(Var_ID, -4)) %>%
  select(GEOID, Year, Varx, value = inf_adj_value)

Geo_demo <- read.csv("Geolytics_ALL.csv")

Geolytics_HomeVal<- Geo_demo %>% select(CensusTractID = GEO2010, 
                                        HomeVal00= MDVALHS0, RentVal00 = MDGRENT0, HH_income00 = MDHHY0, NonFamilyHH00 = NONFAM0, TotalHHs00 = NUMHHS0, 
                                        HomeVal90= MDVALHS9, RentVal90 = MDGRENT9, HH_income90 = MDHHY9, NonFamilyHH90 = NONFAM9, TotalHHs90 = NUMHHS9) %>%
  mutate(pctNonfamily00 = 100*(NonFamilyHH00/TotalHHs00), pctNonfamily90 = 100*(NonFamilyHH90/TotalHHs90))



ToAdjust <- Geolytics_HomeVal %>% select(CensusTractID,HomeVal90, HomeVal00)

longtblwYears <- reshape2::melt(ToAdjust, id.vars= "CensusTractID") %>% mutate(Year = ifelse(str_sub(variable, start= -2)=="00", 2000, 1990))

Inf_adjusted <- longtblwYears %>% left_join(x = longtblwYears, y = yearly_cpi, by = c("Year" = "cpi_year")) %>%
  mutate(Varx = "Median Home Value", Adj_value = round(value/adj_factor,0)) %>% select(GEOID = CensusTractID, Year, value = Adj_value, Varx)


melted_homeval <- rbind(Inf_adjusted, MedHomeValue_f) %>% select(GEOID, year = Year, value, Varx)




##Nonwhite Population
foo3 <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data3 <- get_acs(geography = geo, table = "B02001", state = State, county = County,
                       year = loop_year, cache_table = TRUE) %>%
    mutate(year = loop_year) %>% select(GEOID, variable, estimate, year) %>% spread(key = variable, value = estimate) %>% 
    mutate(frac_nonwhite = (1  - B02001_002 / B02001_001)*100, frac_white = (B02001_002 / B02001_001)*100)
  foo3 <- bind_rows(foo3, acs_data3)
}

NonWhite_f <- foo3 %>% select(GEOID, year , value = frac_white) %>%
  mutate(year = as.integer(as.character(year)), variable = paste("Percentage white-", geo), Var_ID = paste0("White",year)) %>%
  select(GEOID, Var_ID, value) %>% spread(key = Var_ID, value = value) %>% select(GEOID, White2010, White2016)

white10_16 <- foo3 %>% select(GEOID, value = frac_white, year) %>% mutate(Varx = "White Pct")

#Bring in all Geolytics data
Geo_demo <- read.csv("Geolytics_ALL.csv")

Populations_xtab <- Geo_demo %>% select(CensusTractID = GEO2010,
                                        TotalPop70 = TRCTPOP7, WhitePop70 = SHRWHT7N, BlackPop70 = SHRBLK7N, HispPop70 = SHRHSP7N, 
                                        TotalPop80 = TRCTPOP8, WhitePop80 = SHRWHT8N, BlackPop80 = SHRBLK8N, HispPop80 = SHRHSP8N, 
                                        TotalPop90 = TRCTPOP9, WhitePop90 = SHRWHT9N, BlackPop90 = SHRBLK9N, HispPop90 = SHRHSP9N, 
                                        TotalPop00 = TRCTPOP0, WhitePop00 = SHRWHT0N, BlackPop00 = SHRBLK0N, HispPop00 = SHRHSP0N, 
                                        TotalPop10 = TRCTPOP1, WhitePop10 = SHRWHT1N, BlackPop10 = SHRBLK1N, HispPop10 = SHRHSP1N) %>%  
  
  mutate(WhitePopPct70 = (WhitePop70/TotalPop70)*100, BlackPopPct70 = (BlackPop70/TotalPop70)*100, HispPopPct70 = (HispPop70/TotalPop70)*100, WhitePopPct80 = (WhitePop80/TotalPop80)*100, BlackPopPct80 = (BlackPop80/TotalPop80)*100, HispPopPct80 = (HispPop80/TotalPop80)*100, WhitePopPct90 = (WhitePop90/TotalPop90)*100, BlackPopPct90 = (BlackPop90/TotalPop90)*100, HispPopPct90 = (HispPop90/TotalPop90)*100, WhitePopPct00 = (WhitePop00/TotalPop00)*100, BlackPopPct00 = (BlackPop00/TotalPop00)*100, HispPopPct00 = (HispPop00/TotalPop00)*100, WhitePopPct10 = (WhitePop10/TotalPop10)*100, BlackPopPct10 = (BlackPop10/TotalPop10)*100, HispPopPct10 = (HispPop10/TotalPop10)*100)

basePops <- Populations_xtab %>% select(CensusTractID, WhitePopPct2000 = WhitePopPct00, WhitePopPct1990= WhitePopPct90)

white00_LT <- reshape2::melt(basePops, id.vars = "CensusTractID") %>% mutate(Varx = "White Pct", year = str_sub(variable, -4)) %>%
  select(GEOID = CensusTractID, value, year, Varx)

melted_White <- rbind(white10_16, white00_LT)

###############################################################


#Combine Income, Education, Nonwhite, and Home Value into long table

Dep_LT <- rbind(melted_Education, melted_homeval) %>% rbind(., melted_Income) %>% rbind(., melted_White)

saveRDS(Dep_LT, "DependantLT.rds")
