library(pacman)
library(censusapi)
library(acs)
p_load(tidycensus, tidyverse, scales,sf, tigris, viridis, ggthemes, ggplot2,
       stringr, dplyr, leaflet, broom, censusapi, acs, plyr, corrplot, caret, e1071, readxl, lubridate)

#tidycensus
census_api_key('e1bf01270616e86ab00da4cd8e15c94be3f13bd3', overwrite = TRUE, install = TRUE)
## censusapi
Sys.setenv(CENSUS_KEY="e1bf01270616e86ab00da4cd8e15c94be3f13bd3", overwrite = TRUE)
# Reload .Renviron
readRenviron("~/.Renviron")

dist2downtown <- readRDS("C:/School/Research/DistanceAnalysis/Dist2Downtown.rds")

#import and fix col classes for legal propety information (Legal info and includes Landuse description)
raw_legal <- read_xlsx("NameAddressLegal_2018-05-31_Excel.xlsx") %>%
  mutate_at(vars(matches("LandAppraisal|ImprovementsAppraisal|TotalAppraisal|LandAssessment|ImprovementsAssessment|TotalAssessment")), funs(as.integer))

raw_legal$Property_DocumentDate <- as.Date(raw_legal$Property_DocumentDate, "%m/%d/%Y")
raw_legal$Owner_DateAcquired <- as.Date(raw_legal$Owner_DateAcquired, "%m/%d/%Y")
raw_legal$CensusTractID <- paste0("470", raw_legal$CensusBlock)

Legal <- merge(y = raw_legal, x = dist2downtown, by.x = "GEOID", by.y = "CensusTractID")
Legal <- Legal %>% mutate(year = year(Owner_DateAcquired))

raw_sales <- read.delim("SalesAndOwnershipHistory_2018-05-31_Text.txt", sep = ",") %>% 
  mutate_at(vars(matches("UserAccount|DateAcquired|OwnerInstrument|OwnerName|OwnerAddress1|OwnerAddress2|OwnerCity|OwnerState|OwnerZip|OwnerCountry|LocationAddress|LocationCity|LocationZip|CouncilDistrict|TaxDistrict|CensusBlock")), funs(as.character))

raw_sales$DateAcquired1 <- substr(raw_sales$DateAcquired,1,nchar(raw_sales$DateAcquired)-8)
raw_sales$DateAcquired <- as.Date(raw_sales$DateAcquired1,"%m/%d/%Y")

Sales <- raw_sales %>% mutate(Year = year(DateAcquired), CensusTractID = paste0("470",CensusBlock))
SalesDist <- merge(x = Sales, y = dist2downtown, by.x = "CensusTractID", by.y = "GEOID")

TractSales <- Legal %>% filter(LandUse == "SINGLE FAMILY" & OwnerSalePrice >1 & year > 2010) %>% group_by(GEOID) %>% summarise(avgSalePrice = mean(OwnerSalePrice))

#census Values
foo12 <- tibble()
for (loop_year in c(2016)) {
  message("Getting data for ", loop_year)
  acs_data12 <- get_acs(geography = "tract", table = "B25077", state = "Tennessee", county = "Davidson",
                        year = loop_year, cache_table = TRUE, output = "wide") %>%
    mutate(year = loop_year) %>%
    select(GEOID, MedianHomeVal = B25077_001E, year)
  foo12 <- bind_rows(foo12, acs_data12)
}

HomeValT <- foo12 %>% mutate(year = str_c("HomeVal_estimate_", year)) %>% select(GEOID, MedianHomeVal, year) %>%
  spread(key = year, value = MedianHomeVal)

mergedAssessor <- merge(x = TractSales, y = HomeValT, by.x = "GEOID", by.y = "GEOID")
meltedmerged <- melt(mergedAssessor, id = "GEOID")
ggplot(meltedmerged, aes(x = GEOID, y = value, group = variable)) + geom_line(aes(color = variable)) 