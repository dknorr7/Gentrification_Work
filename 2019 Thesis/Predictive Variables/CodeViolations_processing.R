library(pacman)
p_load(tidyverse, ggplot2, censusapi, tidycensus, RSocrata, sp, spatialEco, lubridate, tigris, tmap, readxl)

#codeV_data <- read.socrata(url = "https://data.nashville.gov/resource/xqiv-b6yb.json")
codeV_data <- read_xlsx("ComplaintData.xlsx")
tracts_sp <- tracts("TN", county = "Davidson") 

# codeV_data$latitude <- as.numeric(codeV_data$mapped_location.latitude)
# codeV_data$longitude <- as.numeric(codeV_data$mapped_location.longitude)
# 
# codeV_data <- codeV_data[!with(codeV_data,is.na(mapped_location.latitude)& is.na(mapped_location.longitude)),]
# 
# write.csv(codeV_data, "CodeViolationsRaw.csv")
CodeV_10to18 <- codeV_data %>% filter(Year >= 2010 & Year <= 2016) %>% mutate(CensusTractID = paste0("470", CensusTract))
###Joined Code Violation Points to Census Tracts in ArcGIS below is the resultant file of the join
joinedCodeV <- shapefile("TractCodeViolations.shp")
LegitTracts <- tracts_sp@data

ViolationCount <- left_join(x= CodeV_10to18, y = LegitTracts, by = c("CensusTractID" = "GEOID")) %>% filter(is.na(STATEFP) == FALSE) %>% select(CensusTractID, year = Year, Complaints)

CodeViolations$year <- year(as.Date(CodeViolations$date_recei))
TractViolationSummary <- CodeViolations %>% group_by(GEOID, year) %>% summarize(nViolations = n())
TractViolationsFinal <- TractViolationSummary %>% mutate(variable = "Code Violations") 
names(TractViolationsFinal)[names(TractViolationsFinal) == 'GEOID'] <- 'CensusTractID'


#TractViolationsFinal <- dcast(data = TractViolationSummary, GEOID ~ year, value.var = "nViolations")
saveRDS(ViolationCount, "C://School//Research//Predictive Variables//Master//CodeViolations.rds")


##quick map
joined <- geo_join(tracts_sp, TractViolationSummary, "GEOID", "GEOID", how = "inner")

map <- tm_shape(joined) + tm_fill("nViolations", style = "fixed", palette = "Reds",
                                  title = "Code Violations", breaks = c(0, 150, 300, 450, 600, 750, 1000, Inf)) +
  tm_borders()+
  tm_layout(
    inner.margins = c(.06, .06, .06, .06),
    legend.title.size = .75,
    legend.text.size = .5,
    legend.bg.color = "grey",
    legend.bg.alpha = 0,
    legend.width = 0.235)
