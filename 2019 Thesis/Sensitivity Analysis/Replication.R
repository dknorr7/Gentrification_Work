library(pacman)
p_load(tidyverse, feather, pcaPP, tigris, factoextra, cluster, gridExtra)

rawall <- readRDS("AllVars10to16.rds")
rawdf90 <- readRDS("Geolytics90.rds")
rawdf00 <- readRDS("Geolytics00.rds")



D <- readRDS("DistancetoDowntown.rds")
medD <- median(D$value)
InnerCity <- D %>% filter(value <= medD)
#InnerCity <- D    ##to see effect on removing inner city requirements

medIncome<- median(rawdf90$HH_income90)
LowIncome <- rawdf90 %>% filter(HH_income90 <= medIncome) %>% dplyr::select(CensusTractID, HH_income90)

B_Age90 <- readRDS("BuildingAge90.rds")
medBage <- median(B_Age90$propNew)
Elligible_BAge <- B_Age90 %>% filter(propNew <= medBage) %>% dplyr::select(CensusTractID, propNew)

Potential <- inner_join(InnerCity, LowIncome) %>% inner_join(., Elligible_BAge) %>% mutate(Potential = "Potential")

Ed_Change <- data.frame(rawdf90$CensusTractID, rawdf90$PctCollege90, rawdf00$PctCollege00) %>% mutate(pctCollegeChange = 100 * ((rawdf00.PctCollege00 - rawdf90.PctCollege90) / rawdf90.PctCollege90)) %>% dplyr::select(CensusTractID = rawdf90.CensusTractID, pctCollegeChange )

medED <- median(Ed_Change$pctCollegeChange)

Ed_Gent <- Ed_Change %>% filter(pctCollegeChange >= medED)

HomeVal_change <- data.frame(rawdf90$CensusTractID, rawdf90$HomeVal90, rawdf00$HomeVal00) %>% mutate(HomeValChange = 100*((rawdf00.HomeVal00 - rawdf90.HomeVal90)/ rawdf90.HomeVal90), Increased = ifelse(HomeValChange >= 0, "Increase", "Decreased")) %>% dplyr::select(CensusTractID = rawdf90.CensusTractID, Increased, HomeValChange) %>% filter(Increased == "Increase")

Gent1 <- inner_join(Potential, Ed_Gent) %>% inner_join(., HomeVal_change) %>% mutate(Gent = "Gentrified")


GENTP50 <- left_join(D, Potential, "CensusTractID") %>% left_join(., Gent1, "CensusTractID") %>% dplyr::select(CensusTractID, Potential.x, Gent) %>% mutate(Fillx = paste(Potential.x, Gent))

###MAP
#Davidson <- tracts("TN", "Davidson")
Davidson@data$JOINID = as.character(Davidson@data$GEOID)
sp_jn2 <- geo_join(spatial_data = Davidson, data_frame = GENTP50, by_sp = "JOINID", by_df = "CensusTractID", how = "left")

Davidson_SF <- st_as_sf(sp_jn2)

x50 <- ggplot(Davidson_SF) + geom_sf(aes(fill = Fillx)) + coord_sf() + labs(x="", y = "", subtitle = "Lance Freeman's 50th percentile Definition") + theme(legend.title = element_blank()) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0)) + scale_fill_manual(values = c("grey", "tomato", "steelblue"), labels = c("  Inelligibile", "  Gentrified", "  Elligible, Not Gentrified")) + theme(legend.position = "none")



##################40 percentile

medD <- median(D$value)
InnerCity <- D %>% filter(value <= medD)

Income40 <- as.numeric(quantile(rawdf90$HH_income90,.4)) 
LowIncome40 <- rawdf90 %>% filter(HH_income90 <= Income40) %>% dplyr::select(CensusTractID, HH_income90)

B_Age90 <- readRDS("BuildingAge90.rds")
medBage40 <- as.numeric(quantile(B_Age90$propNew,.4))
Elligible_BAge40 <- B_Age90 %>% filter(propNew <= medBage40) %>% dplyr::select(CensusTractID, propNew)

Potential40 <- inner_join(InnerCity, LowIncome40) %>% inner_join(., Elligible_BAge40) %>% mutate(Potential = "Potential")

Ed_Change <- data.frame(rawdf90$CensusTractID, rawdf90$PctCollege90, rawdf00$PctCollege00) %>% mutate(pctCollegeChange = 100 * ((rawdf00.PctCollege00 - rawdf90.PctCollege90) / rawdf90.PctCollege90)) %>% dplyr::select(CensusTractID = rawdf90.CensusTractID, pctCollegeChange )

ED40 <- as.numeric(quantile(Ed_Change$pctCollegeChange,.4))

Ed_Gent40 <- Ed_Change %>% filter(pctCollegeChange >= ED40)

HomeVal_change <- data.frame(rawdf90$CensusTractID, rawdf90$HomeVal90, rawdf00$HomeVal00) %>% mutate(HomeValChange = 100*((rawdf00.HomeVal00 - rawdf90.HomeVal90)/ rawdf90.HomeVal90), Increased = ifelse(HomeValChange >= 0, "Increase", "Decreased")) %>% dplyr::select(CensusTractID = rawdf90.CensusTractID, Increased, HomeValChange) %>% filter(Increased == "Increase")

Gent40 <- inner_join(Potential40, Ed_Gent40) %>% inner_join(., HomeVal_change) %>% mutate(Gent = "Gentrified")


GENTP <- left_join(D, Potential40, "CensusTractID") %>% left_join(., Gent40, "CensusTractID") %>% dplyr::select(CensusTractID, Potential.x, Gent) %>% mutate(Fillx = paste(Potential.x, Gent))

###MAP
#Davidson <- tracts("TN", "Davidson")
Davidson@data$JOINID = as.character(Davidson@data$GEOID)
sp_jn40 <- geo_join(spatial_data = Davidson, data_frame = GENTP, by_sp = "JOINID", by_df = "CensusTractID", how = "left")

Davidson_SF40 <- st_as_sf(sp_jn40)

x40 <- ggplot(Davidson_SF40) + geom_sf(aes(fill = Fillx)) + coord_sf() + labs(x="", y = "", subtitle = "Lance Freeman's 40th percentile Definition") + theme(legend.title = element_blank()) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0)) + scale_fill_manual(values = c("grey", "tomato", "steelblue"), labels = c("  Ineligibile", "  Gentrified", "  Eligible, Not Gentrified"))

grid.arrange(x50, x40, ncol = 2, nrow = 1, top = textGrob("Gentrification Sensitivity 1990 -2000", gp=gpar(fontsize=20, font = 3)))

figure <- ggpubr::ggarrange(x50, x40, ncol = 2, nrow = 1, common.legend = TRUE, legend="right")
annotate_figure(figure, top = text_grob("Gentrification Sensitivity 1990 - 2000", face = "bold", size = 16))


##Counts
n40Potential <- GENTP %>% filter(Potential.x == "Potential")
n40Gentrified <- GENTP %>% filter(Gent == "Gentrified")

n50Potential <- GENTP50 %>% filter(Potential.x == "Potential")
n50Gentrified <- GENTP50 %>% filter(Gent == "Gentrified")



###############KANSAS GENTRIFICATION#################

p_load(tidycensus, tidyverse, scales,sf, tigris, viridis, ggthemes, ggplot2,
       stringr, leaflet, broom, censusapi, acs, lubridate, reshape)

census_api_key('e1bf01270616e86ab00da4cd8e15c94be3f13bd3', overwrite = TRUE, install = TRUE)
## censusapi
Sys.setenv(CENSUS_KEY="e1bf01270616e86ab00da4cd8e15c94be3f13bd3", overwrite = TRUE)
# Reload .Renviron

#acs key set
api.key.install("e1bf01270616e86ab00da4cd8e15c94be3f13bd3")

readRenviron("~/.Renviron")


#Create inflation adjustment table
monthly_cpi <-
  read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)

monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))

yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2017]

years <- 2016
geo = "tract"
State = "KS"
County = "Shawnee"

foo <- tibble()
for (loop_year in years) {
  message("Getting data for ", loop_year)
  acs_data <- get_acs(geography = geo, table = "B19013", state = State, county = County, 
                      year = loop_year, cache_table = TRUE) %>%
    mutate(year = loop_year)
  foo <- bind_rows(foo, acs_data)
}

#inflation adjustment
MedHHInc_adj <- merge(x = foo, y = yearly_cpi, by.x = "year", by.y = "cpi_year")

MedInc_LT <- MedHHInc_adj %>% mutate(variable = paste("Median HH Income- ", geo), inf_adj_value = estimate/adj_factor) %>%
  select(CensusTractID = GEOID, variable, year, MedIncome = inf_adj_value)
