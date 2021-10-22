library(pacman)
p_load(tidycensus, tidyverse, scales,sf, tigris, viridis, ggthemes, ggplot2,
       stringr, dplyr, leaflet, broom, censusapi, acs, lubridate, reshape, ipumsr, SAScii)

#tidycensus
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


#Load variables from tidycensus
vars2010 <- load_variables(2016, "acs5", cache = TRUE)

#Load variables from censusapi
All_Census_Variables <- listCensusMetadata(name= "2016/acs/acs5/subject", type = "variables")

apis <- listCensusApis()


###Set census call variable years and geography ("tract" or "block group")
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016)
geo = "tract"
State = "TN"
County = "Davidson"
masterPath <- "C://School//Research//Thesis//"

#Median Household Income
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

saveRDS(MedInc_LT, paste0(masterPath, "Census_MedIncome", ".rds"))



###2016 Racial HH Incomes 19013B_001E , hispincome = "B19013I_001E")

rentincomevars <- c("B25119_002E", "B25119_003E")
RentOwnIncomes <- get_acs(county = County,geography = geo,  
                                       variables = rentincomevars,
                                       state = State, year = 2017, output = "wide") %>% mutate(pctRenterIncome = B25119_003E/B25119_002E, pctHomeownerincome = 1 + (B25119_003E/B25119_002E))

RenterIncomeMultiplier = median(RentOwnIncomes$pctRenterIncome, na.rm = TRUE)
HomeownerIncomeMultiplier = median(RentOwnIncomes$pctHomeownerincome, na.rm = TRUE)

incvars <- c("B19013B_001E", "B19013A_001E", "B19013I_001E", "B19013D_001E")
varswap <- c("Black", "White", "Hispanic", "Asian")
incvars_lookup <- c("B19013B_001", "B19013A_001", "B19013I_001", "B19013D_001")
swapdf <- data.frame(incvars_lookup,varswap, stringsAsFactors =FALSE)

# MedIncomes<- get_acs(county = County,geography = geo,  
#                              variables = incvars,
#                              state = State, year = loop_year, output = "wide") %>% select(GEOID, contains("001E"))

CountyIncomes17 <- get_acs(county = County, geography = "county",  
                         variables = incvars,
                         state = State, year = 2017) 

JoinedIncomes17 <- left_join(x=CountyIncomes17, y = swapdf, by = c("variable"="incvars_lookup")) %>% select(Race = varswap, YearlyIncome = estimate) %>% mutate(RentMultiplier = RenterIncomeMultiplier, RenterIncome= YearlyIncome*RentMultiplier, RentermonthlyIncome = RenterIncome/12, RenterBudget30 = RentermonthlyIncome*.3, RenterBudget40 = RentermonthlyIncome*.4, HomeMultiplier = HomeownerIncomeMultiplier, HomeownerIncome= YearlyIncome*HomeMultiplier, HomeownermonthlyIncome = HomeownerIncome/12, HomeownerBudget30 = HomeownerIncome*2.5, HomeownerBudget40 = HomeownerIncome*3, BothMonthlyIncome = YearlyIncome/12, BothBudget30 = BothMonthlyIncome*.3, BothBudget40 = BothMonthlyIncome*.4)


#####Renter Budgets
whiteHomeBudget30_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "White") %>% select(HomeownerBudget30))
BlackHomeBudget30_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Black") %>% select(HomeownerBudget30))
HispHomeBudget30_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Hispanic") %>% select(HomeownerBudget30))
whiteHomeBudget40_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "White") %>% select(HomeownerBudget40))
BlackHomeBudget40_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Black") %>% select(HomeownerBudget40))
HispHomeBudget40_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Hispanic") %>% select(HomeownerBudget40))


####Homeowner Budgets
whiteRentBudget30_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "White") %>% select(RenterBudget30))
BlackRentBudget30_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Black") %>% select(RenterBudget30))
HispRentBudget30_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Hispanic") %>% select(RenterBudget30))
whiteRentBudget40_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "White") %>% select(RenterBudget40))
BlackRentBudget40_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Black") %>% select(RenterBudget40))
HispRentBudget40_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Hispanic") %>% select(RenterBudget40))



###Both Budgets
whiteBothBudget30_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "White") %>% select(BothBudget30))
BlackBothBudget30_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Black") %>% select(BothBudget30))
HispBothBudget30_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Hispanic") %>% select(BothBudget30))
whiteBothBudget40_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "White") %>% select(BothBudget40))
BlackBothBudget40_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Black") %>% select(BothBudget40))
HispBothBudget40_17 <- as.numeric(JoinedIncomes17 %>% filter(Race == "Hispanic") %>% select(BothBudget40))



 
MedRent_17 <- get_acs(county = County,geography = geo,  
                   variables = c("B25064_001E","B25077_001E"),
                   state = State, year = 2017, output = "wide") %>% select(GEOID, MedRent = B25064_001E, MedHomeValue = B25077_001E) %>% mutate(WhiteHomethresh30 = whiteHomeBudget30_17, BlackHomethresh30 = BlackHomeBudget30_17, HispHomethresh30 = HispHomeBudget30_17,WhiteHomethresh40 = whiteHomeBudget40_17, BlackHomethresh40 = BlackHomeBudget40_17, HispHomethresh40 = HispHomeBudget40_17, 
                                                                                                                                                WhiteRentthresh30 = whiteRentBudget30_17, BlackRentthresh30 = BlackRentBudget30_17, HispRentthresh30 = HispRentBudget30_17,WhiteRentthresh40 = whiteRentBudget40_17, BlackRentthresh40 = BlackRentBudget40_17, HispRentthresh40 = HispRentBudget40_17,
                                                                                                                                                WhiteBoththresh30 = whiteBothBudget30_17, BlackBoththresh30 = BlackBothBudget30_17, HispBoththresh30 = HispBothBudget30_17,WhiteBoththresh40 = whiteBothBudget40_17, BlackBoththresh40 = BlackBothBudget40_17, HispBoththresh40 = HispBothBudget40_17)


Affordability_17 <- MedRent_17 %>% mutate(WhiteRentAfford = ifelse(WhiteRentthresh30 >= MedRent, "Affordable30", ifelse(WhiteRentthresh40 >= MedRent, "Affordable40", "Unaffordable")), BlackRentAfford = ifelse(BlackRentthresh30 >= MedRent, "Affordable30", ifelse(BlackRentthresh40 >= MedRent, "Affordable40", "Unaffordable")), HispRentAfford = ifelse(HispRentthresh30 >= MedRent, "Affordable30", ifelse(HispRentthresh40 >= MedRent, "Affordable40", "Unaffordable")),
                                              WhiteHomeAfford = ifelse(WhiteHomethresh30 >= MedHomeValue, "Affordable30", ifelse(WhiteHomethresh40 >= MedHomeValue, "Affordable40", "Unaffordable")), BlackHomeAfford = ifelse(BlackHomethresh30 >= MedHomeValue, "Affordable30", ifelse(BlackHomethresh40 >= MedHomeValue, "Affordable40", "Unaffordable")), HispHomeAfford = ifelse(HispHomethresh30 >= MedHomeValue, "Affordable30", ifelse(HispHomethresh40 >= MedHomeValue, "Affordable40", "Unaffordable")),
                                          WhiteBothAfford = ifelse(WhiteBoththresh30 >= MedRent, "Affordable30", ifelse(WhiteBoththresh40 >= MedRent, "Affordable40", "Unaffordable")), BlackBothAfford = ifelse(BlackBoththresh30 >= MedRent, "Affordable30", ifelse(BlackBoththresh40 >= MedRent, "Affordable40", "Unaffordable")), HispBothAfford = ifelse(HispBoththresh30 >= MedRent, "Affordable30", ifelse(HispBoththresh40 >= MedRent, "Affordable40", "Unaffordable")))                                    
                                                    
                                             


Davidson <- tracts(state = "TN", county = "Davidson")
sp_jn2_17 <- geo_join(spatial_data = Davidson, data_frame = Affordability_17, by_sp = "GEOID", by_df = "GEOID", how = "left")

Davidson_SF_17 <- st_as_sf(sp_jn2_17 )


cols <- c("deepskyblue","deepskyblue4","cornsilk")

Hplot_17 <- ggplot(Davidson_SF_17) + geom_sf(aes(fill = factor(HispRentAfford))) + coord_sf() + theme(legend.title = element_blank()) + scale_fill_manual(values = cols, labels = c( "  Affordable (<30% Income)", "  Affordable (<40% Income)", " Unaffordable (>40% Income)")) +  theme(legend.title = element_blank()) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size = 16, colour = "black", hjust = 0)) 


Bplot_17 <- ggplot(Davidson_SF_17) + geom_sf(aes(fill = factor(BlackRentAfford))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = cols, labels = c( "  Affordable (<30% Income)", "  Affordable (<40% Income)", " Unaffordable (>40% Income)")) +  theme(legend.title = element_blank()) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size = 16, colour = "black", hjust = 0))+ theme(legend.position = "none") 

Wplot_17 <- ggplot(Davidson_SF_17) + geom_sf(aes(fill = factor(WhiteRentAfford))) + coord_sf() + theme(legend.position="none")+ scale_fill_manual(values = cols, labels = c( "  Affordable (<30% Income)", "  Affordable (<40% Income)", " Unaffordable (>40% Income)")) +  theme(legend.title = element_blank()) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size = 16, colour = "black", hjust = 0)) + theme(legend.position = "none") 




HHomeplot_17 <- ggplot(Davidson_SF_17) + geom_sf(aes(fill = factor(HispHomeAfford))) + coord_sf() + theme(legend.title = element_blank()) + scale_fill_manual(values = cols, labels = c( "  Affordable (<2.5x Income)", "  Affordable (<3x Income)", " Unaffordable (>3x Income)")) +  theme(legend.title = element_blank()) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size = 16, colour = "black", hjust = 0)) 


BHomeplot_17 <- ggplot(Davidson_SF_17) + geom_sf(aes(fill = factor(BlackHomeAfford))) + coord_sf() + theme(legend.position="none") + scale_fill_manual(values = cols, labels = c( "  Affordable (<2.5x Income)", "  Affordable (<3x Income)", " Unaffordable (>3x Income)")) +  theme(legend.title = element_blank()) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size = 16, colour = "black", hjust = 0))+ theme(legend.position = "none")

WHomeplot_17 <- ggplot(Davidson_SF_17) + geom_sf(aes(fill = factor(WhiteHomeAfford))) + coord_sf() + theme(legend.position="none")+ scale_fill_manual(values = cols, labels = c( "  Affordable (<2.5x Income)", "  Affordable (<3x Income)", " Unaffordable (>3x Income)")) +  theme(legend.title = element_blank()) + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),  axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.title = element_text(size = 16, colour = "black", hjust = 0)) + theme(legend.position = "none") 


gridExtra::grid.arrange(Hplot_17, Bplot_17, Wplot_17,ncol=3, top = grid::textGrob("2016 Housing Affordability", gp=grid::gpar(fontsize=20, font = 3)))


Rentfigure <- ggpubr::ggarrange(Hplot_17, Bplot_17, Wplot_17, ncol = 3, nrow = 1, common.legend = TRUE, legend="right")
ggpubr::annotate_figure(Rentfigure, top = ggpubr::text_grob("Renting Affordability by Race", face = "italic", size = 16))


Housingfigure <- ggpubr::ggarrange(HHomeplot_17, BHomeplot_17, WHomeplot_17, ncol = 3, nrow = 1, common.legend = TRUE, legend="right")
ggpubr::annotate_figure(Housingfigure, top = ggpubr::text_grob("Home Ownership Affordability by Race", face = "italic", size = 16))
