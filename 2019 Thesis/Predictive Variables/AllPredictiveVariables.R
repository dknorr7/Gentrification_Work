#Master Organizer
#David Knorr 
#8/21/18

library(pacman)
p_load(tidyverse, lubridate, ggplot2, pROC,
       tigris, tmap, corrplot, mlbench, caret, Boruta, verification, randomForest, e1071)

file.names <- list.files(pattern = "\\.rds$")

# read in each file in the directory naming it with the interesting bit of the filename
for(i in 1:length(file.names)){
  start.stripped.i <- unlist(strsplit(x = file.names[i], split = 'RDS'))
  obj.name.i <- unlist(strsplit(x = start.stripped.i, split = '\\.'))[1] # escape character before . so it's not treated as a wildcard 
  X.i <- readRDS(file.names[i])
  assign(x = obj.name.i, value = X.i)
  rm(list = c('start.stripped.i', 'obj.name.i', 'X.i'))
  gc()
}

Dependant <- readRDS("Dependant.rds")

Pct_CollegeEd_tract1 <- Pct_CollegeEd_tract  %>% mutate(CensusTractID = as.character(GEOID)) %>% dplyr::select(CensusTractID, year, CollegePct = value)


All_Census <- inner_join(Census_MedianRentValue, Census_MedIncome, by= c('CensusTractID', 'year')) %>%
                inner_join(., Census_MedianHomeValue, by= c('CensusTractID', "year"))  %>%
  inner_join(., Census_MedianBuildingAge, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_MedianPopAge, by= c('CensusTractID', "year")) %>%
  inner_join(., Pct_CollegeEd_tract1, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_MultiUnitPct, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_NonFamilyPct, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_NonWhitePct, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_RentersPct, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_VacantPct, by= c('CensusTractID', "year")) %>%
  inner_join(., Evictions, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_UnemploymentPct, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_PovertyPct, by= c('CensusTractID', "year"))  %>%
  inner_join(., Census_Under18, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_Over65, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_RentBurdened, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_PublicCommute, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_VehicleAvailability, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_PctBlueCollar, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_PercentNewHomes, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_NoCars, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_Sub20minCommute, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_BigUnits, by= c('CensusTractID', "year")) %>%
  inner_join(., Census_TransitCoverage, by= c('CensusTractID', "year")) %>%
  left_join(., Census_HousingAge, by= c('CensusTractID', "year"))
  
  


df <- All_Census[!grepl('variable', names(All_Census))]

Census_Age2010_tract$CensusTractID <- as.character(Census_Age2010_tract$CensusTractID)

saveRDS(df_predout, "C:/School/Research/Predictive Variables/Master/FinalPredictions/x2016Data.rds")
saveRDS(df, "C:/School/Research/Geolytics/x2010data.rds")
saveRDS(df, "All_Census.rds")

census_permit <- left_join(df, Permit_Improvements, by = c("CensusTractID", "year")) %>%
  left_join(., Demolitions, by = c("CensusTractID", "year")) %>%
  left_join(., CodeViolations, by = c('CensusTractID', 'year')) %>% dplyr::select(-variable.x, -variable.y)




# permit_data <- left_join(Demolitions, CodeViolations, by = c('CensusTractID', 'year')) %>% 
#   inner_join(., Permit_Improvements, by= c('CensusTractID', "year")) %>% 
#   inner_join(., Evictions, by= c('CensusTractID', "year")) %>% 
#   inner_join(., All_Census, by= c('CensusTractID', "year")) %>%
#   select(CensusTractID, year, rent, Income, homeValue, nViolations, Demo = value.x, improvements = value.y, evictions = value)

merged_complete <- census_permit[complete.cases(census_permit), ]

DistToDowntown1 <- DistToDowntown %>% mutate(CensusTractID = as.character(GEOID)) %>% dplyr::select(CensusTractID, Dist2Downtown = value)

SpatialVars <- inner_join(ParkCoverage, DistToDowntown1, by = "CensusTractID") %>% inner_join(., Census_TransitCoverage, by = "CensusTractID") %>% dplyr::select(-variable) %>% dplyr::select(CensusTractID, Park_PctCoverage16, Dist2Downtown)

df_predout <- inner_join(df, SpatialVars)

write_rds(SpatialVars, "SpatialVars00.rds")
Joined_all <- left_join(x = census_permit, y = SpatialVars, by = "CensusTractID") 
Joined_all_complete <- Joined_all[complete.cases(Joined_all), ] 

write_rds(Joined_all_complete, "AllVars10to16.rds")

#Rename Geolytics Columns
Geolytics00x <- Geolytics00 %>% dplyr::select(CensusTractID, rent = RentVal00, Income = HH_income00, homeValue = HomeVal00, Pct_MultiUnit = PctMulti00, PctNonFam = pctNonfamily00, PctNonWhite = nonWhite00, PctRenters = Pct_Renter00, PctVacant = pctVacant00, Pct_Unemployed = Unemployment_00, Pct_Poverty = Pov_00, PctCollege = PctCollege00, Under18 = under18_00, Over65x = Over65_00)

Geolytics90x <- Geolytics90 %>% dplyr::select(CensusTractID, rent = RentVal90, Income = HH_income90, homeValue = HomeVal90, Pct_MultiUnit = PctMulti90, PctNonFam = pctNonfamily90, PctNonWhite = nonWhite90, PctRenters = Pct_Renter90, PctVacant = pctVacant90, Pct_Unemployed = Unemployment_90, Pct_Poverty = Pov_90, PctCollege = PctCollege90, Under18 = Under18_90, Over65x = Over65_90)

write_rds(Geolytics90x, "C://School//Research//Predictive Variables//Master//FinalPredictions//Raw90.rds")
write_rds(Geolytics00x, "C://School//Research//Predictive Variables//Master//FinalPredictions//Raw00.rds")

###create Lagged Time Variables
VarList <- names(Joined_all_complete[, !names(Joined_all_complete) %in% c("CensusTractID", "year")])

lagVarNames <- str_c("lag1_", VarList)
vv <- syms(VarList)

xplist <- purrr::map(vv, ~expr(dplyr::lag(!!.x, lag = 1, default = NA))) %>%
  set_names(lagVarNames)

fn <- quo(mutate(., !!!xplist)) %>% rlang::as_function()

lagged_data_2 <- Joined_all_complete %>% arrange(CensusTractID, year) %>% 
  group_by(CensusTractID) %>% fn() %>% ungroup()


for (var in VarList){
  lagVarName <- paste0("lag1", var)
  var <- sym(var)
  laggeddata <- Joined_all_complete %>% arrange(CensusTractID, year) %>% 
    group_by(CensusTractID) %>%
    mutate(!!lagVarName := dplyr::lag(!!var, n = 1, default = NA)) %>%
    ungroup()
}


laggeddata_test <- Joined_all_complete %>% arrange(CensusTractID) %>% mutate(lag1Demos := dplyr::lag(Demos, n = 1, default = NA))

#GeolyticsData
Geolytics90 <- readRDS("Geolytics90.rds")
Geolytics00 <- readRDS("Geolytics00.rds")

Geolytics_all <- inner_join(Geolytics00, Geolytics90)
Geolytics_all$CensusTractID <- as.character(Geolytics_all$CensusTractID)

#########
#Transformations

#Histogram All Variables
my_plots <- lapply(names(Joined_all_complete), function(var_x){
  p <- 
    ggplot(Joined_all_complete) +
    aes_string(var_x)
  
  if(is.numeric(Joined_all_complete[[var_x]])) {
    p <- p + geom_density()
    } 
})

plot_grid(plotlist = my_plots)


percentcolumns <- c("Pct_MultiUnit", "PctNonFam", "PctNonWhite", "PctRenters", 
                    "PctVacant", "Pct_Unemployed", "Pct_Poverty", "PctCollege",
                    "Under18", "Over65x")

all <- names(Joined_all_complete)

Joined_all_complete[percentcolumns] <- log(Joined_all_complete[percentcolumns])



###############
#Calculate Anomolies

yoi <- 2016

avgdata <- Joined_all_complete %>% filter(year < yoi) %>% reshape::melt(id = c("CensusTractID", "year")) %>% 
dplyr::select(variable, value) %>% group_by(variable) %>% summarize(avg = mean(value), SD = sd(value), year = avgyr)

yoidata <-Joined_all_complete %>% filter(year == yoi) %>%
                  reshape::melt(id = c("CensusTractID", "year")) %>%
                  left_join(., avgdata, by = c("variable")) %>% mutate(Anomaly = (value - avg)/ SD) %>% 
                  dplyr::select(CensusTractID, year = year.x, Anomaly, variable) %>%
                  spread(key = variable, value = Anomaly)

assign(paste0("Anomaly",as.character(yoi)), yoidata )

TempDiff <- 







