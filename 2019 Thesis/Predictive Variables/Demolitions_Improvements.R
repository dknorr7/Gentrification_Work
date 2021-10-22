library(pacman)
p_load(tidyverse, RSocrata, lubridate, tigris, reshape2, readxl, janitor)

#Create Inflation Adjustment Table
monthly_cpi <-
  read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)

monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))

yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2017]


#From ftp (limited to ~2013)
BuildingPermit_raw <- read.socrata(url = "https://data.nashville.gov/resource/p5r5-bnga.json")

#full list dating back to 2010
full_BuildingPermit_raw <- read_excel("Issued Building Permits 2010 to Present.xlsx")
full_BuildingPermit_raw <- clean_names(full_BuildingPermit_raw)

BuildingPermit_raw <- full_BuildingPermit_raw %>% 
              mutate_at(vars(matches("const_cost")), funs(as.integer)) %>% mutate(year_issued = year(date_issued), CensusTractID = paste0("470",census_tract))

TractDemolitions <- BuildingPermit_raw %>% filter(permit_type_description == "Building Demolition Permit") %>%
                        group_by(census_tract, year_issued) %>% summarize(value = n()) %>% mutate(CensusTractID = paste0("470",census_tract))


TractCast<- dcast(TractDemolitions, CensusTractID ~ year_issued)

tracts_sp <- tracts("TN", county = "Davidson")
tracts_df <- tracts_sp@data %>% select(GEOID)

Complete_Tract_Demolitions <- merge(x = tracts_df, y = TractCast, by.x = "GEOID", by.y = "CensusTractID", all.x = TRUE)


Complete_Tract_Demolitions[is.na(Complete_Tract_Demolitions)] <- 0

Tract_Demo_fnl <- Complete_Tract_Demolitions %>% melt(id.vars = "GEOID") %>% select(CensusTractID = GEOID, year = variable, Demos = value) %>% 
                    mutate(variable = "Demolition Permits")
Tract_Demo_fnl$year <- as.numeric(as.character(Tract_Demo_fnl$year))

saveRDS(Tract_Demo_fnl, "C://School//Research//Predictive Variables//Master//Demolitions.rds")


###Tract Improvements

Improvements <- BuildingPermit_raw %>% filter(permit_type_description == "Building Residential - Addition"|permit_type_description == "Building Residential - Rehab") %>%
  group_by(CensusTractID, year_issued) %>% summarize(cost = sum(const_cost))

Improvements_infadj <- merge(x = Improvements, y = yearly_cpi, by.x = 'year_issued', by.y = 'cpi_year', all.x = TRUE) %>%
  mutate(newcost = cost/adj_factor) %>% select (-cpi, - adj_factor, -cost)

ImprovementsCast <-  dcast(Improvements_infadj, CensusTractID ~ year_issued)

Complete_Tract_Improvements <- left_join(x = tracts_df, y = ImprovementsCast, by = c("GEOID" = "CensusTractID"))
Complete_Tract_Improvements[is.na(Complete_Tract_Improvements)] <- 0

Tract_Improvements_fnl <- Complete_Tract_Improvements %>% melt(id.vars = "GEOID") %>% select(CensusTractID = GEOID, year = variable, PermitVal = value) %>% 
  mutate(variable = "Improvement Permit Costs (2017 inf adjusted)")

Tract_Improvements_fnl$year <- as.numeric(as.character(Tract_Improvements_fnl$year))

saveRDS(Tract_Improvements_fnl, "C://School//Research//Predictive Variables//Master//Permit_Improvements.rds")


###Permit Data since 1990
OldBuildingPermits <- read_xlsx("Building Permit Data (2).xlsx") 
colnames(OldBuildingPermits) <- c("CensusTractID", "year", "Permit_Type", "Cost", "Count")

RezImprovements_select <- OldBuildingPermits %>% filter(Permit_Type == "Building Residential - Rehab" | Permit_Type == "Building Residential - Addition" | Permit_Type == "Building Residential - Code Repair" | Permit_Type == "Building Residential - Tenant Finish Out") %>% mutate(constcost = as.numeric(ifelse(Cost == "NULL",0, Cost))) %>% select(-Cost)


YearlyImprovements <- RezImprovements_select%>% group_by(CensusTractID, year) %>% summarize(sumcost = sum(constcost), sumcount = sum(Count))




