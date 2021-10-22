library(pacman)
p_load(tidyverse, readxl, reshape2, lubridate, stringr, tidyr, tidycensus, censusapi, cluster, factoextra)

all_raw <- read.csv('Geolytics/Geolytics_ALL.csv')

outputpath <- "C:/School/ClusteringCities/Nashville Paper/Geolytics"


### Inflation Table
monthly_cpi <-   read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt", skip = 53, header = TRUE)
monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))

yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2019]


Select_geolytics <- all_raw %>% select(CensusTractID = GEO2010, 
                                       HomeVal00= MDVALHS0, RentVal00 = MDGRENT0, HH_income00 = MDHHY0, NonFamilyHH00 = NONFAM0, TotalHHs00 = NUMHHS0, 
                                       HomeVal90= MDVALHS9, RentVal90 = MDGRENT9, HH_income90 = MDHHY9, NonFamilyHH90 = NONFAM9, TotalHHs90 = NUMHHS9) %>%
                                mutate(pctNonfamily00 = 100*(NonFamilyHH00/TotalHHs00), pctNonfamily90 = 100*(NonFamilyHH90/TotalHHs90))


NonFamily <- Select_geolytics %>% select(CensusTractID, pctNonfamily00, pctNonfamily90)

ToAdjust <- Select_geolytics %>% select(-pctNonfamily00, -pctNonfamily90, -NonFamilyHH00, -NonFamilyHH90, -TotalHHs00, -TotalHHs90)

longtblwYears <- melt(ToAdjust, id.vars= "CensusTractID") %>% mutate(Year = ifelse(str_sub(variable, start= -2)=="00", 2000, 1990))

Inf_adjusted <- longtblwYears %>% left_join(x = longtblwYears, y = yearly_cpi, by = c("Year" = "cpi_year")) %>%
  mutate(Adj_value = round(value/adj_factor,0)) %>% select(-adj_factor, -value, -cpi, -Year)

HomeRentIncome_adj_fnl <- Inf_adjusted %>% spread(key = variable, value = Adj_value) 



#Nonwhite calculation
Populations_xtab <- all_raw %>% select(CensusTractID = GEO2010,
                                       TotalPop70 = TRCTPOP7, WhitePop70 = SHRWHT7N, BlackPop70 = SHRBLK7N, HispPop70 = SHRHSP7N, 
                                       TotalPop80 = TRCTPOP8, WhitePop80 = SHRWHT8N, BlackPop80 = SHRBLK8N, HispPop80 = SHRHSP8N, 
                                       TotalPop90 = TRCTPOP9, WhitePop90 = SHRWHT9N, BlackPop90 = SHRBLK9N, HispPop90 = SHRHSP9N, 
                                       TotalPop00 = TRCTPOP0, WhitePop00 = SHRWHT0N, BlackPop00 = SHRBLK0N, HispPop00 = SHRHSP0N, 
                                       TotalPop10 = TRCTPOP1, WhitePop10 = SHRWHT1N, BlackPop10 = SHRBLK1N, HispPop10 = SHRHSP1N) %>%  
  
  mutate(WhitePopPct70 = (WhitePop70/TotalPop70)*100, BlackPopPct70 = (BlackPop70/TotalPop70)*100, HispPopPct70 = (HispPop70/TotalPop70)*100, WhitePopPct80 = (WhitePop80/TotalPop80)*100, BlackPopPct80 = (BlackPop80/TotalPop80)*100, HispPopPct80 = (HispPop80/TotalPop80)*100, WhitePopPct90 = (WhitePop90/TotalPop90)*100, BlackPopPct90 = (BlackPop90/TotalPop90)*100, HispPopPct90 = (HispPop90/TotalPop90)*100, WhitePopPct00 = (WhitePop00/TotalPop00)*100, BlackPopPct00 = (BlackPop00/TotalPop00)*100, HispPopPct00 = (HispPop00/TotalPop00)*100, WhitePopPct10 = (WhitePop10/TotalPop10)*100, BlackPopPct10 = (BlackPop10/TotalPop10)*100, HispPopPct10 = (HispPop10/TotalPop10)*100)

Populations <- Populations_xtab %>% melt(id.vars = "CensusTractID")

Yearlookup <- read.csv("Geolytics/YearLookup.csv")

Tract_PopDemographics <- merge(x = Populations, y = Yearlookup, by.x = "variable", by.y = "Variable")
Tract_PopDemographics$variable = substr(Tract_PopDemographics$variable,1,nchar(as.character(Tract_PopDemographics$variable))-2)

basePops <- Populations_xtab %>% select(CensusTractID, WhitePopPct00, WhitePopPct90) %>%
  mutate(nonWhite00 = 100 - WhitePopPct00, nonWhite90 = 100 - WhitePopPct90) %>%
  select(CensusTractID, nonWhite00, nonWhite90) 



#College degree
Education_raw <- all_raw %>% select(CensusTractID = GEO2010, collegeEd90 = EDUC169,  collegeEd00 = EDUC160, Total90 = EDUCPP9, Total00 = EDUCPP0) %>%
  mutate(PctCollege90 = ((collegeEd90/Total90)*100), PctCollege00 = ((collegeEd00/Total00)*100)) %>% 
  select(CensusTractID, PctCollege90, PctCollege00)


# Rent_Own <- all_raw %>% select(CensusTractID = GEO2010, Renters70 = RNTOCC7, Owners70 = OWNOCC7, Renters80 = RNTOCC8, Owners80 = OWNOCC8, Renters90 = RNTOCC9, Owners90 = OWNOCC9, Renters00 = RNTOCC0, Owners00 = OWNOCC0, Renters10 = RNTOCC1A, Owners10 = OWNOCC1A) %>% 
#   mutate(TotalHH70 = Renters70 + Owners70, TotalHH70 = Renters70 + Owners70, TotalHH80 = Renters80 + Owners80, TotalHH90 = Renters90 + Owners90, TotalHH00 = Renters00 + Owners00, TotalHH10 = Renters10 + Owners10, Pct_Renter70 = (Renters70/TotalHH70)*100, Pct_Renter80 = (Renters80/TotalHH80)*100, Pct_Renter90 = (Renters90/TotalHH90)*100, Pct_Renter00 = (Renters00/TotalHH00)*100, Pct_Renter10 = (Renters10/TotalHH10)*100) %>%
#   select(CensusTractID, Pct_Renter90, Pct_Renter00)
# 
# Vacant <- all_raw %>% select(CensusTractID = GEO2010, OCCHU7, OCCHU8, OCCHU9, OCCHU0, TOTHSUN7, TOTHSUN8, TOTHSUN9, TOTHSUN0) %>%
#   mutate(pctVacant00 = 100*((TOTHSUN0-OCCHU0)/TOTHSUN0),pctVacant90 = 100*((TOTHSUN9-OCCHU9)/TOTHSUN9)) %>%
#   select(CensusTractID, pctVacant00, pctVacant90)
# 
Multiunit <- all_raw %>% select(CensusTractID = GEO2010, TOTHSUN7, TTUNIT17, TTUNIT27, TOTHSUN8, TTUNIT18, TTUNIT28, TOTHSUN9, TTUNIT19, TTUNIT29, TOTHSUN0, TTUNIT10, TTUNIT20) %>%
   mutate(single90 = TTUNIT19 + TTUNIT29, single00 = TTUNIT10 + TTUNIT20, PctMulti90 = 100 *((TOTHSUN9-single90)/TOTHSUN9), PctMulti00 = 100 *((TOTHSUN0-single00)/TOTHSUN0)) %>%
   select(CensusTractID, PctMulti90, PctMulti00)
# 
# Age <- all_raw %>% select(CensusTractID = GEO2010,CHILD9, CHILD0, OLD9, OLD0) %>% 
#   mutate(Under18_90 = 100*CHILD9, under18_00 = 100*CHILD0, Over65_90 = 100*OLD9, Over65_00 = 100*OLD0) %>%
#   select(-CHILD9, -CHILD0, -OLD9, -OLD0)
# 
# Poverty <- all_raw %>% select(CensusTractID = GEO2010, Poverty_90 = POVRAT9, Poverty_00 = POVRAT0) %>% mutate(Pov_90 = Poverty_90*100, Pov_00 = Poverty_00*100)%>%
#   select(-Poverty_90, -Poverty_00)
# 
# Unemployed <- all_raw %>% select(CensusTractID = GEO2010, UNEMPRT9, UNEMPRT0) %>%
#   mutate(Unemployment_90 = 100*UNEMPRT9, Unemployment_00 = 100*UNEMPRT0) %>% select(-UNEMPRT9, -UNEMPRT0)
# 
# Car <- all_raw %>% select(CensusTractID = GEO2010, NOCAR0,OCCHU0, CAR30, NOCAR9, CAR39, OCCHU9) %>% mutate(PctNoCar00 = 100*(NOCAR0/OCCHU0), PctNoCar90 = 100*(NOCAR9/OCCHU9), Pct3Cars00 = 100*(CAR30/OCCHU0), Pct3Cars90 = 100*(CAR39/OCCHU9)) %>% select(CensusTractID,PctNoCar90, Pct3Cars90, PctNoCar00, Pct3Cars00)
# 
# PubTranspo <- all_raw %>% select(CensusTractID = GEO2010, TRVLPB0N, total00 = WRCNTY0D, TRVLPB9N, total90 = WRCNTY9D) %>% mutate(PctPubTranspo90 = 100*(TRVLPB9N/total90), PctPubTranspo00 = 100*(TRVLPB0N/total00)) %>% select(CensusTractID, PctPubTranspo90, PctPubTranspo00) 
# 
# Commute <- all_raw %>% select(CensusTractID = GEO2010, total00 = WRCNTY0D, total90 = WRCNTY9D, COMMUT29, COMMUT20) %>% mutate(Pct_Sub20MinCommunte90 = 100*(COMMUT29/total90), Pct_Sub20MinCommunte00 = 100*(COMMUT20/total00)) %>% select(CensusTractID, Pct_Sub20MinCommunte90, Pct_Sub20MinCommunte00)
# 
# BlueCollar <- all_raw %>% select(CensusTractID = GEO2010, Total00 = INDEMP0, OCC50, OCC60, OCC70,Total90 = INDEMP9, OCC59, OCC69, OCC79) %>% mutate(PctBlue90 = ((OCC59 + OCC69 + OCC79)/Total90)*100, PctBlue00 = ((OCC50 + OCC60 + OCC70)/Total00)*100) %>% select(CensusTractID, PctBlue90, PctBlue00)
# 
# Wellfare <- all_raw %>% select(CensusTractID = GEO2010, WELFARE9, WELFARE0) %>% mutate(PctWelfare90 = WELFARE9 *100, PctWelfare00 = WELFARE0 *100) %>% select(CensusTractID, PctWelfare90, PctWelfare00)
# 
# Units <- all_raw %>% select(CensusTractID = GEO2010, total90 = TOTHSUN9, plus5units90 = TTUNIT59, total00= TOTHSUN0, plus5units00 = TTUNIT50) %>% mutate(BigUnit90 = 100*(plus5units90/total90), BigUnit00 = 100*(plus5units00/total00)) %>% select(CensusTractID, BigUnit90, BigUnit00)
# 
# Married <- all_raw %>% select(CensusTractID = GEO2010, totalfam00 = FAVINC0D, marriedkids00 = MCWKID0, totalfam90 = FAVINC9D, marriedkids90 = MCWKID9) %>% mutate(Pct_MarriedKids90 = 100*(marriedkids90/totalfam00), Pct_MarriedKids00 = 100*(marriedkids90/totalfam90)) %>% select(CensusTractID, Pct_MarriedKids90, Pct_MarriedKids00)
# 
# PctMobileUnits <- all_raw %>% select(CensusTractID= GEO2010, mobunits90 = TTUNITM9 , mobunits00 = TTUNITM0, Total90 = TOTHSUN9, Total00 = TOTHSUN0) %>% mutate(PctMobileUnits90 = 100*(mobunits90/Total90), PctMobileUnits00 = 100*(mobunits00/Total00)) %>% select(CensusTractID, PctMobileUnits90, PctMobileUnits00)
# 
# RentBurdened <- all_raw %>% select(CensusTractID = GEO2010, R49PI0, R50PI0, totalrenters00 = RNTOCC0  ) %>% mutate(Pct_RentBurdened00 = 100*((R49PI0+R50PI0)/totalrenters00)) %>% select(CensusTractID, Pct_RentBurdened00)
# 
# oldpermits <- readxl::read_xlsx("Building Permit Data90.xlsx")
#
# HousingAge2000 <- all_raw %>% select(GEO2010, total = TOTHSUN0, new1 = BLTYR000, new2 = BLTYR980, new3 =  BLTYR940, new4 = BLTYR890, new5 = BLTYR790, old = BLTYR690, old2 = BLTYR590, old3 = BLTYR490, old4 = BLTYR390) %>% mutate(new5x = new5/2, oldx = new5/2) %>% select(-new5) %>% mutate(pctNewHome2000 = (((new1 + new2 + new3 + new4 +new5x)/total)*100), jnid = as.character(GEO2010)) %>% select(CensusTractID = jnid, pctNewHome2000)


#join Dependant Variables 

All_Geolytics <- inner_join(HomeRentIncome_adj_fnl, Education_raw, by= 'CensusTractID') %>%
  inner_join(., Multiunit, by= 'CensusTractID') 
  

saveRDS(All_Geolytics, paste0(outputpath,"/UpdatedPredVars.rds"))

LT_Geolytics <- All_Geolytics %>% melt(id.vars = 'CensusTractID') %>% 
  mutate(Year = ifelse(str_sub(variable, start= -2)=="00", 2000, 1990)) 

LT_Geolytics$variable <- str_sub(LT_Geolytics$variable, 1, str_length(LT_Geolytics$variable)-2)

Geolytics90 <- All_Geolytics %>% select(CensusTractID, HomeVal90, RentVal90, HH_income90, PctCollege90,
                                        Pct_Renter90, pctVacant90, PctMulti90, pctNonfamily90, nonWhite90, Under18_90,
                                        Over65_90, Pov_90, Unemployment_90)

Geolytics00 <- All_Geolytics %>% select(CensusTractID, HomeVal00, RentVal00, HH_income00, PctCollege00,
                                        Pct_Renter00, pctVacant00, PctMulti00, pctNonfamily00, nonWhite00, under18_00,
                                        Over65_00, Pov_00, Unemployment_00)

write_rds(Geolytics90, file.path(outputpath, "Geolytics_Dependant00.rds"))
write_rds(Geolytics00, file.path("C:/School/Research/Predictive Variables/DependantVariable", "Geolytics00.rds"))

