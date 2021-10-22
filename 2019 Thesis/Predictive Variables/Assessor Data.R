library(pacman)
p_load(tidyverse, ggplot2, readxl, lubridate, tigris, tmap, RCurl, data.table)

#create inflation adjustment table
monthly_cpi <-
  read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)

monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))

yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2017]


#Read in tract distances to downtown
dist2downtown <- readRDS("C:/School/Research/DistanceAnalysis/Dist2Downtown.rds")
dist2downtown_final <- dist2downtown %>% select(CensusTractID = GEOID, value = avgDist) %>% mutate(Variable = "Distance to Downtown (Euclidean)", year = 2018)
saveRDS(dist2downtown_final, "C://School//Research//Predictive Variables//Master//DistancetoDowntown.rds")


#Read in Legal Data from County Assessor FTP (updated ~weekly)
legal_url<- "ftp://99pa01:G9*vpD66u1@ftp3.nashville.gov/AssessorPublic/Name%20Address%20Legal%20Data/NameAddressLegal_2018-10-31_Excel.zip"
temp <-"Legal_data.zip"
legdload <- download.file(legal_url, temp)

raw_legal <- read_xlsx(unzip(temp)) %>%
  mutate_at(vars(matches("LandAppraisal|ImprovementsAppraisal|TotalAppraisal|LandAssessment|ImprovementsAssessment|TotalAssessment")), funs(as.integer))

raw_legal$Property_DocumentDate <- as.Date(raw_legal$Property_DocumentDate, "%m/%d/%Y")
raw_legal$Owner_DateAcquired <- as.Date(raw_legal$Owner_DateAcquired, "%m/%d/%Y")
raw_legal$yearAcquired <- year(raw_legal$Owner_DateAcquired)
raw_legal$CensusTractID <- paste0("470", raw_legal$CensusBlock)

raw_legal_infl <- merge(raw_legal, yearly_cpi, by.x = "yearAcquired", by.y = "cpi_year") %>% mutate(adj_SalePrice = OwnerSalePrice/adj_factor)

Legal <- merge(y = raw_legal, x = dist2downtown, by.x = "GEOID", by.y = "CensusTractID")




#import and fix col classes of Roll Data (Information on physical property)

Roll_url<- "ftp://99pa01:G9*vpD66u1@ftp3.nashville.gov/AssessorPublic/Roll%20Data/SingleRecordPerParcel_2018-10-02_Excel.zip"
temp2 <-"RollSales_data.zip"
RollSales <- download.file(Roll_url, temp2)

raw_roll_single_record <- read_xlsx(unzip(temp2)) %>% 
  mutate_at(vars(matches("Land_Appraisal|Improvements_Appraisal|Total_Appraisal|Land_Assessment|Total_Assessment|Improvements_Assessment|Fixtures")), funs(as.integer))

Roll_data <- merge(x = raw_roll_single_record, y = Legal, by = "UserAccount")


##sandbox
#price per sq foot (residential)
Residential_PPSF <- Roll_data%>% filter(LandUseFullDescription == "SINGLE FAMILY" &  Acrage >0 & sf_finished > 0) %>% 
  mutate(PPSF = Improvements_Appraisal/sf_finished, PPA = Land_Appraisal/Acrage)

#plot residential price per sq foot by distance to downtown
res <- Residential_PPSF %>% group_by(GEOID, avgDist) %>% summarize(meanPPSF = mean(PPSF)) 
plot(x = res$avgDist, y = res$meanPPSF)

#plot residential price per sq acre by distance to downtown
acs <- Residential_PPSF %>% group_by(GEOID, avgDist) %>% summarize(meanPPA = mean(PPA))
plot(x = acs$avgDist, y = acs$meanPPA, ylim = c(0,2000000))

#plot residential price per sq foot by building age
age <- Residential_PPSF %>% group_by(yearbuilt_building) %>% summarize(avgyearlyprice = mean(PPSF))
plot(age, xlim = c(1800, 2020))

cortestdata <- Residential_PPSF %>% select(TotalAppraisal, Acres, avgDist, FinishedSqFt, yearbuilt_building, baths, bedroomsunits_building)
cor(cortestdata)

#linear regression of total appraisal using physical characteristics
linmod <- lm(TotalAppraisal ~ Acres + avgDist + FinishedSqFt + yearbuilt_building + baths + bedroomsunits_building, data = Residential_PPSF)
summary(linmod)


#Examine Physical Depreciation Categorical Variable (it makes sense)
physical <- Residential_PPSF %>% group_by(Phys_Depreciation) %>% summarize(avg_PPSF = mean(PPSF))
positions <- c("Excellent", "Very Good", "Good", "Average", "Fair", "Poor", "Very Poor", "Dilapidated", "NA")

ggplot(physical, aes(x = Phys_Depreciation, y = avg_PPSF)) + geom_bar(stat = "identity") + scale_x_discrete(limits = positions) +
  labs(title = "Price per sq. foot vs. Physical Status", x = "Physical Class", y = "Average Price Per Sq Foot (finished)") +
  coord_flip() + labs(caption = "Source: Nashville Tax Assessor", size = .5)

physical_loc_setup <- Residential_PPSF %>% group_by(Phys_Depreciation, GEOID) %>% summarize(tract_count = n())  
physical_fnl <- physical_loc_setup %>% group_by(GEOID) %>% mutate(total = sum(tract_count), pct = (tract_count/total)*100) 

#dummy df to limit plot output (CensusID is categorical)
dummy <- as.data.frame(unique(physical_fnl$GEOID)) %>% mutate(numid = seq(1:157)) 
colnames(dummy) <- c("geoid","numid")

#get columns in right order for plot
physical_fnl$GEOID <- as.character(physical_fnl$GEOID)
physical_fnl <- merge(x = physical_fnl, y = dummy, by.x = "GEOID", by.y = "geoid")
physical_fnl$Phys_Depreciation <- reorder.factor(physical_fnl$Phys_Depreciation, new.order = positions)
physical_fnl <- physical_fnl %>% arrange(Phys_Depreciation)


#plot staked bar chart of tracts
first50 <- ggplot(data = filter(physical_fnl, numid <=50), aes(x = GEOID, y = pct, fill = Phys_Depreciation)) + geom_bar(position = "fill", stat = "identity") +
  coord_flip() + scale_fill_brewer(palette = "RdYlBu") + theme(axis.text = element_text(size = 6)) 
  
second50 <- ggplot(data = filter(physical_fnl, numid >=50 & numid <=100), aes(x = GEOID, y = pct, fill = Phys_Depreciation)) + geom_bar(position = "fill", stat = "identity") +
  coord_flip() + scale_fill_brewer(palette = "RdYlBu") + theme(axis.text = element_text(size = 6))

third50 <- ggplot(data = filter(physical_fnl, numid >= 100), aes(x = GEOID, y = pct, fill = Phys_Depreciation)) + geom_bar(position = "fill", stat = "identity") +
  coord_flip() + scale_fill_brewer(palette = "RdYlBu") + theme(axis.text = element_text(size = 6))

#Reformat Building Quality to Long Table Format 
physical_long <- physical_fnl %>% dcast(GEOID ~ Phys_Depreciation, value.var = "pct") 
physical_long[is.na(physical_long)] <- 0
colnames(physical_long) <- c("GEOID", "Excellent", "Very_Good", "Good", "Average", "Fair", "Poor", "Very_Poor", "Dilapidated", "NA")
physical_long_adj <- physical_long %>% mutate(value = Fair + Poor + Very_Poor + Dilapidated, variable = "Pct of Single Family Homes of below average quality") %>%
                        select(CensusTractID = GEOID, variable, value)

saveRDS(physical_long_adj, "C://School//Research//Predictive Variables//Master//TractHousingQuality.rds")

#Find Mean age of homes
mean_Age <- Residential_PPSF %>% group_by(GEOID) %>% summarize(meanAge = round(mean(yearbuilt_building),0)) %>%
              mutate(variable = "Average Age of Single Family Home") %>% select(CensusTractID= GEOID, value= meanAge, variable)

saveRDS(mean_Age, "C://School//Research//Predictive Variables//Master//AvgHomeAge.rds")


#import and fix raw sales col classes (Information on Sales Transactions)
sales_url <- read.delim("ftp://99pa01:G9*vpD66u1@ftp3.nashville.gov/AssessorPublic/Sales%20and%20Ownership%20History/SalesAndOwnershipHistory_2018-10-31_Text.zip", sep = ",") 

sales_url <- "ftp://99pa01:G9*vpD66u1@ftp3.nashville.gov/AssessorPublic/Sales%20and%20Ownership%20History/SalesAndOwnershipHistory_2018-10-31_Text.zip"

temp3 <-"Sales_data.zip"
RollSales <- download.file(sales_url, temp3)

raw_sales <- read.delim(unzip(temp3), sep=",")
raw_sales <- raw_sales %>% 
  mutate_at(vars(matches("UserAccount|DateAcquired|OwnerInstrument|OwnerName|OwnerAddress1|OwnerAddress2|OwnerCity|OwnerState|OwnerZip|OwnerCountry|LocationAddress|LocationCity|LocationZip|CouncilDistrict|TaxDistrict|CensusBlock")), funs(as.character))

raw_sales$DateAcquired1 <- substr(raw_sales$DateAcquired,1,nchar(raw_sales$DateAcquired)-8)
raw_sales$DateAcquired <- as.Date(raw_sales$DateAcquired1,"%m/%d/%Y")

Legal_extract<- raw_legal_infl %>% select(UserAccount, LandUse, ClassCodes)
Sales <- raw_sales %>% mutate(Year = year(DateAcquired), CensusTractID = paste0("470",CensusBlock))
Sales_Landuse <- merge(x = Sales, y = Legal_extract, by.x = "UserAccount", by.y = "UserAccount" )
Landuse_lookup <- Sales_Landuse %>% select(LocationAddress, LandUse, Year) %>% unique()


#drop Owner info because they were causing duplicates (refinancing, weird transactions, etc.)
Sales_deduped <- Sales_Landuse %>% filter(ClassCodes == "R") %>% select(-OwnerName, -UserAccount, -OwnerAddress1, -OwnerInstrument, -OwnerAddress2, -OwnerCity, -OwnerCity, -OwnerState, -OwnerZip) 
SalesDist <- merge(x = Sales, y = dist2downtown, by.x = "CensusTractID", by.y = "GEOID")



distplotdata <- SalesDist %>% filter(Year >= 1950) %>% group_by(Year) %>% summarize(meandist = mean(avgDist)) %>% 
  mutate(miles = meandist*.000621371)

plot(distplotdata)

ggplot(data = distplotdata, aes(x=Year, y = miles)) +geom_point() +  geom_smooth() + labs(title = "Residential Property Transactions\n Mean Distance to Downtown", y = "Miles", caption = "Source: Davidson County Tax Assessor")+
  theme(text = element_text(size=15)) +
  theme(plot.title = element_text(size = 15, face = "bold"))

Legal_deduped <- raw_legal_infl %>% filter(!is.na(adj_SalePrice ) & adj_SalePrice  > 10 & LandUse == "SINGLE FAMILY") %>% select(-OwnerName, -UserAccount, -OwnerAddress, -OwnerInstrument, -OwnerCity, -OwnerStateCode, -OwnerPostalCode) %>% unique()

Tract_Sales <- Sales_deduped %>% filter(Year >=1990) %>% group_by(CensusTractID) %>% summarize(value = n()) %>% mutate(variable = " Number of Home Sales in Tract")
saveRDS(Tract_Sales, "C://School//Research//Predictive Variables//Master//TractSales.rds")

Tract_Sale_medPrice <- Legal_deduped %>% filter(yearAcquired >=1990) %>% group_by( CensusTractID) %>% summarize(value = median(adj_SalePrice), sale_count = n()) %>% mutate(variable = "Median Price of Tract Sales")
saveRDS(Tract_Sales, "C://School//Research//Predictive Variables//Master//TractSales.rds")

kensplotdata <- Sales %>% filter(Year >=1990) %>% group_by(Year) %>% summarize(count1 = n()) 
plot(kensplotdata)




#quick Map
tracts_sp <- tracts("TN", county = "Davidson") 
joined <- geo_join(tracts_sp, Tract_Sale_medPrice, "GEOID", "CensusTractID", how = "inner")

map <- tm_shape(joined) + tm_fill("value", style = "fixed", palette = "Reds",
                                  title = "Median Home Value Sales", breaks = c(100000, 150000, 200000, 250000, 300000, 400000, Inf)) +
  tm_borders()+
  tm_layout(
    inner.margins = c(.06, .06, .06, .06),
    legend.title.size = .75,
    legend.text.size = .5,
    legend.bg.color = "grey",
    legend.bg.alpha = 0,
    legend.width = 0.235)





