library(pacman)
p_load(tidyverse, ggplot2, censusapi, tidycensus, sf, viridis, httr, lubridate,tigris, maps, mapdata, ggmap, stringr)

options(scipen=999)

Dep_LT <- read_rds("DependantLT.rds")

#filter dataset
# valid years: 1990, 2000, 2010, 2011, 2012, 2013, 2014, 2015, 2016

start_yr <- 2010
end_yr <- 2016
diff <- end_yr - start_yr

DF <- Dep_LT %>% filter(year == start_yr | year == end_yr)

x <- as.symbol(start_yr)
y <- as.symbol(end_yr)

DF_wide <- DF %>% spread(key = year, value = value) 

names(DF_wide)[names(DF_wide) == x] <- paste0('x', start_yr)
names(DF_wide)[names(DF_wide) == y] <- paste0('x', end_yr)

old_col <- paste0("x", start_yr)
new_col <- paste0("x", end_yr)

DF_change <- DF_wide %>% mutate(time = paste0(start_yr,"-", end_yr), PctChange=100*(((!!as.name(new_col))-(!!as.name(old_col)))/(!!as.name(old_col)))) %>%
  select(GEOID, Varx, PctChange, time)


BaseEndWide <- Dep_LT %>% filter(year == start_yr | year == end_yr) %>% mutate(ColID = paste0(Varx, year)) %>% select(-year, -Varx) %>% spread(key = ColID, value = value)

BaseEndWide1 <- BaseEndWide[complete.cases(BaseEndWide), ]
write_rds(BaseEndWide1, "C:/School/Research/Predictive Variables/Master/WideDependant.rds")

ncuts <- 5
lim <- 11

DF_base_quartiles <- DF_wide %>% select(GEOID, Varx, (!!as.name(old_col))) %>% spread(key = Varx, value = (!!as.name(old_col))) %>%
  mutate(college_quartile = as.integer(cut(PctCollege, quantile(PctCollege, probs=0:ncuts/ncuts, na.rm = T), include.lowest=TRUE)),
         Income_quartile = as.integer(cut(`Median HH Income`, quantile(`Median HH Income`, probs=0:ncuts/ncuts, na.rm = T), include.lowest=TRUE)),
         White_quartile = as.integer(cut(`White Pct`, quantile(`White Pct`, probs=0:ncuts/ncuts, na.rm = T), include.lowest=TRUE)),
         HomeVal_quartile = as.integer(cut(`Median Home Value`, quantile(`Median Home Value`, probs=0:ncuts/ncuts, na.rm = T), include.lowest=TRUE)),
         BaseSums = HomeVal_quartile + White_quartile + Income_quartile + college_quartile,
         BaseEllig = ifelse(BaseSums <= lim, "Elligible", "Not Elligible") %>% replace_na("Not Elligible"))  %>% 
  select(GEOID, HomeVal_quartile, White_quartile, Income_quartile, college_quartile, BaseSums, BaseEllig)



DF_change_summary1 <- DF_change %>% filter(complete.cases(.)) %>% group_by(GEOID, time) %>% summarize(GentIndex = sum(PctChange), cnt = n()) %>%
                  filter(cnt == 4) %>% inner_join(., DF_base_quartiles)  %>% mutate(nm_GentIndx = GentIndex/diff, yeardiff = diff) 


DF_Change_wide <- DF_change %>% select(GEOID, Varx, PctChange) %>% spread(key = Varx, value = PctChange) %>% filter(complete.cases(.))

colnames(DF_Change_wide) <- c("GEOID", "CHNG_Income", "CHNG_HomeVal", "CHNG_college", "CHNG_White")
                
medGentIndex <- quantile(DF_change_summary1$GentIndex, .75)  


DF_change_summary <- inner_join(x = DF_change_summary1, y = DF_Change_wide, by = "GEOID") %>% 
  mutate(GentClass = ifelse(BaseEllig == "Elligible" & GentIndex >= medGentIndex, "Gentrified", "Not Gentrified"), BooleanGent = ifelse(BaseEllig == "Elligible" & GentIndex >= medGentIndex, 1,0))
DF_change_summary$BooleanGent <- as.factor(DF_change_summary$BooleanGent)

write_rds(DF_change_summary, "C:/School/Research/Predictive Variables/Master/DEPEND.rds" )







#map
tracts <- get_acs(geography = "tract", variables = "B19013_001", state = "Tennessee", county = "Davidson", geometry = TRUE)
tract_sp <- merge(x = tracts, y = DF_change_summary, by = "GEOID")

natural.interval = classInt::classIntervals(tract_sp$GentIndex, n = 6, style = 'jenks')$brks

tract_sp$population.natural = cut(tract_sp$GentIndex, breaks=natural.interval, include.lowest = TRUE)
colors <- c("blue", "green", "white", "yellow", "red", "#000000") 
scale_fill_lisa <- scale_fill_manual(values=colors)

spdf <- as(tract_sp, 'Spatial')
rgdal::writeOGR(obj=spdf, dsn="nmlz", layer="x1990to2000x", driver="ESRI Shapefile")


# ggplot(data = tract_sp, aes(fill = population.natural, alpha = BaseEllig)) + geom_sf() + scale_fill_lisa + ggtitle(paste("Gentrification Index", start_yr, "-", end_yr)) + scale_alpha_manual(values = c("Elligible" = 1.0, "Not Elligible" = 0.3))

ggplot(data = tract_sp, aes(fill = population.natural, alpha = BaseEllig)) + 
  geom_sf() + 
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  ggtitle(paste("Gentrification Index", start_yr, "-", end_yr)) + 
  scale_alpha_manual(values = c("Elligible" = 1.0, "Not Elligible" = 0.1))











#Join Dependant variable tables
joined_dependant <- inner_join(EducationChange, MedIncChange)
joined_dependant <- joined_dependant[complete.cases(joined_dependant), ]

#Determine eligibility based on starting conditions and percent change
Dependant_f <- joined_dependant %>% mutate(strtElg10 = ifelse(ED2010<= quantile(joined_dependant$ED2010)[3] & INC2010<= quantile(joined_dependant$INC2010)[3], "eligible 2010", "No"),strtElg00 = ifelse(ED2000<= quantile(joined_dependant$ED2000)[3] & INC2000<= quantile(joined_dependant$INC2000)[3], "eligible 2000", "No"),Gent10_16 = ifelse(strtElg10 == "eligible 2010" & EDchange10to16 >= median(joined_dependant$EDchange10to16) & INCchange10to16 >= median(joined_dependant$INCchange10to16),"Gentrified 2010 - 2016", "Not Gentrified 2010 - 2016"),Gent00_16 = ifelse(strtElg00 == "eligible 2000" & EDchange00to16 >= median(joined_dependant$EDchange00to16) & INCchange00to16 >= median(joined_dependant$INCchange00to16),"Gentrified 2000 - 2016", "Not Gentrified 2000 - 2016"))

saveRDS(Dependant_f, "C:/School/Research/Predictive Variables/Master/Dependant.rds")

Dependant_f1 <- joined_dependant %>% mutate(strtElg10 = ifelse(ED2010<= quantile(joined_dependant$ED2010)[2] & INC2010<= quantile(joined_dependant$INC2010)[2], "eligible 2010", "No"),
                                           strtElg00 = ifelse(ED2000<= quantile(joined_dependant$ED2000)[2] & INC2000<= quantile(joined_dependant$INC2000)[2], "eligible 2000", "No"),
                                           Gent10_16 = ifelse(strtElg10 == "eligible 2010" & EDchange10to16 >= median(joined_dependant$EDchange10to16) & INCchange10to16 >= median(joined_dependant$INCchange10to16),"Gentrified 2010 - 2016", "Not Gentrified 2010 - 2016"),
                                           Gent00_16 = ifelse(strtElg00 == "eligible 2000" & EDchange00to16 >= median(joined_dependant$EDchange00to16) & INCchange00to16 >= median(joined_dependant$INCchange00to16),"Gentrified 2000 - 2016", "Not Gentrified 2000 - 2016"))


##Mapping
tract_geos <- get_acs(geography = "tract", variables = "B19013_001", state = "Tennessee", county = "Davidson", geometry = TRUE)
tract_geos2 <- merge(x = tract_geos, y = Dependant_f, by = "GEOID")

Gent00_16_df <- tract_geos2 %>% filter(Gent00_16 == "Gentrified 2000 - 2016")
Gent10_16_df <- tract_geos2 %>% filter(Gent10_16 == "Gentrified 2010 - 2016")



tract_geos2a <- merge(x = tract_geos, y = Dependant_f1, by = "GEOID")

Gent00_16_df1 <- tract_geos2a %>% filter(Gent00_16 == "Gentrified 2000 - 2016")
Gent10_16_df1 <- tract_geos2a %>% filter(Gent10_16 == "Gentrified 2010 - 2016")


#plot for tract gentrified between 2000 and 2016
d <- Gent00_16_df %>% ggplot() + geom_sf(data = tract_geos2) + geom_sf(fill = "blue") + ggtitle("2000 - 2016 (median start threshold)")

#plot for tract gentrified between 2010 and 2016
e <- Gent10_16_df %>% ggplot() + geom_sf(data = tract_geos2) + geom_sf(fill = "red") + ggtitle("2010 - 2016 (median start threshold)")

##sensitivity
f <- Gent00_16_df1 %>% ggplot() + geom_sf(data = tract_geos2a) + geom_sf(fill = "blue") + ggtitle("2000 - 2016 (lower quartile threshold)")


g <- Gent10_16_df1 %>% ggplot() + geom_sf(data = tract_geos2a) + geom_sf(fill = "red") + ggtitle("2010 - 2016 (lower quartile threshold)")

gridExtra::grid.arrange(d, e, f, g, nrow = 2)


#Gentrification Index need to fix
Index <- left_join(x = Geolytics00, y = MedIncChange, by = c("CensusTractID" = "GEOID")) %>%
            left_join(., EducationChange, by = c("CensusTractID" = "GEOID")) %>%
            left_join(., NonWhite_f, by = c("CensusTractID" = "GEOID")) %>%
            left_join(., MedHomeValue_f, by = c("CensusTractID" = "GEOID"))

Index_calcs <- Index %>% mutate(WhiteChange00to16 = (100*(White2016 - White00)/White00),
                                WhiteChange10to16 = (100*(White2016 - White2010)/White2010),
                                HomeValueChange00to16 = (100*(MedHomeVal2016 - HomeVal00)/HomeVal00),
                                HomeValueChange10to16 = (100*(MedHomeVal2016 - MedHomeVal2010)/MedHomeVal2010),
                                sum2000 = NonWhiteChange00to16 + HomeValueChange00to16 + INCchange00to16 + EDchange00to16,
                                sum2010 = NonWhiteChange10to16 + HomeValueChange10to16 + INCchange10to16 + EDchange10to16) %>%
                          select(GEOID = CensusTractID, NonWhiteChange00to16, NonWhiteChange10to16, HomeValueChange00to16,
                                 HomeValueChange10to16, INCchange00to16, INCchange00to16, EDchange00to16, EDchange10to16, sum2000, sum2010)


tract_index <- get_acs(geography = "tract", variables = "B19013_001", state = "Tennessee", county = "Davidson", geometry = TRUE)
tract_index2 <- merge(x = tract_index, y = Index_calcs, by = "GEOID")

natural.interval = classIntervals(tract_index2$sum2000, n = 6, style = 'jenks')$brks
tract_index2$population.natural = cut(tract_index2$sum2000, breaks=natural.interval, include.lowest = TRUE)
colors <- c("blue", "green", "white", "yellow", "red", "#000000") 
scale_fill_lisa <- scale_fill_manual(values=colors)

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

ggplot(data = tract_index2, aes(fill = population.natural)) + geom_sf() + scale_fill_lisa + ggtitle("Gentrification Index 2000 - 2016") 
write.csv(Index_calcs, "Indexcalcs.csv")
write_rds(Index_calcs, file.path("C://School//Research//Predictive Variables//Master" ,"Indexcalcs.rds"))



####Shorter time Variables for lag analysis
Education00_raw$CensusTractID <- as.character(Education00_raw$CensusTractID)
AllEd <- inner_join(x = foo16wide, y = Education00_raw, by = c("GEOID" = "CensusTractID"))
