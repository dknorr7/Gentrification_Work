library(pacman)
library(censusapi)
p_load(tidycensus, tidyverse, scales,sf, tigris, viridis, ggthemes, ggplot2,
       stringr, dplyr, leaflet, broom, censusapi)

##tidycensus
#census_api_key("802147089d1abcbfe2ea820bca13be641fcc9a28", install = TRUE)
Geolytics90 <- readRDS("Geolytics90.rds")
Geolytics00 <- readRDS("Geolytics00.rds")

Vacant <- get_acs(geography = "tract", table = "B25002", state = "Tennessee", county = "Davidson", output = "wide") %>%
  select(CensusTractID = GEOID, Vacant16 = B25002_003E, Occupied16 = B25002_002E, TotalHHs16 = B25002_001E) %>%
  mutate(Pct_Vacant16 = 100*(Vacant16/TotalHHs16)) %>% select(CensusTractID, Pct_Vacant16)

HomeVal <- get_acs(geography = "tract", table = "B25077", state = "Tennessee", county = "Davidson", output = "wide") %>% 
  select(CensusTractID = GEOID, MedianHomeVal16 = B25077_001E)

RentVal <- get_acs(geography = "tract", table = "B25064", state = "Tennessee", county = "Davidson", output = "wide") %>%
  select(CensusTractID = GEOID, MedianRent16 = B25064_001E)

Pct_Renters <- get_acs(geography = "tract", table = "B25033", state = "Tennessee", county = "Davidson", output = "wide", geometry = TRUE) %>%
  select(CensusTractID = GEOID, Total = B25033_001E, RenterHH = B25033_008E, OwnerHH = B25033_002E) %>% mutate(Pct_Renters = 100*(RenterHH/Total)) %>%
  select(CensusTractID, Pct_Renters)

NonWhite <- get_acs(geography = "tract", table = "B02001", state = "Tennessee", county = "Davidson", output = "wide") %>%
  select(CensusTractID = GEOID, Total = B02001_001E, White = B02001_002E) %>% mutate(Pct_Nonwhite = ((Total - White)/ Total)*100) %>%
  select(CensusTractID, Pct_Nonwhite)

OldHousing <- get_acs(geography = "tract", table = "B25034", state = "Tennessee", county = "Davidson", output = "wide") %>%
  select(CensusTractID = GEOID, Total = B25034_001E, built10to13 = B25034_003E, built2014 = B25034_002E, built90to99 = B25034_005E, built00to10 = B25034_004E) %>% 
  mutate(post90 = built10to13 + built2014 + built90to99 + built00to10, Pct_post90 = 100*(post90/Total)) %>%
  select(CensusTractID, Pct_post90)

MultiUnit <- get_acs(geography = "tract", table = "B25024", state = "Tennessee", county = "Davidson", output = "wide") %>%
  select(CensusTractID = GEOID, Total = B25024_001E, single1 = B25024_002E, single2 = B25024_003E) %>%
  mutate(SingleUnit = single1 + single2, Pct_MultiUnit = 100*((Total - SingleUnit)/Total)) %>%
  select(CensusTractID, Pct_MultiUnit)

#vars<- load_variables(2016, "acs5", cache = TRUE)
#view(vars)

##Error bar plot
David %>% ggplot(aes(x = estimate, y = GEOID)) + geom_errorbarh(aes(xmin = estimate - moe,
                                                                    xmax = estimate + moe)) +
  geom_point(color = "Red", size = 3) + scale_x_continuous(labels = scales::dollar) + labs(title = "Household Income by tract",
                                                                                           subtitle = "2012 -2016 ACS survey",
                                                                                           y = "",
                                                                                           x = "ACS estimate (bars represent margin of error)")
p1 <- ggplot() +
  geom_sf(data = Pct_Renters, aes(fill = Pct_Renters)) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16,
                                  face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14,
                                     margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9,
                                    margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis(labels = scales::percent,
                     name = "MHI Estimate") +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Median Household Income for PDX Metro\n at the census tract level",
       subtitle = "An R 'sf' Example") + theme_minimal()

## censusapi
Sys.setenv(CENSUS_KEY="802147089d1abcbfe2ea820bca13be641fcc9a28", overwrite = TRUE)
# Reload .Renviron
readRenviron("~/.Renviron")




#All_Census_Variables <- listCensusMetadata(name= "2016/acs/acs5/subject", type = "variables")

AgeDemographics <- getCensus(name = "acs/acs5/subject", vintage = 2016, vars = c("S0101_C01_025E", "S0101_C01_026E"), region="tract:*", regionin="state:47+county:037") %>%
  mutate(CensusTractID = paste0(state, county, tract), Pct_Under18 = 100 - S0101_C01_025E) %>% select(CensusTractID, Pct_Under18, Pct_Over60 = S0101_C01_026E )

Education <- getCensus(name = "acs/acs5/subject", vintage = 2016, vars = c("S1501_C02_015E"), region="tract:*", regionin="state:47+county:037") %>%
  mutate(CensusTractID = paste0(state, county, tract)) %>% select(CensusTractID, Pct_College = S1501_C02_015E)

Unemployment <- getCensus(name = "acs/acs5/subject", vintage = 2016, vars = c("S2301_C04_001E"), region="tract:*", regionin="state:47+county:037") %>%
  mutate(CensusTractID = paste0(state, county, tract)) %>% select(CensusTractID, Pct_Unemployed = S2301_C04_001E)

Poverty <- getCensus(name = "acs/acs5/subject", vintage = 2016, vars = c("S1701_C03_001E"), region="tract:*", regionin="state:47+county:037") %>%
  mutate(CensusTractID = paste0(state, county, tract)) %>% select(CensusTractID, Pct_Poverty = S1701_C03_001E)

NonFamilyHHs <- getCensus(name = "acs/acs5/subject", vintage = 2016, vars = c("S2501_C01_023E"), region="tract:*", regionin="state:47+county:037") %>%
  mutate(CensusTractID = paste0(state, county, tract)) %>% select(CensusTractID, Pct_NonFamily = S2501_C01_023E)

Census_merged <- data.frame(NonFamilyHHs, Poverty[,-1], Unemployment[,-1], Education[,-1], AgeDemographics[,-1], MultiUnit[,-1], NonWhite[,-1], OldHousing[,-1], RentVal[,-1], HomeVal[,-1], Vacant[,-1], Pct_Renters[,-1])

Census_merged <- Census_merged[,-15]
All_Census_named <- Census_merged[complete.cases(Census_merged),]

saveRDS(All_Census_named, "C:/School/Research/Proposal/All_Census_named.rds" )
All_Census <- Census_merged[complete.cases(Census_merged),] %>% select(-CensusTractID)

saveRDS(All_Census, "C:/School/Research/Proposal/All_Census.rds" )
m <- apply(All_Census, 2, mean)
s <- apply(All_Census, 2, sd)
z <- scale(All_Census, m, s)



fitk <- kmeans(z, 5)
fitk
kcenters <- fitk$centers


#silhoutte 
library(cluster)
plot(silhouette(cutree(hc.c,2),distance))


#Second crack at PCA
#scale
data2 <- data.frame(scale(All_Census))
# Verify variance is uniform
plot(sapply(data2, var))
pc <- princomp(data2)
sumvariance <- sum(pc$sdev^2)
pcpctvar <- pc$sdev^2/13
ggplot() + geom_line(aes(x = seq(1,13,by =1), y = pcpctvar*100), size = 2.5) + 
  scale_x_continuous(name = "Principal Component", limits = c(1,8),breaks = seq(1,8, by =1)) +
  scale_y_continuous(name = "Explained Variance (%)", breaks = seq(0, 40, by = 5)) + ggtitle("PCA Scree Plot")


plot(y= c(summarypc$pctvar), x = seq(c(1,13,1)), type='l',"Explained Variance by PCs")
summary(pc) # 5 components is both 'elbow' and explains >85% variance
# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(data2)
biplot(pc, xlabs=rep("+", nrow(data2)), xlim = c(-.4,.4), ylim = c(-.3, .2))

pc_table <- as.data.frame(pc$rotation)
# First for principal components
comp <- data.frame(pc$x[,1:5])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

# Determine number of clusters
wss <- (nrow(data2)-1)*sum(apply(data2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data2,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within group sum of squares")
CleanVarNames <- c("% Nonfamily", "% Poverty", "% Unemployed", "% Educated (bachelor's +)","% Under 18", 
                   "% Over 60", "% Multiunit", "% NonWhite", "% New housing (>1990)", "Median Rent Value", 
                   "Median Home Value", "% Vacant", "% Renters")
PC_first5 <-  pc_table[,1:5]
row.names(PC_first5) <- CleanVarNames

PC_Clean <- kable(PC_first5, digits = 2)
PC_Clean
# From scree plot elbow occurs at k = 6
# Apply k-means with k=6
k <- kmeans(comp, 6, nstart=25, iter.max=1000)

library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))

# First cluster
row.names(All_Census_named[k$clust==clust[1],])
# Second Cluster
row.names(All_Census_named[k$clust==clust[2],])
# Third Cluster
row.names(All_Census_named[k$clust==clust[3],])
# Fourth Cluster
row.names(All_Census_named[k$clust==clust[4],])
# Fifth Cluster
row.names(All_Census_named[k$clust==clust[5],])
# Sixth Cluster
row.names(All_Census_named[k$clust==clust[6],])

All_Census_named$PCA_cluster <- factor(k$cluster)

summaryClusters <- All_Census_named %>% group_by(PCA_cluster) %>% 
  summarize(avg_renters = mean(Pct_Renters),
            avg_nonwhite = mean(Pct_Nonwhite),
            avg_HomeVal = mean(MedianHomeVal16),
            avg_Rent = mean(MedianRent16),
            avg_PctEducated = mean(Education....1.),
            avg_PctNonfamily = mean(Pct_NonFamily),
            avg_PctUnder18 = mean(Pct_Under18),
            avg_PctOver60 = mean(Pct_Over60),
            avg_Poverty = mean(Poverty....1.),
            avg_Unemplyoment = mean(Unemployment....1.),
            avg_PctMultiUnit = mean(Pct_MultiUnit),
            avg_Pctpost90 = mean(Pct_post90),
            avg_PctVacant = mean(Pct_Vacant16))


#transpose df (more readable)
finalCluster <- t(summaryClusters)[-1,]
colnames(finalCluster) <- c("K1", "K2", "K3", "K4", "K5", "K6")

CleanVarNamesCluster <- c("% Renters","% NonWhite","Median Home Value", "Median Rent Value", "% Educated (bachelor's +)", 
                          "% Nonfamily", "% Under 18", "% Over 60", "% Poverty", "% Unemployed","% Multiunit", "% New housing (>1990)", "% Vacant")

row.names(finalCluster) <- CleanVarNamesCluster
finalCluster <- as.data.frame(finalCluster)
finalCluster$K1 <- as.numeric(as.character(finalCluster$K1))
finalCluster$K2 <- as.numeric(as.character(finalCluster$K2))
finalCluster$K3 <- as.numeric(as.character(finalCluster$K3))
finalCluster$K4 <- as.numeric(as.character(finalCluster$K4))
finalCluster$K5 <- as.numeric(as.character(finalCluster$K5))
finalCluster$K6 <- as.numeric(as.character(finalCluster$K6))

saveRDS(finalCluster, "C:/School/Research/Proposal/finalcluster.rds")
finalKmeanstable <- kable(finalCluster, digits = 0)
#write.csv(transposed, "ClusterSummary.csv")
#map

tract_geos <- get_acs(geography = "tract", table = "B25002", state = "Tennessee", county = "Davidson", geometry = TRUE) %>%
  select(CensusTractID = GEOID)
tract_geos2 <- merge(x = tract_geos, y = All_Census_named)

tract_geos2 %>% ggplot(aes(fill = PCA_cluster)) +
  geom_sf() + scale_fill_manual(name = "K Clusters", values = c("blue", "yellow", "orange", "red", "green", "purple"),
                                labels = c("K1", "K2", "K3", "K4", "K5", "K6"))


###third effort
prin_comp <- prcomp(pca.train, scale. = T)