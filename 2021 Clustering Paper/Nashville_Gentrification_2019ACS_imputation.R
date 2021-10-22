###9/24/2020
###Nashville Clustering Paper


# Setup -------------------------------------------------------------------

# Load Packages 
library(pacman)
p_load(tidyverse, ggplot2, censusapi, tidycensus, factoextra, sf, basemapR,
       viridis, httr, lubridate,tigris, maps, mapdata, ggmap, stringr, osmdata, leaflet, gridExtra)

options(scipen=999)

colors <- c("cornflowerblue", "orange",  "seagreen","purple4","tomato")

#Load Geolytics 2000 data
Dependant2000_LT <- read_rds("Geolytics/Geolytics_Dependant_LT.rds")
Dependant2000_wide <- read_rds("Geolytics/Geolytics_Dependant_wide.rds") %>% mutate(Year = 2000)

#Adjacency Swap table  
NoRenterSwap <- read.csv("AdjacenyForNoRenterImputation.csv", colClasses = c("character", "character"))
names(NoRenterSwap) <- c("Target_ID", "Adjacent_ID")

#Set Census API key
Sys.setenv(CENSUS_KEY="802147089d1abcbfe2ea820bca13be641fcc9a28", overwrite = TRUE)
census_api_key("802147089d1abcbfe2ea820bca13be641fcc9a28", overwrite = TRUE)

# Read Inflation Adjustment Table 
monthly_cpi <-
  read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)

monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(VALUE))

yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2019]



# Import Census Data from API and reformat --------------------------------
#Download Nashville (Davidson County) Census Data
geo <- "tract"

my_vars <- c(
  total_pop = "B01003_001",
  median_income = "B19013_001",
  median_rent = "B25064_001",
  median_home_val = "B25077_001",
  grad_degree ="B06009_005" ,
  bach_degree ="B06009_006", 
  total_college1 = "B06009_001",
  total_race = "B02001_001",
  white = "B02001_002",
  multi_unit_total = "B25024_001", 
  multi_unit_single1 = "B25024_002",
  multi_unit_single2 = "B25024_003"
)

x_2018 <- map2_dfr(
  "Tennessee", "Davidson County",
  ~ get_acs(
    geography = geo,
    variables = my_vars,
    state = .x,
    county = .y,
    year = 2018,
    survey = "acs5",
    geometry = FALSE
  )
) 

x_2019 <- map2_dfr(
  "Tennessee", "Davidson County",
  ~ get_acs(
    geography = geo,
    variables = my_vars,
    state = .x,
    county = .y,
    year = 2019,
    survey = "acs5",
    geometry = FALSE
  )
)



raw_2019 <- x_2019 %>% 
  mutate(year = 2019) %>%
  inner_join(., yearly_cpi, by = c("year" = "cpi_year")) %>%
  mutate(Value = ifelse(variable == "median_income"|variable == "median_rent"|variable == "median_home_val", estimate*(2-adj_factor), estimate)) %>%
  select(-estimate, -moe, -adj_factor, -cpi) %>%
  spread(variable, Value) %>%
  mutate(PCT_COLLEGE = ((grad_degree+bach_degree)/total_college1)*100,
         PCT_WHITE = (white/total_race)*100,
         PCT_MULTIUNIT = 100-((multi_unit_single1+multi_unit_single2)/multi_unit_total)*100,
         CountyID = substring(GEOID, 1,5)) %>%
  select(-NAME, -CountyID,-grad_degree, -bach_degree, -total_college1, -total_pop,  -white, -total_race, -multi_unit_single1, -multi_unit_single2, -multi_unit_total )


Dependant00 <- Dependant2000_wide %>%
  mutate(PCT_WHITE = 100-nonWhite00) %>%
  select(CensusTractID, Year, HomeVal00, HH_income00, RentVal00, PctCollege00, PCT_WHITE, PctMulti00)

names(Dependant00)<- names(raw_2019)

All_LT <- rbind(Dependant00, raw_2019) %>%
  gather("Var", "Val", median_home_val:PCT_MULTIUNIT)



#Perform Imputation on two tracts
Renters2019 <- All_LT %>% filter(Var == "median_rent" & year == 2019)
Imp_Join_data <- left_join(NoRenterSwap, Renters2019, by = c("Adjacent_ID" = "GEOID"))
Imputation_values <- Imp_Join_data %>%
                          group_by(Target_ID) %>% 
                          summarize(ImputedRent = mean(Val))
  

All_LT$SwapID <- paste(All_LT$GEOID, "_", All_LT$year, "_", All_LT$Var)

All_LT[All_LT$SwapID=="47037018203 _ 2019 _ median_rent", "Val"] <- Imputation_values[1,2]
All_LT[All_LT$SwapID=="47037018700 _ 2019 _ median_rent", "Val"] <- Imputation_values[2,2]

All_LT$SwapID <- NULL


City_pct_change <- All_LT %>% spread(year, Val) %>%
  mutate(PCT_CHANGE = ifelse(Var == "PCT_COLLEGE"|Var == "PCT_WHITE"|Var == "PCT_MULTIUNIT",`2019`-`2000`, 100*(`2019`-`2000`)/`2000`)) %>%
  select(-`2000`, -`2019`) %>%
  filter(!is.infinite(PCT_CHANGE))

City_pct_change_summary <- All_LT %>% spread(year, Val) %>%
  mutate(PCT_CHANGE = ifelse(Var == "PCT_COLLEGE"|Var == "PCT_WHITE"|Var == "PCT_MULTIUNIT",`2019`-`2000`, 100*(`2019`-`2000`)/`2000`)) %>% 
  filter(!is.infinite(PCT_CHANGE))


Davidson_change <- City_pct_change %>%
  spread(Var, PCT_CHANGE) %>%
  filter(complete.cases(.))


# Cluster Analysis of Cleaned Data ----------------------------------------
TractLabels <- Davidson_change$GEOID
Davidson_numeric_df <- Davidson_change[,2:7]

scaled_df <- scale(Davidson_numeric_df)

#Optimize number of k clusters
fviz_nbclust(scaled_df, kmeans, method = "wss", print.summary = TRUE)
fviz_nbclust(scaled_df, kmeans, method = "silhouette", print.summary = TRUE)

set.seed(11112020)
km_Nash <- kmeans(scaled_df, centers = 5, nstart = 50)
Nash_results1 <- data.frame(TractLabels, km_Nash$cluster)


cluster_remap_df <- tibble(old = c(1, 2, 3, 4, 5),
                           new = c("K5", "K3", "K2", "K1", "K4"))

#create new cluster swap based on known cluster sizes
# Ive done this so many times that I know how many tracts are assigned with each cluster
#otherwise I have to reassign colors every time and remapping is random every time

ClusterSwap <- Nash_results1 %>% group_by(km_Nash.cluster) %>%
  summarize(cnt = n()) %>% 
  mutate(Cluster = ifelse(cnt == 59, "Cluster 1", 
                   ifelse(cnt == 17, "Cluster 2",
                   ifelse(cnt == 2, "Cluster 3",
                   ifelse(cnt == 4, "Cluster 4",
                   ifelse(cnt == 74, "Cluster 5", "NA"))))))



Nash_results <- Nash_results1 %>%
  inner_join(., ClusterSwap, by = c("km_Nash.cluster"))

cluster_colors <- c("#1b9e77", "#d95f02", "#7570b3", "red4", "skyblue", "hotpink")
fviz_cluster(km_Nash, data = scaled_df) + theme(legend.title = element_text("ff"))


Nash_raw_comparison <- cbind(Davidson_numeric_df, Nash_results)

Nash_K_summary <- Nash_raw_comparison %>% group_by(Cluster) %>% summarize_if(is.numeric, mean)
Nash_K_summary_Median <- Nash_raw_comparison %>% group_by(Cluster) %>% summarize_if(is.numeric, median)

#Get spatial data
#Nash <- tracts("TN", "Davidson", cb = TRUE )
Nash <- st_read("Nashville Paper/GIS/TRACTS.shp")

Nash_sp <- geo_join(spatial_data = Nash, data_frame = Nash_results, by_sp = "GEOID", by_df = "TractLabels", how = "left")

Nash_SF <- st_as_sf(Nash_sp)


# Radar plots of clusters


K_Description <- City_pct_change_summary %>%
  inner_join(., Nash_results, by = c("GEOID"="TractLabels")) %>%
  select(-km_Nash.cluster)

SummaryTable <- K_Description %>% group_by(Cluster, Var) %>% summarize(Mean = mean(PCT_CHANGE, na.rm = TRUE),
                                                                     Median = median(PCT_CHANGE, na.rm = TRUE),
                                                                     Min = min(PCT_CHANGE, na.rm=TRUE),
                                                                     Max = max(PCT_CHANGE, na.rm=TRUE),
                                                                     SD = sd(PCT_CHANGE, na.rm=TRUE),
                                                                     count = n())

SummaryTable$VAR1 <- factor(SummaryTable$Var, levels = c("median_home_val", "median_rent", "median_income", "PCT_COLLEGE", "PCT_MULTIUNIT", "PCT_WHITE"))
OrderedSummaryTable <- SummaryTable[order(SummaryTable$VAR1),]
OrderedSummaryTable2 <- OrderedSummaryTable[order(OrderedSummaryTable$Cluster),]

#write.csv(OrderedSummaryTable2, "ChangeSummaryTableImputed2019.csv")

DavidsonSummaryTable <- K_Description %>% group_by(Var) %>% summarize(Mean = mean(PCT_CHANGE, na.rm = TRUE),
                                                                     Median = median(PCT_CHANGE, na.rm = TRUE),
                                                                     Min = min(PCT_CHANGE, na.rm=TRUE),
                                                                     Max = max(PCT_CHANGE, na.rm=TRUE),
                                                                     SD = sd(PCT_CHANGE, na.rm=TRUE),
                                                                     count = n())

DavidsonSummaryTable$VAR1 <- factor(DavidsonSummaryTable$Var, levels = c("median_home_val", "median_rent", "median_income", "PCT_COLLEGE", "PCT_MULTIUNIT", "PCT_WHITE"))
OrderedSummaryTable_Davidson <- DavidsonSummaryTable[order(DavidsonSummaryTable$VAR1),]

#write.csv(OrderedSummaryTable_Davidson, "Davidson_ChangeSummaryTableImputed2019.csv")

colors <- c("cornflowerblue",  "orange","tomato","seagreen","purple4", "black")
colors <- c("cornflowerblue",  "orange","tomato","seagreen","purple4", "black")

K5Map <- ggplot(Nash_SF) + geom_sf(aes(fill = factor(Cluster))) +
  coord_sf() + theme(legend.position="none") +
  scale_fill_manual(values = colors, name = "Cluster", na.value="transparent") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size=11)) + 
  labs(title = "Nashville Neighborhood Change Clusters", subtitle = "2000 - 2019")

#ggsave("Imputed/Davidson_K5_Imputed_2019_Map.jpg", K5Map, device = "jpg", width = 7, height = 6, dpi = 400)

# Radar plots of clusters

K_Description <- City_pct_change_summary %>%
  inner_join(., Nash_results, by = c("GEOID"="TractLabels")) %>%
  select(-km_Nash.cluster)
  
K_Description_grp <- K_Description %>% group_by(Var, Cluster) %>%
  summarize(cnt = n(),
            med2000 = median(`2000`),
            med2019=median(`2019`),
            mean2000 = mean(`2000`, na.rm=TRUE),
            mean2019=mean(`2019`, na.rm = TRUE),
            min2000 = min(`2000`),
            min2019 = min(`2019`),
            max2000 = max(`2000`),
            max2019 = max(`2019`))

write.csv(K_Description_grp, "Imputed/Cluster_StartEnd_Magnitudes.csv")

K_Description_grp_Davidson <- K_Description %>% group_by(Var) %>%
  summarize(cnt = n(),
            med2000 = median(`2000`),
            med2019=median(`2019`),
            mean2000 = mean(`2000`, na.rm=TRUE),
            mean2019=mean(`2019`, na.rm = TRUE),
            min2000 = min(`2000`),
            min2019 = min(`2019`),
            max2000 = max(`2000`),
            max2019 = max(`2019`))

write.csv(K_Description_grp_Davidson, "Imputed/Davidson_Nashville_Magnitudes.csv")

Nash_setup <- Nash_K_summary  %>%
 select(-km_Nash.cluster, Cluster, -cnt)


K_Description_lt <- K_Description %>% gather("Year", "Value", `2000`:`2019`)
K_Description_lt$Variable <- factor(K_Description_lt$Var, levels = c("median_home_val", "median_rent","median_income","PCT_COLLEGE","PCT_MULTIUNIT","PCT_WHITE"),
                                    labels = c("Median Home Value ($)", "Median Rent ($)", "Median Income ($)", "Percent College Degree", "Percent Multi-Unit", "Percent White"), ordered = TRUE)

Boxplot <- ggplot(K_Description_lt) +
  geom_boxplot(aes(x =Cluster, y = Value, shape = Year, fill = Cluster), position=position_dodge2()) + 
  scale_fill_manual(values=colors, name = "")+
  labs(x="", y="") +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +   theme_light() +
  theme(strip.background = element_rect(fill=NA, color = "gray50"),
        strip.text = element_text(color = "black"), text=element_text(size=14)) 


ggsave( "Imputed/Boxplot2019.jpg",Boxplot,  device = "jpeg", units = "in", width = 10, height = 9, dpi = 400)

#Scaled 
#Everything is relative to Davidson County Averages
scaledx <- Nash_setup %>%  select(-Cluster) %>%
  scale() %>% as_tibble()

Grouped_means <- scaledx %>% mutate(Cluster = Nash_setup$Cluster) 

dat2 <- Grouped_means %>%
  rename("Home" = median_home_val, Rent = median_rent, Income = median_income,
         "College" = PCT_COLLEGE, "Multi-unit" = PCT_MULTIUNIT, "White" = PCT_WHITE)

plot_df <- pivot_longer(dat2, -Cluster, names_to = "variable", 
                        values_to = "value") %>%
  mutate(variable = ordered(variable)) %>%
  arrange(Cluster, variable)


coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

Davidson_scaled_avg <- 

zero <- data.frame(variable = levels(plot_df$variable),
                   value = 0.0)

zero <- map_df(str_c("Cluster ", 1:5), ~mutate(zero, Cluster = .x))

colors <- c("cornflowerblue", "orange", "tomato","seagreen","purple4", "black")


#FIX OVERLAPPING LABELS 
#% Label necessary?
radar <- ggplot(plot_df, aes(x= variable, y = value, fill = Cluster, color = Cluster))  +
  geom_polygon(mapping = aes(group = Cluster), size = 1, alpha= 0.5) +
  geom_text(aes(x = variable, y = I(2), label = variable), size = 5, color = "black") +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(-2.0, 2.5), breaks = seq(-2.0, 2.0, 1.0)) +
  scale_color_manual(values= colors)+
  scale_fill_manual(values= colors)+ 
  coord_radar() +
  geom_polygon(data = zero,
               mapping = aes(x = variable, y = value),
               group = 0,
               fill = NA, color = "black",
               linetype = "dotted", size = .5) +
  labs(x= "", y = "Scaled Change Values", 
       title = "Neighborhood Change Clusters", 
       subtitle = "Scaled Cluster Means") +
  guides(fill=FALSE, color = FALSE) +
  facet_wrap(~Cluster, ncol = 3) +
  theme_light() +
  theme(strip.background = element_rect(fill=NA, color = "gray50"),
        strip.text = element_text(color = "black"),
        text = element_text(size=14))

ggsave("Imputed/K5_Radar_2019.jpg", radar, device = "jpg", width = 12, height = 7, dpi = 700)





# K Sensitivity -----------------------------------------------------------
set.seed(11112020)
k2 <- kmeans(scaled_df, centers = 2, nstart = 50)
k2$cluster <- ifelse(k2$cluster==2, "Cluster 2", "Cluster 1")


k3 <- kmeans(scaled_df, centers = 3, nstart = 50)
k3$cluster <- ifelse(k3$cluster==2, "Cluster 3", ifelse(k3$cluster==3, "Cluster 1", ifelse(k3$cluster==1, "Cluster 2",0)))


k4 <- kmeans(scaled_df, centers = 4, nstart = 50)
k4$cluster <- ifelse(k4$cluster==4, "Cluster 4", ifelse(k4$cluster==2, "Cluster 2", ifelse(k4$cluster==3, "Cluster 3", ifelse(k4$cluster==1, "Cluster 1",0))))


k5 <- kmeans(scaled_df, centers = 5, nstart = 50)
k5$cluster <- ifelse(k5$cluster==5, "Cluster 3", ifelse(k5$cluster==4, "Cluster 2", ifelse(k5$cluster==2, "Cluster 5", ifelse(k5$cluster==3, "Cluster 4", ifelse(k5$cluster==1, "Cluster 1",0)))))

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = scaled_df) + 
  ggtitle("K = 2") + labs(x = "PC1 (56%)", y = "PC2 (18%)")+
  theme_minimal() +  
  scale_colour_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  theme(legend.title = element_blank(), legend.text=element_text(size=11))


p2 <- fviz_cluster(k3, geom = "point",  data = scaled_df) +
  ggtitle("K = 3") + labs(x = "PC1 (56%)", y = "PC2 (18%)")+
  theme_minimal() +  
  scale_colour_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  theme(legend.title = element_blank(), legend.text=element_text(size=11))

p3 <- fviz_cluster(k4, geom = "point",  data = scaled_df) + 
  ggtitle("K = 4") + labs(x = "PC1 (56%)", y = "PC2 (18%)")+
  theme_minimal()+  
  scale_colour_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  theme(legend.title = element_blank(), legend.text=element_text(size=11))

p4 <- fviz_cluster(k5, geom = "point",  data = scaled_df) + ggtitle("K = 5")+
  theme_minimal() + labs(x = "PC1 (56%)", y = "PC2 (18%)")+
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  theme(legend.title = element_blank(), legend.text=element_text(size=11))



grid.arrange(p1, p2, p3, p4, nrow = 2)

sens <- arrangeGrob(p1, p2, p3, p4, nrow = 2)
ggsave(file = "Nashville Paper/Imputed/K_Sensitvity2019.jpg", sens, width =12, height = 8, units = "in", dpi = 800)



## Cluster PC Sensitivity --------------------------------------------------
for(k in seq(2,5, by = 1)){

  print(k)
  set.seed(11112020)
  km_Nash <- kmeans(scaled_df, centers = k, nstart = 50)
  results <- data.frame(TractLabels, km_Nash$cluster) %>% filter(!is.na(km_Nash.cluster))
  
  Nash_raw_comparison <- cbind(Davidson_numeric_df, results)
  
  colors <- c("cornflowerblue","orange",  "tomato","seagreen","purple4", "white")
  #Swap to make sure similar clusters are consistent across runs 
  k2swap <- data.frame(c(1 ,2), c(1, 2)) 
  names(k2swap) <- c("Orig", "Correct")
  
  k3swap <- data.frame(c(1 ,2, 3), c(1, 3, 2)) 
  names(k3swap) <- c("Orig", "Correct")
  
  k4swap <- data.frame(c(1 ,2, 3, 4), c(1, 4, 2, 3)) 
  names(k4swap) <- c("Orig", "Correct")
  
  k5swap <- data.frame(c(1 ,2, 3, 4, 5), c(2, 1, 3, 4, 5)) 
  names(k5swap) <- c("Orig", "Correct")
  
  if (k == 2){
    res <- results %>% inner_join(., k2swap, by = c("km_Nash.cluster"= "Orig"))
    results <- res %>% dplyr::select(TractLabels, cluster_code = Correct)
  }  else if (k == 3) {
    res <- results %>% inner_join(., k3swap, by = c("km_Nash.cluster"= "Orig"))
    results <- res %>% dplyr::select(TractLabels, cluster_code = Correct)
  } else if(k == 4){
    res <- results %>% inner_join(., k4swap, by = c("km_Nash.cluster"= "Orig"))
    results <- res %>% dplyr::select(TractLabels, cluster_code = Correct)
  } else if (k == 5) {
    res <- results %>% inner_join(., k5swap, by = c("km_Nash.cluster"= "Orig"))
    results <- res %>% dplyr::select(TractLabels, cluster_code = Correct)
  } 
  
  
  Nash_K_summary <- Nash_raw_comparison %>%
    group_by(km_Nash.cluster) %>%
    summarize_if(is.numeric, mean) 
  
  Nash_sp <- geo_join(spatial_data = Nash, data_frame = results, by_sp = "GEOID", by_df = "TractLabels", how = "left")
  
  Nash_SF <- st_as_sf(Nash_sp)
  
 Nash_SF$cluster_codex <- ifelse(!is.na( Nash_SF$cluster_code), paste0("Cluster ", Nash_SF$cluster_code), NA)
  
  varname <- paste0("Kmap", k)
  kplot <- ggplot(Nash_SF) + geom_sf(aes(fill = factor(cluster_codex)))+
    coord_sf() +
    theme(legend.position="none") +
    scale_fill_manual(values = colors, name = "",na.translate = F) +
    theme_bw()+ theme(legend.position = "right", axis.text = element_blank(), axis.ticks = element_blank()) 
  assign(varname, kplot)
}

grid.arrange(Kmap2, Kmap3, Kmap4, Kmap5)

sensMap <- arrangeGrob(Kmap2, Kmap3, Kmap4, Kmap5, nrow = 2)
ggsave(file = "Imputed/K_Sensitvity_Map_2019.jpg", sensMap, width =12, height = 8, units = "in", dpi = 700)



# Join Count Statistic ----------------------------------------------------


###Pseudo Moran's i test for spatial autocorelation - Join Count using Queen boundaries and second order queens
p_load(spdep, maptools)
Class_sp <- geo_join(spatial_data = Nash, data_frame = results, by_sp = "GEOID", by_df = "TractLabels", how = "inner")
Nash_results

Nash_sp <- geo_join(spatial_data = Nash, data_frame = Nash_results, by_sp = "GEOID", by_df = "TractLabels", how = "inner")

adj_mat_queen<- poly2nb(Nash_sp, queen = T)

spw_list_queen <- nb2listw(adj_mat_queen, style = "C")

jc_test_queen <- joincount.mc(as.factor(Nash_sp$Cluster), spw_list_queen, nsim = 10000)
jc_test_queen



adj_mat_queen2 <- nblag_cumul(nblag(adj_mat_queen, 2))
spw_list_queen2 <- nb2listw(adj_mat_queen2, style = "C")

jc_test_queen2 <- joincount.mc(as.factor(Nash_sp$Cluster), spw_list_queen2, nsim = 10000)
jc_test_queen2

#summary table 
qID <- as.character(c("K1", "K2", "K3", "K4", "K5"))
q1 <- as.numeric(c(jc_test_queen[[1]]$p.value, jc_test_queen[[2]]$p.value, jc_test_queen[[3]]$p.value, jc_test_queen[[4]]$p.value, jc_test_queen[[5]]$p.value))
q2 <- as.numeric(c(jc_test_queen2[[1]]$p.value, jc_test_queen2[[2]]$p.value, jc_test_queen2[[3]]$p.value, jc_test_queen2[[4]]$p.value, jc_test_queen2[[5]]$p.value))

FinalJC_Table <- data.frame(qID, q1, q2)
saveRDS(FinalJC_Table, "C:/School/DeansOffice/SocialMedia/FinalJC_Table.rds")

#plot neighbor matrix
par(mfrow= c(1,2), mar=c(3,3,3,3))
coords <- coordinates(Class_sp)

plot(Class_sp, main = "First Order Queen")
plot(adj_mat_queen, coords, add = TRUE, col = "red")

plot(Class_sp, main = "Second Order Queen")
plot(adj_mat_queen2, coords, add = TRUE, col = "blue")


x <- 65
ind <- unlist(adj_mat_queen[x])
alladj <- Class_sp[ind,]

ind2 <- unlist(adj_mat_queen2[x])
alladj2 <- Class_sp[ind2,]

plot(alladj[1], col = "red")
plot(alladj[x,1], col = "yellow" , add = TRUE)

plot(alladj2[1], col = "blue" , add = TRUE)
plot(Class_sp[65,1], col = "yellow")

pg <- Class_sp[65,]
ggplot() + geom_sf(pg)
