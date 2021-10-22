# Turn off scientific notation
options(scipen = "999") 
# Ensure strings come in as character types
options(stringsAsFactors = FALSE)

# Install packages
# Note: you must have installed each of these packages before loading them
# Note 2: There may be some versioning issues with ggplot & ggmap. 
# Check out this stackoverflow thread http://bit.ly/2lXHRFJ 
library(pacman)

p_load(ggplot2, raster, lubridate, censusapi,tidycensus, ggmap, maptools, ggthemes, rgeos, broom, dplyr, plyr, grid, gridExtra, reshape2, scales)

###Plot Themes
plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

# And another that we will use for maps
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    strip.text = element_text(size=12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

# Define some palettes
palette_9_colors <- c("#0DA3A0","#2999A9","#458FB2","#6285BB","#7E7CC4","#9A72CD","#B768D6","#D35EDF","#F055E9")
palette_8_colors <- c("#0DA3A0","#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
palette_7_colors <- c("#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
palette_1_colors <- c("#0DA3A0")


###


# Read in a csv of home sale transactions directly from github.
raw_sales <- read.csv("C:\\School\\Research\\Predictive Variables\\Assessor\\SalesAndOwnershipHistory_2018-05-31_Text - Copy.csv") %>%
  mutate_at(vars(matches("UserAccount|DateAcquired|OwnerInstrument|OwnerName|OwnerAddress1|OwnerAddress2|OwnerCity|OwnerState|OwnerZip|OwnerCountry|LocationAddress|LocationCity|LocationZip|CouncilDistrict|TaxDistrict|CensusBlock")), funs(as.character))

raw_sales$DateAcquired1 <- substr(raw_sales$DateAcquired,1,nchar(raw_sales$DateAcquired)-8)
raw_sales$DateAcquired <- as.Date(raw_sales$DateAcquired1,"%m/%d/%Y")

Sales <- raw_sales %>% mutate(Year = year(DateAcquired), CensusTractID = paste0("470",CensusBlock))
# We will need to consider Sale Year as a categorical variable so we convert it from a numeric variable to a factor
Sales$Year1 <- as.factor(Sales$Year)
Sales <- Sales %>% filter(SalePrice >1 & Year >2011) 

#remove outliers
Sales <- Sales[which(Sales$SalePrice < mean(Sales$SalePrice) + (2.5 * sd(Sales$SalePrice))), ]



### Histogram of housing price values
home_value_hist <- ggplot(Sales, aes(SalePrice)) + 
  geom_histogram(fill=palette_1_colors) +
  xlab("Sale Price($)") + ylab("Count") +
  scale_fill_manual(values=palette_1_colors) +
  plotTheme() + 
  labs(x="Sale Price($)", y="Count", title="Distribution of Nashville home prices",
       subtitle="Nominal prices (2010 - 2018)", 
       caption="Source: Davidson County Tax Assessor") +
  scale_x_continuous(labels = comma, limits = c(0,1000000)) + scale_y_continuous(labels = comma)

# Plotting it:
home_value_hist
# And saving it to the working directory:
ggsave("plot1_histogram.png", home_value_hist, width = 8, height = 4, device = "png")


###Violing Plots
home_value_violin <- ggplot(Sales, aes(x=Year1, y=SalePrice, fill=Year1)) + 
  geom_violin(color = "grey50") +
  xlab("Sale Price($)") + ylab("Count") +
  scale_fill_manual(values=palette_7_colors) +
  stat_summary(fun.y=mean, geom="point", size=2, colour="white") +
  plotTheme() + theme(legend.position="none") +
  scale_y_continuous(labels = comma, limits = c(0, 1000000)) +
  labs(x="Year",y="Sale Price($)",title="Distribution of Nashville
       home prices",
       subtitle="Nominal prices (2012 - 2018); Sale price means visualized as points",
       caption="Source: Davidson County Tax Assessor")

home_value_violin

ggsave("plot2_violin.png", home_value_violin, width = 8, height = 4, device = "png")


#read in neighborhood boundaries
neighb <- readShapePoly("C:\\School\\Research\\shapefiles\\neighborhoods_prj.shp")
# Define the bounding box
bbox <- neighb@bbox

# Manipulate these values slightly so that we get some padding on our basemap between the edge of the data and the edge of the map
nash_bbox <- c(left = bbox[1, 1] - .01, bottom = bbox[2, 1] - .005, 
             right = bbox[1, 2] + .01, top = bbox[2, 2] + .005)

Nash_basemap <- get_map("Nashville", maptype = "hybrid", zoom = 10, source = "google")
# Download the basemap
#basemap <- get_stamenmap(
 # bbox = nash_bbox,
  #zoom = 15,
  #maptype = "toner-lite", messaging = FALSE)

# Map it
bmMap <- ggmap(basemap) + mapTheme() + 
  labs(title="Nashville basemap")
bmMap


Sales_summary <- Sales %>% filter(CensusTractID != "47037980100") %>% group_by(CensusTractID, Year) %>% dplyr::summarize(medSalePrice = median(SalePrice), SaleCount = n())

tract_geos <- get_acs(geography = "tract", table = "B25002", state = "Tennessee", county = "Davidson", geometry = TRUE) %>%
  dplyr::select(CensusTractID = GEOID) %>% group_by(CensusTractID) %>% distinct()

tract_geos2 <- merge(x = tract_geos, y = Sales_summary)
tract_geos2 <- tract_geos2 %>% filter(medSalePrice <1000000)

#Median price by neighborhoodR
tract_geos2$brks <- cut(tract_geos2$medSalePrice, breaks = seq(0, 500000, by = 100000), 
                        labels = c("0 -100000", "100000 - 200000", "200000 - 300000", "300000 - 400000", "400000 - 500000"))


mid <- mean(tract_geos2$medSalePrice)
mycols <- colors()[c(26, 439, 30, 53, 118, 72)]
max_val <- max(abs(tract_geos2$medSalePrice))
values <- seq(-max_val, max_val, length = 11)

neighb_map <- ggplot() +
  geom_sf(data = tract_geos2, 
               aes(fill = medSalePrice),
               colour = "white", alpha = 0.75, size = 0.25) + coord_sf(crs = st_crs(tract_geos2)) +
  scale_fill_gradientn(colors = blue2red(7), breaks = c(0, 100000, 200000, 300000, 400000, 500000, 100000)) +
  mapTheme() + theme(legend.position = c(.85, .25)) +
  facet_wrap(~Year, nrow = 2) +
  labs(title="Median Home Price by Neighborhood, Nashville ",
       subtitle="Nominal prices (2012 - 2018)",
       caption="Source: Davidson County Tax Assessor")
neighb_map
