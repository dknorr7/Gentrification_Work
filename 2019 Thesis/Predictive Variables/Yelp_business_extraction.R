library(pacman)
p_load(httr, tidyverse, ggplot2, remotes, yelp, yelpr, leaflet, spatialEco, sp, tigris, raster, rgdal, GISTools)

 
 ###Function to append results of yelp list, workaround from 50 result limit to original function
 mykey <- "tXnlgw5ByJu6k5vzMKm82HiUUfgrYy7-xFG3BYon-BNFI__QxG2l-_z6q4V0_PgEaxLARajKHbjoXeF24wVNPUakSPeS9jhfuBGf5pSuaedJlT1uu8qUPNzT68RgW3Yx"
 
 yelp_full_search <- function(place, keywords){
   
   df_total = data.frame()
   
   offset_i <- 0
   limit_i <- 50
   
   while (nrow(df_total)%%limit_i == 0){
     yelp_setup <- business_search(api_key = mykey,
                                   location = place,
                                   term = keywords,
                                   limit = limit_i, offset = offset_i)
     
     yelp_return <- as.data.frame(yelp_setup$businesses)
     yelp_return$latitude <- yelp_return$coordinates$latitude
     yelp_return$longitude <- yelp_return$coordinates$longitude
     
###These columns are nested dataframes and are dropped but could be included      
     yelp_return <- yelp_return %>% dplyr::select(-c(location, transactions, categories, coordinates))
     
### make sure Price column is explicitly included, sometimes it is, sometimes it isn't
     yelp_return <- yelp_return %>% mutate(price = ifelse("price" %in% colnames(yelp_return), yelp_return$price, "NA"))
     
     df_total <- rbind(yelp_return, df_total)
     
     offset_i <- offset_i + limit_i
     
     
     print("Proceeding to next API Request")
   }
   print("Finished Compiling Yelp Businesses")
   
   return(df_total)
 }




Yelp_Biz <-yelp_full_search("Nashville", "hospital")


###quick map of yelp results
leaflet(data = Yelp_Biz) %>% addTiles() %>%
  addCircleMarkers(radius  = 3, ~longitude, ~latitude, popup = ~as.character(name), label = ~as.character(name))



###Join to Census Tracts (counts)
proj <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

DavidsonTracts <- tracts(state = "Tennessee", county = "Davidson")

Yelp_Biz_sp <- Yelp_Biz
coordinates(Yelp_Biz_sp) <- ~longitude+latitude
proj4string(Yelp_Biz_sp) <- CRS(proj)


pts.poly <- point.in.poly(x = Yelp_Biz_sp, y = DavidsonTracts)

a.data <- over(Yelp_Biz_sp, DavidsonTracts)
joined <- poly.counts(Yelp_Biz_sp, DavidsonTracts)
BusinessCount <- setNames(joined, DavidsonTracts@data$GEOID)
CensusTractID<- c(DavidsonTracts@data$GEOID)
joined_final <- data.frame(BusinessCount, CensusTractID)
