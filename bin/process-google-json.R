# process google location data (json file)


#################################

#library(jsonlite) # RJSONIO is faster (18.3 vs 99 sec for this dataset) 
require(RJSONIO)
require(dplyr)
require(purrr)
#require()


readjson <- function(x) {
  withProgress(message = "Processing data...", 
               min = 0,
               max = 1, {
            incProgress(0.4)
            
            loc <- RJSONIO::fromJSON(x)
            
            setProgress(value = 0.9, message = "Almost done...")
            
            df <- map_df(loc$locations, function(x) c(x['timestampMs'], x['latitudeE7'], x['longitudeE7'])) %>% # improvements possible
                mutate(time = .POSIXct(as.numeric(timestampMs)/1000, tz = "UTC"),
                lat = latitudeE7/1e7,
                lng = longitudeE7/1e7) %>%                              
                as_tibble()
            return(df)
})
}

#### this was the jsonlite version
# loc2 <- jsonlite::fromJSON("data/Location History.json", flatten = TRUE)
# locdf2 <- loc2$locations %>% 
#            mutate(time = .POSIXct(as.numeric(timestampMs)/1000, tz = "UTC"),
#                   lat = latitudeE7/1e7,
#                   lng = longitudeE7/1e7) %>% 
#            as.tibble()
# 
# locdf_clean <- tibble(time = locdf$time, lat = locdf$lat, lng = locdf$lng) #speed = locdf$velocity * 3.6)
# 
###################################################
# df <- locdf_clean %>%
#                   mutate(year = year(time), month = month(time)) %>%
#                   tidyr::unite(year_month, year, month, sep = "-") %>%
#                   as.tibble()
# the above could be just
# locdf_clean %>% mutate(year_month = strftime(time, format = "%Y-%m))





 
