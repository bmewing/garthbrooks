library(magrittr)
library(dplyr)
library(readr)

getZipList = function(maxDist){
  data = readr::read_csv("Zip Codes/us_postal_codes.csv",col_types = "cccccnn")
  alaska = data %>%
    filter(State == "Alaska")
  hawaii = data %>%
    filter(State == "Hawaii")
  lower48 = data %>% 
    filter(!(State %in% c('Alaska','Hawaii')) & !is.na(State))
  output = c(alaska$`Postal Code`,hawaii$`Postal Code`) #this is untested and may be breaking
  while(nrow(lower48) > 0){
    start = lower48 %>% 
      mutate(corner = Longitude - Latitude) %>% 
      filter(corner == min(corner))
    output = c(output,start$`Postal Code`)
    drop = c()
    for(i in 1:nrow(lower48)){
      d = distancing(start$Latitude,start$Longitude,lower48$Latitude[i],lower48$Longitude[i])
      if(d < maxDist) drop = c(drop,i)
    }
    if(length(drop) > 0) lower48 = lower48[-drop,]
    cat("  NROW REMAINING:",nrow(lower48),"\n")
  }
  return(output)
}

deg2rad = function(deg) {
  deg * (pi/180)
}

distancing = function(lat1,lon1,lat2,lon2) {
  rad = 6371
  dLat = deg2rad(lat2-lat1)
  dLon = deg2rad(lon2-lon1)
  a = sin(dLat/2) * sin(dLat/2) + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2)
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  d = rad * c
  return(d*0.62137119)
}

