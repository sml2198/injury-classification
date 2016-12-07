# NIOSH Project 2014-N-15776

# 17 - PREP DATA FOR MAPPING!

# Last edit 10/10/16

######################################################################################################

# Geocoding script for large list of addresses.
# THANKS TO Shane Lynn 10/10/2013!
# https://www.r-bloggers.com/batch-geocoding-with-r-and-google-maps/
  
#load up the ggmap library
library(ggmap)

  # input: mines data
open_data_mines_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Mines.txt"
  # input: mine address records data (downloaded October 12)
open_data_mine_addresses_file_name = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/AddressOfRecord/AddressOfRecord.txt"

  # output/geocode input: addresses data
geocode_infile = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/addresses"

######################################################################################################

# MINES
mines = read.table(open_data_mines_file_name, header = T, sep = "|", na.strings=c("","NA"))
mines = mines[, c("MINE_ID", "LATITUDE", "LONGITUDE", "FIPS_CNTY_NM")]
names(mines)[names(mines) == "MINE_ID"] = "mineid"
names(mines)[names(mines) == "FIPS_CNTY_NM"] = "county"
names(mines)[names(mines) == "LATITUDE"] = "lat"
names(mines)[names(mines) == "LONGITUDE"] = "long"
names(mines) = tolower(names(mines))

# clean long/lat & mark problem observations
# longs should be negative for the US (because we're west of the prime meridian)
mines[, "long"] = ifelse(is.na(mines$long), NA, paste("-", mines[, "long"], sep = ""))
mines[, "long"] = as.numeric(mines$long)
mines[, "lat"] = as.numeric(mines$lat)
mines$problem = ifelse((abs(mines[, "long"]) < 10) |
                       (abs(mines[, "lat"]) < 10), 1, 0)
mines$problem = ifelse(is.na(mines$problem), 0, mines$problem)

# the following mines have problematic long/lats and were identified by loading the data into QGIS, selecting groups of mines by their
# reported state and county, and observing which points in fact fall outside of those territories based on their coordinates
mines$problem = ifelse((mines$mineid == "1518279" | 
                        mines$mineid == "1518369" | 
                        mines$mineid == "3304595" | 
                        mines$mineid == "3401782" | 
                        mines$mineid == "4407087" | 
                        mines$mineid == "4407155" | 
                        mines$mineid == "4601436" |
                        mines$mineid == "4608214" | 
                        mines$mineid == "4608857" | 
                        mines$mineid == "4608892" | 
                        mines$mineid == "3608512" | 
                        mines$mineid == "4405559"), 1, mines$problem)

# MINE ADDRESSES
addresses = read.table(open_data_mine_addresses_file_name, header = T, sep = "|", na.strings=c("","NA"))
addresses = addresses[, c("MINE_ID", "MINE_NAME","STREET", "CITY", "ZIP_CD","STATE_ABBR", "STATE", "COUNTRY")]
names(addresses)[names(addresses) == "MINE_ID"] = "mineid"
names(addresses)[names(addresses) == "MINE_NAME"] = "mine"
names(addresses)[names(addresses) == "ZIP_CD"] = "zip"
# sum(is.na(addresses$POSTAL_CD)) # FYI: 86100/86111 - none of these merge on mines w/ problems so now we don't keep it
names(addresses) = tolower(names(addresses))

# merge mines & address records (only those mines that have problematic long/lats)
data = merge(mines[mines$problem == 1, c("mineid", "county")], addresses, by = "mineid", all = F)

# make city names prettier
data$city = tolower(data$city)
data$city = paste(toupper(substr(data$city, 1, 1)), substr(data$city, 2, nchar(data$city)), sep = "")
for (i in 1:nrow(data)) {
  city = data$city[i]
  if (grepl(" ", city)) {
    data$city[i] = paste(unlist(strsplit(city, " "))[1], paste(toupper(substr(unlist(strsplit(city, " "))[2], 1, 1)), substr(unlist(strsplit(city, " "))[2], 2, nchar(city[1])), sep = "") , sep = " ")
  }
}

# clean street and county - RD = rural delivery
clean_names = function(var) {
  data[, var] = gsub("Rte", "Route", data[, var])
  data[, var] = gsub("Rt", "Route", data[, var])
  data[, var] = gsub("General Delivery", "", data[, var])
  data[, var] = gsub("P O", "P.O.", data[, var])
  data[, var] = gsub("PO", "P.O.", data[, var])
}
data$street = clean_names("street")
data$county = paste(data[, "county"], "County", sep = " ")

# identify post office boxes
data$pobox = ifelse(grepl("(P\\.O\\.)|(Box|box|BOX)", data[, "street"]), 1, 0)

# make single address field if not missing street address & not a PO box
data$address = ifelse(!is.na(data$street) & data$pobox == 0, 
                      paste(data[, "street"], data[, "city"], data[, "county"], data[, "state_abbr"], data[, "zip"], data[, "country"], sep = ", "), NA)
#sum(is.na(data$address)) # 147 - BUMMER. For these, and those with just PO box, we just use the city, zipcode, and state.
data$address = ifelse(is.na(data$street) | data$pobox == 1, 
                      paste(data[, "city"], data[, "county"], data[, "state_abbr"], data[, "zip"], data[, "country"], sep = ", "), data$address)

# prepare geocode-ready datasets
no_geocode = data[is.na(data$address), ]
data = data[!is.na(data$address), ]
#write.table(addresses, geocode_infile, sep = ",", col.names = T, row.names = F)

######################################################################################################

# get the input data
addresses = data$address
                
# define a function that will process Google's server responses
getGeoDetails = function(address){   
  # use the gecode function to query Google servers
  geo_reply = geocode(address, output = 'all', messaging = T, override_limit = T)
  # now extract the bits that we need from the returned list
  answer = data.frame(lat = NA, long = NA, accuracy = NA, formatted_address = NA, address_type = NA, status = NA)
  answer$status = geo_reply$status
  
  # if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output = 'all', messaging = T, override_limit = T)
    answer$status = geo_reply$status
  }
  
  # return NA's if we didn't get a match
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  # else, extract what we need from the Google server reply into a dataframe
  answer$lat = geo_reply$results[[1]]$geometry$location$lat
  answer$long = geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy = geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type = paste(geo_reply$results[[1]]$types, collapse = ',')
  answer$formatted_address = geo_reply$results[[1]]$formatted_address
  return(answer)
}

# initialize a dataframe to hold the results
geocoded = data.frame()
# find out where to start in the address list (if the script was interrupted before)
startindex = 1
# if a temp file exists - load it up and count the rows
tempfilename = paste0(geocode_infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded = readRDS(tempfilename)
  startindex = nrow(geocoded)
  print(startindex)
}

# start the geocoding process address by address - geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  # query the Google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index = ii
  # append the answer to the results file.
  geocoded = rbind(geocoded, result)
  # save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

# now add the latitude and longitude to the main data
data$lat = geocoded$lat
data$long = geocoded$long
data$accuracy = geocoded$accuracy
data = data[, c(-grep("pobox", names(data)),
                -grep("street", names(data)),
                -grep("state_abbr", names(data)))]

# finally write it all to the output files
saveRDS(data, paste0(geocode_infile ,"_geocoded.rds"))
write.table(data, file = paste0(geocode_infile, "_geocoded.csv"), sep = ",", row.names = F)

######################################################################################################