# NIOSH Project 2014-N-15776

# 17 - PREP DATA FOR MAPPING!

# Last edit 10/10/16

######################################################################################################

library(plyr)
library(zoo)
library(ggmap)

# input
  # MR
classified_MR_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_coded/MR_accidents_with_predictions.rds"
  # PS
classified_PS_file_name = "X:/Projects/Mining/NIOSH/analysis/data/4_coded/PS_accidents_with_predictions.rds"
  # mines
open_data_mines_file_name = "X:/Projects/Mining/NIOSH/analysis/data/0_originals/MSHA/open_data/Mines.txt"
  # mine address records data (downloaded October 12)
open_data_mine_addresses_file_name = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/AddressOfRecord/AddressOfRecord.txt"

# output
  # MR
coded_MR_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/MR_injuries.csv"
collapsed_MR_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/MR_injuries_collapsed.csv"
collapsed_mines_MR_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/MR_mines_collapsed.csv"
  # PS
coded_PS_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/PS_injuries.csv"
collapsed_PS_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/PS_injuries_collapsed.csv"
collapsed_mines_PS_out_file_name = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/PS_mines_collapsed.csv"
  # output/geocode input: addresses data
geocode_infile = "X:/Projects/Mining/NIOSH/analysis/data/6_mapping/addresses"

# SET INJ TYPE
inj.type = "MR"
# inj.type = "PS"

# REMOVE EXISTING GEOCODE TEMPFILE (unless process was broken)
tempfilename = paste0(geocode_infile, '_temp_geocoded.rds')
file.remove(tempfilename)

######################################################################################################

# load & clean coded accidents 
if (inj.type == "MR"){
  coded_inj = readRDS(classified_MR_file_name)
  coded_inj = coded_inj[, c("mineid", "MR", "accidentdate", "accidenttime", "activitycode", 
                            "bodypart", "sourceofinjury", "mineractivity", 
                            "jobexperience", "documentno", "natureofinjury", "narrative")]
  names(coded_inj)[names(coded_inj) == "MR"] = "inj"
}
if (inj.type == "PS"){
  coded_inj = readRDS(classified_PS_file_name)
  coded_inj = coded_inj[, c("mineid", "PS", "accidentdate", "accidenttime", "activitycode", 
                            "bodypart", "sourceofinjury", "mineractivity", 
                            "jobexperience", "documentno", "natureofinjury", "narrative")]
  names(coded_inj)[names(coded_inj) == "PS"] = "inj"
}

# clean injs
coded_inj$accidentdate = as.Date(as.character(coded_inj$accidentdate), "%m/%d/%Y")
coded_inj$quarter = as.yearqtr(coded_inj$accidentdate)
coded_inj$quarter = as.yearqtr(coded_inj$accidentdate)

######################################################################################################

# THE GEOCODE PREP

# load and clean mines
mines = read.table(open_data_mines_file_name, header = T, sep = "|", na.strings = c("", "NA"))
mines = mines[, c("MINE_ID", "LATITUDE", "LONGITUDE", "FIPS_CNTY_NM", "STATE")]
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

# MINEID | SUPPOSED STATE | ACTUAL STATE (BASED ON COORDINATES)
# 1518279     KY             SC
# 1518369     KY             VA
# 3304595     OH             GA
# 3401782     OK             AR
# 4407087     VA             KY
# 4407155     VA             KY
# 4601436     WV             OH
# 4608214     WV             VA
# 4608857     WV             KY
# 4608892     WV             GA
# 3608512     PA             ? Canada
# 4405559     VA             ? Colombia

#   # 1518279 KY, Martin
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 1518369 KY, Pike
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 3304595 OH, Meigs
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 3401782 OK, Le Flore
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 4407087  VA
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 4407155 VA
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 4601436 WV
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 4608214 WV
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 4608857 WV
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 4608892 WV
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 3608512 PA
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)
#   # 4405559 VA 
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)

# # for this one (which appears in Canada - go figure) I picked a random spot in Clearfield County, PA
# # https://www.google.com/maps/place/Clearfield+County,+PA/@40.9958579,-78.5989015,10.84z/
# #     data=!4m5!3m4!1s0x89cc3b90115f118f:0x2dad211d577a725!8m2!3d40.9519685!4d-78.5660852
# mines[, "latitude"] = ifelse(mines$mineid == "3608512", 40.993542, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "3608512", -78.462343, mines$longitude)
# 
# # one mine - 4405559 - has a "problem" long/lat at this point. we looked it up using the mine data retrieval system,
# # found the proper address - Wise County, VA - grabbed an approximate long/lat, and recode it here.
# mines[, "latitude"] = ifelse(mines$mineid == "4405559", 36.980513, mines$latitude)
# mines[, "longitude"] = ifelse(mines$mineid == "4405559", -82.623650, mines$longitude)

# mines[, "longitude"] = as.character(as.numeric(mines$longitude))
# mines[, "latitude"] = as.character(as.numeric(mines$latitude))

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

######################################################################################################

# MINE ADDRESSES
addresses = read.table(open_data_mine_addresses_file_name, header = T, sep = "|", na.strings = c("","NA"))
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
data$street = ifelse(data$street == "", NA, data$street)
data$county = paste(data[, "county"], "County", sep = " ")

# identify post office boxes
data$pobox = ifelse(grepl("(P\\.O\\.)|(Box|box|BOX)", data[, "street"]), 1, 0)

# make single address field if not missing street address & not a PO box
data$address = ifelse(!is.na(data$street) & data$pobox == 0, 
                      paste(data[, "street"], data[, "city"], data[, "county"], data[, "state_abbr"], data[, "zip"], data[, "country"], sep = ", "), NA)
#sum(is.na(data$address)) # 147 - BUMMER. For these, and those with just PO box, we just use the city, zipcode, and state.
data$address = ifelse(is.na(data$street) | data$pobox == 1, 
                      paste(data[, "city"], data[, "county"], data[, "state_abbr"], data[, "zip"], data[, "country"], sep = ", "), data$address)

######################################################################################################

# IT'S GEOCODEEEEEEEEE TIMEEEEEEEEEEEEEE!!!!!!!!!!!!!!!

# get the input data
addresses = data$address

# define a geocode function to process Google's server responses
getGeoDetails = function(address){   
  # use gecode function to query Google server
  geo_reply = geocode(address, output = 'all', messaging = T, override_limit = T)
  # now extract what we need from the returned list
  answer = data.frame(lat = NA, long = NA, accuracy = NA, formatted_address = NA, address_type = NA, status = NA)
  answer$status = geo_reply$status
  
  # if over the query limit then pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output = 'all', messaging = T, override_limit = T)
    answer$status = geo_reply$status
  }
  
  # return NA's if no match
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  # else, put Google server reply into dataframe
  answer$lat = geo_reply$results[[1]]$geometry$location$lat
  answer$long = geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy = geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type = paste(geo_reply$results[[1]]$types, collapse = ',')
  answer$formatted_address = geo_reply$results[[1]]$formatted_address
  return(answer)
}

# initialize a dataframe & find out where to start in the address list
geocoded = data.frame()
startindex = 1
# if a temp file exists - load it up and count the rows
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded = readRDS(tempfilename)
  startindex = nrow(geocoded)
  print(startindex)
}

# start the geocoding process address by address
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  # query the Google geocoder
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index = ii
  # append output to the results file.
  geocoded = rbind(geocoded, result)
  # save temp results
  saveRDS(geocoded, tempfilename)
}

# merge back on lat and long data
data$lat_geo = geocoded$lat
data$long_geo = geocoded$long
data$accuracy = geocoded$accuracy
data = data[, c(-grep("pobox", names(data)),
                -grep("street", names(data)),
                -grep("state_abbr", names(data)))]

# finally write it all to the output files
saveRDS(data, paste0(geocode_infile ,"_geocoded.rds"))
write.table(data, file = paste0(geocode_infile, "_geocoded.csv"), sep = ",", row.names = F)

######################################################################################################

# merge new long/lats onto mines
mines = merge(mines, data[, c("mineid", "long_geo",
                              "lat_geo", "accuracy")], by = "mineid", all = T)

# replace long and lat with geocoded values
mines$lat = ifelse(!is.na(mines$lat_geo), mines$lat_geo, mines$lat)
mines$long = ifelse(!is.na(mines$long_geo), mines$long_geo, mines$long)

# merge mines onto accidents
coded_inj = merge(coded_inj, mines[, c(-match("problem", names(mines)),
                                        -match("long_geo", names(mines)),
                                        -match("lat_geo", names(mines)))], by = "mineid")

# make sure lat/long are properly stored
coded_inj$lat = as.character(as.numeric(coded_inj$lat))
coded_inj$long = as.character(as.numeric(coded_inj$long))

# create all inj indicator $ rename MR/PS
coded_inj$all_injs = 1

# collapse to mine-quarter 
collapsed_inj = ddply(coded_inj[, c(match("inj", names(coded_inj)),
                                    match("all_injs", names(coded_inj)),
                                    match("mineid", names(coded_inj)),
                                    match("quarter", names(coded_inj)))], c("mineid", "quarter"), 
                      function(x) colSums(x[, c(match("inj", names(x)),
                                                match("all_injs", names(x)))], na.rm = T))

# collapse to mine-level
collapsed_mines = ddply(collapsed_inj[, c(match("inj", names(collapsed_inj)),
                                          match("all_injs", names(collapsed_inj)),
                                          match("mineid", names(collapsed_inj)))], c("mineid"), 
                        function(x) colSums(x[, c(match("inj", names(x)),
                                                  match("all_injs", names(x)))], na.rm = T))

# create unique dataset of mines, states, lats and longs
other_info = coded_inj[, c("mineid", "state", "county", "lat", "long", "accuracy")]
other_info = unique(other_info)

# merge back on states, long, lat
collapsed_inj = merge(collapsed_inj, other_info, by = "mineid", all = F)
collapsed_mines = merge(collapsed_mines, other_info, by = "mineid", all = F)

# make sure there are no pipes in any of the narrative fields, so that we can produce a pipe-delimited csv
varlist = names(coded_inj)
for (i in 1:length(varlist)) {
  coded_inj[, varlist[i]] = gsub("\\|", " ", coded_inj[, varlist[i]])
}

# save collapsed data (csv) and then raw data 
if (inj.type == "MR"){
  names(collapsed_inj)[names(collapsed_inj) == "inj"] = "MR"
  names(collapsed_mines)[names(collapsed_mines) == "inj"] = "MR"
  names(coded_inj)[names(coded_inj) == "inj"] = "MR"
    # keep only obs that have at least one MR injury
  collapsed_inj = collapsed_inj[which(collapsed_inj$MR > 0),]
  collapsed_mines = collapsed_mines[which(collapsed_mines$MR > 0),]
    # save collapsed data sets
  write.table(collapsed_mines, file = collapsed_mines_MR_out_file_name, sep = "|", col.names = T, row.names = F)
  write.table(collapsed_inj, file = collapsed_MR_out_file_name, sep = "|", col.names = T, row.names = F)
    # keep only MR injuries
  coded_inj = coded_inj[which(coded_inj$MR == 1),]
    # save raw data (csv)
  write.table(coded_inj, file = coded_MR_out_file_name, sep = "|", col.names = T, row.names = F)
  
#   # test code to save shapefile
#   coords = coded_inj[, c("longitude", "latitude")]
#   coords$longitude = as.numeric(coords$longitude)
#   coords$latitude = as.numeric(coords$latitude)
#   MR_shape = SpatialPointsDataFrame(coords, data = coded_inj, coords.nrs = numeric(0), 
#                          proj4string = CRS(as.character(NA)), match.ID = TRUE)
#   writeOGR(obj = MR_shape, dsn = "tempdir", layer = "data", driver = "ESRI Shapefile")
}
if (inj.type == "PS"){
  names(collapsed_inj)[names(collapsed_inj) == "inj"] = "PS"
  names(collapsed_mines)[names(collapsed_mines) == "inj"] = "PS"
  names(coded_inj)[names(coded_inj) == "inj"] = "PS"
    # keep only obs that have at least one PS injury
  collapsed_inj = collapsed_inj[which(collapsed_inj$PS > 0),]
  collapsed_mines = collapsed_mines[which(collapsed_mines$PS > 0),]
    # save collapsed data sets
  write.table(collapsed_mines, file = collapsed_mines_PS_out_file_name, sep = "|", col.names = T, row.names = F)
  write.table(collapsed_inj, file = collapsed_PS_out_file_name, sep = "|", col.names = T, row.names = F)
    # keep only PS injuries
  coded_inj = coded_inj[which(coded_inj$PS == 1),]
    # save raw data (csv)
  write.table(coded_inj, file = coded_PS_out_file_name, sep = "|", col.names = T, row.names = F)
}

######################################################################################################

