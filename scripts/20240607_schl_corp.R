library(fuzzyjoin)
library(ggmap)
library(readxl)
library(sf)
library(tidyverse)
library(tigris)

# create dataframes with boundaries of Indiana school corporations that align with IDOE naming, not names used by census data

# create dataframe of Indiana school corporations in 2024 

# import corporation details  
corp <- read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx",
        sheet = 1) |> 
  
  # select columns to keep and rename 
  select('corp_id' = 'IDOE_CORPORATION_ID',
         'corp_name' = 'CORPORATION_NAME',
         'typ' = 'CORPORATION_TYPE',
         'address' = 'ADDRESS',
         'city' = 'CITY',
         'state' = 'STATE',
         'zip' = 'ZIP',
         'county' = 'COUNTY_NAME',
         ) |> 
  
  # consolidate address columns and remove redundant columns
  rowwise() |> 
  
  mutate(address = paste(address, city, state, zip, sep = ', '),
         address2 = paste(city, state, zip, sep = ", ")) |> 
  
  select(-c(city, state, zip))


# remove abbreviations from school corporation names ####

# create list of regex patterns to identify abbreviations

set_names <- c(
  "Sch(?=[space:])|Sch(?![:alpha:])", # Sch for School with space after or no letters behind
  "Schls(?=[space:])|Schls(?![:alpha:])", # Schls for Schools with space after or no letters
  "Schs(?=[space:])|Schs(?![:alpha:])", # Schs for Schools with space after or no letters
  "Sci(?![:alpha:])",   # Sci for Science with no letters behind
  "St(?![:alpha:])",    # St  for Saint with no letters behind
  "Corp(?=[space:])|Corp(?![:alpha:])",   # Corp for Corporation with space after but not with letters behind
  "Con(?![:alpha:])|Con(?=[:space:])",   # Con for Consolidated no letters behind
  "Co(?![:alpha:])",    # Co  for County with no letters behind
  "Com(?![:alpha:])",   # Com for Community with no letters behind
  "Ctl(?![:alpha:])",   # Ctl for Central with no letters behind
  "IN(?![:alpha:])",    # IN for Indiana no letters behind
  "Hmn(?![:alpha:])",   # Hmn for Humanities no letters behind
  "Dist(?![:alpha:])",  # Dist for District no letters behind
  "Inc(?![:alpha:])",   # Inc for Incorporated no letters behind
  "Cnt(?![:alpha:])",   # Cnt for Central no letters behind
  "Cath(?![:alpha:])",  # Cath for Catholic
  "Acad(?![:alpha:])",  # Acad for Academy no letters behind
  "Cons(?![:alpha:])",  # Cons for Consolidated
  "C S C(?![:alpha:])", # C S C for Community School Corporation
  "Twp(?![:alpha:])",   # Twp for Township
  "M S D(?![:alpha:])", # M S D for Metropolitan School District
  "MSD(?![:alpha:])",   # MSD for Metropolitan School District
  "Cthlc(?![:alpha:])", # Cthlc for Catholic
  "Schl(?![:alpha:])",  # Schl for School
  "Spec(?![:alpha:])",  # Spec for Special
  "CSUSA(?![:alpha:])", # CSUSA for Charter Schools USA
  "Tech(?![:alpha:])",  # Tech for Technology
  "Excellenc(?![:alpha:])",  # Excellenc for Excellence
  "Vis Imprd(?![:alpha:])",  # Vis Imprd for Visually Impaired
  "Am(?![:alpha:])",   # Am for America
  "NW(?![:alpha:])",   # NW for Northwest
  "Coop(?![:alpha:])", # Coop for Cooperative
  "Serv(?![:alpha:])", # Serv for Service
  "PLA(?![:alpha:])",  # PLA for Phalen Leadership Academy
  "Ind(?![:alpha:])",  # Ind for Indiana
  "Sou(?![:alpha:])",  # Sou for South
  "Scho(?![:alpha:])", # Scho for School
  "Ev(?![:alpha:])",   # Ev for Evangelical
  "SE(?![:alpha:])",   # SE for Southeast
  "Sp(?![:alpha:])",   # Sp for Special
  "Srvs(?![:alpha:])", # Srvs for Services
  "SS(?![:alpha:])",   # SS for Saints
  "India(?![:alpha:])", # India for Indiana
  "Mdl/High(?![:alpha:])", # Mdl/High for Middle/High
  "Sts(?![:alpha:])", # Sts for Sisters
  "Saint.(?![:alpha:])", # Saint. for Saint
  "(9-12)(?![:alpha:])", #  for (9-12)
  "Leadersh(?![:alpha:])", #Leadersh  for Leadership
  "Franc(?![:alpha:])", #Franc  for Francis Scott Key School 103
  "Louis(?![:alpha:])", #Louis for Louis B Russell School 48
  "Annuc(?![:alpha:])", #Annuc for Annuciation
  "@(?![:alpha:])" #@ for at
)

# replacements in order of above abbreviations
rplc_names <- c(
  "School",
  "Schools",
  "Schools",
  "Science",
  "Saint",
  #"School ",
  "Corporation",
  "Consolidated",
  "County",
  "Community",
  "Central",
  "Indiana",
  "Humanities",
  "District",
  "Incorporated",
  "Central",
  "Catholic",
  "Academy",
  "Consolidated", 
  "Community School Corporation", 
  "Township",
  "Metropolitan School District",
  "Metropolitan School District", 
  "Catholic",
  "School",
  "Special",
  "Charter Schools USA",
  "Technology",
  "Excellence",
  "Visually Impaired",
  "America",
  "Northwest",
  "Cooperative",
  "Service",
  "Phalen Leadership Academy",
  "Indiana",
  "South",
  "School",
  "Evangelical",
  "Southeast",
  "Special",
  "Services",
  "Saints",
  "Indiana",
  "Middle/High", 
  "Sisters", 
  "Saint", 
  "",
  "Leadership",
  "for Francis Scott Key School 103",
  "Louis B Russell School 48",
  "Annunciation",
  "at"
)

# replace abbreviations from dataframe 
corp <- corp |> 
  
  mutate(
    corp_name =
      str_replace_all(corp_name,
                      setNames(rplc_names, set_names)),
    # remove any extra spaces from name columns
    corp_name = str_replace_all(corp_name, "  ", " ")
  )


# calculate coordinates for school corporations
corp <- corp |> 
  
  filter(typ == "School Corporation") |> 
  
  mutate(
    geocode(
      location = str_c(paste0(
        corp_name, ", ", address)),
        output = "latlon")
  ) |> 
  
  rename('corp_lat' = "lat",
         'corp_lon' = 'lon',
         )

# save warnings
warnings <- last_dplyr_warnings(23)

# build dataframe for loop to extract data from warning list
wrn_msg <- as.data.frame(x = 'msg')

# rename column to msg
wrn_msg <- wrn_msg |> rename('msg' = `"msg"`)

# create sequence length of list for loop
wn <- seq(1, length(warnings), 1)

# extract warning with row number to examine error; assign to dataframe w
for (i in 1:length(wn)){
  wrn_msg <-
    as.data.frame(rbind(wrn_msg,
          warnings[[i]][["message"]][2])
    )
}

# remove first row with msg entry
wrn_msg <- wrn_msg[2:nrow(wrn_msg),]

# remove everything but row number where error occurred
wrn_msg <- as.data.frame(wrn_msg) |> 
  mutate(wrn_msg = str_replace_all(wrn_msg, "In row ", ""),
         wrn_msg = str_remove_all(wrn_msg, "\\.")
  )


# what corp_names didn't geocode correctly

corp_name <- 'corp_name'

# create rgc dataframe to hold corp names that need to be re-geocoded
rgc <- as.data.frame(x = corp_name)

for (i in 1:n_distinct(wrn_msg)){

  rgc <- 
    rbind(rgc,
          
          corp[i, 2])
}

# remove unnecessary first (corp_name) row of vector -
rgc <- rgc[2:nrow(rgc),]

# re-run geocode but reduce address entry to corp_name and city. sometimes the weird abbreviated components of specific addresses throw ggmap::geocoding off.

corp2 <- corp |> filter(corp_name %in% rgc) |> 
  
  select(-c(corp_lat, corp_lon)) |> 
  
  mutate(geocode(location = str_c(paste0(
    corp_name, ", ", address2)),
    output = "latlon"),
    ) |> 
  
  rename('corp_lat' = "lat",
         'corp_lon' = 'lon',
         )

# warning: Adams Central Com..." not uniquely geocoded
# check location assigned for Adams Central: it checks out.

# to add corrected geocoded entries, first remove from corp entries in rgc that needed to be rerun with modified addresses

corp <- corp |> 
  filter(!corp_name %in% rgc)


# add the corporation's and their accompanying points that were re-geocoded back to the corp dataframe. remove unecessary address2 column.
corp <- rbind(corp, corp2) |> 
  
  select(-address2)




# need to determine if points are within right boundaries to verify names are same

# download school district boundary geometry from tigris package
in_dis <- school_districts(state = "Indiana", year = '2023')


# convert coordinates into spatial feature
corp_sf <- st_as_sf(x = corp,
                    coords = c('corp_lon', 'corp_lat')
                    )

# remove IDOE Test LEA from corp_sf
corp_sf <- corp_sf |>  filter(corp_name != "IDOE Test LEA")

# assign corp_sf same crs as in_dis
st_crs(corp_sf) <- st_crs(in_dis)

# determine which spatial polygons hold the points generated from geolocating the IN DOE school corporations
pts <- st_within(corp_sf, in_dis, sparse = T)

# in order to match the school corporation id numbers used by Indiana Dept. of Education with the spatial polygons provided by the Tigris package (they don't all agree), need to first locate the IDOE school corporation's points using their addresses (done above). Then use st_within to determine which polygons hold the points of corresponding school corporations. Loop through list created by st_within to extract the corporation name associated with the polygon (in_dis) that holds the points of the INDOE corporation.

# create a dataframe that includes a vector of names used by corp_sf
df <- tibble(row = seq(1, length(pts), 1), 
             name = corp_sf$corp_name,
             in_dis_name = rep(NA, 291)
             )

for (i in 1:length(pts)){
  
  # extract name that matches the polygon from in_dis that contains the point pair generated from the IN DOE data of corporation names
  df[i,3] <- in_dis[pts[[i]], 5]$NAME
  
}

#

# make new column for polygon matching points geocoded from IDOE corp_name
corp_sf$NAME <- df$in_dis_name

# rename geometry column that contains points from corp_sf to coords to distinguish from
# multipolygon geometry column from in_dis
corp_sf <- corp_sf |> rename("coords" = "geometry")

# remove IDOE Test LEA row from corp
corp <- corp |> filter(corp_name !="IDOE Test LEA")

# remove NAME from corp_sf
# corp_sf <- corp_sf |> select(-NAME)

# save corporation locations to file before removing to add spatial geometry column 
st_write(corp_sf, 
         
         layer = "corporation_pts.geojson",
         dsn = "clean_data/20240607_corporation_pts.geojson"
         )


# save as csv 
write_csv(corp_sf,
          "clean_data/20240607_corporation_pts.csv")



# join in_dis and corp_sf (placing in_dis first preserves multipolygons rather than points coordinates)
corp <- st_join(in_dis, corp_sf, left = T) |> 
  
  select(c(corp_id, corp_name, address, geometry)) 

# four rows with NA's; leave for now.

# save corporation boundaries to file 
st_write(corp, 
         
         layer = "corporation_bounds.geojson",
         dsn = "clean_data/20240607_corporation_bounds.geojson"
)

# clean up
rm(corp_sf, corp2, df, pts, warnings, wrn_msg, rgc)
