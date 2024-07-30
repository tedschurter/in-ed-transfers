library(readxl)
library(tidyverse)
library(ggmap)
library(sf)



# geolocate points for settlement school corporation and enrolled school for mapping purposes. 


# import details of school transfers that include settlement school and enrolled school

tfr <- read_csv("clean_data/20240612_transfers_18_24.csv")

corp_pts <- read_csv("clean_data/20240607_corporation_pts.csv")
# add points for school corporations to tfr stlmt_corp


tfr <- left_join(tfr, corp_pts, join_by("stlmt_corp_id" == "corp_id")) |>
  
  rowwise() |> 
  
  mutate(stlmt_corp_lon =  
           str_extract_all(coords, pattern = "-?\\d+\\.\\d+")[[1]][1],
         
         stlmt_corp_lat = 
           str_extract_all(coords, pattern = "-?\\d+\\.\\d+")[[1]][2],
         
         .after = stlmt_corp_id)|> 
  
  select(-c(coords, corp_name, typ, address, county, NAME)) 


# any NA's in stlmt_corp_lon
sum(is.na(tfr$stlmt_corp_lon))  # 115

# what corporations are NA
tfr |> 
  filter(is.na(stlmt_corp_lon)) |>
  select(stlmt_corp_name, stlmt_corp_id) |> 
  distinct(stlmt_corp_name, .keep_all = T) |> 
  View()


# West Clark Community Schools   id# 0940
# IDOE Test LEA id# 1025
# Rockville Community School Corporation id# 6300
# Out Of State id# 9999

# remove IDOE Test LEA

tfr <- tfr |> filter(stlmt_corp_name != "IDOE Test LEA")

#  West Clark Community Schools was replaced by Borden-Henryville School Corporation; it was formed on July 1, 2020 after the dissolution of the West Clark Community Schools. via www.bhsc.school/en-US/about-the-district-fb6849e4

# west clark corp_id is 0940; Borden-Henryville is 0935. use Borden-Henryville's settlement lat and lon for West Clark entries in tfr
#
# settlement lon
tfr$stlmt_corp_lon[tfr$stlmt_corp_id == '0940'] <- tfr$stlmt_corp_lon[tfr$stlmt_corp_id == '0935']

# settlement lat
tfr$stlmt_corp_lat[tfr$stlmt_corp_id == '0940'] <- tfr$stlmt_corp_lat[tfr$stlmt_corp_id == '0935']


# Rockville Community School Corp became North Central Parke Community School Corporation in 2013: www.ncp.k12.in.us/cms/One.aspx?portalId=739734&pageId=40953665 

# I don't see any indication that Rockville has been restored as a school corp; converting its school corp id, name and stlmt lat and lon to that of NCPH

# lon 
tfr$stlmt_corp_lon[tfr$stlmt_corp_id == "6300"] <- tfr$stlmt_corp_lon[tfr$stlmt_corp_id == "6375"][1]

# lat
tfr$stlmt_corp_lat[tfr$stlmt_corp_id == "6300"] <- tfr$stlmt_corp_lat[tfr$stlmt_corp_id == "6375"][1]

# corp_id
tfr$stlmt_corp_id[tfr$stlmt_corp_id == "6300"] <- "6375"

# corp name
tfr$stlmt_corp_id[tfr$stlmt_corp_id == "Rockville Community School Corporation"] <- "North Central Parke Comm School Corporation"


# any NA's in stlmt_corp_lat
sum(is.na(tfr$stlmt_corp_lat)) # 19

# what corporations are NA
tfr |> 
  filter(is.na(stlmt_corp_lon)) 

# Those transfers are into Indiana schools from out of state. They are outside the scope of this project so will be filtered out.

tfr <- tfr |> filter(stlmt_corp_id != "9999")

# any NA's in stlmt_corp_lat
sum(is.na(tfr$stlmt_corp_lat)) # 0




# add locations for enrolled locations that are school corporations
tfr <- left_join(tfr, 
                 
                 corp_pts, 
                 
                 join_by(nrl_schl_name == corp_name)) |> 
  rowwise() |> 
  mutate(nrl_lon =  
           str_extract_all(coords, pattern = "-?\\d+\\.\\d+")[[1]][1],
         
         nrl_lat = 
           str_extract_all(coords, pattern = "-?\\d+\\.\\d+")[[1]][2],
         
         .after = nrl_schl_id)|> 
  
  select(-c(coords, typ, address, county, NAME, corp_id)) 


# make dataframe of enrolled locations that aren't school corporations that were matched in above join. These should be charter schools and private schools.

gc_nrl <- tfr |> 
  filter(is.na(nrl_lat)) |> 
  group_by(nrl_schl_name) |> 
  distinct(nrl_schl_id, .keep_all = T) |> 
  select(nrl_schl_id, nrl_schl_name)

# import dataframe of private school information to obtain addresses
pr_schl <-
  read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx",
            sheet = 3) 

# how many schools from gc_nrl within that directory

pr_schl |> 
  filter(IDOE_SCHOOL_ID %in% gc_nrl$nrl_schl_id) # 357

# geocode those 357 schools
pr_schl_2 <-  pr_schl |> 
  filter(IDOE_SCHOOL_ID %in% gc_nrl$nrl_schl_id) |> 
  
  select(IDOE_SCHOOL_ID, SCHOOL_NAME, ADDRESS, CITY, STATE, ZIP) |> 
  
  mutate(
    ad =  paste(ADDRESS, CITY, STATE, ZIP, sep = ", "),
    geocode(location = ad,
            output = "latlon",
            coord = str_c(lat, lon, sep = ","),
            typ = "np"
    )) 

# two warnings for entries not uniquely geocoded:
# 15300 N Gray Rd, ..." not uniquely geocoded, using "15300 gray rd, noblesville, in 46062, usa

# 15529 E Lincoln H not uniquely geocoded, using lincoln hwy e, new haven, in 46774, usa

tfr <- left_join(tfr, 
                 pr_schl_2 |> 
                   select("nrl_schl_id" = "IDOE_SCHOOL_ID", lat, lon), 
                 'nrl_schl_id') |> 
  
  mutate(nrl_lon = as.numeric(nrl_lon),
         nrl_lat = as.numeric(nrl_lat),
         nrl_lat = if_else(is.na(nrl_lat), lat, nrl_lat),
         nrl_lon = if_else(is.na(nrl_lon), lon, nrl_lon)
  ) |> 
  
  select(-c(lat, lon)) 

# back to two entries that didn't geocode uniquely

# pull up map from first address that wasn't geocoded properly:
get_googlemap(center = 
                paste0("15300 gray rd, noblesville, in 46062"),
              zoom = 14) |>
  ggmap()

  # shows Guerin Catholic High School.
# what school associated with that address:
pr_schl$SCHOOL_NAME[pr_schl$ADDRESS == "15300 N Gray Rd"] # Guerin Catholic High School.  All set on that one


# what city associated with second address?
pr_schl[str_which(pr_schl$ADDRESS, "15529 E Lincoln"), 8] # in New Haven

# which school
pr_schl[str_which(pr_schl$ADDRESS, "15529 E Lincoln"), 2] # Saint Louis Besancon Catholic Sch

get_googlemap(center = 
                paste0(pr_schl_2[str_which(pr_schl_2$ADDRESS, "15529 E Lincoln"), 9], ", ",
                       pr_schl_2[str_which(pr_schl_2$ADDRESS, "15529 E Lincoln"), 8]),
              zoom = 12) |>
  ggmap()
# shows New Haven, IN

# try using school name and city
get_googlemap(center = 
                paste0(pr_schl[str_which(pr_schl$ADDRESS, "15529 E Lincoln"), 2], ", ",
                       pr_schl[str_which(pr_schl$ADDRESS, "15529 E Lincoln"), 8], ", Indiana"),
              zoom = 13) |>
  ggmap()
# shows location more to the east; given scale of final map, first version is close enough

# clean up
rm(pr_schl_2, gc_nrl, pr_schl)


# assign missing nrl_schl_id to vector
gc_nrl <- tfr |> 
  filter(is.na(nrl_lat)) |> 
  group_by(nrl_schl_id, nrl_schl_name) |> 
  distinct(nrl_schl_id) |> 
  select(nrl_schl_id, nrl_schl_name)


# check gc_nrl list against public schools - many are charter schools - and geocode those locations

nrl_schl <-
  read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx",
            sheet = 2) |> 
  filter(IDOE_CORPORATION_ID %in% gc_nrl$nrl_schl_id) |> 
  
  select(IDOE_CORPORATION_ID, SCHOOL_NAME, ADDRESS, CITY, STATE, ZIP) |> 
  
  mutate(ad1 = paste(ADDRESS, CITY, STATE, ZIP, sep = ", "),
         geocode(location = ad1,
                 output = "latlon",
                 coord = str_c(lat, lon, sep = ","),
                 typ = "np"
         )) 

warnings <- last_dplyr_warnings(n=4)

# tibble of warning details
warn <- tibble(x = c("408 10th St, LaPo",
                     "1100 E US 224, Os",
                     "1100 E US 224, Os",
                     "3408 Ardmore Trai"))

# first entry
# what school
nrl_schl[str_which(nrl_schl$ad1, warn[1,1]$x), 2] # Handley Elementary School

# what address 
nrl_schl$ad1[str_which(nrl_schl$ad1, warn[1,1]$x)] # 408 10th St, LaPorte, IN, 46350

# check to see if it shows on map for points generated
get_googlemap(center = 
                paste0(nrl_schl$lat[str_which(nrl_schl$ad1, warn[1,1]$x)], ", ",
                       nrl_schl$lon[str_which(nrl_schl$ad1, warn[1,1]$x)]),
              zoom = 18) |>
  ggmap()  # right point.


# second and third entry
# what school
nrl_schl[str_which(nrl_schl$ad1, warn[2,1]$x), 2] # Norwell High School and Norwell Middle School with same address

# what address 
nrl_schl$ad1[str_which(nrl_schl$ad1, warn[2,1]$x)] # 1100 E US 224, Ossian, IN, 46777


# two entries for warn have the same addresses. 
nrl_schl$SCHOOL_NAME[str_which(nrl_schl$ad1, warn[2,1]$x)]


# check to see what is at location generated 
get_googlemap(center = 
                paste0(nrl_schl$lat[nrl_schl$SCHOOL_NAME == "Norwell Middle School"], ", ",
                       nrl_schl$lon[nrl_schl$SCHOOL_NAME == "Norwell Middle School"]),
              zoom = 18) |>
  ggmap()  # shows both schools. 

# fourth entry
# what school
nrl_schl[str_which(nrl_schl$ad1, warn[4,1]$x), 2] # Success Academy Primary School

# what address 
nrl_schl$ad1[str_which(nrl_schl$ad1, warn[4,1]$x)] # 3408 Ardmore Trail, South Bend, IN, 46628

# check to see if it shows on map for points generated
get_googlemap(center = 
                paste0(nrl_schl$lat[str_which(nrl_schl$ad1, warn[4,1]$x)], ", ",
                       nrl_schl$lon[str_which(nrl_schl$ad1, warn[4,1]$x)]),
              zoom = 19) |>
  ggmap()  # showing Career Academy South Bend Public ... 
# using google maps via web browser finds alternate location that aligns with address above 

# assign correct address from web
nrl_schl$lat[str_which(nrl_schl$ad1, warn[4,1]$x)] <- '41.687654296655765' 
nrl_schl$lon[str_which(nrl_schl$ad1, warn[4,1]$x)] <-' -86.29719727116473'


# add newly geocoded enrolled schools to tfr dataframe
tfr <- left_join(tfr, 
                 
                 nrl_schl |> 
                   select('nrl_schl_id' = 'IDOE_CORPORATION_ID', lon, lat, SCHOOL_NAME),
                 
                 join_by('nrl_schl_name' == 'SCHOOL_NAME')) |> 
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat),
         nrl_lon = if_else(is.na(nrl_lon), lon, nrl_lon),
         nrl_lat = if_else(is.na(nrl_lat), lat, nrl_lat)
  ) |> 
  rename('nrl_schl_id' = 'nrl_schl_id.x') |> 
  select(-c(nrl_schl_id.y, lon,lat)) 

# clean up
rm(gc_nrl, warn, warnings, nrl_schl)


# what enrolled locations are left without points?
gc_nrl <- 
  tfr |> 
  filter(is.na(nrl_lat)) |> 
  distinct(nrl_schl_name, .keep_all = T) |> 
  select(nrl_schl_name, nrl_schl_id)


# geocode using school corporation id 

nrl_schl <-
  read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx",
            sheet = 1) |> 
  filter(IDOE_CORPORATION_ID %in% gc_nrl$nrl_schl_id) |> 
  
  select(IDOE_CORPORATION_ID, ADDRESS, CITY, STATE, ZIP) |> 
  
  mutate(ad1 = paste(ADDRESS, CITY, STATE, ZIP, sep = ", "),
         geocode(location = ad1,
                 output = "latlon",
                 coord = str_c(lat, lon, sep = ","),
                 typ = "np"
         )) 


tfr <- left_join(tfr, 
                 
                 nrl_schl |> 
                   select('nrl_schl_id' = 'IDOE_CORPORATION_ID', lon, lat),
                 
                 join_by('nrl_schl_id' )) |> 
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat),
         nrl_lon = if_else(is.na(nrl_lon), lon, nrl_lon),
         nrl_lat = if_else(is.na(nrl_lat), lat, nrl_lat)
  ) |> 
  
  select(-c(lon,lat)) 


# clean up
rm(gc_nrl, nrl_schl)



# what schools left
gc_nrl <- 
  tfr |> 
  filter(is.na(nrl_lat)) |> 
  distinct(nrl_schl_name, .keep_all = T) |> 
  select(nrl_schl_name, nrl_schl_id)

# check by school id for private schools
read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx",
          sheet = 3) |> 
  filter(IDOE_SCHOOL_ID %in% gc_nrl$nrl_schl_id) 
# none

# check by school name for private schools
read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx",
          sheet = 3) |> 
  filter(SCHOOL_NAME %in% gc_nrl$nrl_schl_name) |> 
  select(SCHOOL_NAME, IDOE_SCHOOL_ID) 
# SCHOOL_NAME                IDOE_SCHOOL_ID
# <chr>                      <chr>         
#   1 Saint Paul Lutheran School A245          
# 2 Saint Lawrence School      A700          
# 3 Saint Mary School          B930          
# 4 Saint Mary School          C190          
# 5 Saint Lawrence School      C280          
# 6 Saint Paul Lutheran School C825  

# leave those for now and try geocoding the rest

dupes <- read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx",
                   sheet = 3) |> 
  filter(SCHOOL_NAME %in% gc_nrl$nrl_schl_name) |> 
  select(SCHOOL_NAME, IDOE_SCHOOL_ID) 


gc_nrl <- gc_nrl |> 
  filter(!nrl_schl_id %in% dupes) |> 
  mutate(ad1 = paste0(nrl_schl_name, ", Indiana"),
         geocode(location = ad1,
                 output = "latlon",
                 coord = str_c(lat, lon, sep = ","),
                 typ = "np"
         )) 

# save warnings
warnings <- last_dplyr_warnings(n=2)

# row 23
# row 29




# add results to tfr dataframe

tfr <- left_join(tfr, 
                 gc_nrl |> 
                   select(nrl_schl_id, lon, lat), 
                 join_by('nrl_schl_id')
) |> 
  
  mutate(
    nrl_lon = if_else(is.na(nrl_lon), lon, nrl_lon),
    nrl_lat = if_else(is.na(nrl_lat), lat, nrl_lat)
  ) |> 
  
  select(-c(lon,lat)) 


# revisit geocoded errors

# check first entry
gc_nrl[23, 1] # Charter Schools USA Donnan
gc_nrl[23, 2] # id is 8825
get_googlemap(center = 
                paste0(gc_nrl[23,5], ", ",
                       gc_nrl[23,4]),
              zoom = 19) |>
  ggmap()
# nope, locates in downtown Indy
# web search provides address as 1202 E Troy
get_googlemap(center = "1202 East Troy, Indianapolis, IN",
              zoom = 18) |>
  ggmap()

# that works. need lat and lon. insert directly into tfr dataframe

# lon
tfr$nrl_lon[tfr$nrl_schl_id == paste(gc_nrl[23,2])] <- 
  geocode("1202 East Troy, Indianapolis, IN", "latlon") |> select(lon) |> pull(1)

# lat
tfr$nrl_lat[tfr$nrl_schl_id == paste(gc_nrl[23,2])] <- 
  geocode("1202 East Troy, Indianapolis, IN", "latlon") |> select(lat) |> pull(1)


# check second entry
gc_nrl[29, 1] # Delaware Christian Academy
gc_nrl[29, 2] # id is C223       
get_googlemap(center = 
                paste0(gc_nrl[29,5], ", ",
                       gc_nrl[29,4]),
              zoom = 18) |>
  ggmap()
# nope, locates Sweetwater in Fort Wayne, nice store but not a school.
# web search  reveals it is permanently closed. Its coordinates are 40.250526184151, -85.2890425162077

# lat
tfr$nrl_lat[tfr$nrl_schl_id == paste(gc_nrl[29,2])] <- '40.250526184151'

# lon
tfr$nrl_lon[tfr$nrl_schl_id == paste(gc_nrl[29,2])] <- '-85.2890425162077'




# revisit errors that had duplicate names 
dupes
# SCHOOL_NAME                IDOE_SCHOOL_ID
# <chr>                      <chr>         
# 1 Saint Paul Lutheran School A245          
# 2 Saint Lawrence School      A700          
# 3 Saint Mary School          B930          
# 4 Saint Mary School          C190          
# 5 Saint Lawrence School      C280          
# 6 Saint Paul Lutheran School C825          

# starting with row 1 that has duplicate name from row 6

# look at where other transfers are coming for that school

# import school corporation boundaries
in_dis <- st_read("clean_data/20240607_corporation_bounds.geojson")


# vector of names sending transfers to dupes[1,1]

d1 <- tfr |> filter(nrl_schl_id == dupes[1,2]) |> 
  distinct(stlmt_corp_name)

# vector of names sending transfers to dupes[6,1]
d2 <- tfr |> filter(nrl_schl_id == dupes[6,2]) |> 
  distinct(stlmt_corp_name)

ggplot()+
  geom_sf(data = in_dis, 
          color = "light gray")+
  # row one school in red
  geom_sf(data = in_dis |> 
            filter(corp_name %in% d1$stlmt_corp_name),
          color = "red")+
  # row 6 schools in blue
  geom_sf(data = in_dis |> 
            filter(corp_name %in% d2$stlmt_corp_name),
          color = "blue")

# look for dupes[1,1] in south bend area and dupes[6,1] around Fort Wayne
# michigan city location 41.74653372718243, -86.89919346706904
# lat
tfr$nrl_lat[tfr$nrl_schl_id == paste(dupes[1,2])] <- '41.74653372718243'

# lon
tfr$nrl_lon[tfr$nrl_schl_id == paste(dupes[1,2])] <- '-86.89919346706904'


# Fort Wayne location is 41.07687720217215, -85.13500559629608

# add directly to tfr
# lat
tfr$nrl_lat[tfr$nrl_schl_id == paste(dupes[6,2])] <- '41.07687720217215'

# lon
tfr$nrl_lon[tfr$nrl_schl_id == paste(dupes[6,2])] <- '-85.40580264793687'


# Saint Lawrence School dupes[2,] dupes[5,]
# vector of names sending transfers to dupes[1,1]

d1 <- tfr |> filter(nrl_schl_id == dupes[2,2]) |> 
  distinct(stlmt_corp_name)

# vector of names sending transfers to dupes[6,1]
d2 <- tfr |> filter(nrl_schl_id == dupes[5,2]) |> 
  distinct(stlmt_corp_name)

ggplot()+
  geom_sf(data = in_dis, 
          color = "light gray")+
  # row one school in red
  geom_sf(data = in_dis |> 
            filter(corp_name %in% d1$stlmt_corp_name),
          color = "red")+
  # row 6 schools in blue
  geom_sf(data = in_dis |> 
            filter(corp_name %in% d2$stlmt_corp_name),
          color = "blue")

# look for dupes[2,1] around eastern border near Cincinatti and dupes[6,1] around Indy metro area

# dupes[2,1] is in Lawrenceburg right on the river 39.09691268578699, -84.85210198839741

# add directly to tfr

# lat
tfr$nrl_lat[tfr$nrl_schl_id == paste(dupes[2,2])] <- '39.09691268578699'

# lon
tfr$nrl_lon[tfr$nrl_schl_id == paste(dupes[2,2])] <- '-84.85210198839741'


# indy location 39.841755113258344, -86.04691617526056

# lat
tfr$nrl_lat[tfr$nrl_schl_id == paste(dupes[5,2])] <- '39.841755113258344'

# lon
tfr$nrl_lon[tfr$nrl_schl_id == paste(dupes[5,2])] <- '-86.04691617526056'


# SaintSaint Mary School dupes[3,] dupes[4,]
# vector of names sending transfers to dupes[1,1]

d1 <- tfr |> filter(nrl_schl_id == dupes[3,2]) |> 
  distinct(stlmt_corp_name)

# vector of names sending transfers to dupes[6,1]
d2 <- tfr |> filter(nrl_schl_id == dupes[4,2]) |> 
  distinct(stlmt_corp_name)

ggplot()+
  geom_sf(data = in_dis, 
          color = "light gray")+
  # row one school in red
  geom_sf(data = in_dis |> 
            filter(corp_name %in% d1$stlmt_corp_name),
          color = "red")+
  # row 6 schools in blue
  geom_sf(data = in_dis |> 
            filter(corp_name %in% d2$stlmt_corp_name),
          color = "blue")

# look for dupes[3,1] around Gary/Hammond area and dupes[4,1] around Anderson/Muncie area

# dupes[3,1] is in Merrilville 41.456751025470815, -87.35087632874819

# add directly to tfr

# lat
tfr$nrl_lat[tfr$nrl_schl_id == paste(dupes[3,2])] <- '41.456751025470815'

# lon
tfr$nrl_lon[tfr$nrl_schl_id == paste(dupes[3,2])] <- '-87.35087632874819'


# Anderson area is  40.11073580470255, -85.67456576078074

# lat
tfr$nrl_lat[tfr$nrl_schl_id == paste(dupes[4,2])] <- '40.11073580470255'

# lon
tfr$nrl_lon[tfr$nrl_schl_id == paste(dupes[4,2])] <- '-85.67456576078074'

# check for NA's in enrolled schools
sum(is.na(tfr$nrl_lat)) # zero
sum(is.na(tfr$nrl_lon)) # zero
sum(is.na(tfr$stlmt_corp_lon)) # zero
sum(is.na(tfr$stlmt_corp_lat)) # zero


# shorten stlmt_corp_lat/lon column names
tfr <- rename(tfr, 
              stlmt_lat = stlmt_corp_lat,
              stlmt_lon = stlmt_corp_lon)


write_csv(tfr, "clean_data/20240613_transfer_pts_18_24.csv")

