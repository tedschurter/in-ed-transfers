library(ggmap)
library(readxl)
library(tidyverse)
library(ggplot2)


# path to files in raw_data folder
path  <- "raw_data"

#list of all files in raw_data folder
files <- list.files(path, pattern = "^[^~]", full.names = T)

# object with all files that include word transfer
transfer_files <- grep("transfer", files, value = T)

# remove duplicates for years when second version (denoted by v2) of transfer data was released (2021-2022; 2022-2023)
transfer_files <- transfer_files[c(1,5,2,6,7,4)]


# loop to aggregate all the data into one dataframe
for (i in 1:length(transfer_files)) {
  tfr <- read_xlsx(transfer_files[i],
                   sheet = 4,
                   skip = 1)
  
  # standardize column names
  colnames(tfr) <-
    c(
      "stlmt_corp_id",
      "stlmt_corp_name",
      "nrl_schl_id",
      "nrl_schl_name",
      "par_ch",
      "p_xfer_oth",
      "p_xfer_chrtr",
      "pr_ch_sclrs"
    )
  
  # select necessary columns
  tfr <- 
    tfr |>
    select(
      c(
        "stlmt_corp_id",
        "stlmt_corp_name",
        "nrl_schl_id",
        "nrl_schl_name",
        "par_ch",
        "p_xfer_oth",
        "p_xfer_chrtr",
        "pr_ch_sclrs"
      )
    ) |>
    # extract year information from filename and add column to indicate year
    mutate(
      yr = as.numeric(sub(
        "^(.{4}).*-(\\d{4}).*", "\\2",
        transfer_files[i])),
      
      yr = year(
        as.Date(
          paste0(yr, "_01_01"),
          format = "%Y_%m_%d")
      )
    ) |>
    
    group_by(stlmt_corp_name, nrl_schl_name) |>
    
    # add column for total number of transfers from settlement school to nrl school
    mutate(tot_xfr = sum(par_ch, p_xfer_oth, p_xfer_chrtr, pr_ch_sclrs, na.rm = T)) |> 
    
    mutate(
      # add color for map
      clr = if_else(stlmt_corp_name == nrl_schl_name, '#fdae61', '#0868ac')
    ) |>
    ungroup() |>
    
    # add column to control size of circles in leaflet map
    mutate(xfr_sqrt = sqrt(tot_xfr)) |> 
    
    # select final columns 
    select(stlmt_corp_name,
           stlmt_corp_id,
           nrl_schl_name,
           nrl_schl_id,
           yr,
           par_ch,
           p_xfer_oth,
           p_xfer_chrtr,
           pr_ch_sclrs,
           tot_xfr,
           xfr_sqrt,
           clr)
  
  # save dataframe and bind subsequent additions
  if (i == 1) {
    tfr_2 <- tfr
  } else {
    tfr_2 <-  rbind(tfr, tfr_2)
  }
}

# clean up
tfr <- tfr_2

rm(tfr_2)

####

# CLEAN NAMES ####

# replace abbreviations in school corporation and enrolled school names

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
tfr <- tfr |> 
  filter(stlmt_corp_id != nrl_schl_id) |> 
  # replace abreviations in corporation and school names for use in titles
  
  mutate(
    # add column that identifies both settlement school corporation id and the enrolled school/school corporation id to use in loop and filename
    
    stlmt_corp_name =
      str_replace_all(stlmt_corp_name,
                      setNames(rplc_names, set_names)),
    nrl_schl_name =
      str_replace_all(nrl_schl_name,
                      setNames(rplc_names, set_names)),
    
    # remove any extra spaces from name columns
    across(ends_with("_name"),
           .fns = ~ str_replace_all(., "  ", " "))) 



# check the stlmt_corp_name column for abbreviations, etc and add corrections to above list of set_names and rplc_names
tfr |> distinct(stlmt_corp_name) |> View()


# replace Union County/Clg Corner Joint School District with Union County-College Corner Joint School District
tfr$stlmt_corp_name[tfr$stlmt_corp_name == "Union County/Clg Corner Joint School District"] <- "Union County-College Corner Joint School District"

# replace the same for nrl_schl_name column
tfr$nrl_schl_name[tfr$nrl_schl_name == "Union County/Clg Corner Joint School District"] <- "Union County-College Corner Joint School District"


# check the rest of nrl_schl_name column for abbreviations, etc and add corrections to above list of set_names and rplc_names
tfr |> 
  distinct(nrl_schl_name) |> 
  View()

# Greenwood Christian Academy High S - should be Greenwood Christian Academy High School
tfr$stlmt_corp_name[tfr$stlmt_corp_name == "Greenwood Christian Academy High S"] <- "Greenwood Christian Academy High School"

# replace the same for nrl_schl_name column
tfr$nrl_schl_name[tfr$nrl_schl_name == "Greenwood Christian Academy High S"] <- "Greenwood Christian Academy High School"

# HIM By HER Collegiate School for t = HIM By HER Collegiate School for the Arts  ###Note permanently closed in 2022
tfr$stlmt_corp_name[tfr$stlmt_corp_name == "HIM By HER Collegiate School for t"] <- "HIM By HER Collegiate School for the Arts"

# replace the same for nrl_schl_name column
tfr$nrl_schl_name[tfr$nrl_schl_name == "HIM By HER Collegiate School for t"] <- "HIM By HER Collegiate School for the Arts"


# above code replaces Tech with Technology but Lake Ridge New Technology Schools should be Lake Ridge New Tech Schools
tfr$stlmt_corp_name[tfr$stlmt_corp_name == "Lake Ridge New Technology Schools"] <- "Lake Ridge New Tech Schools"

# replace the same for nrl_schl_name column
tfr$nrl_schl_name[tfr$nrl_schl_name == "Lake Ridge New Technology Schools"] <- "Lake Ridge New Tech Schools"

# Saint Michael Saint Gabriel Archangels S = Saint Michael Saint Gabriel Archangels School
tfr$stlmt_corp_name[tfr$stlmt_corp_name == "Saint Michael Saint Gabriel Archangels S"] <- "Saint Michael Saint Gabriel Archangels School"

# replace the same for nrl_schl_name column
tfr$nrl_schl_name[tfr$nrl_schl_name == "Saint Michael Saint Gabriel Archangels S"] <- "Saint Michael Saint Gabriel Archangels School"

# The Hope Academy, Incorporated.* and 	The Hope Academy, Incorporated. should both be 	The Hope Academy, Incorporated
tfr$stlmt_corp_name[tfr$stlmt_corp_name == "The Hope Academy, Incorporated.*"] <- "The Hope Academy, Incorporated"

# replace the same for nrl_schl_name column
tfr$nrl_schl_name[tfr$nrl_schl_name == "The Hope Academy, Incorporated.*"] <- "The Hope Academy, Incorporated"

tfr$stlmt_corp_name[tfr$stlmt_corp_name == "The Hope Academy, Incorporated."] <- "The Hope Academy, Incorporated"

# replace the same for nrl_schl_name column
tfr$nrl_schl_name[tfr$nrl_schl_name == "The Hope Academy, Incorporated."] <- "The Hope Academy, Incorporated"

# also need to remove * from the end of multiple names

tfr <- tfr |> 
  mutate(
    nrl_schl_name = (str_remove(nrl_schl_name, "\\*")),
    nrl_schl_name = str_trim(nrl_schl_name, "both"),
    stlmt_corp_name = str_trim(stlmt_corp_name, "both")
  ) 



##### 


# Are there any NAs in stlmt_corp_name?
sum(is.na(tfr$stlmt_corp_name)) # 0

# Are there any nrl_schl_name
sum(is.na(tfr$nrl_schl_name)) # 0

# check number of distinct stlmt corp ids
tfr |>
  group_by(stlmt_corp_id) |>
  count() |>
  nrow() # 291

# ensure number of distinct stlmt corp names matches number of stlmt corp ids
tfr |>
  group_by(stlmt_corp_name) |>
  count() |>
  nrow() #291



###
# write cleaned transfer data to csv
write_csv(tfr, "clean_data/transfers_18_23.csv")


# ADD LOCATION DATA ####

# add latitude and longitude for both school corporations and enrolled locations for mapping.

# # geocode lat and lon for each school corporation ####

corp <-
  read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx") |>
  select(1:3, 9:12)

colnames(corp) <-
  c("nces_id",
    "corp_id",
    "corp_name",
    "address",
    "city",
    "state",
    "zip")

# geocode locations
corp <- corp |>
  mutate(geocode(location = str_c(paste(
    address, city, state, zip
  ), sep = ", "),
  output = "latlon"),
  # concatenate to format gmapsdistance package will use - lat+lon
  coord = str_c(lat, lon, sep = "+"))
#select(-c(address, city, state, zip))

warn <- last_dplyr_warnings()



# visualize locations on map to ensure they are within Indiana borders 
ggplot() +
  geom_sf() +
  geom_point(data = corp,
             aes(lon, lat),
             size = 1,
             color = "black")

# two points outside state boundaries

# one point faaaaaaar east 
corp |> filter(lon > -10) 

# North Newton School Corp was geocoded as Morocco, not Morocco, IN because address is listed as PO Box 8. Try again without PO box
geocode("North Newton School Corporation, Morocco, Indiana")
# works

# replace longitude
corp$lon[corp$corp_name == "North Newton School Corp"] <- geocode("North Newton School Corporation, Morocco, Indiana")[[1]]

# replace latitude for North Newton School Corp
corp$lat[corp$corp_name == "North Newton School Corp"] <- geocode("North Newton School Corporation, Morocco, Indiana")[[2]]


# check what points are left outside of Indiana
ggplot() +
  geom_sf() +
  geom_point(data = corp,
             aes(lon, lat),
             size = 1,
             color = "black")


# still one way west of Indiana

# find locations for point west of -88
corp %>%
  filter(lon < -88) %>%
  select(corp_name)

# Blue River Valley Schools

# attempt geocode with limited address
geocode("Blue River Valley Schools, New Castle, Indiana")

# replace longitude coordinates in dataframe
corp$lon[corp$corp_name   == "Blue River Valley Schools"]   <- geocode("Blue River Valley Schools, New Castle, Indiana")[[1]]

# replace latitude coordinates in dataframe
corp$lat[corp$corp_name   == "Blue River Valley Schools"]   <- geocode("Blue River Valley Schools, New Castle, Indiana") [[2]]

#

# check what points are left outside of Indiana
ggplot() +
  geom_sf() +
  geom_point(data = corp,
             aes(lon, lat),
             size = 1,
             color = "black")



# replace abbreviations in corporation names using same vectors as used for transfer data

# school corporation directory
corp <- corp |> 
  mutate(
    corp_name =
      str_replace_all(corp_name,
                      setNames(rplc_names, set_names)))

# replace Union County/Clg Corner Joint School District with
# Union County-College Corner Joint School District

corp$corp_name[corp$corp_name == "Union County/Clg Corner Joint School District"] <- "Union County-College Corner Joint School District"


# Greenwood Christian Academy High S - should be Greenwood Christian Academy High School
corp$corp_name[corp$corp_name == "Greenwood Christian Academy High S"] <- "Greenwood Christian Academy High School"

# HIM By HER Collegiate School for t = HIM By HER Collegiate School for the Arts  ###Note permanently closed in 2022

corp$corp_name[corp$corp_name == "HIM By HER Collegiate School for t"] <- "HIM By HER Collegiate School for the Arts"

# above code replaces Tech with Technology but Lake Ridge New Technology Schools should be Lake Ridge New Tech Schools

corp$corp_name[corp$corp_name == "Lake Ridge New Technology Schools"] <- "Lake Ridge New Tech Schools"

# Saint Michael Saint Gabriel Archangels S = Saint Michael Saint Gabriel Archangels School  
corp$corp_name[corp$corp_name == "Saint Michael Saint Gabriel Archangels S"] <- "Saint Michael Saint Gabriel Archangels School"

# The Hope Academy, Incorporated.* and 	The Hope Academy, Incorporated. should both be 	The Hope Academy, Incorporated

corp$corp_name[corp$corp_name == "The Hope Academy, Incorporated.*"] <- "The Hope Academy, Incorporated"


# write corp name data with new coordinates and replaced abbreviations to csv
write_csv(corp, "clean_data/corp_11.csv")



# see what corporations don't mesh between corp dataframe and tfr dataframe before joining
anti_join(
  tfr,
  corp |> select(corp_id, corp_name), 
  
  join_by("stlmt_corp_name" == "corp_name")) |> 
  distinct(stlmt_corp_name, .keep_all = T) |> 
  select(stlmt_corp_id, stlmt_corp_name)

# returns: 
# stlmt_corp_id stlmt_corp_name                 

#  0940          West Clark Community Schools    
#  8435          Northern Wells Community Schools

# West Clark Community Schools doesn't show up in corp dataframe.

# turns out West Clark Community Schools was replaced by Borden-Henryville School Corporation; it was formed on July 1, 2020 after the dissolution of the West Clark Community Schools. via www.bhsc.school/en-US/about-the-district-fb6849e4

# Northern Wells

# what details from corp dataframe using corp_id 8435
corp |> filter(corp_id == tfr$stlmt_corp_id[tfr$stlmt_corp_name == "Northern Wells Community Schools"][1])

# corp directory returns different corporation name: "Norwell Community Schools"
# replace "Northern Wells Community Schools" corporation name in tfr stlmt colmns and nrl columns with "Norwell Community Schools"
tfr$stlmt_corp_name[tfr$stlmt_corp_name == "Northern Wells Community Schools"] <-
  corp$corp_name[corp$corp_id == tfr$stlmt_corp_id[tfr$stlmt_corp_name == "Northern Wells Community Schools"][1]]

# do the same for nrl_schl_name
tfr$nrl_schl_name[tfr$nrl_schl_name == "Northern Wells Community Schools"] <-
  corp$corp_name[corp$corp_id == tfr$nrl_schl_id[tfr$nrl_schl_name == "Northern Wells Community Schools"][1]]

####

# check again what corporations don't mesh between corp dataframe and tfr dataframe before joining
anti_join(
  tfr,
  corp |> select(corp_id, corp_name), 
  
  join_by("stlmt_corp_name" == "corp_name")) |> 
  distinct(stlmt_corp_name, .keep_all = T) |> 
  select(stlmt_corp_id, stlmt_corp_name)

# still shows West Clark, not surprising, name hasn't changed. 

#  stlmt_corp_id stlmt_corp_name             
# 1 NA            West Clark Community Schools


# join transfer data to school corporation location data 

tfr <- 
  left_join(
    tfr,
    corp,
    join_by("stlmt_corp_name" == "corp_name")
  ) |> 
  rename('stlmt_lon' = 'lon', "stlmt_lat" = 'lat') |> 
  select(-c(corp_id, nces_id, coord))

sum(is.na(tfr$stlmt_lon)) #92
sum(is.na(tfr$stlmt_lat)) #92

tfr |> filter(is.na(stlmt_lat))

# issue is West Clark Community Schools; replace address, city, state, zip lat and lon with data from Borden_Henryville School Corporation

tfr$address[tfr$stlmt_corp_name == "West Clark Community Schools"] <-   tfr$address[tfr$stlmt_corp_name == "Borden-Henryville School Corporation"][1]

tfr$city[tfr$stlmt_corp_name == "West Clark Community Schools"] <- 
  tfr$city[tfr$stlmt_corp_name == "Borden-Henryville School Corporation"][1]

tfr$state[tfr$stlmt_corp_name == "West Clark Community Schools"] <- 
  tfr$state[tfr$stlmt_corp_name == "Borden-Henryville School Corporation"][1]

tfr$zip[tfr$stlmt_corp_name == "West Clark Community Schools"] <- 
  tfr$zip[tfr$stlmt_corp_name == "Borden-Henryville School Corporation"][1]

tfr$stlmt_lon[tfr$stlmt_corp_name == "West Clark Community Schools"] <- 
  tfr$stlmt_lon[tfr$stlmt_corp_name == "Borden-Henryville School Corporation"][1]

tfr$stlmt_lat[tfr$stlmt_corp_name == "West Clark Community Schools"] <- 
  tfr$stlmt_lat[tfr$stlmt_corp_name == "Borden-Henryville School Corporation"][1]


# check stlmt lon and lat again
sum(is.na(tfr$stlmt_lon)) #0
sum(is.na(tfr$stlmt_lat)) #0


# add coordinates for enrolled school corporations
tfr <-
  
  left_join(tfr |> 
              select(-c(address, city, state, zip)), 
            
            (corp |> select(corp_id, lat, lon)),
            
            join_by("nrl_schl_id" == "corp_id")) |>
  
  rename('nrl_lon' = 'lon', "nrl_lat" = 'lat')


sum(is.na(tfr$nrl_lat)) #14985
sum(is.na(tfr$nrl_lon)) #14985

### 

# as expected, private schools didn't get lat and lon from the corp dataframe of public school corporations


#### adding steps needed to create all_schl_ll.csv

# list of private schools 2023
npschl <-
  read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx",
            sheet = 3) 

# geocode private schools ####
npschl_ll <- npschl %>%
  # combine facility name, County and IN and send to ggmap::geocode to generate lat and lon coordinates
  mutate(
    geocode(location = str_c(paste(
      ADDRESS, CITY, STATE, ZIP
    ),
    sep = ", "),
    output = "latlon"),
    # combine lat and lon into one column
    coord = str_c(lat, lon, sep = "+"),
    type = "np"
  )

warnings <- last_dplyr_warnings()

# 101 National Road..." not uniquely geocoded, using "101 n national rd, columbus, in 47201, usa"
# 15529 E Lincoln H..." not uniquely geocoded, using "lincoln hwy e, new haven, in 46774, usa"
# 15300 N Gray Rd N..." not uniquely geocoded, using "15300 gray rd, noblesville, in 46062, usa"
# 5885 N Crittenden..." not uniquely geocoded, using "5885 n, 5885 crittenden ave, indianapolis, in 46220, usa"



# how many NA's
sum(is.na(npschl_ll$lat)) # returns 0

# write csv with latitude and longitude
write_csv(npschl_ll, "clean_data/npschl_ll.csv")

# geocode public schools data ####

# list of public schools 2023
schl <-
  read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx",
            sheet = 2)

# geocode public schools
schl_ll <- schl %>%
  # combine facility name, County and IN and send to ggmap::geocode to generate
  # lat and lon coordinates
  mutate(
    geocode(location = str_c(paste(
      ADDRESS, CITY, STATE, ZIP
    ),
    sep = ", "),
    output = "latlon"),
    # concatenate to format gmapsdistance package will use - lat+lon
    coord = str_c(lat, lon, sep = "+"),
    type = "p"
  )

# returns 51 warnings; write them to df
warn <- dplyr::last_dplyr_warnings(n = 64)


# some of the warnings show out of state locations; ggmap sometimes mislocates addresses that include city names from other states or countries. check school locations by mapping them
ggplot() +
  geom_sf() +
  geom_point(data = schl_ll,
             aes(lon, lat, color = type))


# one point waaay east and north
schl_ll |> 
  filter(lon > -80)

# incorrectly located Ireland Elementary School from Greater Jasper Consolidated Schs in the country of Ireland

# reduce address fields and replace longitude
schl_ll$lon[schl_ll$SCHOOL_NAME == "Ireland Elementary School"] <- geocode("Ireland Elementary School, Indiana")[[1]]

# reduce address fields and replace latitude
schl_ll$lat[schl_ll$SCHOOL_NAME == "Ireland Elementary School"] <-geocode("Ireland Elementary School, Indiana")[[2]]


# check again to see if any schools are located outside Indiana boundaries
ggplot() +
  geom_sf() +
  geom_point(data = schl_ll,
             aes(lon, lat, color = type))


# one spot north of 41.5ish
schl_ll %>% filter(lat == max(schl_ll$lat, na.rm = T)) %>% select(SCHOOL_NAME)
# returns LaVille Jr.-Sr. High School

# manually geocode and add values
schl_ll$lon[schl_ll$SCHOOL_NAME == "LaVille Jr-Sr High School"] <- geocode("LaVille Jr.-Sr. High School, Indiana")[[1]]

# manually geocode and add values
schl_ll$lat[schl_ll$SCHOOL_NAME == "LaVille Jr-Sr High School"] <- geocode("LaVille Jr.-Sr. High School, Indiana")[[2]]

# check again to see if any schools are located outside Indiana boundaries
ggplot() +
  geom_sf() +
  geom_point(data = schl_ll,
             aes(lon, lat, color = type))


# looks good.


# returning to error message generated after initial geocode; run those locations through again using adjusted address

# create sequence as long as the warning list created when schl_ll was geocoded to loop through list of warnings
dur <- seq(1, length(warn), 1)

# loop through list and extract error message and assign to wn_1 dataframe
for (i in 1:length(dur))  {
  wn <- warn[[i]][["parent"]][["message"]]
  
  if (dur[i] == 1) {
    wn_1 <- wn }
  
  else {
    wn_1 <-  rbind(wn, wn_1)
  } 
}

## extract address portion form warning
wn_2 <- tibble(warning = wn_1) |> 
  #filter(str_detect(warning, "failed") == T) |> 
  
  # add new column with address portion of error message
  mutate(address = 
           str_sub(warning,  start = 13L, end = 30L)
  )

# vector of unique extracted addresses
wn_3 <- distinct(wn_2[,2])

# set number of times to run loop to equal the number of addresses that need to be re-geocoded
dur <- seq(1, length(wn_3$address))

# the ggmap error message includes incomplete combined address fields; in order to use this combined string that sometimes includes the beginning of a city and sometimes doesn't, the schl_ll address fields must be combined into a new column first

# loop through the list of addresses from warning list and filter the schl_ll dataframe by a newly created column - new_ad that combines the ADDRESS and CITY columns - and add any rows that contain matching address components into a new dataframe 

for (i in 1:length(wn_3$address)){
  
  failed <- schl_ll |> 
    mutate(new_ad = paste0(ADDRESS, " ", CITY)) |> 
    filter(str_detect(new_ad, paste(wn_3[i,1])) == T)
  
  if (dur[i] == 1) {
    failed_1 <- failed }
  
  else {
    failed_1 <-  rbind(failed, failed_1)
  }
}


# clean up
rm(failed, wn_1, wn_2, wn_3)

# geocode those entries again using just the school name, city and state
failed_1 <- failed_1 |> 
  mutate(lon = geocode(paste(SCHOOL_NAME, CITY, STATE, sep = " "))[1],
         lat = geocode(paste(SCHOOL_NAME, CITY, STATE, sep = " "))[2]
  ) 


# vector of IDOE school id's that didn't geocode properly
f <- failed_1$IDOE_SCHOOL_ID


# use for loop to replace lon and lat values that didn't geocode correctly first time through
for (i in 1:length(f)){
  
  # replace longitude with re-geocoded longitude using reduced address from failed_1 dataframe
  schl_ll$lon[schl_ll$IDOE_SCHOOL_ID == f[i]] <- failed_1 |> filter(IDOE_SCHOOL_ID == f[i]) |> select(lon) |> pull()
  
  # replace latitude with re-geocoded latitude using reduced address from failed_1 dataframe
  schl_ll$lat[schl_ll$IDOE_SCHOOL_ID == f[i]] <- failed_1 |> filter(IDOE_SCHOOL_ID == f[i]) |> select(lat) |> pull()
}
#

# double check failed list to ensure lat and lon were successfully moved to schl_ll

schl_ll |> 
  filter(IDOE_SCHOOL_ID %in% f) |> 
  View()
#

# clean up 
rm(failed_1, failed_2, warn, warnings)

# make lat and lon numeric, not list
schl_ll$lat <- as.numeric(schl_ll$lat)
schl_ll$lon <- as.numeric(schl_ll$lon)

# write data to csv
write_csv(schl_ll, "clean_data/schl_ll.csv")

# compare public and private school lat/lon dataframes before merging
janitor::compare_df_cols(npschl_ll, schl_ll)

# combine public and private school dataframes; remove unneeded columns
all_schl_ll <-
  rbind(schl_ll %>% select(-c(
    CORPORATION_NAME, IDOE_CORPORATION_ID, LOCALE, NCES_ID
  )), npschl_ll %>% select(-c(CHOICE_FLAG)))

# check for NA's

sum(is.na(all_schl_ll$lat)) # 0
sum(is.na(all_schl_ll$lon)) # 0


# write combined data with lat and lon to csv
write_csv(all_schl_ll, file = "clean_data/all_schl_ll.csv")

#####

# join with private schools only from all_schl_ll 
tfr <- left_join(
  tfr, 
  all_schl_ll |>
    
    select(IDOE_SCHOOL_ID, SCHOOL_NAME, lon, lat, type) |> 
    
    filter(type != "p"), # "p" denotes public schools
  
  join_by("nrl_schl_id" == "IDOE_SCHOOL_ID")) |>
  
  rename('nrl_lon_2' = 'lon', "nrl_lat_2" = 'lat') |> 
  
  mutate(
    nrl_lat = if_else(
      is.na(nrl_lat) == T, nrl_lat_2, nrl_lat),
    
    nrl_lon  = if_else(
      is.na(nrl_lon) == T, nrl_lon_2, nrl_lon)
  ) |>
  
  select(-c(nrl_lat_2, nrl_lon_2, SCHOOL_NAME, type))

# check for NA's in enrolled schools
sum(is.na(tfr$nrl_lon)) # 2237
sum(is.na(tfr$nrl_lat)) # 2237


# create separate dataframe of enrolled schools missing lat and lon data and use ggmaps to geocode them and assign coordinates to nrl_lat and nrl_lon
gc_nrl <- tfr |> group_by(nrl_schl_name) |>
  filter(is.na(nrl_lat)) |>
  distinct(nrl_schl_name, .keep_all = T) |>
  mutate(geocode(paste0(nrl_schl_name, " Indiana"), "latlon"))|> 
  select(nrl_schl_id, lat, lon) |> 
  rename('nrl_lat' = 'lat', 'nrl_lon' = 'lon')


# save warnings
warnings <- dplyr::last_dplyr_warnings() 

# join them back together
tfr <- left_join(tfr,
                 gc_nrl,
                 'nrl_schl_id') |> 
  mutate(
    nrl_lat = if_else(
      is.na(nrl_lat.x) == T, nrl_lat.y, nrl_lat.x),
    
    nrl_lon  = if_else(
      is.na(nrl_lon.x) == T, nrl_lon.y, nrl_lon.x)
  ) |>
  
  select(-c(nrl_lon.y, nrl_lat.y, nrl_lon.x, nrl_lat.x, nrl_schl_name.y)) |>
  
  rename("nrl_schl_name" = "nrl_schl_name.x") 


sum(is.na(tfr$nrl_lon)) # 0
sum(is.na(tfr$nrl_lat)) # 0


# check warnings and add back as necessary

as.data.frame(do.call(rbind, warnings)) |> 
  select(message) |> 
  mutate(schl = str_extract(message, "(?<=nrl_schl_name = ).*"),
         schl = str_extract(schl, "(?<=\\\\\")[^\\\\]*(?=\\\\\")")) |> 
  select(-message)

# returns: 
#  Delaware Christian Academy

# go through them one at a time

# # import school corp directory as first resource to check against
sdir <-
  read_xlsx("raw_data/2023-2024-School-Directory-10.2.23.xlsx", sheet = 2)

# search within school directory name for Delaware Christian Academy
sdir |> filter(str_detect(SCHOOL_NAME, "Delaware Christian Academy")) |> select(SCHOOL_NAME, ADDRESS, CITY)

# returns nothing try Delaware Christian
sdir |> filter(str_detect(SCHOOL_NAME, "Delaware Christian")) |> select(SCHOOL_NAME, ADDRESS, CITY)

# returns nothing; try Delaware
sdir |> filter(str_detect(SCHOOL_NAME, "Delaware")) |> select(SCHOOL_NAME, ADDRESS, CITY) 
# returns 
# 1 Delaware Trail Elementary School 3680 S Hornaday Rd Brownsburg
# 2 Delaware Elementary School       700 N Garvin St    Evansville
# what about filtering directory using school id from tfr dataframe

sdir |> filter(IDOE_SCHOOL_ID == paste(tfr$nrl_schl_id[tfr$nrl_schl_name ==  "Delaware Christian Academy"][1]))  

# returns nothing; check online for address 
# turns out it permanently closed. address was 8400 E County Rd 400 N, Albany, IN 

# replace Delaware Christian Academy lon
tfr$nrl_lon[tfr$nrl_schl_name == "Delaware Christian Academy"] <- 
  geocode("8400 East County Rd 400 N, Albany, IN", "latlon") |> select(lon) |> pull(1)

# replace Delaware Christian Academy lat
tfr$nrl_lat[tfr$nrl_schl_name == "Delaware Christian Academy"] <- 
  geocode("8400 East County Rd 400 N, Albany, IN", "latlon") |> select(lat) |> pull(1)


# check for NA's in coordinate columns
sum(is.na(tfr$stlmt_lon))
sum(is.na(tfr$stlmt_lat))
sum(is.na(tfr$nrl_lon))
sum(is.na(tfr$nrl_lat))



# 20240319 update: 21st Century Charter High School in Gary was not located properly

# replace 21st Century Charter High School in Gary latitude
tfr$nrl_lat[tfr$nrl_schl_id == 9545] <- geocode("1440 E 35th Ave, Gary, IN 46409", "latlon") |> select(lat) |> pull(1)

# replace 21st Century Charter High School in Gary latitude
tfr$nrl_lon[tfr$nrl_schl_id == 9545] <- geocode("1440 E 35th Ave, Gary, IN 46409", "latlon") |> select(lon) |> pull(1)



# write transfer data with latitude and longitude to csv ####
write_csv(tfr, "clean_data/transfers_18_23_ll.csv")
