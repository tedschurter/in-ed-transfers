library(tidyverse)
library(tigris)
library(fuzzyjoin)
library(sf)

# Indiana school district boundaries from tigris don't share a common key with Indiana Department of Education data. Create matches using fuzzy join.

# import data 
options(tigris_class = "sf", tigris_use_cache = T)

in_dis <- school_districts("IN", cb = T)

# import state data (this was needed when creating maps with sxfer data; similar process could be used with other state data)

# 20240319 NOTE: student transfer data was updated and replaced with transfers_18_23.csv; this script uses original version 
sxfer <- read_csv("clean_data/transfers_18_23.csv") |> 
  select(stlmt_corp_id, stlmt_corp_name) |> 
  distinct(stlmt_corp_name, .keep_all = T)



# join school corporation boundary information from tirgris with transfer data using corporation names and fuzzy join to match similar names
f_in_dis <- 
  stringdist_join(
    in_dis, sxfer,
    by = c('NAME' = 'stlmt_corp_name'),
    mode = 'inner', 
    method = 'lv',
    max_dist = 0, ignore_case = T,
    distance_col = 'distance') |> 
  select(stlmt_corp_name, 
         distance, 
         colnames(in_dis), 
         -NAME) |> 
  distinct(stlmt_corp_name, .keep_all = T)

# not all school corporations aligned; adjust settings on fuzzy join

# check results when filter set to 4 and assign to x
x <- stringdist_join(
  in_dis, sxfer,
  by = c('NAME' = 'stlmt_corp_name'),
  mode = 'inner', 
  method = 'lv',
  max_dist = 4, ignore_case = T,
  distance_col = 'distance') |> 
  select(NAME, stlmt_corp_name, distance, colnames(in_dis)) |> 
  distinct(stlmt_corp_name, .keep_all = T) |> 
  filter(distance >= 1) 

View(x)

# check results to see where increasing max distance to four determined correct matches; manually update f_in_dis dataframe as needed

# Perry Central Community Schools is the same
f_in_dis <- rbind(f_in_dis, x[32, 2:13])

# Mt Vernon Community School Corporation
f_in_dis <- rbind(f_in_dis, x[41, 2:13])

# New Albany-Floyd County Consolidated Schools is the same
f_in_dis <- rbind(f_in_dis, x[42, 2:13])

# C A Beard Memorial School Corporation is the same
f_in_dis <- rbind(f_in_dis, x[47, 2:13])

# Evansville Vanderburgh School Corporation is the same
f_in_dis <- rbind(f_in_dis, x[50, 2:13])

# LaPorte Community School Corporation is the same
f_in_dis <- rbind(f_in_dis, x[52, 2:13])


# 
nrow(f_in_dis) # 242
nrow(in_dis)   # 291
n_distinct(sxfer$stlmt_corp_name) # 291


x <- anti_join(sxfer,f_in_dis,  'stlmt_corp_name') |> ungroup() |> 
  distinct(stlmt_corp_name) 

# to make it easier to copy columns, changing NAME to stlmt_corp_name in in_dis and remove distance column from f_in_dis
in_dis <- in_dis |> select("stlmt_corp_name" = "NAME", everything())

# remove distance column
f_in_dis$distance <- NULL

# manually clean up remaining data. Assign right name from transfer data set to in_dis, then apply 

# MSD Southwest Allen County Schools 
in_dis[96,1] <- x[1,]
f_in_dis <- rbind(f_in_dis, in_dis[96,])

# Bartholomew County School Corporation = Bartholomew County School Corporation
in_dis[163,1] <- x[2,]
f_in_dis <- rbind(f_in_dis, in_dis[163,])

# Western Boone County Community School Corporation = 	Western Boone County Community School District
in_dis[229,1] <- x[3,]
f_in_dis <- rbind(f_in_dis, in_dis[229,])

# Brown County School Corporation = Brown County County School Corporation
in_dis[118,1] <- x[4,]
f_in_dis <- rbind(f_in_dis, in_dis[118,])  

# Lewis Cass Schools is the former Southeastern School Corp: https://www.pharostribune.com/news/local_news/article_501009b8-76a0-5a11-97df-ef91f0135238.html
in_dis[257,1] <- x[5,]
f_in_dis <- rbind(f_in_dis, in_dis[257,]) 

# Borden-Henryville School Corp = Borden-Henryville School Corporation
in_dis[80,1] <- x[6,]
f_in_dis <- rbind(f_in_dis, in_dis[80,]) 

# x[7, ] West Clark Community Schools

# Community Schools of Frankfort = Frankfort Community Schools
in_dis[85,1] <- x[8,]
f_in_dis <- rbind(f_in_dis, in_dis[85,])

# Barr-Reeve Community Schools Incorporated = Barr-Reeve Community School Corporation
in_dis[115,1] <- x[9,]
f_in_dis <- rbind(f_in_dis, in_dis[115,])

# Washington Community Schools = Washington Community School Corporation
in_dis[140,1] <- x[10,]
f_in_dis <- rbind(f_in_dis, in_dis[140,])

# Garrett-Keyser-Butler Community School Corporation = Garrett-Keyser-Butler Community Schools
in_dis[227,1] <- x[11,]
f_in_dis <- rbind(f_in_dis, in_dis[227,])

# x[12,] Yorktown Community Schools = ? not sure yet

# Eastern Greene Schools = Eastern Greene County School District
in_dis[10,1] <- x[13,]
f_in_dis <- rbind(f_in_dis, in_dis[10,])

# MSD Shakamak Schools = Shakamak Schools Metropolitan School District
in_dis[261,1] <- x[14,]
f_in_dis <- rbind(f_in_dis, in_dis[261,])

# Kokomo School Corporation = Kokomo-Center Township Consolidated School Corporation
in_dis[211,1] <- x[15,]
f_in_dis <- rbind(f_in_dis, in_dis[211,])

# Southwestern Jefferson County Consolidated = Southwestern Jefferson County Consolidated Schools
in_dis[212,1] <- x[16,]
f_in_dis <- rbind(f_in_dis, in_dis[212,])

# Jennings County School Corporation = Jennings County Schools
in_dis[131,1] <- x[17,]
f_in_dis <- rbind(f_in_dis, in_dis[131,])

# Nineveh-Hensley-Jackson United = Nineveh-Hensley-Jackson United School Corporation
in_dis[81,1] <- x[18,]
f_in_dis <- rbind(f_in_dis, in_dis[81,])

# Merrillville Community School Corporation = Merrillville Community School
in_dis[223,1] <- x[19,]
f_in_dis <- rbind(f_in_dis, in_dis[223,])

# Lake Ridge New Tech Schools = Lake Ridge Schools
in_dis[30,1] <- x[20,]
f_in_dis <- rbind(f_in_dis, in_dis[30,])

#School City of East Chicago = City of East Chicago School District
in_dis[221,1] <- x[21,]
f_in_dis <- rbind(f_in_dis, in_dis[221,])

# School City of Hammond = Hammond School City
in_dis[172,1] <- x[22,]
f_in_dis <- rbind(f_in_dis, in_dis[172,])

# School City of Highland = Highland School Town
in_dis[224,1] <- x[23,]
f_in_dis <- rbind(f_in_dis, in_dis[224,])

# School City of Hobart = Hobart School City
in_dis[173,1] <- x[24,]
f_in_dis <- rbind(f_in_dis, in_dis[173,])

# School City of Munster = Munster School Town
in_dis[104,1] <- x[25,]
f_in_dis <- rbind(f_in_dis, in_dis[104,])

# School City of Whiting = Whiting School City
in_dis[176,1] <- x[26,]
f_in_dis <- rbind(f_in_dis, in_dis[176,])

# MSD of New Durham Township = New Durham Township Metropolitan School District
in_dis[86,1] <- x[27,]
f_in_dis <- rbind(f_in_dis, in_dis[86,])

# MSD Decatur Township = Decatur Township Metropolitan School District
in_dis[235,1] <- x[28,]
f_in_dis <- rbind(f_in_dis, in_dis[235,])

# MSD Lawrence Township = Lawrence Township Metropolitan School District
in_dis[265,1] <- x[29,]
f_in_dis <- rbind(f_in_dis, in_dis[265,])

# Perry Township Schools = Perry Township Metropolitan School District
in_dis[287,1] <- x[30,]
f_in_dis <- rbind(f_in_dis, in_dis[287,])

# MSD Pike Township = Pike Township Metropolitan School District
in_dis[236,1] <- x[31,]
f_in_dis <- rbind(f_in_dis, in_dis[236,])

# MSD Warren Township = Warren Township Metropolitan School District
in_dis[215,1] <- x[32,]
f_in_dis <- rbind(f_in_dis, in_dis[215,])

# MSD Washington Township = Washington Township Metropolitan School District
in_dis[266,1] <- x[33,]
f_in_dis <- rbind(f_in_dis, in_dis[266,])

# MSD Wayne Township = Wayne Township Metropolitan School District
in_dis[238,1] <- x[34,]
f_in_dis <- rbind(f_in_dis, in_dis[238,])

# School Town of Speedway = Speedway School Town
in_dis[237,1] <- x[35,]
f_in_dis <- rbind(f_in_dis, in_dis[237,])

# MSD Martinsville Schools = Martinsville Schools Metropolitan School District
in_dis[219,1] <- x[36,]
f_in_dis <- rbind(f_in_dis, in_dis[219,])

# Rising Sun-Ohio County Community =  Rising Sun-Ohio County Community Schools
in_dis[122,1] <- x[37,]
f_in_dis <- rbind(f_in_dis, in_dis[122,])

# North Central Parke Comm Schl Corporation = North Central Parke Community School Corporation
in_dis[274,1] <- x[38,]
f_in_dis <- rbind(f_in_dis, in_dis[274,])

# MSD Boone Township = Boone Township Metropolitan School District
in_dis[22,1] <- x[39,]
f_in_dis <- rbind(f_in_dis, in_dis[22,])

# MSD Mount Vernon = Mount Vernon Metropolitan School District
in_dis[241,1] <- x[40,]
f_in_dis <- rbind(f_in_dis, in_dis[241,])

# MSD North Posey County Schools = North Posey County Schools Metropolitan School District
in_dis[240,1] <- x[41,]
f_in_dis <- rbind(f_in_dis, in_dis[240,])

# School City of Mishawaka = Mishawaka School City
in_dis[88,1] <- x[42,]
f_in_dis <- rbind(f_in_dis, in_dis[88,])

# Southwestern Consolidated School Shelby County = Southwestern Shelby County Consolidated Schools
in_dis[3,1] <- x[43,]
f_in_dis <- rbind(f_in_dis, in_dis[3,])

# MSD Steuben County = Steuben County Metropolitan School District
in_dis[260,1] <- x[44,]
f_in_dis <- rbind(f_in_dis, in_dis[260,])

# Tri-Central Community Schools = ? unclear

# MSD Wabash County Schools = Wabash County Schools Metropolitan School District
in_dis[246,1] <- x[46,]
f_in_dis <- rbind(f_in_dis, in_dis[246,])

# MSD Warren County = Warren County Metropolitan School District
in_dis[281,1] <- x[47,]
f_in_dis <- rbind(f_in_dis, in_dis[281,])

# Richmond Community Schools = Richmond Community School Corporation
in_dis[214,1] <- x[48,]
f_in_dis <- rbind(f_in_dis, in_dis[214,])

# MSD Bluffton-Harrison = Bluffton-Harrison Metropolitan School District
in_dis[98,1] <- x[49,]
f_in_dis <- rbind(f_in_dis, in_dis[98,])

# West Clark Community Schools = also Borden-Henryville School Corp (West Clark dissovlved; B-H formed July 2020)
# create new row for West Clark
in_dis[292,] <- in_dis[80,]
x[50,] <- "West Clark Community Schools"
in_dis[292,1] <- x[50,]
f_in_dis <- rbind(f_in_dis, in_dis[292,])


f_in_dis |> distinct(GEOID, .keep_all = T)

n_distinct(f_in_dis$GEOID) #288


## What corporations are left over?
anti_join(sxfer |> distinct(stlmt_corp_name), f_in_dis, 'stlmt_corp_name') |> 
  ungroup() |> distinct(stlmt_corp_name) 

# Yorktown Community Schools 
# Tri-Central Community Schools left

# Description of Yorktown (https://www.yorktown.k12.in.us/apps/pages/index.jsp?uREC_ID=1525799&type=d&pREC_ID=1660656) mentions Mt. Pleasant Township which is how its listed in in_dis in row 89

# Yorktown Community Schools = Mount Pleasant Township Community School Corporation
in_dis[89,1] <- x[12,]
f_in_dis <- rbind(f_in_dis, in_dis[89,])

# Tri-Central Community Schools = Tipton County Northern Community School Corporation
# https://www.niche.com/k12/d/tri-central-community-schools-in/ same boundaries...
in_dis[253,1] <- x[45,]
f_in_dis <- rbind(f_in_dis, in_dis[253,])


# check again if any coordinates for corporation outlines are missing
anti_join(sxfer |> distinct(stlmt_corp_name), f_in_dis, 'stlmt_corp_name') |> 
  ungroup() |> distinct(stlmt_corp_name) 

# add corp_id to this data for easier future use with Indiana DOE data

f_in_dis <- 
  left_join(f_in_dis, 
            sxfer |> ungroup() |> distinct(stlmt_corp_id, .keep_all = T) |> 
              select(stlmt_corp_name, stlmt_corp_id),
            'stlmt_corp_name'
  ) |> select('corp_name' = 'stlmt_corp_name', 'corp_id' = 'stlmt_corp_id', everything())


# write csv
write_csv(f_in_dis, file = "clean_data/corp_geo.csv")

# write as geojson to easily provide sf class if needed later
st_write(f_in_dis, dsn = "clean_data/corp_geo.geojson", 
         layer = "corp_geo.geojson")


