library(readxl)
library(tidyverse)

# aggregate student transfer data totals from 2018 to 2024 ####


# loop through xlsx sheets to create dataframe of number of students, transfers in and out of school corporation for spring and fall of school year
# create list of files that include the word transfer
# path to files
# list of all files in raw_data folder
path  <- "raw_data"
#list of all files in raw_data folder
files <- list.files(path, pattern = "^[^~]", full.names = T)
# object with all files that include word transfer
transfer_files <- grep("transfer", files, value = T)

# > transfer_files
# [1] "raw_data/fall-2017-2018-public-corporation-transfer-report.xlsx"     
# [2] "raw_data/fall-2019-2020-public-corporation-transfer-report.xlsx"     
# [3] "raw_data/fall-2021-2022-public-corporation-transfer-report.xlsx"     
# [4] "raw_data/fall-2022-2023-public-corporation-transfer-report-v2.xlsx"  
# [5] "raw_data/fall-2023-2024-public-corporation-transfer-report.xlsx"     
# [6] "raw_data/spring-2018-2019-public-corporation-transfer-report.xlsx"   
# [7] "raw_data/spring-2020-2021-public-corporation-transfer-report.xlsx"   
# [8] "raw_data/spring-2021-2022-public-corporation-transfer-report-v2.xlsx"
# [9] "raw_data/spring-2023-2024-public-corporation-transfer-report.xlsx"   

# Use spring data when available 
transfer_files <- transfer_files[c(1, 6,2, 7, 8,4, 9)]


for (i in 1:length(transfer_files)) {
  res <- read_xlsx(transfer_files[i],
                   sheet = 3,
                   skip = 1)
  
  # rename columns
  colnames(res) <-
    c(
      "corp_id",
      "corp_name",
      "in_stlmt_tot",
      "res_nrl",
      "public_xfr_in",
      "public_xfr_out",
      "net_pub_xfr",
      "xfr_ch_schlsp_out",
      "net_xfr"
    )
  
  res <- res |>
    mutate(
      # total transfers out of school corporation
      xfr_out_tot = public_xfr_out + xfr_ch_schlsp_out,
      
      # rate of total transfers out of school corporation per 100 students
      xfr_out_tot_rate = round(100 * (xfr_out_tot / in_stlmt_tot)),
      
      # rate of total transfers into school corporation per 100 students
      xfr_in_rate = round(100 * (public_xfr_in / in_stlmt_tot)),
      .after = xfr_out_tot_rate,
      
      # rate of total transfers to school choice scholarships
      pr_xfr_out_rate = round(100 * (xfr_ch_schlsp_out / in_stlmt_tot)),
      
      # net transfer rate out of school
      net_xfr_rate = round(100 * ((net_xfr) / in_stlmt_tot)),
      
      # determine semester (fall or spring) transfers occurred
      # semester = str_extract(transfer_files[i],
      #                        pattern = "^(.{2})"),
      
      # extract year from filename
      yr = as.numeric(sub(
        "^(.{4}).*-(\\d{4}).*", "\\2",
        transfer_files[i]
      ))
    )
  
  # remove semester column
  # select(-semester)
  
  if (i == 1) {
    res_2 <- res
  } else {
    res_2 <-  rbind(res, res_2)
  }
}

# clean up
res <- res_2
rm(res_2)



# check for unique years
unique(res$yr) # [1] 2024 2023 2022 2021 2020 2019 2018

# check for NA'S across all columns
colSums(is.na(res))  # four  with public transfers in

# Gary Community School Corp
# School Town of Speedway
# Scott County School District 1
# MSD Warren County

# checking 2024 data, all Public Transfers Incoming entries for those corporations was blank, not zero.
# update to add zero instead of NA


# replace NA's in public_xfr_in with 0
res[is.na(res$public_xfr_in),]$public_xfr_in <- 0

# replace NA's in xfr_in_rate with 0
res[is.na(res$xfr_in_rate),]$xfr_in_rate <- 0

# check for NA'S across all columns
colSums(is.na(res)) # zero


# write to csv
write_csv(res, "clean_data/20240612_student_res_xfer_rate_2018_2024.csv")



# transfer details from settlement corporation to enrolled school ####
# create dataframe showing transfer details including:
# - how many transfers to and from each school corporation
# - how many transfers charter and private schools received from which corporations

  
# path to files in raw_data folder
path  <- "raw_data"

#list of all files in raw_data folder
files <- list.files(path, pattern = "^[^~]", full.names = T)

# object with all files that include word transfer
transfer_files <- grep("transfer", files, value = T)

# remove duplicates for years when second version (denoted by v2) of transfer data was released (2021-2022; 2022-2023)
transfer_files <- transfer_files[c(1, 6,2, 7, 8,4, 9)]


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



# create functions to replace settlement and enrolled school names with non-abbreviated school names

nrl <- function(schl_name, rplc){
  tfr$nrl_schl_name[tfr$nrl_schl_name == schl_name] <- rplc
}

stl <- function(schl_name, rplc){
  tfr$nrl_schl_name[tfr$stlmt_corp_name == schl_name] <- rplc
}


# Greenwood Christian Academy High S - should be Greenwood Christian Academy High School
nrl('Greenwood Christian Academy High S', 
    "Greenwood Christian Academy High School")

# HIM By HER Collegiate School for t = HIM By HER Collegiate School for the Arts  ###Note permanently closed in 2022

# replace the same for nrl_schl_name column
nrl("HIM By HER Collegiate School for t", 
    "HIM By HER Collegiate School for the Arts")


# above code replaces Tech with Technology but Lake Ridge New Technology Schools should be Lake Ridge New Tech Schools
stl("Lake Ridge New Technology Schools",
    "Lake Ridge New Tech Schools")

# enrolled school column
nrl("Lake Ridge New Technology Schools",
    "Lake Ridge New Tech Schools")

# Saint Michael Saint Gabriel Archangels S = Saint Michael Saint Gabriel Archangels School
# nrl_schl_name column
nrl("Saint Michael Saint Gabriel Archangels S",
    "Saint Michael Saint Gabriel Archangels School")

# The Hope Academy, Incorporated.* and 	The Hope Academy, Incorporated. should both be 	The Hope Academy, Incorporated
nrl("The Hope Academy, Incorporated.*",
    "The Hope Academy, Incorporated")


nrl("The Hope Academy, Incorporated.",
    "The Hope Academy, Incorporated")

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
  nrow() # 294

# ensure number of distinct stlmt corp names matches number of stlmt corp ids
tfr |>
  group_by(stlmt_corp_name) |>
  count() |>
  nrow() #296


tfr |> group_by(stlmt_corp_id, stlmt_corp_name) |> 
  distinct(stlmt_corp_id, .keep_all = T) |> 
  View()

# two stlmt_corp_names that don't belong:
# IDOE Test LEA
# Out of State

tfr <- tfr |> filter(stlmt_corp_name != "IDOE Test LEA" |
                       stlmt_corp_name != "Out of State")


tfr |> 
  distinct(stlmt_corp_id, .keep_all = T) |> 
  nrow() # 293

###
# write cleaned transfer data to csv
write_csv(tfr, "clean_data/20240612_transfers_18_24.csv")
