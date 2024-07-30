library(readxl)
library(tidyverse)

# aggregate school enrollment data 

# loop over school enrollment data and create dataframe of total attendance for each school from 2014 to 2024

yrs <- seq(from = 2024, to = 2014)

# standardized column names 
c_names <-
  c("corp_ID",
    "corp_name",
    "schl_ID",
    "schl_name",
    "enrlmnt",
    "year")

# private schools ####

for (i in 1:length(yrs)) {
  np_nrl <-
    read_xlsx("raw_data/accredited-non-public-school-enrollment-2011-24.xlsx",
              sheet = i) |>
    select(1:4, 20) |>
    mutate(yr = yrs[i])
  
  # standardize column names
  colnames(np_nrl) <- c_names
  
  if (yrs[i] == 2024) {
    np_nrl_2 <- np_nrl
  } else {
    np_nrl_2 <- rbind(np_nrl, np_nrl_2)
  }
  np_nrl <- np_nrl_2
}

# clean up
rm(np_nrl_2)

# public schools ####


for (i in 1:length(yrs)) {
  p_nrl <-
    read_xlsx("raw_data/school-enrollment-grade-2006-24.xlsx", sheet = i) |>
    select(1:4, 20) |>
    mutate(yr = yrs[i])
  
  # standardize column names
  colnames(p_nrl) <- c_names
  
  if (yrs[i] == 2024) {
    p_nrl_2 <- p_nrl
  } else {
    p_nrl_2 <- rbind(p_nrl, p_nrl_2)
  }
  p_nrl <- p_nrl_2
}
# clean up
rm(p_nrl_2)

# check to ensure all years represented
unique(p_nrl$year)

# assign type of school (public, private) to school enrollment dataframes

# private school type = pr
np_nrl$type <- "pr"

# public school type = p
p_nrl$type <- "p"

# remove school_id in the school name column of private schools that was used for some years
np_nrl <- np_nrl |>
  mutate(schl_name = str_remove_all(string = schl_name,
                                    pattern = "^\\w{4} - "))

# join enrollment dataframes into one dataframe
nrl <- rbind(np_nrl, p_nrl)

# clean up
rm(np_nrl, p_nrl)

# write csv for public and private school enrollment data from 2014:23
write_csv(nrl, "clean_data/20240612_enrollment_14_24.csv")
#


# add test scores to enrollment data ####

# import previously cleaned test scores

scores <- read_csv("clean_data/20240612_tst_scores_23.csv")
scores <- 
  left_join(scores,
            
            # keep year at 2023 since test scores for 2024 not available yet
            nrl |> filter(year == 2023) |> 
              
              select('schl_id' = 'schl_ID',
                     enrlmnt),
            'schl_id') 

# write to csv
write_csv(scores, "clean_data/test_nrl_str_23.csv")




