library(readxl)
library(tidyverse)



# General school information from Indiana Department of Education
# updated 10/2/2023 www.in.gov/doe/it/data-center-and-reports/


iread <- read_xlsx("raw_data/iread3-final-disaggregated-report-2023.xlsx", 
                   sheet = 2,
                   skip = 1, )

# columns are labeled using multiple rows; of those two rows, the first five columns of the first row are NA values. They need to be replaced

cn <- colnames(iread)
# first row of dataframe that includes 2nd row of column names
cn2 <- as.vector(iread[1,])

# bind them and assign them to object 'cols'
cols <- as_tibble(cbind(cn, cn2)) |> 
  mutate(cols = str_c(cn, cn2, sep = " ")) |> 
  select(cols)

# assign them to columnames for iread
colnames(iread) <- cols$cols

# remove residual columnames from first row
iread <- iread[2:nrow(iread),]

# select necessary columns: school and corp id, name, overall score; No total count of students is present but by overall score can be determined by adding percents for a given category's components. In this case, the columns for paid lunch and free/reduced lunch pass percent will be added together and divided by 2 to get overall IREAD_pct so those columns will be included in select.



iread <- iread |> 
  select('corp_id' = colnames(iread[2]), 
         'corp_name' = colnames(iread[3]),
         'schl_id' = colnames(iread[4]),
         'schl_name' = colnames(iread[5]),
         'pd_meal_pct_ps' = colnames(iread[29]),
         'fr_meal_pct_pss' = colnames(iread[32])) |> 
  
  mutate(pd_meal_pct_ps = as.numeric(pd_meal_pct_ps),
         fr_meal_pct_pss = as.numeric(fr_meal_pct_pss),
         IREAD_pct = 
           round(.5*(pd_meal_pct_ps + fr_meal_pct_pss),3)) |> 
  
  select(corp_id, corp_name, schl_id, schl_name, IREAD_pct) 

# write as csv
write_csv(iread, "clean_data/20240612_iread_23.csv")

# clean up
rm(cn, cn2, cols)


# ILEARN scores ####

# Indiana’s Learning Evaluation and Assessment Readiness Network (ILEARN) is the summative accountability assessment for Indiana students in grades three through eight and high school biology. ILEARN measures student achievement and growth according to Indiana Academic Standards for English/Language Arts for grades three through eight, Mathematics for grades three through eight, Science for grades four and six, and Social Studies for grade five. 

# Students are required to participate in the ILEARN Biology End-of-Course Assessment (ECA) upon completion of the high school biology course to fulfill a federal participation requirement. 

# additional information: in.gov/doe/students/assessment/ilearn/#Frequently_Asked_Questions


# ILEARN English Language Arts percent proficient

ilearn_ela <- read_xlsx("raw_data/ILEARN-2023-Grade3-8-Final-School.xlsx", 
                        sheet = 1, 
                        skip = 4,
                        .name_repair = "universal_quiet" )

# select appropriate columns and rename
ilearn_ela <- ilearn_ela |>
  select('corp_id' = 'Corp.ID',
         'corp_name' = 'Corp.Name',
         'schl_id' = 'School.ID',
         'schl_name' = 'School.Name',
         'ela_prof_pct' = colnames(ilearn_ela[ncol(ilearn_ela)])
  ) |> 
  # convert ela_math_prof_pct to numeric and round to 3 places
  mutate(ela_prof_pct = round(as.numeric(ela_prof_pct),3))

write_csv(ilearn_ela, "clean_data/20240612_learn_ela_23.csv")

# ILEARN Math percent proficient

ilearn_math <- read_xlsx("raw_data/ILEARN-2023-Grade3-8-Final-School.xlsx", 
                         sheet = 2, 
                         skip = 4,
                         .name_repair = "universal_quiet" )

# select appropriate columns and rename
ilearn_math <- ilearn_math |>
  select('corp_id' = 'Corp.ID',
         'corp_name' = 'Corp.Name',
         'schl_id' = 'School.ID',
         'schl_name' = 'School.Name',
         'math_prof_pct' = colnames(ilearn_math[ncol(ilearn_math)])
  ) |> 
  # convert ela_math_prof_pct to numeric and round to 3 places
  mutate(math_prof_pct = round(as.numeric(math_prof_pct),3))

write_csv(ilearn_math, "clean_data/20240612_ilearn_math_23.csv")


#
# ILEARN Science percent proficient

ilearn_sc <- read_xlsx("raw_data/ILEARN-2023-Grade3-8-Final-School.xlsx", 
                       sheet = 4, 
                       skip = 4,
                       .name_repair = "universal_quiet" )

# select appropriate columns and rename
ilearn_sc <- ilearn_sc |>
  select('corp_id' = 'Corp.ID',
         'corp_name' = 'Corp.Name',
         'schl_id' = 'School.ID',
         'schl_name' = 'School.Name',
         'sc_prof_pct' = colnames(ilearn_sc[ncol(ilearn_sc)])
  ) |> 
  # convert ela_math_prof_pct to numeric and round to 3 places
  mutate(sc_prof_pct = round(as.numeric(sc_prof_pct),3))

write_csv(ilearn_sc, "clean_data/20240612_ilearn_sc_23.csv")

# ILEARN Social Studies percent proficient

ilearn_ss <- read_xlsx("raw_data/ILEARN-2023-Grade3-8-Final-School.xlsx", 
                       sheet = 5, 
                       skip = 4,
                       .name_repair = "universal_quiet" )

# select appropriate columns and rename
ilearn_ss <- ilearn_ss |>
  select('corp_id' = 'Corp.ID',
         'corp_name' = 'Corp.Name',
         'schl_id' = 'School.ID',
         'schl_name' = 'School.Name',
         'ss_prof_pct' = colnames(ilearn_ss[ncol(ilearn_ss)])
  ) |> 
  # convert ela_math_prof_pct to numeric and round to 3 places
  mutate(ss_prof_pct = round(as.numeric(ss_prof_pct),3))

write_csv(ilearn_ss, "clean_data/20240612_ilearn_ss_23.csv")

# ILEARN biology percent proficient

ilearn_bio <- read_xlsx("raw_data/ILEARN-2023-Biology-Final-School.xlsx", 
                        sheet = 1, 
                        skip = 4,
                        .name_repair = "universal_quiet" )

# select appropriate columns and rename
ilearn_bio <- ilearn_bio |>
  select('corp_id' = 'Corp.ID',
         'corp_name' = 'Corp.Name',
         'schl_id' = 'School.ID',
         'schl_name' = 'School.Name',
         'bio_prof_pct' = colnames(ilearn_bio[ncol(ilearn_bio)])
  ) |> 
  # convert ela_math_prof_pct to numeric and round to 3 places
  mutate(bio_prof_pct = round(as.numeric(bio_prof_pct),3))

write_csv(ilearn_bio, "clean_data/20240612_ilearn_bio_23.csv")


# SAT data
sat_23 <- read_xlsx("raw_data/SAT-2023-Grade11-Final-School.xlsx", 
                    sheet = 3, 
                    skip = 4,
                    .name_repair = "universal_quiet" ) |> 
  select(
    'corp_id' = 'Corp.ID',
    'corp_name' = 'Corp.Name',
    'schl_id' = 'School.ID',
    'schl_name' = 'School.Name',
    'pct_bmk' = 'Both.EBRW...Math..Benchmark....'
  ) |> 
  # convert pct_bmk to numeric and round to 3 places
  mutate(
    pct_bmk = round(as.numeric(pct_bmk), 3)
  )

# write to csv
write_csv(sat_23, "clean_data/20240612_sat_23.csv")




# join iread math and iread ELA dataframes to make score dataframe; both have same number of rows
scores <- left_join(ilearn_math, ilearn_ela, 
                    'schl_id') |> 
  select(
    'corp_id' = 'corp_id.x',
    'corp_name' = 'corp_name.x',
    schl_id,
    'schl_name' = 'schl_name.x',
    math_prof_pct, 
    ela_prof_pct
  )

# add science score to scores dataframe

# check if are any values in ilearn_sc are not within scores
anti_join(ilearn_sc, scores, 'schl_id') # returns 0.

scores <- left_join(scores, ilearn_sc,
                    'schl_id') |>
  select(
    'corp_id' = 'corp_id.x',
    'corp_name' = 'corp_name.x',
    schl_id,
    'schl_name' = 'schl_name.x',
    math_prof_pct,
    ela_prof_pct,
    sc_prof_pct) 



# add social studies score to scores dataframe

# check if are any values in ilearn_sc are not within scores
anti_join(ilearn_ss, scores, 'schl_id') # returns 0.

scores <- left_join(scores, ilearn_ss,
                    'schl_id') |>
  select(
    'corp_id' = 'corp_id.x',
    'corp_name' = 'corp_name.x',
    schl_id,
    'schl_name' = 'schl_name.x',
    math_prof_pct,
    ela_prof_pct,
    sc_prof_pct,
    ss_prof_pct
  ) 

# add biology score to scores dataframe

anti_join(ilearn_bio, scores, 'schl_id') # 295 rows from ilearn_bio not in scores; use full_join

# scores$bio_prof_pct <- NULL

scores <- full_join(scores, ilearn_bio,
                    'schl_id', keep = T) |> 
  # eliminate redundant columns
  mutate(
    corp_id.x = if_else(
      is.na(corp_id.x) == T, corp_id.y, corp_id.x),
    corp_name.x = if_else(
      is.na(corp_name.x) == T, corp_name.y, corp_name.x),
    schl_id.x = if_else(
      is.na(schl_id.x) == T, schl_id.y, schl_id.x),
    schl_name.x = if_else(
      is.na(schl_name.x) == T, schl_name.y, schl_name.x),
  ) |> 
  select(
    'corp_id' = 'corp_id.x',
    'corp_name' = 'corp_name.x',
    'schl_id' = 'schl_id.x',
    'schl_name' = 'schl_name.x',
    math_prof_pct,
    ela_prof_pct,
    sc_prof_pct,
    ss_prof_pct,
    bio_prof_pct
  ) 

# add SAT scores

anti_join(sat_23, scores, 'schl_id') # returns 10 rows; use full join

scores <- full_join(scores, sat_23, 
                    'schl_id', keep = T) |> 
  # adjust column names; remove redundant rows
  mutate(
    corp_id.x = if_else(
      is.na(corp_id.x) == T, corp_id.y, corp_id.x),
    corp_name.x = if_else(
      is.na(corp_name.x) == T, corp_name.y, corp_name.x),
    schl_id.x = if_else(
      is.na(schl_id.x) == T, schl_id.y, schl_id.x),
    schl_name.x = if_else(
      is.na(schl_name.x) == T, schl_name.y, schl_name.x),
  ) |>
  select(
    'corp_id' = 'corp_id.x',
    'corp_name' = 'corp_name.x',
    'schl_id' = 'schl_id.x',
    'schl_name' = 'schl_name.x',
    math_prof_pct,
    ela_prof_pct,
    sc_prof_pct,
    ss_prof_pct,
    bio_prof_pct,
    'sat_bmk_pct' = 'pct_bmk'
  ) 

# what rows aren't in scores that are in ilearn_bio
anti_join(sat_23, scores, by="schl_id")  # zero



# add IREAD scores

anti_join(iread, scores, 'schl_id')
# returns 2 schools, both with NA's on their score; left_join

scores <- left_join(scores, iread,
                    'schl_id') |> 
  select(
    'corp_id' = 'corp_id.x',
    'corp_name' = 'corp_name.x',
    schl_id,
    'schl_name' = 'schl_name.x',
    'iread_pct' = 'IREAD_pct',
    math_prof_pct,
    ela_prof_pct,
    sc_prof_pct,
    ss_prof_pct,
    bio_prof_pct,
    sat_bmk_pct
  ) 


# write 2023 scores to csv
write_csv(scores, "clean_data/20240612_tst_scores_23.csv")
