library(readxl)
library(tidyverse)


# assign colors for type
pub <-  '#d95f02'     # public schools
chr <-  '#1b9e77'     # charter schools
pr <-   '#7570b3'     # private (Choice Scholarship) schools


# import transfer data 
sxfer <- read_csv("clean_data/transfers_18_23_ll.csv") |>
  rename('Public' = 'par_ch', 'Charter' = 'p_xfer_chrtr', 'Choice Scholarship' = 'pr_ch_sclrs', "Other" = 'p_xfer_oth') |> 
  # remove unneeded columns
  select(-c(tot_xfr, lon)) |> 
  
  mutate(
    # replace NA's with 0's
    across(
    c(Public, Other, Charter, 'Choice Scholarship'),
    ~replace_na(.,0))
  ) |>
  
  # pivot to long data
  pivot_longer(cols = c(Public, Other, Charter, 'Choice Scholarship'), names_to = 'typ', values_to = 'tot_xfr') |>
  
  group_by(yr, nrl_schl_id, stlmt_corp_id, typ) |>
  mutate(
    # calculate total by yr, nrl and stlmt id
    tot_xfr = sum(tot_xfr),
    
    # assign fill color based on type 
    fillcolor = case_when(
           typ == 'Public' ~              pub, 
           typ == 'Charter' ~             chr, 
           typ == 'Choice Scholarship'  ~ pr
         ),
    
    # assign unique name to identify combiation of stlmt_corp and nrl_schl
    fname = paste0(stlmt_corp_id, "-", nrl_schl_id),
    
    # convert 'other' transfers to 'public'; though Indiana classifies Charter schools as public also, in this case we're making a distinction between parent choice and other public and charter public.
    typ = case_match(typ,
                     "Other" ~ 'Public',
                     .default = typ),
    # add columns for stroke and stroke color for circle marker denoting school corp
    # stroke size
    weight = 0,
    # stroke color 
    color = NA) |>
  
  group_by(stlmt_corp_id, nrl_schl_id, yr, typ) |> 
  
  # with 'Other' and 'public' both now labeled 'public', combine their transfer totals
  mutate(tot_xfr = sum(tot_xfr)) |> 
  
  # remove rows with na fill (rows with formerly 'other' transfer types) to avoid duplicating counts
  filter(!is.na(fillcolor)) |> 
  
  group_by(stlmt_corp_id, yr) |> 
  
  mutate(
    # calculate percent of total transfers to school corporation given row represents
    pct_of_xfr = round(100*(tot_xfr/sum(tot_xfr)),2),
    # convert to log for better circleMarkers in leaflet
    log = log(pct_of_xfr)) |> 
  
  ungroup() |> 
 
  filter(
    # remove rows with zero transfer totals
    stlmt_corp_id != nrl_schl_id &
      tot_xfr > 0) 

# write to csv
write_csv(sxfer, "clean_data/sxfer.csv")
