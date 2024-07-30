library(tidyverse)
library(ggtext)
library(readxl)
library(aws.s3)




# background color 
bg_c <-  "#fbf9f6"

# custom theme
t_theme <- function(base_size = 10){
  
  theme_classic() %+replace%
    
    theme(
      axis.title = element_blank(),
      axis.text.x   = element_text(size = rel(x = .55), color = "#505050"),
      axis.text.y   = element_text(size = rel(x = .8), color = "#505050",
                                   margin=margin(0,-10,0,0)), #(0,-20,0,0)),
      axis.line = element_blank(),
      #axis.ticks.x = element_line(color = "#636363"),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = NA, color = NA),
      legend.key.width= unit(1.25, 'cm'),
      legend.position = c(.15,.95),
      legend.text  = element_text(size = 9),
      legend.title = element_blank(),
      legend.key = element_blank(),
      plot.title.position = "plot",
      plot.margin = margin(.1,.1,.1,.1, "in"), # border around entire chart
      plot.background = element_rect(fill  = bg_c, color = NA),
      panel.background = element_rect(fill = bg_c, color = NA),
      plot.title = element_textbox(
        family = "serif", 
        size = rel(x = .8),
        width = unit(1, "npc"),
        padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
        margin  = margin(.25, .25, 2, .25),   #(8, 0, 0, 0),
        lineheight = 1
      ),
      plot.subtitle = element_textbox(
        family = "sans", 
        size = rel(x = .55),
        width = unit(1, "npc"),
        padding = margin(.15, .25, .25, .25),  
        margin  = margin(1.75, .45, 6.25, .25),
        lineheight = 1
      ),
      plot.caption = element_markdown(
        margin = margin(0, 0, .1, 0), 
        hjust = 1, 
        halign = 1,
        size = rel(x = .43))
    )
}

# updating corporation/school names ####
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

# import data on transfers out of school corporations ####

res <- read_csv("clean_data/20240612_student_res_xfer_rate_2018_2024.csv"
                #student_res_xfer_rate_2018_2023.csv"
                ) |>
  
  # replace common abbreviations in corporation names
  mutate(corp_name =
           str_replace_all(corp_name,
                           setNames(rplc_names, set_names))) |>
  
  group_by(corp_name) |>
  
  mutate(
    xfr_out_tot_rate_avg = mean(xfr_out_tot_rate),
    in_stlmt_tot_avg = mean(in_stlmt_tot),
    #yr = year(yr),
    title = paste0(
      "An average of ",
      round(xfr_out_tot_rate_avg),
      case_when(abs(xfr_out_tot_rate_avg) > 0   ~ " students",
                abs(xfr_out_tot_rate_avg) == 0  ~ " students",
                abs(xfr_out_tot_rate_avg) == 1  ~ " student",),
      " transfer out for every 100 living within the boundaries of **",
      corp_name,
      "**; the average resident student population is ",
      format(round(in_stlmt_tot_avg), big.mark = ","), "."
    )
  ) |>
  
  ungroup() |>
  
  mutate(xfr_out_tot_rate = xfr_out_tot_rate * -1,
         pr_xfr_out_rate = pr_xfr_out_rate * -1) 

#####
# need a dataframe that shows types of transfers out by year

# create list of files that include the word transfer

# path to files
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

# assemble dataframe 
for (i in 1:length(transfer_files)) {
  xfr <- read_xlsx(transfer_files[i],
                   sheet = 4,
                   skip = 1)
  
  # rename columns
  colnames(xfr) <-
    c(
      "stlmt_corp_id",
      "stlmt_corp_name",
      "nrl_id",
      "nrl_name",
      "prnt_ch",
      "other",
      "charter",
      "choice"
    )
  
  xfr <- xfr |>
    select("stlmt_corp_id",
           "stlmt_corp_name",
           "nrl_id",
           "nrl_name",
           "prnt_ch",
           "other",
           "charter",
           "choice") |> 
    
    mutate(
      # determine semester (fall or spring) transfers occurred
      semester = str_extract(transfer_files[i],
                             pattern = "^(.{2})"),
      
      # extract year from filename
      yr = as.numeric(sub(
        "^(.{4}).*-(\\d{4}).*", "\\2",
        transfer_files[i]
      )),
      
      # based on semester, add month and day to year for sorting purposes
      yr = case_when(
        semester == "sp" ~ paste0(yr, "_01_01"),
        .default = paste0(yr, "_08_01")
      ),
      
      # format as Date; reduce to year
      yr = as.Date(yr, format = "%Y_%m_%d"),
      yr = year(yr)
    ) |>
    
    # remove semester column
    select(-semester)
  
  # save to dataframe
  if (i == 1) {
    xfr_2 <- xfr
  } else {
    xfr_2 <-  rbind(xfr, xfr_2)
  }
}

# clean up
xfr <- xfr_2
rm(xfr_2)


# summarise totals, pivot longer and update type for plot label use ####

xfr <- xfr |> 
  group_by(stlmt_corp_id, yr) |> 
  
  # total transfer types by settlement corporation
  mutate(prnt_ch = sum(prnt_ch, na.rm = T),
         other = sum(other, na.rm = T),
         charter = sum(charter, na.rm = T),
         choice = sum(choice, na.rm = T)) |>
  
  # remove unneeded columns
  select(-c(nrl_id, nrl_name)) |> 
  
  # group by stlmt corp and yr
  group_by(stlmt_corp_id, yr) |> 
  
  # reduce to one row per settlement corporation
  distinct(stlmt_corp_id, .keep_all = T) |> 
  
  # pivot to long data for plotting
  pivot_longer(cols = c("prnt_ch", "other", "charter", "choice"), 
               names_to = "typ", values_to = "xfr_tot") |> 
  
  # adjust typ names for labels
  mutate(typ = case_when(
    typ == "prnt_ch"  ~ "Parent choice (public school)",
    typ == "other"    ~ "Other (public school)",
    typ == "charter"  ~ "Charter school",
    typ == "choice"   ~ "Choice Scholarship (private school)"),
    stlmt_corp_name =
      str_replace_all(stlmt_corp_name,
                      setNames(rplc_names, set_names))
  )


####


# loop to create popup charts for settlement school corp circle markers ####

schl_corps <- unique(xfr$stlmt_corp_name)

sum(is.na(schl_corps))
# na in last entry

# remove NA from schl_corps list
schl_corps <- schl_corps[1:length(schl_corps)-1]

for (i in 1:length(schl_corps)){
  
  ggplot(xfr |>  
           filter(stlmt_corp_name == schl_corps[i] ) |> 
           mutate(
             typ = factor(typ, 
                          levels = c(
                            "Charter school",
                            "Choice Scholarship (private school)", 
                            "Other (public school)",
                            "Parent choice (public school)"
                          )
             )))+
    
    geom_line(aes(yr, xfr_tot, color = typ, group = 1), 
              linewidth = .35,
              show.legend = F)+
    
    scale_y_continuous(#breaks = scales::pretty_breaks(),
      labels = scales::label_comma(accuracy = 1))+
    
    scale_x_continuous(limits = c(min(xfr$yr), max(xfr$yr)),
                       expand = c(0, .65),
                       breaks = seq(2018, 2024, 1),
                       labels = c("2018", "'19", "'20", "'21", "'22", "'23", "2024"),
    )+
    
    scale_color_manual(
      
      values = c("Parent choice (public school)"       = "#fc8d62", 
                 "Other (public school)"               = "#fc8d62",
                 "Charter school"                      = "#66c2a5",
                 "Choice Scholarship (private school)" = "#8da0cb" )
    )+
    
    t_theme()+
    
    theme(
      
      panel.grid.major.y = element_line(color = "lightgray",
                                        linewidth = .25,
                                        linetype = "dotted"),
      axis.text.y = element_text(size = rel(.6)),
      axis.text.x = element_text(size = rel(.6)),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(.485), 
                                hjust = 0,
                                margin = margin(0,0,3,15)),
      plot.title = element_textbox(
        family = "serif", 
        size = rel(x = .7),
        width = unit(1, "npc"),
        padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
        margin  = margin(.25, .25, 4, .25),   #(8, 0, 0, 0),
        lineheight = 1
      ),
      plot.subtitle = element_textbox(
        family = "sans", 
        size = rel(x = .55),
        width = unit(1, "npc"),
        padding = margin(.15, .25, .25, .25),  
        margin  = margin(1.75, .45, 6.25, .25),
        lineheight = 1
      ), 
      plot.margin = margin(.1,.12,.1,.12, "in")
    )+
    
    facet_wrap(~typ, ncol = 1, scales = "free")+
    
    labs(
      title = res$title[res$corp_name == schl_corps[i]][1],
      subtitle = "Outgoing transfers:",
      caption = "<br>**Data**:   Indiana Department of Education<br>(c)   tedschurter.com"
    )
  
  
  ggsave(filename = paste0("plots/stlmt_plots2/", xfr$stlmt_corp_id[xfr$stlmt_corp_name == schl_corps[i]][1], "-", xfr$stlmt_corp_id[xfr$stlmt_corp_name == schl_corps[i]][1], ".png"),
         #paste0("tstplot3a2/", nrl_fn[i], ".png"),
         width  = 3,
         height = 4,
         units = "in", plot = last_plot())  
}
#

# upload files to AWS S3 bucket ####


# assign bucket name
bucket <- "tedschurter-data"
region <- "us-east-2" # bucket assigned to us-east-2 region; also set by default in r environment


# check that it's there
bucket_exists(bucket) # returns TRUE


#loop through folder adding plots 

plots <- unique(list.files("plots/stlmt_plots"))

for(i in 1:length(plots)){
  
  # put object in bucket
  put_object(
    object = paste0("2024-in-ed-",plots[i]),
    bucket = bucket,
    file = paste0("plots/stlmt_plots/", plots[i]),
    region = region,
    headers = c('Content-Type' = 'image/png') # without headers argument, content-type metadata set to binary/octet-stream and file downloads rather than displays
  )
}



# loop to delete all above objects if needed

# for(i in 1:length(plots)){
#   delete_object(
#     object = paste0("2024-in-ed-",plots[i]),
#     bucket = bucket,
#     quiet = TRUE)
# }


