library(tidyverse)
library(tigris)
library(sf)
library(ggtext)
library(png)
library(cowplot)

# create function to make map showing school corp/charter or private school using already assigned colors and show the school corporations from which they came as white circles in order to match the corresponding leaflet map on the same page.

# import data ####
 
sxfer <- read_csv("clean_data/20240614_sxfer.csv")

# list of school corporation names
corp <- sxfer |> 
  distinct(stlmt_corp_name)

res <- read_csv("clean_data/20240612_enrollment_14_24.csv")


# import state map and districts ####
in_dis <-
  st_read("clean_data/20240607_corporation_bounds.geojson")

# state border
ind <- states() |> 
  filter(NAME == "Indiana")

# colors ####
# background color 
bg_c <-   "#fcfdfd" 

# axis and annotation text color
atxt <- "#525252"

# transfer type colors
# legal settlement color
lgst     <-   '#d0ccbe' 
# slightly darker shade of legal settlement color for use in headlines
lgst_txt <-   "#b0aca0"
# public transfers
pub      <-   '#d95f02'      
# charter school color
chr      <-   '#1b9e77'
# Choice Scholarship 
pr       <-   '#7570b3'  
# incoming transfers
inc      <-   '#525252'  

# values for scale_manual
vals <- c(
  '#d0ccbe' = '#d0ccbe',
  '#d95f02' = '#d95f02',    
  '#1b9e77' = '#1b9e77',    
  '#7570b3' = '#7570b3', 
  '#525252' = '#525252',
  "#fcfdfd" = "#fcfdfd")

# match webpage background to plot background
wbg_c <- "#fcfdfd"

# custom theme
t_theme <- function(base_size = 10){
  
  theme_classic() %+replace%
    
    theme(
      axis.title = element_blank(),
      axis.text.x   = element_text(size = rel(x = .85), color = "#505050"),
      axis.text.y   = element_text(size = rel(x = .95), color = "#505050",
                                   margin=margin(0,-20,0,-10)), #(0,-20,0,0)),
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
        size = rel(x = 1.3),
        width = unit(1, "npc"),
        padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
        margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
        lineheight = 1.15
      ),
      plot.subtitle = element_textbox(
        family = "sans", 
        size = rel(x = .95),
        width = unit(1, "npc"),
        padding = margin(.15, .25, .25, .25),  
        margin  = margin(1.75, .45, 6.25, .25),
        lineheight = 1
      ),
      plot.caption = element_markdown(
        family = "sans",
        color = "#808080",
        margin = margin(0, 0, .1, 0), 
        hjust = 1, 
        halign = 1,
        size = rel(x = .75))
    )
}



# chart for overall trend by type since 2018 ####

# function for 2024 total transfers by type; variables are typ and year
transfer_totals <- function(type, year){
  sxfer |> 
    filter(typ == type & yr == year) |> 
    ungroup() |> 
    summarise(tot_xfr = sum(tot_xfr, na.rm=T)) |> 
    pull()
}

# Choice scholarship totals for most recent year
sc_tot  <- transfer_totals("Choice Scholarship", max(sxfer$yr))
# public totals
pub_tot <- transfer_totals("Public",  max(sxfer$yr)) 
# charter school totals
chr_tot <- transfer_totals("Charter",  max(sxfer$yr))

# yearly totals by type 
yr_tot_typ <- sxfer |>
  group_by(yr, typ) |>
  summarise(tot = sum(tot_xfr))

# dataframe of yearly total transfers 
yr_tot <- yr_tot_typ |> 
  group_by(yr) |> 
  summarise(tot = sum(tot))

# function to determine pct change by year
p_inc <- function(end_yr, start_yr){
  100*((yr_tot$tot[yr_tot$yr == end_yr] - 
          yr_tot$tot[yr_tot$yr == start_yr])/
         yr_tot$tot[yr_tot$yr == start_yr])
}


# function to determine percent change by type and year

p_inc_typ <- function(type, end_yr, start_yr){
  100*((yr_tot_typ$tot[yr_tot_typ$yr == end_yr & yr_tot_typ$typ == type]-
          yr_tot_typ$tot[yr_tot_typ$yr == start_yr & yr_tot_typ$typ == type])/yr_tot_typ$tot[yr_tot_typ$yr ==start_yr & yr_tot_typ$typ == type])
}

# choice scholarship percent change since 2021 to 2024
cs_inc <- round(p_inc_typ("Choice Scholarship", 2024, 2021)) # 97

# change for public from 2021 to 2024
pub_inc_18 <- round(p_inc_typ("Public", 2024, 2018)) # 62

pub_inc_21 <- round(p_inc_typ("Public", 2024, 2021)) # 18


# change in total transfers since 2018
tot_pinc <- round(p_inc(2024, 2018)) # 52.1


# percent of transfers choice scholarships make of total

# in 2024
(yr_tot_typ$tot[yr_tot_typ$typ == "Choice Scholarship" &
                  yr_tot_typ$yr == 2024] /
    yr_tot$tot[yr_tot$yr == 2024])*100   # 33.4%


# in 2018
(yr_tot_typ$tot[yr_tot_typ$typ == "Choice Scholarship" &
                  yr_tot_typ$yr == 2018] /
    yr_tot$tot[yr_tot$yr == 2018])*100  # 26.2%

# chart

# grid lines df 
x <- rep(min(sxfer$yr), 4)
xend <- rep(max(sxfer$yr), 4)
y <- rep(c(0, 25000, 50000, 75000), 4)
yend  <-  rep(c(0, 25000, 50000, 75000), 4)
lines <- data.frame(x, xend,y, yend)

ggplot()+
  geom_line(data = sxfer |> group_by(typ, yr) |> 
              summarise(tot_xfr = sum(tot_xfr, na.rm=T)),
            aes(yr, tot_xfr, color = typ, group = typ), 
            linewidth = .75, show.legend = F
  )+
  scale_color_manual(values = c(
    "Public"             =  pub,
    "Charter"            =  chr,
    "Choice Scholarship" =  pr
  )
  )+
  
  # line labels 
  geom_text(data = sxfer |> group_by(typ, yr) |> 
              summarise(tot_xfr = sum(tot_xfr, na.rm=T)) |> 
              filter(yr == max(yr)),
            aes(
              yr, tot_xfr, 
              label = str_wrap(typ, 10)),
            color = atxt, lineheight = 1,
            size = 3, hjust = 0, nudge_x = .15
  )+
  
  # public transfer 2024 total
  annotate("text", x = 2024, y = pub_tot+pub_tot*.05, 
           label = format(pub_tot, big.mark = ","), 
           color = pub,
           size = 3.25,
           hjust = 1)+
  
  # Choice Scholarship transfer 2024 total
  annotate("text", x = 2024, y = sc_tot+sc_tot*.15, 
           label = format(sc_tot, big.mark = ","),
           color = pr,
           size = 3.25,
           hjust = 1)+
  
  # Charter transfer 2024 total
  annotate("text", x = 2024, y = chr_tot-chr_tot*.2, 
           label = format(chr_tot, big.mark = ","),
           color = chr,
           size = 3.25,
           hjust = 1)+
  
  # grid lines
  geom_segment(data = lines,
               aes(x = x,
                   xend = xend,
                   y = y,
                   yend = yend), 
               color = "lightgray",
               linetype = "dotted", 
               linewidth = .15)+
  
  scale_x_continuous(
    expand = expansion(add = c(.7, 1)),
    breaks = c(seq(2018, 2024, 1)),
    labels = c("2018", "'19", "'20", "'21", "'22", "'23", "2024")
  )+
  
  scale_y_continuous(
    limits = c(0, 100000),
    breaks = c(0, 25000, 50000, 75000),
    labels = c("0", "25K", "50K ", "75K"), 
    name = "Transfers"
  )+
  
  labs(
    title = paste0("After remaining relatively flat for years, <b><span style='color:", pr, "'>Choice Scholarship</span></b> transfers surged ", cs_inc, "% since 2021. Transfers to <b><span style='color:", pub, "'>public</span></b> schools rose ", pub_inc_21, "% in that time and ", pub_inc_18, "% since 2018." ),
    
    subtitle = paste0("Total transfers have increased ", tot_pinc, "% since 2018.<br>" ),
    
    caption = "<br>**Data**:   Indiana Department of Education<br>(c)   tedschurter.com"
  )+
  
  t_theme()+
  
  # theme adjustments
  theme(
    axis.text.y   = element_text(size = rel(x = .8), 
                                 color = "#505050",
                                 hjust = 0, 
                                 # margin=margin(0,-20,0,-10)
    ),
    axis.title.y = element_text(size = rel(x = .85),
                                color = "#505050",
                                hjust = 1,
                                angle = 1,
                                vjust = 1.05,
                                margin=margin(15,-4,0,3)
    ), 
    # adjust margin to align chart with text on webpage
    plot.margin = margin(0,0,0,0, "in")
  )

# save plots

ggsave("docs/images/typ-trend.svg", plot = last_plot(), 
       height = 1300, width = 1800, units = "px")

ggsave("docs/images/typ-trend.png", plot = last_plot(), 
       height = 1300, width = 1800, units = "px")


# chart showing percent increase in enrollment for private, public enrollment and all enrollment ####

# create dataframe that includes the percent change in total transfers, public, private and total enrollment

# total percent change in public and private enrollment
res_pc <- res |> 
  group_by(type, year) |> 
  summarise(tot = sum(enrlmnt)) |> 
  arrange(year) |> 
  mutate(pc = 
           round(100*(tot-lag(tot))/lag(tot),1)) 

# adjust for easier binding to yr_tot dataframe
res_pc <- res_pc |> 
  rename("typ" = "type", 
         "yr" = "year") |> 
  select(yr, typ, tot, pc)

# create dataframe with total enrollment figure to bind to res_pc
res_pc_b <-  res_pc |> 
  group_by(yr) |> 
  summarise(tot = sum(tot)) |> 
  mutate(pc = 
           round(100*(tot-lag(tot))/lag(tot),1)) 


# and in the darkness bind them ... 
res_pc_b$typ <- "Enrollment"

res_pc <- rbind(
  
  res_pc_b |> 
    select(yr, typ, tot, pc),
  
  res_pc
  
) 

# clean up
rm(res_pc_b)

# add column to yr_tot for total transfers
yr_tot$typ <- "Transfer"

yr_tot <- yr_tot |> 
  select(yr, typ, tot) |> 
  mutate(pc = 
           round(100*(tot-lag(tot))/lag(tot),1)) 

# bind them
yr_tot_pc <- rbind(res_pc, yr_tot) 


# grid lines dataframe
x <-    rep(min(yr_tot_pc$yr,  na.rm = T), 4)
xend <- rep(max(yr_tot_pc$yr, na.rm = T), 4)
y <- rep(c(-6, -3, 0, 3, 6, 9, 12), 4)
yend  <-  rep(c(-6, -3, 0, 3, 6, 9, 12), 4)
lines <- data.frame(x, xend,y, yend)

ggplot()+
  
  geom_segment(aes(x = min(yr_tot_pc$yr), xend = max(yr_tot_pc$yr),
                   y = 0, yend = 0),
               color = "gray",
               linewidth = .25)+
  
  # grid lines
  geom_segment(data = lines,
               aes(x = x,
                   xend = xend,
                   y = y,
                   yend = yend), 
               color = "lightgray",
               linetype = "dotted", 
               linewidth = .1)+
  
  # private, public enrollment percent change; transfers percent change 
  geom_line(data = yr_tot_pc,
            aes(yr, pc, color = typ, linetype = typ, size = typ), 
            show.legend = F
  )+
  
  geom_text(data = yr_tot_pc |> filter(yr == 2024) |> 
              mutate(typ = 
                       case_when(
                         typ == "pr"          ~ "Private\nEnrollment", 
                         typ == "p"           ~ "Public\nEnrollment",
                         typ == "Transfer"    ~ "Total\nTransfers",
                         typ == "Enrollment"  ~ "Total\nEnrollment",
                         .default = typ
                       )),
            aes(yr, pc, label = str_wrap(typ, 10)),
            color = atxt, lineheight = .9,
            size = 3, hjust = 0, nudge_x = .15,
            vjust = c(-.25,1,.25, .5) #order: total; public; private; transfers
  )+
  
  scale_color_manual(values = c(
    "pr"         = pr,
    "p"          = pub, 
    "Enrollment" = "black",
    "Transfer"   = "gray"))+
  
  scale_linetype_manual (values = c(
    "pr"         = "solid",
    "p"          = "solid", 
    "Enrollment" = "solid",
    "Transfer"   = "dashed"))+
  
  scale_size_manual (values = c(
    "pr"         = .55,
    "p"          = .55, 
    "Enrollment" = .85,
    "Transfer"   = .55))+
  
  
  scale_x_continuous(
    expand = expansion(add = c(.75, 1.75)),
    breaks = c(seq(2014, 2024, 1)),
    labels = 
      c("2014", "'15", "'16", "'17", "'18", "'19", "'20", "'21", "'22", "'23", "2024")
  ) +
  
  scale_y_continuous(
    breaks = unique(lines$y),
    labels = scales::percent(unique(lines$y), scale = 1),
    name = "% change"
  )+
  
  labs(
    title = paste0("Annual percentage changes in <b>total enrollment </b>remained relatively stable despite dynamic swings in <b><span style='color:", pr, "'>private enrollment</span></b>."),
    
    subtitle = paste0("<b><span style='color:#969696'>Total transfers</span></b> have grown annually since 2018.  <br><br>" ), 
    
    alt = "Line chart showing the percent change in annual enrollment for public, private and total enrollment, as well as total transfers, in Indiana from 2014 to 2024. Private enrollment drops and climbs dramatically between 2020 and 2022, shifting 12%; public enrollment and total enrollment are less volatile, shifting less than 1%.",
    
    caption = "<br>**Data**:   Indiana Department of Education<br>(c)   tedschurter.com"
  )+
  
  t_theme()+
  theme(
    axis.text.y   = element_text(size = rel(x = .8), 
                                 color = "#505050",
                                 hjust = 0, 
                                 # margin=margin(0,-20,0,-10)
    ),
    axis.title.y = element_text(size = rel(x = .7),
                                color = "#505050",
                                hjust = 1,
                                angle = 1,
                                vjust = 1.05,
                                margin=margin(15,-4,0,3)
    ),
    # shift margin to align with webpage
    plot.margin = margin(0.0,0,0,0, "in"),
  )


ggsave("docs/images/res_pc.png", plot = last_plot(),
       height = 1300, width = 1800, units = "px")

ggsave("docs/images/res_pc.svg", plot = last_plot(),
       height = 1300, width = 1800, units = "px")


# figures for webpage text

# percent of total enrollment all transfers make up 2018 
round((yr_tot_pc$tot[yr_tot_pc$typ == "Transfer" & yr_tot_pc$yr == 2018]/
         yr_tot_pc$tot[yr_tot_pc$typ == "Enrollment" & yr_tot_pc$yr == 2018])*100) # 12%

# percent of total enrollment all transfers make up 2024
round((yr_tot_pc$tot[yr_tot_pc$typ == "Transfer" & yr_tot_pc$yr == 2024]/
         yr_tot_pc$tot[yr_tot_pc$typ == "Enrollment" & yr_tot_pc$yr == 2024])*100) # 18%

# percent of total enrollment all Choice Scholarship transfers make up 2018
round((yr_tot_typ$tot[yr_tot_typ$typ == "Choice Scholarship" & yr_tot_typ$yr == 2018]/
         yr_tot_pc$tot[yr_tot_pc$typ == "Enrollment" & yr_tot_pc$yr == 2018])*100) # 3%

# percent of total enrollment all Choice Scholarship transfers make up 2024
round((yr_tot_typ$tot[yr_tot_typ$typ == "Choice Scholarship" & yr_tot_typ$yr == 2024]/
         yr_tot_pc$tot[yr_tot_pc$typ == "Enrollment" & yr_tot_pc$yr == 2024])*100) # 6%

# percent of total enrollment private school enrollment in 2024
(yr_tot_pc$tot[yr_tot_pc$typ == "pr" &
                 yr_tot_pc$yr == 2024]/
    res_pc$tot[res_pc$typ == "Enrollment" &
                 res_pc$yr == 2024])*100  # 8% 

# percent of private school enrollment made up by choice scholarship in 2024
(yr_tot_typ$tot[yr_tot_typ$typ == "Choice Scholarship" &
                  yr_tot_typ$yr == 2024]/
    yr_tot_pc$tot[yr_tot_pc$typ == "pr" &
                    yr_tot_pc$yr == 2024])*100 # 74%

# percent of private school enrollment made up by choice scholarship in 2024
(yr_tot_typ$tot[yr_tot_typ$typ == "Choice Scholarship" &
                  yr_tot_typ$yr == 2018]/
    yr_tot_pc$tot[yr_tot_pc$typ == "pr" &
                    yr_tot_pc$yr == 2018])*100 # 41%


# scatter plot ####

xfr_rate <- read_csv("clean_data/20240612_student_res_xfer_rate_2018_2024.csv") |> 
# import data
  filter(yr == 2024)
# remove abbreviations for xfr_rate to make left_join on nrl_schl_name possible (avoids confusion of using nrl_schl_id where id's aren't unique when mixed with school corporation ids)

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
xfr_rate <- xfr_rate |> 
  
  mutate(
    # add column that identifies both settlement school corporation id and the enrolled school/school corporation id to use in loop and filename
    
    corp_name =
      str_replace_all(corp_name,
                      setNames(rplc_names, set_names)),
    
    
    # remove any extra spaces from name columns
    across(ends_with("_name"),
           .fns = ~ str_replace_all(., "  ", " "))) 


# import and transform transfer data
xfr <- read_csv("clean_data/20240612_transfers_18_24.csv") |>
  
  filter(yr == 2024) |>
  
  rename('Public' = 'par_ch', 'Charter' = 'p_xfer_chrtr', 'Choice Scholarship' = 'pr_ch_sclrs', "Other" = 'p_xfer_oth') |>
  
  select(-tot_xfr) |>
  
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
    
    
    
    # convert 'other' transfers to 'public'; though Indiana classifies Charter schools as public also, in this case we're making a distinction between parent choice and other public and charter public.
    typ = case_match(typ,
                     "Other" ~ 'Public',
                     .default = typ),
  ) |>
  
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
      tot_xfr > 0) |>
  
  # add weight for stroke to increase diameter to make smallest markers more visible
  mutate(weight = 2.5)


# create dataframes of transfers from and transfers to and join them

xfr_from <- 
  xfr |> 
  filter(yr == 2024) |> 
  # group_by(stlmt_corp_name, nrl_schl_name) |> 
  count(name = "xfr_from",
        stlmt_corp_id) 


xfr_rate <- left_join(xfr_rate, 
                      
                      xfr_from, 
                      
                      join_by("corp_id" == "stlmt_corp_id"))



xfr_to <- 
  xfr |> 
  filter(yr == 2024) |> 
  count(name = "xfr_to",
        nrl_schl_name) 


xfr_rate <- left_join(xfr_rate, 
                      
                      xfr_to, 
                      
                      join_by('corp_name' == "nrl_schl_name")) |> 
  
  # add color based on transfer rate: blue for positive, red for negative
  mutate(color = if_else(net_xfr_rate > 0, "black", "red"))


# figures for net transfer copy 

## NOTE: these apply to corporations only and do not include all schools

# positive net transfer

# transfer to
xfr_rate |> 
  filter(net_xfr > 0) |> 
  summarise(mean(xfr_to, na.rm = T)) # 22

# remove outliers
xfr_rate |> 
  filter(net_xfr > 0) |> 
  summarise(mean(xfr_to, na.rm = T, trim = .2)) # 12

# median
xfr_rate |> 
  filter(net_xfr > 0) |> 
  summarise(median(xfr_to, na.rm = T))  # 12

# transfer from
xfr_rate |> 
  filter(net_xfr > 0) |> 
  summarise(mean(xfr_from, na.rm = T)) # 24

# remove outliers
xfr_rate |> 
  filter(net_xfr > 0) |> 
  summarise(mean(xfr_from, na.rm = T, trim = .2)) # 22

# median
xfr_rate |> 
  filter(net_xfr > 0) |> 
  summarise(median(xfr_from, na.rm = T))  # 22


# legal settlement

# trimmed mean
xfr_rate |> 
  filter(net_xfr > 0) |> 
  summarise(mean(in_stlmt_tot, na.rm = T, trim = .2)) # 1224


# negative net transfer

# transfer to
xfr_rate |> 
  filter(net_xfr < 0) |> 
  summarise(mean(xfr_to, na.rm = T)) # 14

# remove outliers
xfr_rate |> 
  filter(net_xfr < 0) |> 
  summarise(mean(xfr_to, na.rm = T, trim = .2)) # 11

# median
xfr_rate |> 
  filter(net_xfr < 0) |> 
  summarise(median(xfr_to, na.rm = T))  # 11


# transfer from
xfr_rate |> 
  filter(net_xfr < 0) |> 
  summarise(mean(xfr_from, na.rm = T)) # 39

# remove outliers
xfr_rate |> 
  filter(net_xfr < 0) |> 
  summarise(mean(xfr_from, na.rm = T, trim = .2)) # 32

# median
xfr_rate |> 
  filter(net_xfr < 0) |> 
  summarise(median(xfr_from, na.rm = T))  # 29

# legal settlement

# trimmed mean
xfr_rate |> 
  filter(net_xfr < 0) |> 
  summarise(mean(in_stlmt_tot, na.rm = T, trim = .2)) # 3000

# how many school corporations have negative net transfers
xfr_rate |> 
  filter(net_xfr < 0) |> 
  nrow() # 186

# how many school corporations have positive net transfers
xfr_rate |> 
  filter(net_xfr > 0) |> 
  nrow()  # 104 

# average number of schools corporation receives transfers from 
xfr_rate |> 
  summarise(mean(xfr_to, na.rm = T)) # 17

xfr_rate |> 
  summarise(median(xfr_to, na.rm = T)) # 11

# scatter plot ####

# create scatter plot
set.seed(23)

# in order to create and place custom legends on combination plot the main plot needs to be run with the legend position theme adjust active. Then assign the legend to the object main_l. Re-run the plot 'main' again this time with the legend them adjustment set to 'none'. 

# the same steps need to be followed for the inset plot: ensure the theme adjustment allows the legend to be shown, assign the legend to its own object, re-run the plot with the legend set to none.

main <- 
  ggplot()+
  geom_jitter(data = xfr_rate,
              aes(xfr_from, xfr_to, size = in_stlmt_tot, color = color),
              alpha = .3)+
  scale_color_manual(values = c("black" = "blue",
                                "red" = "red"))+
  
  # inset box
  geom_rect(aes(
    xmin = 0, xmax = 50,
    ymin = 0, ymax = 50),
    color = atxt,
    fill = NA,
    linewidth = .15
  )+
  
  geom_curve(aes(x = 25,  xend = 78.5,
                 y = 55,  yend = 115),
             color = atxt,
             curvature = -.2,
             linewidth = .35,
             arrow = arrow(length=unit(0.33,"cm"))
  )+
  # 
  # # label for curve
  geom_text(aes(
    x = 75,
    y = 93,
    color = atxt),
    label = "Detail\narea",
    size = 2.5, hjust = 1, lineheight = .9) +
 
  # # main annotation
  annotate("text_box", x = 83.5, y = 72,
           label = "**Seventy-four percent** of all corporations send or receive transfers from fewer than **40** schools.",
           size = 3.65, hjust = 0, vjust = .5, color= atxt, box.color = bg_c,
           fill = NA, lineheight = 1.05, width = unit(2.7, "inch"))+
  
  
  # # Union SC annotation
  annotate("text_box", x = 35, y = 260,
           label = "**Union School Corporation**, top, and **Clarksville Community School Corporation** receive transfers from nearly every corporation in the state.",
           size = 3, hjust = 0, vjust = 1, color= atxt, box.color = bg_c,
           fill = NA, lineheight = 1.05, width = unit(1, "inch"))+
  # 
  # # Union SC curve
  geom_curve(aes(x = 35,  xend = 20,
                 y = 260,  yend = 280),
             color = atxt,
             curvature = .2,
             linewidth = .35,
             arrow = arrow(length=unit(0.23,"cm"))
  )+
  # 
  
  # y axis labels
  
  # custom y axis labels and arrow for x axis  ####
geom_text(aes(
  x = -5,
  y = c(0, 50, 100, 200, 300)),
  color = atxt,
  label = c(0, 50, 100, 200, 300),
  size = 2.5) +
  
  # Transfers from fewer corporations label
  annotate("text", x = -20, y = 65,
           label = "Transfers\nfrom fewer\nschool\ncorporations",
           size = 2.55, hjust = 1, vjust = 1, color= atxt, 
           lineheight = 1, angle = 0)+
  
  # Transfers from more corporations label
  annotate("text", x = -20, y = 265,
           label = "Transfers\nfrom more\nschool\ncorporations",
           size = 2.55, hjust = 1, vjust = 1, color= atxt, 
           lineheight = 1, angle = 0)+
  
  geom_segment(aes(x = -15, xend = -15,
                   y = 150, yend = 10),
               color = atxt,
               linewidth = .2,
               arrow = arrow(length=unit(0.20,"cm") )
  )+
  
  geom_segment(aes(x = -15, xend = -15,
                   y = 150, yend = 280),
               color = atxt,
               linewidth = .2,
               arrow = arrow(length=unit(0.20,"cm") )
  )+
  
  
  # # custom x axis labels and arrow for x axis  ####
geom_text(aes(
  x = c(0, 50, 100, 200, 300),
  y = -20),
  color = atxt,
  label = c(0, 50, 100, 200, 300),
  size = 2.5) +
  
  # Transfers from smaller corporations label
  annotate("text", x = 0, y = -37,
           label = "Transfers to fewer corporations and schools",
           size = 2.75, hjust = -.20, vjust = 1, color= atxt,
           lineheight = 1.1)+
  
  # # Transfers from larger corporations label
  annotate("text", x = 290, y = -37,
           label = "Transfers to more corporations and schools",
           size = 2.75, hjust = 1.1, vjust = 1, color= atxt, lineheight = 1.1)+
  
  geom_segment(aes(x = 150, xend = 10,
                   y = -31, yend = -31),
               color = atxt,
               linewidth = .2,
               arrow = arrow(length=unit(0.20,"cm") )
  )+
  
  geom_segment(aes(x = 150, xend = 290,
                   y = -31, yend = -31),
               color = atxt,
               linewidth = .2,
               arrow = arrow(length=unit(0.20,"cm") )
  )+
  
  scale_size_continuous(
    range = c(.25,6),
    breaks = c(500,1000,5000,10000,20000,40000),
    labels = scales::label_comma(accuracy = 1)
  )+
  
  guides(color = "none",
         size = guide_legend("Legend")
  )+
  
  xlim(-32, 300)+
  ylim(-40, 300)+
  
  labs(
    title = "Larger school corporations are more likely than smaller ones to loose more students than they gain from transfers, and to lose them to a larger number of schools and corporations.",
    
    subtitle = "Each circle represents a school corporation and its size reflects the number of students with legal settlement. <span style='color:blue'>Blue</span> circles indicate a <span style='color:blue'>positive</span> transfer rate; <span style='color:red'>red</span> circles indicate a <span style='color:red'>negative</span> transfer rate.",
    
    caption = "**Data**:   Indiana Department of Education<br>(c)   tedschurter.com"
  )+
  
  t_theme()+
  theme(
    legend.background = element_rect(fill = wbg_c, color = atxt),
    legend.key.width= unit(.05, 'cm'),
    legend.key.height = unit(.3, "cm"),
    # legend.position = c(.95,.75),
    legend.position = 'none',
    legend.text  = element_text(size = 6,
                                hjust = .5,
                                margin = margin(0,0,0,0)),
    legend.title = element_text(size = 7, hjust = 0),
    # adjust margin to align chart with text on webpage
    plot.margin = margin(0,0,0,0, "in"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_rect(
      fill  = bg_c,
      linewidth = .55,
      color = NA), 
    
    
    plot.subtitle = element_textbox(
      family = "sans",
      size = rel(x = .95),
      width = unit(1, "npc"),
      padding = margin(.15, .25, .05, .25),
      margin  = margin(1.75, .45, 0, .25),
      lineheight = 1,
      color = "#252525"
    ),
    
    plot.title = element_textbox(
      family = "serif",
      size = rel(x = 1.3),
      width = unit(1, "npc"),
      margin  = margin(.25, .25, 5, .25),  
      lineheight = 1,
      color = "#252525"
    )
  )

# uncomment if legend needs to be adjusted and enable legend.position theme element above; comment back out after writing legend to object
# main_l <- as_ggplot(get_legend(main))

inset <- ggplot()+
  geom_jitter(data = xfr_rate |> filter(xfr_from <= 40 & xfr_to <=40),
              aes(xfr_from, xfr_to, size = in_stlmt_tot, color = color),
              alpha = .3)+
  geom_rect(aes(xmin = -2, xmax = 41,
                ymin = -2, ymax = 41),
            fill = NA,
            color = "gray")+
  scale_color_manual(values = c("black" = "blue",
                                "red" = "red"))+
  
  guides(color=F,
         size = guide_legend("Detail area\nlegend:")  
  )+ 
  
  
  
  scale_size_continuous(
    range = c(.25,6),
    breaks = c(500, 1000,5000,10000,20000,40000),
    labels = scales::label_comma(accuracy = 1)
  )+
  
  # custom y axis labels and arrow for x axis  ####
geom_text(aes(
  x = -1,
  y = c(0, 10, 20, 30, 39.5)),
  color = atxt,
  label = c(0, 10, 20, 30, 40),
  size = 2.5) +
  
  # custom x axis labels and arrow for x axis  ####
geom_text(aes(
  x = c(10, 20, 30, 39.5)),
  y = 0,
  color = atxt,
  label = c(10, 20, 30, 40),
  size = 2.5) +
  
  xlim(-2, 41.25)+
  ylim(-2, 41.25)+
  t_theme()+
  # theme_void()+
  theme(
    panel.border = element_blank(),
    legend.background = element_rect(fill = wbg_c, color = atxt),
    legend.key.width= unit(.05, 'cm'),
    legend.key.height = unit(.26, "cm"),
    # legend.position = c(.5,.75),
    legend.position = 'none',
    legend.text  = element_text(size = 6,
                                hjust = .5,
                                margin = margin(0,0,0,0)),
    legend.title = element_text(size = 7, hjust = 0),
    # legend.key = element_blank(),
    
    
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = margin(0.0,0,0,0, "in"),
    
    plot.background = element_rect(fill = wbg_c,
                                   # fill  = bg_c,
                                   linewidth = .55,
                                   color = NA), #atxt),
    plot.title.position = "plot"
  )

# # uncomment if legend needs to be adjusted
# inset_l <- as_ggplot(get_legend(inset))


ggdraw(main) +
  draw_plot(inset,
            x = .36,
            y = .365,
            width  =  .45,   
            height =  .45    
  )+
  draw_plot(main_l,
            x = .8,           
            y = .64,          
            width  =  .06,    
            height =  .06     
  )+
  draw_plot(inset_l,
            x = .8262,        
            y = .442,         
            width  =  .06,    
            height =  .06     
  )

ggsave(file = "docs/images/new-scatter_03a.svg",
       plot = last_plot(), 
       width = 2350,
       height = 1710, units = "px", 
       # in order to save as svg, background must be set 
       # github.com/tidyverse/ggplot2/issues/4212
       bg = "white")


# 
