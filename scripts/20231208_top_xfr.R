library(tidyverse)
library(ggrepel)
library(ggtext)
library(aws.s3)


# background color 
bg_c <-  "#fbf9f6"
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


# load prepared trasnfer data ####
sxfer <- read_csv("clean_data/transfers_18_23_ll.csv")


# create dataframe with total transfers into public schools
sxfer <-  sxfer |> 
  
  # rename tot_xfr to xfr
  rename("xfr" = "tot_xfr") |> 
  
  # cacluate total transfers to enrolled school by year
  group_by(nrl_schl_name, yr) |> 
  
  mutate(xfr = sum(xfr)) |>  
  
  # select distinct enrolled school
  distinct(nrl_schl_name, .keep_all = T) |>
  
  # remove settlement corporation columns and transfer type columns
  select(-c(stlmt_corp_id, stlmt_corp_name, par_ch, p_xfer_oth, p_xfer_chrtr, pr_ch_sclrs, lon)) |>  
  
  # determine if school is public or private (id begins with ALPHA character for private schools)
  mutate(pv = if_else(
    str_starts(nrl_schl_id, pattern = "[:alpha:]") == T, "pv", "pb")) |> 
  
  # filter to public schools 
  filter(
    pv == "pb") |> 
  
  # remove redundant rows
  distinct(nrl_schl_name, .keep_all = T) 


# import resident enrollment data
res <- read_csv("clean_data/student_res_xfer_rate_2018_2023.csv") |>
  
  # select year, corporation name and id and the count of enrolled students from the corporation
  select(yr, corp_name, corp_id, res_nrl) |> 
  
  # convert year to 4 digits
  mutate(yr = year(yr)) |>
  
  # group by school corporation and year and calculate total enrollment
  group_by(corp_name, yr) |> 
  mutate(res_nrl = sum(res_nrl)) |> 
  
  # select distinct year
  distinct(yr, .keep_all = T) 



# create dataframe to loop through with totals for both resident enrollment and transfers ####

# join transfer data and resident enrollment dataframes
xfr <- left_join(res |> select(yr, corp_id, res_nrl),
                 
                 sxfer |> 
                   filter(nrl_schl_id %in% res$corp_id) |>
                   select(yr, nrl_schl_id, xfr, nrl_lat, nrl_lon),
                 
                 join_by("corp_id" == "nrl_schl_id",
                         "yr" == "yr")
) |>
  
  select(-corp_name, yr, nrl_schl_name, res_nrl, xfr)  |> 
  
  # calculate transfers as percent of student body
  group_by(nrl_schl_name) |> 
  arrange(desc(yr)) |> 
  mutate(
    xfr_pct = round(100*(xfr/(res_nrl+xfr)),1),
    pct_avg = round(mean(xfr_pct),1),
    
    # determine max transfer 
    max_xfr = max(xfr)) 


# replace NAs in nrl_schl_name with names from res dataframe (same entities were matched in above join using IDOE id number assignments)

xfr <- xfr |> 
  mutate(
    nrl_schl_name = if_else(
      is.na(nrl_schl_name) == T, corp_name, nrl_schl_name),
    # remove any extra spaces from stlmt and nrl name columns
    across(ends_with("_name"),
           .fns = ~ str_replace_all(., "  ", " "))
  ) 



# loop for plots ####

# list of unique school names to loop through
cname <- unique(xfr$nrl_schl_name)

for(i in 1:length(cname)) {
  
  # calculate average transfer number by enrolled school for each school 
  avg_xfr_pct <- xfr |> 
    group_by(nrl_schl_name) |> 
    filter(nrl_schl_name == cname[i]) |> 
    summarise(avg = mean(xfr_pct, na.rm = T)) |> 
    select(avg)
  
  # create unique title for [i] in cname list
  title <-
    paste0("<span style='color:#0868ac'>Transfer </span>students at **", cname[i], 
           "** make up an average of ", round(avg_xfr_pct,1), "% of the student population.")
  
  # generate plot
  ggplot() +
    
    geom_col(
      data = xfr |>
        filter(nrl_schl_name == cname[i]),
      aes(yr, xfr),
      fill = "#0868ac",
      width = .45
    ) +
    
    # transfer total labels w/out "students"
    geom_text(
      data = xfr |>
        filter(nrl_schl_name == cname[i] & yr != 2023),
      aes(yr, xfr,
          label = format(xfr, big.mark = ",")),
      size = 1.5,
      color = "black",
      hjust = -.2
    ) +
    
    # transfer total labels w/"students" for 2023 year
    geom_text(
      data = xfr |>
        filter(nrl_schl_name == cname[i] & yr == 2023),
      aes(yr, xfr,
          label = paste0(format(xfr, big.mark = ","), " students")),
      size = 1.5,
      color = "black",
      hjust = -.1
    ) +
    
    # resident enrollment columns
    geom_col(
      data = xfr |>
        filter(nrl_schl_name == cname[i]),
      aes(yr,-1 * res_nrl),
      fill = "#fdae61",
      width = .45
    ) +
    
    # resident enrollment label
    geom_text(
      data = xfr |>
        filter(nrl_schl_name == cname[i]),
      aes(yr,-1 * res_nrl,
          label = format(res_nrl, big.mark = ",")),
      size = 1.5,
      color = "black",
      hjust = 1.4
    ) +
    
    # flip axis 
    coord_flip() +
    
    # scales 
    scale_x_continuous(
      #limits = c(2017, 2024),
      breaks = c(2018, 2019, 2020, 2021, 2022, 2023),
      labels = c("2018", "'19", "'20", "'21", "'22", "2023")
    ) +
    
    scale_y_continuous(expand = c(.25, .5),
                       labels = NULL) +
    
    # titles 
    labs(title = title,
         
         caption = "**Data**:   Indiana Department of Education<br>(c)   tedschurter.com") +
    
    # theme adjustments
    t_theme()+
    
    theme(
      axis.text.y   = element_text(size = rel(x = .4), color = "#505050",
                                   margin=margin(0,-10,0,0)), #(0,-20,0,0)),
      
      plot.title = element_textbox(
        family = "serif", 
        size = rel(x = .8),
        width = unit(1, "npc"),
        padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
        margin  = margin(.25, .25, 4, .25),   #(8, 0, 0, 0),
        lineheight = 1
      ),
      plot.margin = margin(.15,.1,.05,.1, "in")
    )
  
  # save plot
  ggsave(
    filename = paste0("plots/xfr_plots/",
                      xfr$corp_id[xfr$nrl_schl_name == cname[i]][1],
                      ".png"),
    width  = 3,
    height = 2,
    units = "in",
    plot = last_plot()
  )
}

# ####



# upload files to AWS S3 bucket ####

# assign bucket name
bucket <- "tedschurter-data"
region <- "us-east-2" # bucket assigned to us-east-2 region; can also set by default in r environment


# check that it's there
bucket_exists(bucket) # returns TRUE


#loop through folder adding plots 

plots <- unique(list.files("plots/xfr_plots"))

for(i in 1:length(plots)){
  
  # put object in bucket
  put_object(
    object = paste0("2024-in-ed-",plots[i]),
    bucket = bucket,
    file = paste0("plots/xfr_plots/", plots[i]),
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