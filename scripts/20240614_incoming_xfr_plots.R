library(tidyverse)
library(ggrepel)
library(ggtext)
library(ggbeeswarm)
library(cowplot)
library(aws.s3)



# script creates a combination chart that shows the annual number of students from 2018 - 2024 transferring into a given school from any school corporation that sends transfer students; chart also contrasts the most recent available (2023) test state and SAT test scores between the enrolled school and the school corporation sending the transfer student(s). *Not all schools have test data available.



# set colors for transfer types ####
pub  <-  '#d95f02'  # Public school   
chr  <-  '#1b9e77'  # Charter school
pr   <-  '#7570b3'  # Choice Scholarship 
bg_c <-  "#fbf9f6"  # background 

# set custom theme
t_theme <- function(base_size = 10){
  
  theme_classic() %+replace%
    
    theme(
      axis.title = element_blank(),
      axis.text.x   = element_text(size = rel(x = .65), color = "#505050"),
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


# prepare data

# import transfer data
ixfr <- read_csv("clean_data/20240613_transfer_pts_18_24.csv") |> 
  
  # remove unneeded columns 
  select(-c(clr, xfr_sqrt,)) |> 
  
  # replace NAs with zeros, trim spaces
  mutate(across(
    c(par_ch, p_xfer_oth, p_xfer_chrtr, pr_ch_sclrs), 
    
    # replace NA entries in columns of transfer types with 0
    ~replace_na(.,0)),
    
    # remove any extra spaces after names
    across(ends_with("_name"),
           .fns = ~ str_replace_all(., "  ", " ")
    ) 
  ) |> 
  
  # exclude transfers from within school corporation
  filter(
    stlmt_corp_name != nrl_schl_name 
  ) |> 
  
  # add column (fname) that identifies both settlement school corporation id and the enrolled school/school corporation id to use in loop and filename; to distinguish between outgoing transfer fnames, at prefix 'it'
  mutate(
    fname = paste0("it-", nrl_schl_id, "-", stlmt_corp_id),
  ) |>
  
  ungroup() |>
  
  # calculate average transfer to the nrl school from the schl_corp (column = avg_nrl_xfr)
  
  group_by(nrl_schl_id, yr, ) |>
  mutate(
    # need count of all transfers to given nrl_schl in the year
    nrl_tot_an_xfr = sum (tot_xfr),
    
    # pct of nrl_schl that schl_corp's transfers represent in given year
    pct_nrl_xfr = round(100*(tot_xfr/nrl_tot_an_xfr))) |> 
  
  group_by(nrl_schl_id, stlmt_corp_id) |> 
  
  # calculate average pct of nrl_schl's annual transfers that have come from stlmt_corp
  mutate(
    avg_nrl_xfr = round(mean(pct_nrl_xfr)),
    avg_nrl_xfr_tot = round(mean(tot_xfr))) |> 
  
  # pivot to long data
  pivot_longer(cols = c(par_ch, p_xfer_oth, p_xfer_chrtr, pr_ch_sclrs), names_to = 'typ', values_to = 'tot_xfr2') |>
  
  # select rows with total transfers greater than 0
  filter(tot_xfr2 > 0) |>
  
  # remove redundant tot_xfr column that is the same as the new column (tot_xfr2) from pivot_longer above
  select(-tot_xfr) |>
  
  # rename typ variables to align with color assignments
  mutate(typ =
           case_when(typ == "par_ch" ~ 'pub',
                     typ == "p_xfer_oth" ~ 'pub',
                     typ == "p_xfer_chrtr" ~ 'chr',
                     typ == "pr_ch_sclrs" ~ 'pr',),
         
         color =
           case_when(typ == 'pub' ~ pub,
                     typ == "chr" ~ chr,
                     typ == "pr" ~ pr,)) |> 
  
  
  group_by(nrl_schl_id, stlmt_corp_id, yr, typ) |>
  
  mutate(
    tot_xfr2 = sum(tot_xfr2),
    
    title =
      paste0(
        "On average, ",
       
        if_else(avg_nrl_xfr <= 1,
                "less than ",
                ""),
        
        if_else(
          avg_nrl_xfr == 0, 1, avg_nrl_xfr
        ),
        
       "% of all transfers into **<span style='color:", color, "'>",
        nrl_schl_name,
        
        "**, ",
        round(avg_nrl_xfr_tot),
        
       if_else(
          avg_nrl_xfr_tot <= 1.4,
          " student, ",
          " students, "),
       
        if_else(
          avg_nrl_xfr_tot <= 1.5,
          " comes ",
          " come "),
       
        "from **<span style='color:#525252'>",
       
        stlmt_corp_name,
       
        "**</span>."
      )
  ) |>
  
  rename(tot_xfr = tot_xfr2)


# import test score data ####
str_tst <- read_csv("clean_data/20240612_tst_scores_23.csv") |> 
  
  # remove unneeded grouped corporation scores
  select(-c(starts_with("corp_") & ends_with("_pct"))) |> 
  
  # pivot to longer data
  pivot_longer(
    cols = ends_with("_pct") & !starts_with("corp_"),
    names_to = "metric",
    values_to = "pct",
  ) |>
  
  # reduce metric column contents to one word entries, deleting anything after the first _
  mutate(metric = str_extract(metric, "[^_]*"))


# add dataframe for beeswarm plot axis lines 
sq <- data.frame(
  x = rep(0,5), # five total lines
  xend = rep(7.75, 5), # want the lines to extend just past the seventh category
  y = c(0, .25, .5, .75, 1), # vertical lines every at 1/4 interval
  yend = c(0, .25, .5, .75, 1)) 


# create plots ####

# run loop based on the number of distinct settlement/enrolled combinations present in the ixfr dataframe
nrl_fn <- unique(ixfr$fname)  

# function for getting integer y-axis values. via: joshuacook.netlify.app/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 3, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n=3, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# vector of letters used in fname to designate private schools; used to filter test scores (beeswarm plot) for either an enrolled private school or school corporation 
pv <- c("A", "B", "C", "D")

# loop 
for(i in 1:length(nrl_fn)){
  
# some transfer relationships between a school corporation and an enrolled school only exist in one year. In those instances, geom_line isn't appropriate and geom_point should be used instead. if else statement checks if a school corp/enrolled school relationship exists for more than one year (if more than 1 row exists) and assigns a plot with geom_point or geom_line accordingly.
  
  if (ixfr |>
      
      filter(nrl_schl_id == str_sub(nrl_fn[i], 4,7)&
             stlmt_corp_id == str_sub(nrl_fn[i], 9,12)) |> nrow() <= 1) {
    
    p1 <- 
      
      
      ggplot()+
      
      geom_point(data =
                   ixfr |> 
                   #group_by(stlmt_corp_id, nrl_schl_name) |> 
                   filter(nrl_schl_id == str_sub(nrl_fn[i], 4,7) &
                            stlmt_corp_id == str_sub(nrl_fn[i], 9,12)),
                 
                 aes(yr, tot_xfr, color = color),
                 
                 color = ixfr$color[ixfr$nrl_schl_id == str_sub(nrl_fn[i], 4,7) &
                                      ixfr$stlmt_corp_id == str_sub(nrl_fn[i], 9,12)][1]
      )+
      
      scale_x_continuous(limits = c(2018, 2024),
                         breaks = seq(2018, 2024, 1),
                         labels = c("2018", "'19", "'20", "'21", "'22", "'23", "2024"))+
      
      scale_y_continuous(
        limits = c(0, max(ixfr |>
                            group_by(stlmt_corp_id, nrl_schl_name) |>
                            filter(nrl_schl_id == str_sub(nrl_fn[i], 4,7) &
                                     stlmt_corp_id == str_sub(nrl_fn[i], 9,12)) |>
                            
                            ungroup() |>
                            arrange(desc(tot_xfr)) |>
                            slice_head(n = 1) |>
                            select(tot_xfr))),
        breaks = integer_breaks(if_else(ixfr$tot_xfr[ixfr$nrl_schl_id == str_sub(nrl_fn[i], 4,7) &
                                                       ixfr$stlmt_corp_id == str_sub(nrl_fn[i], 9,12)][1] <= 1, 2, 4)),
        
        label = scales::label_comma(accuracy = 1),
        expand = expansion(mult = c(.1, .1)),
        name = "Transfers") +
      
      #title and subtitle
      labs(
        title = ixfr$title[ixfr$fname == nrl_fn[i]][1],
        subtitle = ""
        
      )+
      
      t_theme()+
      
      theme(
        axis.text.y   =
          element_text(size = rel(x = .65),   #rel(x = .52), 
                       color = "#858381",
                       margin=margin(5,5,0, -22),
                       hjust = .5),
        axis.title.y =
          element_markdown(
            size = rel(x = .6), #rel(x = .55),
            color = "#858381",
            angle = 0,
            vjust = 1.11, # move y axis title up a smidge
            hjust = 1,
            margin=margin(.05,0,.15,0, "in"),
          ),
        panel.grid.major.y = element_line(color = "gray",
                                          linewidth = .25,
                                          linetype = "dotted"),
        plot.margin = margin(.15,.12,.1,.12, "in")
      )   
    
  }  else {
    
    p1 <- 
      
      ggplot(data = 
               ixfr |> 
               group_by(stlmt_corp_id, nrl_schl_name) |> 
               filter(nrl_schl_id == str_sub(nrl_fn[i], 4,7)&
                        stlmt_corp_id == str_sub(nrl_fn[i], 9,12)))+
      
      geom_line(
        aes(yr, tot_xfr, group = 1),
        
        color = ixfr$color[ixfr$nrl_schl_id == str_sub(nrl_fn[i], 4,7) &
                             ixfr$stlmt_corp_id == str_sub(nrl_fn[i], 9,12)][1]
      )+
      
      scale_x_continuous(limits = c(2018, 2024),
                         breaks = seq(2018, 2024, 1),
                         labels = c("2018", "'19", "'20", "'21", "'22", "'23", "2024"))+
      
      scale_y_continuous(
        limits = c(0, max(ixfr |> 
                            group_by(stlmt_corp_id, nrl_schl_name) |> 
                            filter(nrl_schl_id == str_sub(nrl_fn[i], 4,7) &
                                     stlmt_corp_id == str_sub(nrl_fn[i], 9,12)) |> 
                            
                            ungroup() |> 
                            arrange(desc(tot_xfr)) |> 
                            slice_head(n = 1) |> 
                            select(tot_xfr))),
        breaks = integer_breaks(if_else(ixfr$tot_xfr[ixfr$nrl_schl_id == str_sub(nrl_fn[i], 4,7) &
                                                       ixfr$stlmt_corp_id == str_sub(nrl_fn[i], 9,12)][1] <= 1, 2, 4)), 
        label = scales::label_comma(accuracy = 1),
        expand = expansion(mult = c(.1, .1)),
        name = "Transfers") +
      
      # title and subtitle 
      labs(
        title = ixfr$title[ixfr$fname == nrl_fn[i]][1],
        subtitle = ""
        
      )+
      
      t_theme()+
      
      theme(
        axis.text.y   =
          element_text(size = rel(x = .65),   #rel(x = .52), 
                       color = "#858381",
                       margin=margin(5,5,0, -22),
                       hjust = .5),
        axis.title.y =
          element_markdown(
            size = rel(x = .6), #rel(x = .55),
            color = "#858381",
            angle = 0,
            vjust = 1.11, # move y axis title up a smidge
            hjust = 1,
            margin=margin(.05,0,.15,0, "in"),
          ),
        panel.grid.major.y = element_line(color = "gray",
                                          linewidth = .25,
                                          linetype = "dotted"),
        plot.margin = margin(.15,.12,.1,.12, "in")
      ) 
    
  }
  
# lower half of full chart is beeswarm plot contrasting test scores between sending and receiving schools. *Not all schools have test data available.
  
  p2 <- 
    ggplot()+
    
    # beeswarm for enrolled school
    geom_beeswarm(
      # if nrl_fn[i] contains an alphanumeric character, the enrolled school is a choice scholarship school and the data need to be filtered by the schl_id column; if not, filter by the corp_id column
      data = if (str_sub(nrl_fn[i], 4,4) %in% pv) {
        str_tst |>  filter(schl_id == str_sub(nrl_fn[i], 4,7))
      } else {
        str_tst |>  filter(corp_id == str_sub(nrl_fn[i], 4,7))
      },
      
      aes(metric, pct),
      pch = 21,
      size = 1.25, #1,
      
      fill = ixfr$color[ixfr$nrl_schl_id == str_sub(nrl_fn[i], 4,7) &
                          ixfr$stlmt_corp_id == str_sub(nrl_fn[i], 9,12)][1],
      color =  ixfr$color[ixfr$nrl_schl_id == str_sub(nrl_fn[i], 4,7) &
                            ixfr$stlmt_corp_id == str_sub(nrl_fn[i], 9,12)][1],
      cex = 2.25, alpha = .6)+
    
    # beeswarm for corporation/school transfer from
    geom_beeswarm(data = str_tst |>
                    filter(corp_id == str_sub(nrl_fn[i], 9,12)),
                  aes(metric, pct), 
                  pch = 21,
                  fill = "white",
                  size =  1.25, #1,
                  color = "#252525",
                  cex = 2.25, 
                  side = 0, # jitter points both directions
                  alpha = .6) +
    
    # axis lines
    geom_segment(data = sq,
                 aes(x = x, xend = xend,
                     y = y, yend = yend),
                 color = "light gray",
                 linetype = "dotted",
                 linewidth = .25) +
    scale_y_continuous(labels = scales::percent,
                       breaks = c(0, .25,.5, .75, 1),
                       limits = c(0,1))+
    scale_x_discrete(
      limits = c(
        "ela",
        "math",
        "iread",
        "ss",
        "sc",
        "bio",
        "sat"
      ),
      labels = c(
        "English\nLanguage\nArts",
        "Math",
        "Reading",
        "Social\nstudies",
        "Science",
        "Biology",
        "SAT"
      ))+
    
    labs(
      title = "Students meeting or exceeding 2023 state and SAT testing standards.",
      subtitle = "Each circle represents one school’s test results.",
      caption = "<br>**Data**:   Indiana Department of Education<br>(c)   tedschurter.com")+
    
    # theme and adjustments
    t_theme()+
    theme(
      axis.text.x   = element_text(size = rel(x = .65), #rel(x = .55), 
                                   color = "#636363"),
      axis.text.y   = element_text(size = rel(x = .65), #rel(x = .52), 
                                   color = "#636363",
                                   margin=margin(0,5,0, -2)
      ),
      # reduce bottom plot margin
      plot.margin = margin(0.05,.12,.1,.12, "in"),
      plot.caption = element_markdown(
        margin = margin(0, 0, .1, 0), 
        hjust = 1, 
        halign = 1.03,
        size = rel(x = .43))
    )
  
  
  
  # assemble plots using cowplot
  plot_grid(
    p1, p2, ncol = 1)  
  
  
  # save file using filename from nrl_fn   
  ggsave(filename = paste0("plots/xfr_to/", 
                           nrl_fn[i], 
                           ".png"),
         width  = 3,
         height = 4,
         units = "in", plot = last_plot())
}

# load to AWS ####


# assign bucket name
bucket <- "tedschurter-data"
region <- "us-east-2" # bucket assigned to us-east-2 region; also set by default in r environment


# check that it's there
bucket_exists(bucket) # returns TRUE


# loop through folder adding plots 

plots <- unique(list.files("plots/xfr_to"))

for(i in 1:length(plots)){
  
  # put object in bucket
  put_object(
    object = paste0("2024-in-ed-",plots[i]),
    bucket = bucket,
    file = paste0("plots/xfr_to/", plots[i]),
    region = region,
    headers = c('Content-Type' = 'image/png') # without headers argument, content-type metadata set to binary/octet-stream and file downloads rather than displays
  )
}


# loop to delete all above objects if needed
# 
# for(i in 1:length(plots)){
#   delete_object(
#     object = paste0("2024-in-ed-",plots[i]),
#     bucket = bucket,
#     quiet = TRUE)
# }
