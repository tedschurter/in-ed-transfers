library(tidyverse)
library(png)
library(ggpubr)
library(ggimage)
library(ggmap)
library(ggtext)
library(cowplot)
library(readxl)
library(sf)
library(tigris)


# import data ####
sxfer <- read_csv("clean_data/20240614_sxfer.csv") |> 
  
  filter(yr == '2024' 
         # filter Rockville Community School Corporation which is duplicated
         & stlmt_corp_name != "Rockville Community School Corporation")

res <- read_csv("clean_data/20240612_enrollment_14_24.csv")

cp <- read_xlsx("raw_data/fall-2023-2024-public-corporation-transfer-report.xlsx",
                sheet = 3,
                skip = 1) 

# new column names  
nc <- 
  c(
    "stlmt_corp_id",
    "stlmt_corp_name",
    "leg_stl_tot",
    "res_nrl_tot",
    "pub_incoming",
    "pub_outgoing",
    "net_pub",
    "non_pub_outgoing",
    "net_xfr"
  )

colnames(cp) <- nc

cp <- cp |> 
  mutate(xfr_rate = round((net_xfr/leg_stl_tot)*100)) 

# update column names
nc <- colnames(cp) 

# update cp with corrected school corporation names (done already in sxfer)
cp <- 
  left_join(
    
    sxfer |> 
      select(stlmt_corp_id, stlmt_corp_name) |> 
      # need to remove West Clark Community Schools - present in sxfer dataframe but was replaced by Borden Henryville in Jul 2020
      
      filter(stlmt_corp_id != "0940") |> 
      
      distinct(),
    
    cp, 
    
    
    by = 'stlmt_corp_id') |> 
  # remove extra stlmt_corp_name column  
  select(-stlmt_corp_name.y)

# update column names 
colnames(cp) <- nc


# import state map and districts ####
in_dis <-
  st_read("clean_data/20240607_corporation_bounds.geojson")

# state border
ind <- states() |> 
  
  filter(NAME == "Indiana")

# set colors and theme ####

# background color 
bg_c <-   "#fcfdfd" 

# transfer type colors
lgst <-  '#d0ccbe' 
# slightly darker shade of legal settlement color for use in headlines
lgst_txt <- "#b0aca0"
# public school
pub <-   '#d95f02'      
# charter school
chr <-   '#1b9e77'      
# Choice Scholarship
pr  <-   '#7570b3'  
# incoming transfers
inc <-   '#525252'  

# values for scale_manual
vals <- c(
  '#d0ccbe' = '#d0ccbe',
  '#d95f02' = '#d95f02',    
  '#1b9e77' = '#1b9e77',    
  '#7570b3' = '#7570b3', 
  '#525252' = '#525252',
  "#fcfdfd" = "#fcfdfd")


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

# icons ####

# path to icon images/charts for first scrolly section
sPath <- "icons/"
# plain girl image
gt <- paste0(sPath, 'girl_plain.png')
# cutout girl image
gcu <- paste0(sPath, 'girl_cu.png')
# plain boy image
bt <- paste0(sPath, 'boy_plain.png')
# cutout boy image
bcu <- paste0(sPath, 'boy_cu.png')

# vector of student types
student <- c(gt, bt)

# vector of students types transfer out (use cutouts instead to signify students that have left)
out_st <- c(gcu, bcu)


# building blocks for transfer explainer scrolly images ####

# create grid of 10 by 10 student icons with columns to adjust color, icon choice
df <- data.frame(matrix(nrow = 1,
                        ncol = 4))

colnames(df) <- c('x', 'y', 'color', 'image')

df <- df |> 
  
  filter(x != is.na(x), 
         y != is.na(y)
  )

b <- seq(1, 100, 1)


for(i in 1:length(seq)){
  
  df <- rbind(df, 
              
              data.frame(
                x = rep(seq(i, 10, 1),10),
                y = rep(seq(1, 10, 1), each = 10), 
                color = NA, 
                image = NA
              )
  )
}

# vector of student color types to populate grids
students <- c('pub',
              'chr',
              'pr', 
              'lst')


# all legal settlement grid object 

ls <- df

# add students, color and type

# color and type
ls$color <- lgst
# ls$type <- pub

# sample between male and female student icons
ls$image <- sample(c(paste0(sPath, 'boy_plain.png'), paste0(sPath, 'girl_plain.png')), 100, T)


# scrolly step 1 ####

# step 1 left side

lst <- 
  ggplot(ls)+
  
  geom_image(
    aes(x, y, color = color, image = image,
        #size = .047
    )
  )+
  
  scale_color_manual(values = vals)+
  
  labs(
    title = paste0("Students with **<span style ='color:", lgst_txt, "'>legal settlement</span>** in Western Boone County Community School District live within its boundaries.<br>")
  )+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_textbox(
      family = "serif", 
      size = rel(x = .7),
      width = unit(1, "npc"),
      padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
      margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
      lineheight = 1.15
    ),
  )


# map with highlighted district
# base state map with one county highlighted for example 

map_1_1 <- 
  ggplot()+
  
  geom_sf(data = in_dis,
          color = "#eeeded",  #"lightgray", 
          size = .01,
          fill = bg_c)+
  
  geom_sf(data = ind,
          color = "black",
          fill = NA)+
  
  geom_sf(data = in_dis |> 
            filter(corp_name == "Western Boone County Community School District"),
          color = "black", size = .01,
          fill = lgst)+
  
  geom_label(data = sxfer |> 
               filter(stlmt_corp_name == "Western Boone County Community School District"),
             aes(stlmt_lon, stlmt_lat, 
                 label = "Western Boone County\nCommunity School District",
                 lineheight = 1, hjust = .1), 
             nudge_y = .45, size = 1.7, fill = bg_c, label.size = .05)+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )

# combine plots

plot_grid(lst, map_1_1, align = "v", axis = "tb", 
          rel_widths = c(1,1), scale = c(1, 1.15))+
  t_theme()

ggsave("docs/images/lst_1.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")



# scrolly step 2 public school transfers ####

# how many students transfer to public schools per 100
# assign legal settlement total for Western Boone to wb
wb <- cp |> filter(stlmt_corp_name == "Western Boone County Community School District") |> 
  
  select(leg_stl_tot) |> pull()

z <- round(100*((sxfer |> 
                   
                   filter(yr == 2024 & stlmt_corp_name == "Western Boone County Community School District"  & typ == "Public") |> 
                   
                   summarise(tot = sum(tot_xfr)) |> 
                   
                   select(tot)/
                   wb)
) 
)|>
  
  pull()  # 8

# randomly assign z number of icon rows to change to outline shape to represent outgoing transfers 
zz <- sample(100, abs(z), F) 

# if transfers should be grouped together instead of randomly dispersed use simple sequence instead
# zz <- seq(1, z, by = 1)  

# replace the full figures with outline figure to represent their absence


for (a in (zz)){ # if transfers are to be grouped together, use 1:length(zz)
  ls[a,4] <- sample(out_st, 1)
  ls[a,3] <- pub
}

lst_2 <- 
  ggplot(ls)+
  
  geom_image(
    aes(x, y, color = color, image = image)
  )+
  
  scale_color_manual(values = vals)+
  
  labs(
    title = paste0("For every 100 students with legal settlement in Western Boone County Community School District, <span style='color:#d95f02'>**", z, "</span>** transfer to another **<span style='color:#d95f02'>public school corporation</span>**.")
  )+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_textbox(
      family = "serif", 
      size = rel(x = .7),
      width = unit(1, "npc"),
      padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
      margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
      lineheight = 1.15
    ),
  )

# same map with public transfers mapped
map_1_2 <- 
  ggplot()+
  
  geom_sf(data = in_dis,
          color = "#eeeded",  #"lightgray", 
          size = .01,
          fill = bg_c)+
  
  geom_sf(data = ind,
          color = "black",
          fill = NA)+
  
  geom_sf(data = in_dis |> 
            filter(corp_name == "Western Boone County Community School District"),
          color = "black", size = .01,
          fill = lgst)+
  
  geom_point(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Public"),
             aes(x = nrl_lon,
                 y = nrl_lat, size = tot_xfr),
             color = pub,
             alpha = .75)+
  
  geom_curve(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Public"),
             aes(x = stlmt_lon, xend = nrl_lon,
                 y = stlmt_lat, yend = nrl_lat), 
             color = pub, 
             size = .15, 
             alpha = .75)+
  
  scale_size(range = c(1,5))+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )

# combine plots

plot_grid(lst_2, map_1_2, align = "v", axis = "tb", 
          rel_widths = c(1,1), scale = c(1, 1.15))+
  t_theme()


ggsave("docs/images/lst_2.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")


# scrolly step 3 charter school transfers ####

# how many charter school transfers
z <- round(100*(sxfer |> 
                  
                  filter(yr == 2024 & stlmt_corp_name == "Western Boone County Community School District"  & typ == "Charter") |> 
                  
                  summarise(tot = sum(tot_xfr)) |> 
                  
                  select(tot)/wb)
) |>
  pull() #2

# randomly assign z number of icon rows to change to outline shape to represent outgoing transfers 
zz <- sample(100, abs(z), F) 
# if transfers should be grouped together use simple sequence instead
# zz <- seq(1, z, by = 1)  

# replace the full figures with outline figure to represent their absence
for (a in (zz)){ # if transfers are to be grouped together, use 1:length(zz)
  ls[a,4] <- sample(out_st, 1)
  ls[a,3] <- "#1b9e77"
}

lst_2 <- 
  ggplot(ls)+
  
  geom_image(
    aes(x, y, color = color, image = image)
    
  )+
  
  scale_color_manual(values = vals)+
  
  labs(
    title = paste0("For every 100 students with legal residence in Western Boone County Community School District, **<span style='color:#1b9e77'>", z, "**</span> transfer to a **<span style='color:#1b9e77'> public charter school</span>**.")
  )+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_textbox(
      family = "serif", 
      size = rel(x = .7),
      width = unit(1, "npc"),
      padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
      margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
      lineheight = 1.15
    ),
  )



# same map with public transfers mapped
map_1_2 <- 
  ggplot()+
  
  geom_sf(data = in_dis,
          color = "#eeeded",  #"lightgray", 
          size = .01,
          fill = bg_c)+
  
  geom_sf(data = ind,
          color = "black",
          fill = NA)+
  
  geom_sf(data = in_dis |> 
            filter(corp_name == "Western Boone County Community School District"),
          color = "black", size = .01,
          fill = "lightgray")+
  
  geom_point(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Public"),
             aes(x = nrl_lon,
                 y = nrl_lat, size = tot_xfr),
             color = pub,
             alpha = .15)+
  
  geom_curve(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Public"),
             aes(x = stlmt_lon, xend = nrl_lon,
                 y = stlmt_lat, yend = nrl_lat), 
             color = pub, 
             size = .15, 
             alpha = .3)+
  
  # charter schools
  geom_point(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Charter"),
             aes(x = nrl_lon,
                 y = nrl_lat, size = tot_xfr),
             color = chr,
             alpha = 1)+
  
  geom_curve(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Charter"),
             aes(x = stlmt_lon, xend = nrl_lon,
                 y = stlmt_lat, yend = nrl_lat),
             color = chr, 
             size = .25, 
             alpha = 1)+
  
  scale_size(range = c(1,5))+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )

# combine plots

plot_grid(lst_2, map_1_2, align = "v", axis = "tb", 
          rel_widths = c(1,1), scale = c(1, 1.15))+
  t_theme()


ggsave("docs/images/lst_3.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")


# scrolly step 4 choice scholarship transfers ####

# how many transfer to a choice scholarship school
z <- round(100*(sxfer |> 
                  filter(yr == 2024 & stlmt_corp_name == "Western Boone County Community School District"  & typ == "Choice Scholarship") |> 
                  
                  summarise(tot = sum(tot_xfr)) |> 
                  
                  select(tot)/wb)
) |>
  pull() #2 

# randomly assign z number of icon rows to change to outline shape to represent outgoing transfers 
zz <- sample(100, abs(z), F) 
# if transfers should be grouped together use simple sequence instead
# zz <- seq(1, z, by = 1)  

# replace the full figures with outline figure to represent their absence
for (a in (zz)){ # if transfers are to be grouped together, use 1:length(zz)
  ls[a,4] <- sample(out_st, 1)
  ls[a,3] <- pr
}

lst_3 <- 
  ggplot(ls)+
  
  geom_image(
    aes(x, y, color = color, image = image)
  )+
  
  scale_color_manual(values = vals)+
  
  labs(
    title = paste0("For every 100 students with legal residence in Western Boone County Community School District, **<span style='color:#7570b3'>", z, "**</span> transfer to a **<span style='color:#7570b3'>Choice Scholarship school</span>**.")
  )+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_textbox(
      family = "serif", 
      size = rel(x = .7),
      width = unit(1, "npc"),
      padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
      margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
      lineheight = 1.15
    ),
  )


# same map with public transfers mapped
map_1_3 <- 
  ggplot()+
  
  geom_sf(data = in_dis,
          color = "#eeeded", 
          size = .01,
          fill = bg_c)+
  
  geom_sf(data = ind,
          color = "black",
          fill = NA)+
  
  geom_sf(data = in_dis |> 
            filter(corp_name == "Western Boone County Community School District"),
          color = "black", size = .01,
          fill = "lightgray")+
  
  geom_point(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Public"),
             aes(x = nrl_lon,
                 y = nrl_lat, size = tot_xfr),
             color = pub, 
             alpha = .15
  )+
  
  geom_curve(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Public"),
             aes(x = stlmt_lon, xend = nrl_lon,
                 y = stlmt_lat, yend = nrl_lat), 
             color = pub, 
             size = .15, 
             alpha = .3 #.7
  )+
  
  # charter schools
  geom_point(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Charter"),
             aes(x = nrl_lon,
                 y = nrl_lat, size = tot_xfr),
             color = chr,
             alpha = .15
  )+
  
  geom_curve(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Charter"),
             aes(x = stlmt_lon, xend = nrl_lon,
                 y = stlmt_lat, yend = nrl_lat),
             color = chr, 
             size = .15, 
             alpha = .3 #.7
  )+
  
  # choice scholarship schools
  geom_point(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Choice Scholarship"),
             aes(x = nrl_lon,
                 y = nrl_lat, size = tot_xfr),
             color = pr,
             alpha = 1
  )+
  
  geom_curve(data = sxfer |> filter(stlmt_corp_name == "Western Boone County Community School District" & typ == "Choice Scholarship"),
             aes(x = stlmt_lon, xend = nrl_lon,
                 y = stlmt_lat, yend = nrl_lat),
             color = pr, 
             size = .15, 
             alpha = 1)+
  
  scale_size(range = c(1,5))+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )

# combine plots

plot_grid(lst_3, map_1_3, align = "v", axis = "tb", 
          rel_widths = c(1,1), scale = c(1, 1.15))+
  t_theme()


ggsave("docs/images/lst_4.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")



# scrolly step 5 incoming transfers ####

# need to determine z, the number of incoming transfers / 100 students with legal settlement; 20240808 update: to preserve consistency, incoming transfer total will come from sxfer data - sheet 3 vs sheet 4 (net transfers) of IDOE transfer xlsx file. 
z <- round(100*(
  sxfer |> 
    filter(nrl_schl_name == "Western Boone County Community School District" &
             yr == 2024) |> 
    
    summarise(tot = sum(tot_xfr)) |> 
    
    pull()/
    
    wb)
)# 17 (16.89)


# need to add enough rows to cover z: two rows of ten to existing ls dataframe to preserve figures removed for outgoing transfers


# add enough incoming transfer students to existing ls dataframe to cover z students (17)
ls <- rbind(ls, 
            
            # add a row of 10 student icons filled with incoming transfer color
            data.frame(x = seq(1,10),
                       y = rep(11, 10), 
                       color = inc,
                       image = sample(student, size = 10, T)),
            
            # add second row of 10 student icons filled with incoming transfer color
            data.frame(x = seq(1,10),
                       y = rep(12, 10), 
                       color = inc,
                       image = sample(student, size = 10, T))
)

# need to remove the last five rows to remove five figures from total of 120
ls <- ls[1:(100+z),]

# make plot
lst_4 <- 
  ggplot(ls)+
  
  geom_image(
    aes(x, y, color = color, image = image),
    size = .04
  )+
  
  scale_color_manual(values = vals)+
  
  labs(
    title = paste0("For every 100 students with legal residence in Western Boone County Community School District, **<span style='color:", inc, "'>", round(z), "**</span> transfer in from other school corporations.")
  )+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_textbox(
      family = "serif", 
      size = rel(x = .7),
      width = unit(1, "npc"),
      padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
      margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
      lineheight = 1.15
    ),
  )



# map with reduced opacity for outgoing transfers; need to add incoming transfers
map_1_4 <- 
  ggplot()+
  
  geom_sf(data = in_dis,
          color = "#eeeded", 
          size = .01,
          fill = bg_c)+
  
  geom_sf(data = ind,
          color = "black",
          fill = NA)+
  
  geom_sf(data = in_dis |> 
            filter(corp_name == "Western Boone County Community School District"),
          color = "black", size = .01,
          fill = lgst)+
  
  # incoming transfers
  geom_point(data = sxfer |>
               filter(yr == 2024 & nrl_schl_name == "Western Boone County Community School District"),
             aes(x = stlmt_lon,
                 y = stlmt_lat, size = sqrt(tot_xfr)/4),
             color = inc,
             alpha = .7)+
  
  geom_segment(data = sxfer |>
                 filter(yr == 2024 & nrl_schl_name == "Western Boone County Community School District"),
               aes(xend = stlmt_lon, x = nrl_lon,
                   yend = stlmt_lat, y = nrl_lat),
               color = inc, 
               size = .15, 
               alpha = .75,
  )+
  
  scale_size(range = c(0,4))+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )

# combine plots

plot_grid(lst_4, map_1_4, align = "v", axis = "tb", 
          rel_widths = c(1,1), scale = c(1, 1.15))+
  t_theme()


ggsave("docs/images/lst_5.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")


# scrolly step 6 Western Boone incoming transfers ####


# to show final net transfer rate use block of 100 icons + 5 (net transfer rate for W. Boone...) incoming; for continuity, make total icons same, but as plot above, make the difference background color so they disappear.

# all legal settlement grid object ls
ls <- df

# add students, color and type

# color and type
ls$color <- lgst
# ls$type <- pub

# sample between male and female student icons
ls$image <- sample(c(paste0(sPath, 'boy_plain.png'), paste0(sPath, 'girl_plain.png')), 100, T)


# NOTE: These charts use whole figures and when rounding up to preserve that, and when separating out a rate for public, charter and choice scholarship outgoing transfers for illustration purposes, the rates end up slightly higher than if calculated using the raw figures collectively. (Specifically, the rate of 23 for both charter and choice scholarship transfers is 1.59 which each round to 2 and add four total icons; if taken together would add only 3 icons) This example is using five - the incoming rate of 17 - the outgoing rate (boosted by the rounding) of 12 to get a net rate of 5 rather than the actual rate of 5.54, which would ordinarily be rounded to 6. 

z <- 5

# add incoming transfer students to existing ls dataframe
ls <- rbind(ls, 
            
            data.frame(x = seq(1,10),
                       y = rep(11, 10), 
                       color = inc,
                       image = sample(student, size = 10, T)),
            
            # data.frame(x = seq(1,10),
            #            y = rep(12, 10), 
            #            color = inc,
            #            image = sample(student, size = 10, T)),
            
            deparse.level = 0
)

# fill unneeded icons with background color to hide but retain proportion
ls[106:110,3] <- bg_c



# make plot
lst_4a <- 
  ggplot(ls)+
  
  geom_image(
    aes(x, y, color = color, image = image),
    size = .04
  )+
  
  scale_color_manual(values = vals)+
  
  labs(
    title = paste0("Western Boone County Community School District's net transfer rate of **",z,"** means it has more students attending its schools than living within its borders.")
  )+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_textbox(
      family = "serif", 
      size = rel(x = .7),
      width = unit(1, "npc"),
      padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
      margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
      lineheight = 1.15
    ))


# color district to match upcoming choropleth map
map_1_1_a <- 
  ggplot()+
  
  geom_sf(data = in_dis,
          color = "#eeeded",  
          size = .01,
          fill = bg_c)+
  
  geom_sf(data = ind,
          color = "black",
          fill = NA)+
  
  geom_sf(data = in_dis |> 
            filter(corp_name == "Western Boone County Community School District"),
          color = "black", size = .01,
          fill = lgst, alpha = 1)+
  
  geom_label(data = sxfer |> 
               filter(stlmt_corp_name == "Western Boone County Community School District"),
             aes(stlmt_lon, stlmt_lat, 
                 label = "Western Boone County\nCommunity School District",
                 lineheight = 1, hjust = .1), 
             nudge_y = .45, size = 1.7, fill = bg_c, label.size = .05)+
  
  t_theme()+
  
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )

# combine plots

plot_grid(lst_4a, map_1_1_a, align = "v", axis = "tb",
          rel_widths = c(1,1), scale = c(1, 1.15))+
  t_theme()


ggsave("docs/images/lst_6.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")


# clean up
rm(map_1_1, map_1_1_a, map_1_2, map_1_3, map_1_4, lst, lst_1, lst_2, lst_3, lst_4, lst_4a, df, a, b, bcu, bt, gcu, gt, i, out_st, sPath, student, students, z, zz)

# net transfer text figures ####

# corps with positive net rate

# filter to only corporations with positive rate 
pr_c <- cp |> 
  
  filter(xfr_rate >0) |> 
  
  select(stlmt_corp_id) |> 
  
  select(stlmt_corp_id)

# average transfers out for corporations with positive transfer rate
sxfer |> 
  
  filter(stlmt_corp_id %in% pr_c$stlmt_corp_id &
           yr == 2024 
  ) |> 
  
  group_by(stlmt_corp_id) |> 
  
  count(stlmt_corp_id) |> 
  
  ungroup() |> 
  
  summarise(mean(n, na.rm = T))  # 24


# average transfers in by corporations with a positive transfer rate 
sxfer |> 
  
  filter(nrl_schl_id %in% pr_c$stlmt_corp_id &
           yr == 2024
  ) |> 
  
  group_by(nrl_schl_name) |> 
  
  count()|> 
  
  arrange(desc(n)) |> 
  
  ungroup() |> 
  
  summarise(mean(n, na.rm = T)) #22



# corporations with a negative transfer rate 
nr <- cp |> 
  filter(xfr_rate <0) |> 
  
  select(stlmt_corp_id) |> 
  
  select(stlmt_corp_id)

# average transfer rate out for corporations with a negative transfer rate
sxfer |> 
  
  filter(stlmt_corp_id %in% nr$stlmt_corp_id &
           yr == 2024 
  ) |> 
  
  group_by(stlmt_corp_id) |> 
  
  count(stlmt_corp_id) |> 
  
  ungroup() |> 
  
  summarise(mean(n, na.rm = T))  # 40

#average transfer rate in for corporations with a negative transfer rate
sxfer |> 
  
  filter(nrl_schl_id %in% nr$stlmt_corp_id &
           yr == 2024 
  ) |> 
  
  group_by(nrl_schl_name) |> 
  
  count()|> 
  
  arrange(desc(n)) |> 
  
  ungroup() |> 
  
  summarise(mean(n, na.rm = T)) #14


# count of corporations with positve net transfers
cp |> 
  
  filter(net_xfr > 0) |>
  
  nrow()  # 103
# 
# count of corporations with negative net transfers
cp |>
  
  filter(net_xfr < 0) |>
  
  nrow() # 187
# 
# # percent of school corps with negative net transfers
(cp |>
    
    filter(net_xfr < 0) |>
    
    nrow()/nrow(cp))*100 #64%
# 
# # corporations with positve net transfer rates
cp |> 
  
  filter(xfr_rate > 0) |>
  
  nrow() # 98
# 
# # corporations with negative net transfer rates
cp |> 
  
  filter(xfr_rate < 0) |>
  
  nrow() # 182


# percent of school corporations with negative transfer rate
(cp |>
    
    filter(xfr_rate < 0) |>
    
    nrow()/nrow(cp))*100 #63%

# # total outgoing transfer students
cp |>
  
  summarise(sum(pub_outgoing))  # 135930

# # total incoming transfer students
cp |>
  
  summarise(sum(pub_incoming, na.rm=T)) # 88514
# 
# # percent of transfer students that enrolled in another public school corporation
cp |>
  
  summarise(sum(pub_incoming, na.rm = T))/cp |>
  
  summarise(sum(pub_outgoing, na.rm = T))   # 65%
# 

# how many schools/corporations is a given school corp sending transfers to
xfr_schls_to <- sxfer |>
  
  filter(yr == 2024)|>
  
  group_by(stlmt_corp_name, stlmt_corp_id) |> 
  
  count(name = 'xfr_to')

# how many corporations is a given corporation receiving transfers fr
xfr_schls_from <- sxfer |>
  
  filter(yr == 2024 ) |>
  
  group_by(nrl_schl_name, nrl_schl_id) |>
  
  count(name = 'xfr_from') 

# join dataframes with counts of corporations transfering to and from
xfrs <- full_join(xfr_schls_from, xfr_schls_to,
                  join_by('nrl_schl_name' == 'stlmt_corp_name')) |>
  
  ungroup() |>
  
  select(-c(stlmt_corp_id))


# join to cp dataframe
cp2 <- left_join(cp, xfrs, by = c('stlmt_corp_name' = 'nrl_schl_name')) 

# second scrolly section ####

# determine what school corporations are home to private or charter schools in addition to the local school corporation and if so, how many.

# what is the coordinate reference system for in_dis
st_crs(in_dis) # EPSG 4269 

# get transfer data for most recent year, including lat and lon for enrolled schools
nrl_schls <- 
  sxfer |> 
  
  filter(yr == max(sxfer$yr)) |> 
  
  select(stlmt_corp_name, stlmt_corp_id, nrl_schl_name, nrl_schl_id, nrl_lat, nrl_lon)

# assign same coordinate reference system as in_dis
nrl_schls_2 <- 
  st_as_sf(nrl_schls, coords=c("nrl_lon","nrl_lat"), crs=4269)

# spatially join the two dataframes
nrl_schls_3 <- st_join(
  nrl_schls_2,
  in_dis,
  left = F)

# for plotting purposes (and scrolly steps), break down into categories by size

# corporations with 10 or more additional schools within their borders
top_nrl_corp <- nrl_schls_3 |> 
  
  group_by(stlmt_corp_name) |> 
  
  filter(stlmt_corp_name == corp_name) |> 
  
  summarise(count=n()) |> 
  
  filter(count >=10) |> 
  
  select(stlmt_corp_name) # there are 6 total; 5% of schools with more than 1 school alternative

# average transfer rate for those six schools
mean(cp2$xfr_rate[cp2$stlmt_corp_name %in% top_nrl_corp$stlmt_corp_name]) # -26
median(cp2$xfr_rate[cp2$stlmt_corp_name %in% top_nrl_corp$stlmt_corp_name]) # -20


# corporations with 2-9 or more additional schools within their borders
mdl_nrl_corp <- nrl_schls_3 |> 
  
  group_by(stlmt_corp_name) |> 
  
  filter(stlmt_corp_name == corp_name) |> 
  
  summarise(count=n()) |> 
  
  filter(count %in% 2:9) |> 
  
  select(stlmt_corp_name) # 63 total; 49%

# average and median transfer rate for those schools
mean(cp2$xfr_rate[cp2$stlmt_corp_name %in% mdl_nrl_corp$stlmt_corp_name]) # -12
median(cp2$xfr_rate[cp2$stlmt_corp_name %in% mdl_nrl_corp$stlmt_corp_name]) # -11

# corporations with 1 or more additional schools within their borders
nrl_schls_3 |> 
  
  group_by(stlmt_corp_name) |> 
  
  filter(stlmt_corp_name == corp_name) |> 
  
  summarise(count=n()) |> 
  
  filter(count >= 1 ) |> 
  
  select(stlmt_corp_name) |> 
  
  nrow() # 128  44% of all corps

# corporations with a single school alternative
sngl_nrl_corp <- nrl_schls_3 |> 
  
  group_by(stlmt_corp_name) |> 
  
  filter(stlmt_corp_name == corp_name) |> 
  
  summarise(count=n()) |> 
  
  filter(count == 1 ) |> 
  
  select(stlmt_corp_name) 

sngl_nrl_corp |> 
  
  nrow() #59; 46% 

# average and median transfer rate for those schools
mean(cp2$xfr_rate[cp2$stlmt_corp_name %in% sngl_nrl_corp$stlmt_corp_name]) # 1
median(cp2$xfr_rate[cp2$stlmt_corp_name %in% sngl_nrl_corp$stlmt_corp_name]) # -7


# how many private/charter schools within Indianapolis Public Schools?
nrl_schls_3 |> 
  
  filter(stlmt_corp_name == "Indianapolis Public Schools" &
           stlmt_corp_name == corp_name) |> 
  
  summarise(count=n()) #83


# sequential plots for scrolly 2 ####

# vector of corporations with multiple private or charter schools
mult <- nrl_schls_3 |> 
  
  filter(corp_name == stlmt_corp_name) |> 
  
  distinct(stlmt_corp_id)



# just over top 1/4 (27% have four to 9)
mdl_nrl_corp <- nrl_schls_3 |> 
  
  group_by(stlmt_corp_name) |> 
  
  filter(stlmt_corp_name == corp_name) |> 
  
  summarise(count=n()) |> 
  
  filter(count %in% 4:9) |> 
  
  select(stlmt_corp_name)

# both top and middle
top_mdl_nrl_corp <- nrl_schls_3 |> 
  
  group_by(stlmt_corp_name) |> 
  
  filter(stlmt_corp_name == corp_name) |> 
  
  summarise(count=n()) |> 
  
  filter(count %in% 4:83) |> # 83 is max number of schools - Indianapolis public schools
  
  select(stlmt_corp_name)



# percent of 2024 school corps with more than one private or charter school
(nrl_schls_3 |> 
    
    group_by(stlmt_corp_name) |> 
    
    filter(stlmt_corp_name == corp_name) |> 
    
    summarise(count=n()) |> 
    
    arrange(desc(count)) |> 
    
    nrow())/(sxfer |> 
               filter(yr==2024) |> 
               
               distinct(stlmt_corp_id) |>
               
               nrow()) #44%


# 43% of Indiana's school corporations have at least one charter or private school within their boundaries students can transfer to. 

plot <- ggplot()+
  
  geom_sf(data = in_dis,
          color = '#f0f0f0', fill = "white")+
  
  geom_sf(data = in_dis |> filter(corp_id %in% mult$stlmt_corp_id),
          color = lgst_txt, fill = lgst)+
  
  geom_sf(data = ind,
          color = "#525252", fill = NA, size = .04)+
  
  scale_size(range = c(0, .75))+ 
  theme_void()


# color legend
lg_key <- ggplot()+
  geom_rect(aes(
            xmin = 0, xmax = 1.5,
            ymin = 0, ymax = 1.5
            ),
            fill = lgst_txt,
            linewidth = .25
            )+
  
theme_void()


ggdraw(plot)+
  
  draw_plot(lg_key,
            x = .613,
            y = .2,
            width  =  .04,
            height =  .035
  ) +
  
  draw_plot_label(
    label =
      str_wrap("One or more alternative educational facilities within borders.", width =  35),
    x = .611, 
    y = .215, 
    size = 5.5, hjust = 0, lineheight = .9, color = "#525252")


# combo plot
ggsave("docs/images/scrolly2-mult_schls-1.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")

# scroll step 2

# how many corps with multiple options have just 1 charter/private
round((nrl_schls_3 |> 
         
         group_by(stlmt_corp_name) |> 
         
         filter(stlmt_corp_name == corp_name) |> 
         
         summarise(count=n()) |> 
         
         filter(count == 1) |> 
         
         nrow())/sxfer |> 
        
        filter(yr==2024 & 
                 stlmt_corp_id %in% mult$stlmt_corp_id) |> 
        
        distinct(stlmt_corp_id) |> 
        
        nrow(),2) #46%

# create vector of corp names
mult_sngl <- nrl_schls_3 |> 
  
  group_by(stlmt_corp_name) |> 
  
  filter(stlmt_corp_name == corp_name) |> 
  
  summarise(count=n()) |> 
  
  filter(count == 1) |> 
  
  select(stlmt_corp_name)

# average transfer rate for these schools
mean(cp2$xfr_rate[cp2$stlmt_corp_name %in% mult_sngl$stlmt_corp_name]) # 1.1
mean(cp2$xfr_rate[cp2$stlmt_corp_name %in% top_nrl_corp$stlmt_corp_name]) # -26

# 40% of those corporations have only a single alternative to the public school. They're average transfer rate is 2.

plot <- ggplot()+
  
  geom_sf(data = in_dis |> filter(corp_name %in% mult_sngl$stlmt_corp_name),
          color = 'darkgray', fill = lgst)+
  
  
  geom_sf(data = nrl_schls_3 |>
            filter(stlmt_corp_name == corp_name &
                     corp_name %in% mult_sngl$stlmt_corp_name),
          aes(),
          size = .01, color = '#969696')+
  
  geom_sf(data = in_dis |> filter(corp_id %in% mult$stlmt_corp_id),
          color = lgst_txt, fill = NA, linewidth = .1)+
  
  geom_sf(data = ind,
          color = "black", fill = NA, size = .04)+
  scale_size(range = c(0, .75))+ 
  
  theme_void()

# add legend to explain points as school locations


ggdraw(plot)+
  
  draw_plot(lg_key,
            x = .61,
            y = .2,
            width  =  .04,
            height =  .035
  ) +
  
  draw_plot_label(
    label =
      str_wrap("Only one alternative educational facility within borders.", width =  35),
    x = .611, 
    y = .215, 
    size = 5.5, hjust = 0, lineheight = .9, color = "#525252")+
  
  draw_plot_label(label =
                    str_wrap("Each \u25CF represents a\nprivate or charter school.", 30),
                  .61, .105, size = 5.5, hjust = 0, lineheight = .9, color = "#525252")



# combo plot
ggsave("docs/images/scrolly2-mult_schls-2.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")

# average legal settlement
cp |> 
  filter(stlmt_corp_name %in% mult_sngl$stlmt_corp_name) |> 
  summarise(avg = mean(leg_stl_tot, rm.na=T)) # 3,384


# scroll step 3

round((nrl_schls_3 |> 
         
         group_by(stlmt_corp_name) |> 
         
         filter(stlmt_corp_name == corp_name) |> 
         
         summarise(count=n()) |> 
         
         filter(count %in% 2:9) |> 
         
         nrow()/nrl_schls_3 |> 
         
         group_by(stlmt_corp_name) |> 
         
         filter(stlmt_corp_name == corp_name) |> 
         
         summarise(count=n()) |> 
         
         nrow())*100) #49%

# create vector of corp names
mult_med <- nrl_schls_3 |> 
  
  group_by(stlmt_corp_name) |> 
  
  filter(stlmt_corp_name == corp_name) |> 
  
  summarise(count=n()) |> 
  
  filter(count %in% 2:9) |>  
  
  select(stlmt_corp_name)

# average transfer rate for these schools
mean(cp2$xfr_rate[cp2$stlmt_corp_name %in% mult_med$stlmt_corp_name]) #-12

plot <- ggplot()+
  
  geom_sf(data = in_dis |> filter(corp_name %in% mult_med$stlmt_corp_name),
          color = 'darkgray', fill = lgst)+
  
  
  geom_sf(data = nrl_schls_3 |>
            filter(stlmt_corp_name == corp_name &
                     corp_name %in% mult_med$stlmt_corp_name),
          aes(),
          size = .01, color = '#969696')+
  
  
  geom_sf(data = in_dis |> filter(corp_id %in% mult$stlmt_corp_id),
          color = lgst_txt, fill = NA, linewidth = .1)+
  
  geom_sf(data = ind,
          color = "black", fill = NA, size = .04)+
  
  scale_size(range = c(0, .75))+ 
  
  theme_void()



ggdraw(plot)+
  
  # add legend to explain points as school locations
  draw_plot(lg_key,
            x = .61,
            y = .2,
            width  =  .04,
            height =  .035
  ) +
  
  # label text
  draw_plot_label(
    label =
      str_wrap("Between 2 to 9 alternative educational facilities within borders.", width =  30),
    x = .61, 
    y = .225, 
    size = 5.5, hjust = 0, lineheight = .9, color = "#525252")+
  
  draw_plot_label(label =
                    str_wrap("Each \u25CF represents a\nprivate or charter school.", 30),
                  .61, .105, size = 5.5, hjust = 0, lineheight = .9, color = "#525252")

ggsave("docs/images/scrolly2-mult_schls-3.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")


# avg legal settlement
cp |> 
  filter(stlmt_corp_name %in% mult_med$stlmt_corp_name) |> 
  summarise(avg = mean(leg_stl_tot, rm.na=T)) # 7,548


# scroll step 4 

# create enlarged versions of school corporations with many additional educational options to more easily view where they are on locator map

# axis and annotation text color
atxt <- "#525252"

m3 <- ggplot()+
  
  geom_sf(data = in_dis |> filter(corp_name %in% top_nrl_corp$stlmt_corp_name),
          color = 'darkgray', fill = lgst)+
  
  geom_sf(data = in_dis |> filter(corp_id %in% mult$stlmt_corp_id),
          color = lgst_txt, fill = NA, linewidth = .1)+
  
  geom_sf(data = ind,
          color = "black", fill = NA, size = .04)+
  
  # south bend segment
  geom_segment(aes(x = -86.1, xend = -85.7,
                   y = 41.65, yend = 41.9),
               color = atxt,
               linewidth = .3, 
               arrow = arrow(length=unit(0.15,"cm") )
  )+
  
  # Fort Wayne segment
  geom_segment(aes(x = -84.95, xend = -84.5,
                   y = 41.1, yend = 40.95),
               color = atxt,
               linewidth = .3, 
               arrow = arrow(length=unit(0.20,"cm") )
  )+
  
  # Evansville Vanderburgh School Corporation
  geom_segment(aes(x = -87.65, xend = -88.05,
                   y = 38.225, yend = 38.5),
               color = atxt,
               linewidth = .3, 
               arrow = arrow(length=unit(0.20,"cm"))
  )+
  
  # metro indy segment
  geom_segment(aes(x = -86.38, xend = -86.8467,
                   y = 39.77719, yend = 40.1),
               color = atxt,
               linewidth = .4, 
               arrow = arrow(length=unit(0.20,"cm") )
  )+
  
  coord_sf(
    xlim = c(-88.9,-84),
    ylim = c(37.6,42.05 ),
    expand = FALSE)+
  
  theme_void()



# South Bend
schl <- "South Bend Community School Corporation"

sbsc <- nrl_schls_3 |> 
  
  filter(stlmt_corp_name == schl & corp_name == schl) |> 
  
  distinct(nrl_schl_id)

m3_sbsc <- 
  ggplot()+
  
  geom_sf(data = in_dis |> filter(corp_name == schl), fill = lgst)+
  
  geom_sf(data = 
            nrl_schls_3 |>
            filter(stlmt_corp_name == schl & 
                     corp_name == schl),
          aes(),
          size = .01, color = '#969696')+
  scale_size(range =c(0,.75))+
  theme_void()


# fort wayne
schl <- "Fort Wayne Community Schools"

fwcs <- nrl_schls_3 |> 
  
  filter(stlmt_corp_name == schl & corp_name == schl) |> 
  
  distinct(nrl_schl_id)

m3_fwsc <- 
  ggplot()+
  
  geom_sf(data = in_dis |> filter(corp_name == schl), fill = lgst)+
  
  geom_sf(data = 
            nrl_schls_3 |>
            filter(stlmt_corp_name == schl & 
                     corp_name == schl),
          aes(),
          size = .01, color = '#969696')+
  scale_size(range =c(0,.75))+
  theme_void()

# Evansville Vanderburgh School Corporation
schl <-"Evansville Vanderburgh School Corporation"

evsc <- nrl_schls_3 |> 
  
  filter(stlmt_corp_name == schl & corp_name == schl) |>
  
  distinct(nrl_schl_id)

m3_evsc <- 
  ggplot()+
  
  geom_sf(data = in_dis |> filter(corp_name == schl), fill = lgst)+
  
  geom_sf(data = 
            nrl_schls_3 |>
            filter(stlmt_corp_name == schl & 
                     corp_name == schl),
          aes(),
          size = .01, color = '#969696')+
  scale_size(range =c(0,.75))+
  theme_void()

# indy + metro indy
schl <- c("Indianapolis Public Schools",
          "Metropolitan School District Lawrence Township",
          "Metropolitan School District Washington Township"
)

ips_metro <- nrl_schls_3 |> 
  
  filter(stlmt_corp_name %in% schl & corp_name %in% schl) |> 
  
  distinct(nrl_schl_id)

m3_ips_metro <- 
  ggplot()+
  
  geom_sf(data = in_dis |> filter(corp_name %in% schl), fill = lgst)+
  
  geom_sf(data = 
            nrl_schls_3 |>
            filter(stlmt_corp_name %in% schl & 
                     corp_name %in% schl),
          aes(),
          size = .01, color = '#969696')+
  scale_size(range =c(0,.75))+
  theme_void()


ggdraw(m3) +
  # south bend
  draw_plot(m3_sbsc,
            x = .52,
            y = .8565,
            width  =  .15,
            height =  .15
  )+
  
  draw_plot_label(label = "South Bend\nSchool Corporation", .42, 1.015, size = 5, 
                  hjust = 0, lineheight = .8, color = "#525252")+
  # fort wayne
  draw_plot(m3_fwsc,
            x = .6,
            y = .54,
            width  =  .2,
            height =  .2
  )+
  
  draw_plot_label(label = "Fort Wayne\nCommunity Schools", .68, .82, size = 5, 
                  hjust = 0, lineheight = 1, color = "#525252")+
  
  # evansville
  draw_plot(m3_evsc,
            x = .24,
            y = .17,
            width  =  .2,
            height =  .2
  )+
  
  draw_plot_label(label = "Evansville\nVanderburgh\nSchool\nCorporation", .274, .2, 
                  size = 5, hjust = 0, lineheight = 1, color = "#525252")+
  
  # metro indy
  draw_plot(m3_ips_metro,
            x = .285,
            y = .44,
            width  =  .3,
            height =  .3
  )+
  
  draw_plot_label(label =
                    "Indianapolis Public Schools,\nMSD Lawrence Township,\nMSD Washington Township",
                  # .21, .73, 
                  .2, .76,
                  size = 5, hjust = 0, lineheight = 1, color = "#525252")+
  
  # plot key
  draw_plot(lg_key,
            x = .61,
            y = .2,
            width  =  .04,
            height =  .035
  ) +
  
  # label text
  draw_plot_label(
    label =
      str_wrap("Ten or more alternative educational facilities within borders.", width =  30),
    x = .61, 
    y = .225, 
    size = 5.5, hjust = 0, lineheight = .9, color = "#525252")+
  
  draw_plot_label(label =
                    str_wrap("Each \u25CF represents a\nprivate or charter school.", 30),
                  .61, .105, size = 5.5, hjust = 0, lineheight = .9, color = "#525252")



# combo plot
ggsave("docs/images/scrolly2-mult_schls-4.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")


# avg legal settlement
cp |> 
  filter(stlmt_corp_name %in% c("Fort Wayne Community Schools",
                                "South Bend Community School Corporation", 
                                "Evansville Vanderburgh School Corporation",
                                "Indianapolis Public Schools",
                                "Metropolitan School District Lawrence Township",
                                "Metropolitan School District Washington Township")) |> 
  summarise(avg = median(leg_stl_tot, rm.na=T)) # 24,349



# scrolly step 6

schl <- "Indianapolis Public Schools"

sbsc <- nrl_schls_3 |> 
  
  filter(stlmt_corp_name == schl & corp_name == schl) |> 
  
  distinct(nrl_schl_id)


# state w/ IPS boundaries
mult_schls_2a <- 
  ggplot()+
  
  # corps with multiple education options
  geom_sf(data = in_dis |> filter(corp_id %in% mult$stlmt_corp_id),
          color = lgst_txt, fill = NA, linewidth = .06)+
  
  # schl 
  geom_sf(data = in_dis |> filter(corp_name == schl),
          color = NA, fill = lgst)+
  
  # state border
  geom_sf(data = ind,
          color = "#525252", fill = NA, size = .1)+
  
  # segment 
  geom_segment(aes(x = -86.38, xend = -86.8467,
                   y = 39.77719, yend = 40.1),
               color = atxt,
               linewidth = .4, 
               arrow = arrow(length=unit(0.20,"cm") )
  )+
  
  geom_text(data = sxfer |> 
              filter(stlmt_corp_name == "Indianapolis Public Schools"),
            aes(stlmt_lon, stlmt_lat,
                label = "Indianapolis\nPublic\nSchools",
                lineheight = 1, hjust = 0),
            nudge_y = .25, 
            nudge_x = -2.35, 
            size = 1.7, 
            fill = bg_c, 
            label.size = .05, color = "#525252")+
  
  scale_size(range =c(0,.75))+
  theme_void()


mult_schls_2b <- 
  ggplot()+
  
  geom_sf(data = in_dis |> filter(corp_name == schl), fill = lgst)+
  
  geom_sf(data = 
            nrl_schls_3 |>
            filter(stlmt_corp_name == schl & 
                     corp_name == schl),
          aes(),
          size = .01, color = '#969696' )+
  
  scale_size(range =c(0,.75))+
  theme_void()


ggdraw(mult_schls_2a) +
  draw_plot(mult_schls_2b,
            x = .32,
            y = .54,
            width  =  .25,
            height =  .25
  )+
  
  draw_plot_label(label =
                    "Each \u25CF represents a\nprivate or charter school.",
                  .61, .105, size = 5.5, hjust = 0, lineheight = 1, color = "#525252")

# combo plot
ggsave("docs/images/scrolly2-mult_schls-5.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")

# determine what percent of transfers from IPS enroll in schools within IPS boundaries

# dataframe of transfers out of Indianapolis Public Schools
ips <- 
  sxfer |> 
  filter(stlmt_corp_name == "Indianapolis Public Schools") |> 
  select(nrl_schl_name, nrl_schl_id, nrl_lon, nrl_lat) 

# convert to sf dataframe in order to use with st_within
ips <- st_as_sf(x = ips,
                coords = c('nrl_lon', 'nrl_lat')
)

# assign same CRS as in_dis
st_crs(ips) <- st_crs(in_dis)

# reduce in_dis to boundary for IPS
ipsb <-   in_dis |> 
  filter(corp_name == "Indianapolis Public Schools")

# check if enrolled schools are within IPS boundaries
st_within(ips, ipsb, sparse = F) 

# Calculate percent of transferred schools from IPS that remain within IPS boundaries
tibble(within = st_within(ips, ipsb, sparse = F)) |> 
  filter(within[,1] == TRUE) |> 
  nrow() /
  tibble(within = st_within(ips, ipsb, sparse = F)) |> 
  nrow()  # 39%

#

# clean up
rm(m3, m3_evsc, m3_fwsc, m3_ips_metro, m3_sbsc, plot, evsc, fwcs, ips_metro, sbsc, ips, ipsb)

# scrolly 3 - incoming transfers ####


# create function to make map showing school corp/charter or private school using already assigned colors; show the school corporations from which they came as white circles in order to match the corresponding leaflet map on the web page.

corp <- sxfer |> distinct(stlmt_corp_name)

inc_plot <- function(schl_name){
  
  
  if (schl_name %in% corp$stlmt_corp_name){
    
    plot <-   
      ggplot()+
      
      geom_sf(data = in_dis,
              color = "#eeeded",  
              size = .01,
              fill = bg_c)+
      
      geom_sf(data = ind,
              color = "black",
              fill = NA)+
      
      geom_point(data = sxfer |> 
                   filter(nrl_schl_name == schl_name),
                 aes(x = stlmt_lon,
                     y = stlmt_lat, size =  tot_xfr), 
                 shape = 21,
                 color = "black", 
                 fill = "white",
                 alpha = .75, 
                 show.legend = T)+
      
      # originating corp or school location
      geom_point(data = sxfer |> 
                   filter(stlmt_corp_name == schl_name),
                 aes(x = stlmt_lon,
                     y = stlmt_lat),
                 color = pub,
                 size = 2,
                 alpha = .75, 
                 show.legend = F)+
      
      geom_segment(data = sxfer |> 
                     filter(nrl_schl_name == schl_name) |> 
                     head(1),
                   aes(x = nrl_lon,
                       xend = -88.18,
                       y = nrl_lat, 
                       yend = 39.864068),
                   color = inc, 
                   linewidth = .2
      ) +
      
      # assign colors
      scale_color_manual(values = vals)+
      
      
      # guides
      guides(color = "none",
             guide_legend(byrow = T))+
      
      scale_size_continuous(name = "\u25EF Indicate\nnumber of\ntransfers\nfrom school\ncorporation"
      )+
      
      # theme adjustments
      theme_void()+
      theme(
        plot.margin = margin(.1,.1,.1,.1, "in"), # border around entire chart
        plot.background = element_rect(fill  = bg_c, color = NA),
        panel.background = element_rect(fill = bg_c, color = NA),
        legend.title = element_text(
          margin = margin(
            b = -1 + grid::convertUnit(unit(30, "pt"), "cm", valueOnly = TRUE),
            unit = "cm"),
          size = 7
        ),
        legend.text = element_text(size = 6), 
        legend.position = c(1.2, .59),
        plot.title = element_blank(),
      )
  } else {
    plot <- 
      ggplot()+
      
      geom_sf(data = in_dis,
              color = "#eeeded",  
              size = .01,
              fill = bg_c)+
      
      geom_sf(data = ind,
              color = "black",
              fill = NA)+
      
      geom_point(data = sxfer |> 
                   filter(nrl_schl_name == schl_name),
                 aes(x = stlmt_lon,
                     y = stlmt_lat, size = tot_xfr), 
                 shape = 21,
                 color = "black", 
                 fill = "white",
                 alpha = .75, 
      )+
      
      # originating corp or school location
      geom_point(data = sxfer |> 
                   filter(nrl_schl_name == schl_name),
                 aes(x = nrl_lon,
                     y = nrl_lat, color = fillcolor),
                 size = 2,
                 alpha = .75, 
                 show.legend = F)+
      
      geom_segment(data = sxfer |> 
                     filter(nrl_schl_name == schl_name) |> 
                     head(1),
                   aes(x = nrl_lon,
                       xend = -88.18,
                       y = nrl_lat, 
                       yend = 39.864068),
                   color = inc, 
                   linewidth = .2
      ) +
      
      # assign colors
      scale_color_manual(values = vals)+
      
      # guides
      guides(color = "none",
             guide_legend(byrow = T))+
      
      scale_size_continuous(name = "\u25EF Indicate\nnumer of\ntransfers\nfrom school\ncorporation",
      )+
      
      # theme adjustments
      theme_void()+
      theme(
        plot.margin = margin(.1,.1,.1,.1, "in"), # border around entire chart
        plot.background = element_rect(fill  = bg_c, color = NA),
        panel.background = element_rect(fill = bg_c, color = NA),
        legend.title = element_text(
          margin = margin(
            b = -1 + grid::convertUnit(unit(30, "pt"), "cm", valueOnly = TRUE),
            unit = "cm"),
          size = 7
        ),
        legend.text = element_text(size = 6), 
        plot.title = element_blank(),
      )
    
  }
  ggdraw(plot)+
    draw_plot_label(label =
                      str_wrap(schl_name, 10),
                    .35, .55, size = 5.5, hjust = 1, vjust = 1, lineheight = 1, color = "#525252")
}

## IMPORTANT TO NOTE: though the figures are the same, the naming of xfr_to and xfr_from is inconsistent with naming used in 20240724_page_charts.R for similar calculations (calculating the number of corporations from which a corporation sends or receives transfers). Be careful to ensure incoming vs outgoing transfers are understood in the context of "incoming" and "outgoing". "To" and "From" is too ambiguous and should be replaced as time permits; 

# what is the average number of corps/schools a corporation sends transfers to?
avg_to <- mean(xfr_schls_to$xfr_to)   # 34
med_to <- median(xfr_schls_to$xfr_to) # 26

# what is the average number of corps/schools a corporation receives transfers from?
avg_fr <- mean(xfr_schls_from$xfr_from)   # 13
med_fr <- median(xfr_schls_from$xfr_from) # 9



# how many school corporations had transfers from average number of school corporations
sxfer |> group_by(nrl_schl_name) |>
  
  count() |> 
  
  arrange(desc(n)) |> 
  
  ungroup() |> 
  
  filter(n>= avg_fr) |> 
  
  nrow() # 195; 67%

# average transfer from from count by type
sxfer |>
  
  filter(yr == 2024 ) |>
  
  group_by(nrl_schl_name, nrl_schl_id, typ) |>
  
  count(name = 'xfr_from') |> 
  
  group_by(typ) |> 
  
  summarise(avg_typ = mean(xfr_from))

# A tibble: 3 × 2
# typ                avg_typ
# <chr>                <dbl>
# 1 Charter              19.1 
# 2 Choice Scholarship    8.61
# 3 Public               17.0 


# median by type
sxfer |>
  
  filter(yr == 2024 ) |>
  
  group_by(nrl_schl_name, nrl_schl_id, typ) |>
  
  count(name = 'xfr_from') |> 
  
  group_by(typ) |> 
  
  summarise(avg_typ = median(xfr_from))

# A tibble: 3 × 2
# typ                `median(n, trim = 0.01)`
# <chr>                                 <dbl>
# 1 Charter                                   9
# 2 Choice Scholarship                        8
# 3 Public                                   11




# what school has similar legal settlement to Union School corp and median enrolled school corps
ls <- read_csv("clean_data/20240612_student_res_xfer_rate_2018_2024.csv")

view(ls) # Eminence Community School Corporation

sxfer |> 
  
  filter(yr == 2024 & stlmt_corp_name == "Eminence Community School Corporation") |> 
  
  summarise(sum(tot_xfr)) # 131 total outgoing transfers

sxfer |> 
  
  filter(yr == 2024 & nrl_schl_name == "Eminence Community School Corporation") |> 
  
  summarise(sum(tot_xfr)) # 42 incoming transfers


xfrs |> 
  filter(nrl_schl_name == "Eminence Community School Corporation")

# Eminence received transfers from 7 schools and sent them to 16 schools


# create plots using inc_plot function to contrast scenarios with different numbers of corporations sending transfers

# Maconaquah School Corporation received transfer students from the average number of corporations
inc_plot("Maconaquah School Corporation")

ggsave("docs/images/scrolly3_in_xfr_1.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")

# Eminence has similiar legal settlement as Union School Corp, the school/corp with the most number of schools/corps sending transfer students
inc_plot("Eminence Community School Corporation")

ggsave("docs/images/scrolly3_in_xfr_2.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")


inc_plot("Union School Corporation")

ggsave("docs/images/scrolly3_in_xfr_3.png", plot = last_plot(),
       width = 1600, height = 800, units = "px")


