library(readxl)
library(tidyverse)
library(png)
library(ggpubr)
library(ggimage)
library(ggtext)
library(aws.s3)


# create charts using icons of students to visualize how many students transfer in or out of a school corporation in 2023


# import data

sxfer <- read_csv("clean_data/sxfer.csv") |> 
  filter(yr == 2023)

cp <- read_xlsx("raw_data/fall-2022-2023-public-corporation-transfer-report-v2.xlsx",
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
    
    sxfer |> select(stlmt_corp_id, stlmt_corp_name) |> 
      # need to remove West Clark Community Schools - present in sxfer dataframe but was replaced by Borden Henryville in Jul 2020
      filter(stlmt_corp_id != "0940") |> 
      distinct(),
    
    cp, 
    
    by = 'stlmt_corp_id') |> 
  
  # remove extra stlmt_corp_name column  
  select(-stlmt_corp_name.y)

# update column names 
colnames(cp) <- nc

# add caption/label to reflect net transfer rate
cp <- cp |> 
  group_by(stlmt_corp_id) |> 
  mutate(
    lab2 =
      case_when(
        # if rate is more than 1 student
        xfr_rate >= 2 ~ paste0("**", stlmt_corp_name, "** gained <span style = 'color: ", inc, "' >", format(abs(xfr_rate), big.mark = ","), " **transfer students** </span>for every 100 living within its boundaries in 2023."),
        # if rate is 0
        xfr_rate == 0 ~ paste0("**", stlmt_corp_name,
                               if_else(
                                 str_sub(stlmt_corp_name, nchar(stlmt_corp_name)) == "s", "'", "'s"), "** net <span style = 'color: ", inc, "' >transfer rate </span>was zero for every 100 students within its boundaries in 2023."),
        # if rate is less than or equal to 2 students
        xfr_rate <= -2 ~ paste0("**", stlmt_corp_name, "** lost ", format(abs(xfr_rate), big.mark = ","), " <span style = 'color: ", inc, "' >**transfer students** </span>for every 100 living within its boundaries in 2023."),
        # if rate is - 1
        xfr_rate == -1 ~ paste0("**", stlmt_corp_name, "** lost ", format(abs(xfr_rate), big.mark = ","), " <span style = 'color: ", inc, "' >**transfer student** </span>for every 100 living within its boundaries in 2023."),
        # if rate is + one student
        xfr_rate == 1 ~ paste0("**", stlmt_corp_name, "** gained <span style = 'color: ", inc, "' >", format(abs(xfr_rate), big.mark = ","), " **transfer student** </span>for every 100 living within its boundaries in 2023.")
      )
  )

# clean up
rm(nc, sxfer)


#####

# set plot background color and custom theme
# background color 
bg_c <-  "#fbf9f6"
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
        size = rel(x = 1.1),
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

# set up png images ####
# path to folder
sPath <- "icons/"

# girl icon, full 
gt <- paste0(sPath, 'girl_plain.png')
# girl icon - transfer out
gcu <- paste0(sPath, 'girl_cu_2.png')
# boy icon full
bt <- paste0(sPath, 'boy_plain.png')
# boy icon transfer out
bcu <- paste0(sPath, 'boy_cu_2.png')

# vector of students
student <- c(gt, bt)

# vector of students transfer out
out_st <- c(gcu, bcu)


# set icon color to match legal settlement color used within story
lgst <-  '#d0ccbe' 
inc  <-  '#525252'
# loop to create plots

# vector of settlement corp id but exclude Union School Corporation - an extreme outlier that needs to be handled differently
sc <- unique(cp$stlmt_corp_id[cp$stlmt_corp_id != "6795"])

for (i in 1:length(sc)){
  
  if (cp$xfr_rate[cp$stlmt_corp_id == sc[i]] < 0){
    
    # build dataframe to make grid of 100 student icons
    
    df <- data.frame(matrix(nrow = 100,
                            ncol = 2))
    colnames(df) <- c('x', 'y')
    
    df$x <- rep(seq(1, 10, 1), 10)
    df$y <- rep(1:10, each = 10)
    
    # assign color to match legal settlement color from accompanying web page
    df$color <- lgst
    
    # randomly select male or female student icon
    df$image <- sample(student, size = 100, T)
    
    # assign net transfer rate to z
    z <- cp$xfr_rate[cp$stlmt_corp_id == sc[i]]
    
    # randomly assign z number of icon rows to switch to outline shape to represent outgoing transfers 
    zz <- sample(100, abs(z), F) 
    # if transfers should be grouped together use simple sequence instead
    # zz <- seq(1, z, by = 1)  
    
    # replace the full figures with outline figure to represent their absence
    for (a in (zz)){ # if transfers are to be grouped together, use 1:length(zz)
      df[a,4] <- sample(out_st, 1)
      df[a,3] <- inc
    }
    
    ggplot()+
      # students with legal settlement
      geom_image(data = df,
                 aes(x, y,
                     image = image,
                     color = color),
                 size = .044,
                 #color = NA
      ) +
      scale_color_manual(values = c(
        "#d0ccbe" = "#d0ccbe",
        '#d9d8d8' = '#d9d8d8',
        '#525252' = '#525252'
      )
      )+
      labs(
        title = cp$lab2[cp$stlmt_corp_id == sc[i]],
        subtitle = "<br>",
        caption = "<br>**Data**:   Indiana Department of Education<br>(c)   tedschurter.com"
      )+
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.margin = margin(.1,.1,.1,.1, "in"), # border around entire chart
        plot.background = element_rect(fill  = bg_c, color = NA),
        panel.background = element_rect(fill = bg_c, color = NA),
        plot.title = element_textbox(
          family = "serif", 
          size = rel(x = 1.1),
          width = unit(1, "npc"),
          padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
          margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
          lineheight = 1.1
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
    
    
    ggsave(paste0("plots/net2/net-", sc[i], ".png"), plot = last_plot(),
           width = 4, height = 4, units = "in")
    
    rm(df)
    
  } else if (cp$xfr_rate[cp$stlmt_corp_id == sc[i]] == 0 ) { 
    
    # net transfers per 100 students with legal settlement are 0 (doesn't mean no transfers, though) 
    df <- data.frame(matrix(nrow = 100,
                            ncol = 2))
    colnames(df) <- c('x', 'y')
    
    df$x <- rep(seq(1, 10, 1), 10)
    df$y <- rep(1:10, each = 10)
    
    # set color to match legal settlement color on accompanying web page
    df$color <- lgst
    
    # randomly select male or female student icon
    df$image <- sample(student, size = 100, T)
    
    
    ggplot()+
      # students with legal settlement
      geom_image(data = df,
                 aes(x, y,
                     image = image,
                     color = color),
                 size = .044,
      ) +
      
      scale_color_manual(values = c(
        "#d0ccbe" =  "#d0ccbe",
        '#d9d8d8' = '#d9d8d8',
        '#525252' = '#525252'
      )
      )+
      labs(
        title = cp$lab2[cp$stlmt_corp_id == sc[i]],
        subtitle = "<br>",
        caption = "<br>**Data**:   Indiana Department of Education<br>(c)   tedschurter.com"
      )+
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.margin = margin(.1,.1,.1,.1, "in"), # border around entire chart
        plot.background = element_rect(fill  = bg_c, color = NA),
        panel.background = element_rect(fill = bg_c, color = NA),
        plot.title = element_textbox(
          family = "serif", 
          size = rel(x = 1.1),
          width = unit(1, "npc"),
          padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
          margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
          lineheight = 1.1
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
    
    
    ggsave(paste0("plots/net2/net-", sc[i], ".png"), plot = last_plot(),
           width = 4, height = 4, units = "in")
    
    rm(df)
    
  } else {
    
    # to accommodate transfers added to base of 100, increase dataframe size to 300 and then remove extra (maximum outgoing transfers excluding outlier is 170) after z is determined 
    
    # build dataframe  
    df <- data.frame(matrix(nrow = 1,
                            ncol = 2))
    
    colnames(df) <- c('x', 'y')
    
    # create dataframe that repeats x axis sequence to 20 and y axis repetition of row number needed (15 rows of 20 each = 300)
    
    for(b in seq(1, 15,1)){
      
      df <- rbind(df,
                  data.frame(
                    x = seq(1,20),
                    y = rep(b,20))
      )
      
      # remove NA's
      df <- df |> filter(!is.na(x),
                         !is.na(y))
    }
    
    # assign color to match legal settlement color on accompanying web page
    df$color <- lgst
    
    # randomly assign male or female student icon
    df$image <- sample(student, size = 300, T)
    
    # assign net transfer rate o z
    z <- cp$xfr_rate[cp$stlmt_corp_id == sc[i]]
    
    # need to reduce 200 rows to 100 + 100-z
    df <- df[1:(200-(100-z)),]
    
    df[101:(100+z),3] <- inc
    
    # plot
    ggplot()+
      # students with legal settlement
      geom_image(data = df,
                 aes(x, y,
                     image = image,
                     color = color),
                 # reduce size of icon if number of incoming transfers is especially high
                 size = if_else(z > 100, .03, .044)
      ) +
      scale_color_manual(values = c(
        '#d0ccbe' = '#d0ccbe',
        '#525252' = '#525252'  
      )
      )+
      xlim(c(1, 20))+
      ylim(c(1, ceiling(((100+z)/20))) # total number of students (100 base + transfers) / length of row(20).
      )+  
      
      labs(
        title = cp$lab2[cp$stlmt_corp_id == sc[i]],
        subtitle = "<br>",
        caption = "<br>**Data**:   Indiana Department of Education<br>(c)   tedschurter.com"
      )+
      # unknown issue causing sporadic headline spacing issue with t_theme set theme manually
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.margin = margin(.1,.1,.1,.1, "in"), # border around entire chart
        plot.background = element_rect(fill  = bg_c, color = NA),
        panel.background = element_rect(fill = bg_c, color = NA),
        plot.title = element_textbox(
          family = "serif", 
          size = rel(x = 1.1),
          width = unit(1, "npc"),
          padding = margin(.05, .25, .25, .25),   #(4, 4, 1, 4),
          margin  = margin(.25, .25, 5, .25),   #(8, 0, 0, 0),
          lineheight = 1.1
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
    
    ggsave(paste0("plots/net2/net-", sc[i], ".png"), plot = last_plot(),
           width = 4, height = 4, units = "in")
    
    rm(df)
    
  }
}


# Union School Corporation's net transfer rate of 1,780 is too large to accommodate the use of student icons within the confines of a popup chart. Using geom_point instead.


name <- "Union School Corporation"
# build dataframe
df <- data.frame(matrix(nrow = 1,
                        ncol = 2))

colnames(df) <- c('x', 'y')

for(b in seq(1, 40,1)){
  
  df <- rbind(df,
              data.frame(
                x = seq(1,50),
                y = rep(b,50))
  )
  
  # remove NA's
  df <- df |> filter(!is.na(x),
                     !is.na(y))
}


# assign net transfer to z
z <- cp$xfr_rate[cp$stlmt_corp_name == name]


# assign color 
df$color <- lgst

# reduce number of rows to 100 + 100-z
df <- df[1:(200-(100-z)),]

# assign color blue to outgoing transfer rows
df[101:(100+z),3] <- inc

# plot

ggplot(df)+
  geom_point(
    aes(x, y, color = color),
    size = .95,
    show.legend = F
  )+
  scale_color_manual(values = c(
    '#d0ccbe' = '#d0ccbe',
    '#525252' = '#525252'  
  )
  )+
  labs(
    title = cp$lab2[cp$stlmt_corp_name == name],
    # subtitle = "<br>",
    caption = "<br>**Data**:   Indiana Department of Education<br>(c)   tedschurter.com"
  )+
  t_theme()+
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave(paste0("plots/net2/net-", cp$stlmt_corp_id[cp$stlmt_corp_name == name], ".png"), plot = last_plot(),
       width = 4, height = 4, units = "in")



# upload files to AWS S3 bucket ####


# assign bucket name
bucket <- "tedschurter-data"
region <- "us-east-2" # bucket assigned to us-east-2 region; also set by default in r environment


# check that it's there
bucket_exists(bucket) # returns TRUE


# loop through folder adding plots 

plots <- unique(list.files("plots/net"))

for(i in 1:length(plots)){
  
  # put object in bucket
  put_object(
    object = paste0("2024-in-ed-",plots[i]),
    bucket = bucket,
    file = paste0("plots/net/", plots[i]),
    region = region,
    headers = c('Content-Type' = 'image/png') # without headers argument, content-type metadata set to binary/octet-stream and file downloads rather than displays
  )
}



# # loop to delete all above objects if needed
# 
# for(i in 1:length(plots)){
#   delete_object(
#     object = paste0("2024-in-ed-",plots[i]),
#     bucket = bucket,
#     quiet = TRUE)
# }
  
