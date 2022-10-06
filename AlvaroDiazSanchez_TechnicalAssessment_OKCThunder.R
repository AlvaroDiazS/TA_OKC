##### libraries and options #####
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggpubr)
library(jpeg)
options(warn=-1)
options(dplyr.summarise.inform = FALSE)
rm(list = ls())

#############################################
#############################################
#### Read the data and clasify each shot ####

setwd("~")
df <- read.csv("OKC assigment/shots_data.csv")

# Following court_diagram annotation
# Non Corner 3 -> y > 7.8, rad > 23.75
# Corner 3 -> y <= 7.8, abs(x) > 22
# 2PT rest of shoots.
df <- df %>% 
  mutate(rad = sqrt(x^2 + y^2),
         shot_type = case_when(y <= 7.8 & abs(x) > 22 ~ "C3",
                               y > 7.8 & rad > 23.75 ~ "NC3",
                               TRUE ~ "2PT"))

#############################################
#############################################
################# Shot chart ################ 

# Court image
img <- readJPEG("OKC assigment/court_diagram.jpg" )

# color made shot
df <- df %>% mutate(cMade = case_when (fgmade == 1 & shot_type == "C3" ~ "green",
                                       fgmade == 1 & shot_type == "NC3" ~ "blue",
                                       fgmade == 1 & shot_type == "2PT" ~ "red",
                                       TRUE ~ "white")
)

# shot_chart, filled if the shot is made
ggplot(df, aes(x,y)) +
  background_image(img) +
  geom_point(shape = 21, size = 4, stroke = 3.5,aes(color = shot_type), fill = df$cMade) + 
  xlim(-23.1,23.1) +
  ylim(-3,38) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


#############################################
#############################################
##### Shoot distribution per shot_type ######

print("Shoot distribution per shot_type")
shot_dist <- df %>% 
  group_by(team) %>% 
  mutate(shot_team = n()) %>% #total shot by each team
  group_by(team, shot_type) %>% 
  summarise(shot_dist = 100*n()/shot_team) %>% # % typy of shot
  ungroup() %>% 
  unique() %>% 
  spread(key = shot_type, value = shot_dist) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  as.data.frame
print(shot_dist)


#############################################
#############################################
############# eFG per shot_type #############
print("eFG per shot_type")
eFG <- df %>%  
  group_by(team, shot_type) %>% 
  mutate(FGA = n(), #total shot by team and type
         FGM = sum(fgmade) #total shot made by team and type
  ) %>% 
  # calculate eFG
  summarise(eFG =100*case_when(shot_type == "2PT" ~ FGM/FGA,
                               TRUE ~ 1.5*FGM/FGA)
  ) %>% 
  unique() %>% 
  ungroup() %>% 
  spread(key = shot_type, value = eFG) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  as.data.frame
print(eFG)


