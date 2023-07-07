#load libraries
library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(ggpubr)
library(car)
library(rstatix)
library(patchwork)
library(ggtext)
library(ARTool)
library(phia) 
library(emmeans)
library(reshape2)
library(ggsignif)
library(svglite)
library(ggetho)
library(damr)
library(sleepr)
library(behavr)
library(scopr)
library(zeitgebr)

#create metadata dataframes
metadata54 <- data.table( file = "Monitor54.txt",
                        start_datetime = "2022-06-24 10:05:00",
                        stop_datetime = "2022-06-30",
                        region_id = paste(1:32),
                        sex = "M",
                        genotype = c("Naam RNAi", "attP40 Control"))

metadata56 <- data.table( file = "Monitor56.txt",
                        start_datetime = "2022-06-24 10:05:00",
                        stop_datetime = "2022-06-30",
                        region_id = paste(1:32),
                        sex = "M",
                        genotype = c("Naam RNAi", "attP40 Control"))
#save
#write.csv(metadata, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/Shaker Experiment/LD DD Shaker Series/metadata56.csv")

#load DAM5 files
DATA_DIR <- "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/Shaker Experiment/LD DD Shaker Series"
#list.files(path = DATA_DIR, pattern = "txt|csv")
monitor54 <- read_dam_file(path = "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/Shaker Experiment/LD DD Shaker Series/Monitor54.txt",
              start_datetime = "2022-06-24 10:05:00",
              stop_datetime = "2022-06-30 22:04:00")
monitor56 <- read_dam_file(path = "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/Shaker Experiment/LD DD Shaker Series/Monitor56.txt",
                           start_datetime = "2022-06-24 10:05:00",
                           stop_datetime = "2022-06-30 22:04:00")

#link metadata to DAM5 monitor files
draft <- monitor54 %>% mutate(region_id = str_sub(id, start= -2))
draft <- draft %>% mutate(region_id = as.numeric(draft$region_id))
behavr(draft, metadata54)