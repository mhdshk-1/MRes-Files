#load libraries
library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(ggpubr)
library(car)
library(rstatix)
library(stringr)

# Import all txt files (NB- make sure there are no other txt files in the folder OR duplicates!)
temp <- list.files(path = "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/25deg/ZT0/Naam ASCII Plots",pattern = '.*\\.txt',
                   recursive = TRUE, ignore.case = TRUE, include.dirs = TRUE, full.names = TRUE)
temp
myfiles = lapply(temp, read.delim)
# Create metadata dataframe
pathlist <- data.frame(Flyname= c(temp))
#split path along forward slashes
pathlist_fragmented_list <- str_split(pathlist$Flyname, "/")
#retarded landscape dataframe output from above coerced into an actual fucking dataframe
pathlist_fragmented_df <- print(as.data.frame(do.call(cbind, pathlist_fragmented_list)))
pathlist_fragmented_df_reoriented <- as.data.frame(t(pathlist_fragmented_df))
#remove unnecessary cols
flynames <- transmute(pathlist_fragmented_df_reoriented, flynames = V13)
#remove file extension to create flyname df
flynames$flynames = substr(flynames$flynames,1,nchar(flynames$flynames)-4)
# save as CSV for use in Sigmaplot (import through Excel - then copy paste)
write.csv(flynames, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/25deg/flynames_1.csv")

#load files
#25deg condition
#set working directory
setwd("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg")

ZT0_naam_parameters_30deg <- read.csv("30deg_ZT0_naam_parameters.csv")
ZT0_empty_parameters_30deg <- read.csv("30deg_ZT0_empty_parameters.csv")
ZT6_naam_parameters_30deg <- read.csv("30deg_ZT6_naam_parameters.csv")
ZT6_empty_parameters_30deg <- read.csv("30deg_ZT6_empty_parameters.csv")
ZT12_naam_parameters_30deg <- read.csv("30deg_ZT12_naam_parameters.csv")
ZT12_empty_parameters_30deg <- read.csv("30deg_ZT12_empty_parameters.csv")
ZT18_naam_parameters_30deg <- read.csv("30deg_ZT18_naam_parameters.csv")
ZT18_empty_parameters_30deg <- read.csv("30deg_ZT18_empty_parameters.csv")

#data frame formatting, f0 = best frequency, Q = tuning sharpness, kBT = energy gain
df9  <- ZT0_naam_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'naam', zeitgeber_time = 0, f0 = f0, Q = Q, kBT = kBT)
df10 <- ZT0_empty_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'empty', zeitgeber_time = 0, f0 = f0, Q = Q, kBT = kBT)
df11 <- ZT6_naam_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'naam', zeitgeber_time = 6, f0 = f0, Q = Q, kBT = kBT)
df12 <- ZT6_empty_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'empty', zeitgeber_time = 6, f0 = f0, Q = Q, kBT = kBT)
df13 <- ZT12_naam_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'naam', zeitgeber_time = 12, f0 = f0, Q = Q, kBT = kBT)
df14 <- ZT12_empty_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'empty', zeitgeber_time = 12, f0 = f0, Q = Q, kBT = kBT)
df15 <- ZT18_naam_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'naam', zeitgeber_time = 18, f0 = f0, Q = Q, kBT = kBT)
df16 <- ZT18_empty_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'empty', zeitgeber_time = 18, f0 = f0, Q = Q, kBT = kBT)

#read flynames csv's
flynames_1 <- read.csv("flynames_1.csv")
flynames_2 <- read.csv("flynames_2.csv")
flynames_3 <- read.csv("flynames_3.csv")
flynames_4 <- read.csv("flynames_4.csv")
flynames_5 <- read.csv("flynames_5.csv")
flynames_6 <- read.csv("flynames_6.csv")
flynames_7 <- read.csv("flynames_7.csv")
flynames_8 <- read.csv("flynames_8.csv")

df <- bind_rows(flynames_1,
                flynames_2,
                flynames_3,
                flynames_4,
                flynames_5,
                flynames_6,
                flynames_7,
                flynames_8)
                
all_flynames_30deg <- transmute(df, flynames = flynames)

df <- bind_rows(df9, df10, df11, df12, df13, df14, df15, df16)

all_data_30deg <- bind_cols(all_flynames_30deg, df)

write.csv(all_data_30deg, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg/parameters_all_30deg.csv")

#guff

setwd("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/25deg")

#25deg files
ZT0_naam_parameters_25deg <- read.csv("25deg_ZT0_naam_parameters.csv")
ZT0_empty_parameters_25deg <- read.csv("25deg_ZT0_empty_parameters.csv")
ZT6_naam_parameters_25deg <- read.csv("25deg_ZT6_naam_parameters.csv")
ZT6_empty_parameters_25deg <- read.csv("25deg_ZT6_empty_parameters.csv")
ZT12_naam_parameters_25deg <- read.csv("25deg_ZT12_naam_parameters.csv")
ZT12_empty_parameters_25deg <- read.csv("25deg_ZT12_empty_parameters.csv")
ZT18_naam_parameters_25deg <- read.csv("25deg_ZT18_naam_parameters.csv")
ZT18_empty_parameters_25deg <- read.csv("25deg_ZT18_empty_parameters.csv")

#30 deg files
setwd("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg")

ZT0_naam_parameters_30deg <- read.csv("30deg_ZT0_naam_parameters.csv")
ZT0_empty_parameters_30deg <- read.csv("30deg_ZT0_empty_parameters.csv")
ZT6_naam_parameters_30deg <- read.csv("30deg_ZT6_naam_parameters.csv")
ZT6_empty_parameters_30deg <- read.csv("30deg_ZT6_empty_parameters.csv")
ZT12_naam_parameters_30deg <- read.csv("30deg_ZT12_naam_parameters.csv")
ZT12_empty_parameters_30deg <- read.csv("30deg_ZT12_empty_parameters.csv")
ZT18_naam_parameters_30deg <- read.csv("30deg_ZT18_naam_parameters.csv")
ZT18_empty_parameters_30deg <- read.csv("30deg_ZT18_empty_parameters.csv")

data_25deg <- bind_rows(ZT0_naam_parameters_25deg,
                        ZT0_empty_parameters_25deg,
                        ZT6_naam_parameters_25deg,
                        ZT6_empty_parameters_25deg,
                        ZT12_naam_parameters_25deg,
                        ZT12_empty_parameters_25deg,
                        ZT18_naam_parameters_25deg,
                        ZT18_empty_parameters_25deg)

data_30deg <- bind_rows(ZT0_naam_parameters_30deg,
                        ZT0_empty_parameters_30deg,
                        ZT6_naam_parameters_30deg,
                        ZT6_empty_parameters_30deg,
                        ZT12_naam_parameters_30deg,
                        ZT12_empty_parameters_30deg,
                        ZT18_naam_parameters_30deg,
                        ZT18_empty_parameters_30deg)

data_25deg <- data_25deg %>% transmute(F0divm = F0divm, tss = tss, rss = rss, r.squared = r.squared, mass = Masse, stiffness = Steifheit, Ks = Ks, dE = dE)
data_30deg <- data_30deg %>% transmute(F0divm = F0divm, tss = tss, rss = rss, r.squared = r.squared, mass = Masse, stiffness = Steifheit, Ks = Ks, dE = dE)

parameters_all_25deg <- read.csv("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/25deg/parameters_all_25deg.csv")
parameters_all_30deg <- read.csv("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg/parameters_all_30deg.csv")

dataset_25deg <- bind_cols(parameters_all_25deg, data_25deg)
dataset_30deg <- bind_cols(parameters_all_30deg, data_30deg)

full_dataset <- bind_rows(dataset_25deg, dataset_30deg)

full_dataset$X <- NULL

write.csv(full_dataset, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/model_parameters_all_flies.csv")
