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
library(tidyverse)

#import flynames, drop v column, convert to list, convert list to vector
flynames <- read.csv("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg/flynames_8.csv")
flynames <- transmute(flynames, flynames = flynames)
flynames <- as.list(flynames)
flynames <- unlist(flynames)
#import ascii FFTs
temp <- list.files(path = "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg/ZT18/Empty ASCII Plots",pattern = '.*\\.txt',
                   recursive = TRUE, ignore.case = TRUE, include.dirs = TRUE, full.names = TRUE)
temp
myfiles = lapply(temp, read.delim)
# Create metadata dataframe
pathlist <- data.frame(Flyname= c(temp))
#Delete three top rows 
myfiles <- lapply(myfiles, function(x) x[-1,])
  myfiles <- lapply(myfiles, function(x) x[-1,])
    myfiles <- lapply(myfiles, function(x) x[-1,])

print(myfiles[1])

# Rename comlumn headers "Hz" and "ms"
new_names <- c("Hz", "ms")
myfiles <- lapply(myfiles, setNames, nm= new_names)

#Change object class
cols = c(1, 2)
  myfiles <- lapply(myfiles, function(x) {
    x[,2] <- as.numeric(x[,2])
    x
  })
    myfiles <- lapply(myfiles, function(x) {
      x[,1] <- as.numeric(x[,1])
      x
    })

#Delete all rows where Hz is between 0-50
myfiles <-lapply(myfiles, function(x) x[rowSums(x)>50,])

#left join df's in myfiles and rotate df
df <- myfiles %>% reduce(left_join, by = "Hz")
df <- as.data.frame(t(df))
#remove top row
df <- df[-1,]
#rename columns from 50-3200 Hz
list <- c(50:3200)
names(df) <- c(list)
#rename rows
rownames(df) <- c(flynames)
#save file
write.csv(df, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg/flynames_8_plots.csv")

#and in the darkness bind them
setwd("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/25deg")

df1 <- read.csv("flynames_1_plots.csv")
df2 <- read.csv("flynames_2_plots.csv")
df3 <- read.csv("flynames_3_plots.csv")
df4 <- read.csv("flynames_4_plots.csv")
df5 <- read.csv("flynames_5_plots.csv")
df6 <- read.csv("flynames_6_plots.csv")
df7 <- read.csv("flynames_7_plots.csv")
df8 <- read.csv("flynames_8_plots.csv")

df_25 <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

setwd("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg")

df9  <- read.csv("flynames_1_plots.csv")
df10 <- read.csv("flynames_2_plots.csv")
df11 <- read.csv("flynames_3_plots.csv")
df12 <- read.csv("flynames_4_plots.csv")
df13 <- read.csv("flynames_5_plots.csv")
df14 <- read.csv("flynames_6_plots.csv")
df15 <- read.csv("flynames_7_plots.csv")
df16 <- read.csv("flynames_8_plots.csv")

df_30 <- bind_rows(df9, df10, df11, df12, df13, df14, df15, df16)

df <- bind_rows(df_25, df_30)
df_draft <- df

df_draft_2 <- df_draft[,-1]
rownames(df_draft_2) <- df_draft[,1]

write.csv(df_draft_2, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/FFT_plots_all_flies_50-3200Hz.csv")
