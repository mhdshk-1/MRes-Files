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
library(svglite)

#load master data frame
df <- read.csv("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/model_parameters_all_flies.csv")
  #convert temperature_condition, genetic_condition & zeitgeber_time as factors and recode levels
  df$temperature_condition <- factor(df$temperature_condition,
                                     levels = c(25, 30),
                                     labels = c("25_deg", "30_deg"))
  
  df$genetic_condition <- factor(df$genetic_condition,
                                 levels = c("naam", "empty"),
                                 labels = c("naam", "empty"))
  
  df$zeitgeber_time <- factor(df$zeitgeber_time,
                              levels = c(0, 6, 12, 18),
                              labels = c("ZT0", "ZT6", "ZT12", "ZT18"))
#check format
str(df)

#define fit equation function
fit_eq <- function(F0divm, f0, x, Q) {
  ((F0divm)/sqrt((((2*f0*pi)^2-(x*2*pi)^2)^2)+((2*f0*pi)^2*(2*pi*x)^2/Q^2)))*2*pi*x
}

#plots
  #random data subset for speed
  df <- df[sample(nrow(df), 10), ]
  #plot
  p <- ggplot(data.frame(x = c(51, 3200)), aes(x = x)) +
    scale_x_continuous(trans = "log10", breaks = c(100, 200, 300, 400, 500, 1000, 2000, 3000), limits = c(51, 3200)) +
    ylab("Velocity (m/s)") +
    xlab("Frequency (Hz)") +
    theme_minimal()
    #individual plots
    for (i in 1:length(df$X)) {
    p <- p + stat_function(fun = fit_eq, geom = "line", size = 0.1, n = 3200,
                      args = list(F0divm = df$F0divm[i], f0 = df$f0[i], Q = df$Q[i])) 
    }
    #mean line
    p <- p + stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                  args = list(F0divm = median(df$F0divm), f0 = median(df$f0), Q = median(df$Q)))
#print
p
#save as vector
#ggsave(file="all_fly_fits_(n=963)_black.svg", path = "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment", plot = p, width = 10, height = 10)
