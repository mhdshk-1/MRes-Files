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
library(ggtext)
library(ARTool)
library(phia) 
library(emmeans)

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
#filter criteria
df_25 <- filter(df, temperature_condition == "25_deg")
naam <- filter(df, genetic_condition == "naam")
empty <- filter(df, genetic_condition == "empty")

ZT0 <- filter(df_25, zeitgeber_time == "ZT0")
ZT0_naam <- filter(ZT0, genetic_condition == "naam")
ZT0_empty <- filter(ZT0, genetic_condition == "empty")

ZT6 <- filter(df_25, zeitgeber_time == "ZT6")
ZT6_naam <- filter(ZT6, genetic_condition == "naam")
ZT6_empty <- filter(ZT6, genetic_condition == "empty")

ZT12 <- filter(df_25, zeitgeber_time == "ZT12")
ZT12_naam <- filter(ZT12, genetic_condition == "naam")
ZT12_empty <- filter(ZT12, genetic_condition == "empty")

ZT18 <- filter(df_25, zeitgeber_time == "ZT18")
ZT18_naam <- filter(ZT18, genetic_condition == "naam")
ZT18_empty <- filter(ZT18, genetic_condition == "empty")
#define fit equation function
fit_eq <- function(F0divm, f0, x, Q) {
  ((F0divm)/sqrt((((2*f0*pi)^2-(x*2*pi)^2)^2)+((2*f0*pi)^2*(2*pi*x)^2/Q^2)))*2*pi*x
}

#FFTs
#ZT0
    #ggplot canvas
    fft0 <- ggplot(data.frame(x = c(51, 3200)), aes(x = x)) +
      scale_x_continuous(trans = "log10", breaks = c(100, 500, 1000, 3000), limits = c(51, 3200)) +
      ylab("Velocity (m/s)") +
      xlab("Frequency (Hz)") +
      ggtitle("ZT0") +
      theme_minimal() 
      #naam
          #individual plots naam
          for (i in 1:length(ZT0_naam$X)) {
            fft0 <- fft0 + stat_function(fun = fit_eq, geom = "line", size = 0.05, n = 3200,
                                   args = list(F0divm = ZT0_naam$F0divm[i], f0 = ZT0_naam$f0[i], Q = ZT0_naam$Q[i]),
                                   aes(colour = "#00bfc4", alpha = 0.3))
          }
          #mean line naam
          fft0 <- fft0 + stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                                 args = list(F0divm = median(ZT0_naam$F0divm), f0 = median(ZT0_naam$f0), Q = median(ZT0_naam$Q)),
                                 aes(colour = "#00bfc4"))
       #empty
          #individual plots empty
          for (i in 1:length(ZT0_empty$X)) {
            fft0 <- fft0 + stat_function(fun = fit_eq, geom = "line", size = 0.05, n = 3200,
                                   args = list(F0divm = ZT0_empty$F0divm[i], f0 = ZT0_empty$f0[i], Q = ZT0_empty$Q[i]),
                                   aes(colour = "#f8766d", alpha = 0.3))
          }
          #mean line empty
          fft0 <- fft0 + stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                                 args = list(F0divm = median(ZT0_empty$F0divm), f0 = median(ZT0_empty$f0), Q = median(ZT0_empty$Q)),
                                 aes(colour = "#f8766d"))
       #plot specifications
          fft0 <- fft0 + theme(legend.position = "none") +
                   ylim(0, 5e-06)
#ZT6
          #ggplot canvas
          fft6 <- ggplot(data.frame(x = c(51, 3200)), aes(x = x)) +
            scale_x_continuous(trans = "log10", breaks = c(100, 500, 1000, 3000), limits = c(51, 3200)) +
            xlab("Frequency (Hz)") +
            ylab("") +
            ggtitle("ZT6") +
            theme_minimal()
          #naam
          #individual plots naam
          for (i in 1:length(ZT6_naam$X)) {
            fft6 <- fft6 + stat_function(fun = fit_eq, geom = "line", size = 0.05, n = 3200,
                                     args = list(F0divm = ZT6_naam$F0divm[i], f0 = ZT6_naam$f0[i], Q = ZT6_naam$Q[i]),
                                     aes(colour = "#00bfc4", alpha = 0.3))
          }
          #mean line naam
          fft6 <- fft6 + stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                                   args = list(F0divm = median(ZT6_naam$F0divm), f0 = median(ZT6_naam$f0), Q = median(ZT6_naam$Q)),
                                   aes(colour = "#00bfc4"))
          #empty
          #individual plots empty
          for (i in 1:length(ZT6_empty$X)) {
            fft6 <- fft6 + stat_function(fun = fit_eq, geom = "line", size = 0.05, n = 3200,
                                     args = list(F0divm = ZT6_empty$F0divm[i], f0 = ZT6_empty$f0[i], Q = ZT6_empty$Q[i]),
                                     aes(colour = "#f8766d", alpha = 0.3))
          }
          #mean line empty
          fft6 <- fft6 + stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                                   args = list(F0divm = median(ZT6_empty$F0divm), f0 = median(ZT6_empty$f0), Q = median(ZT6_empty$Q)),
                                   aes(colour = "#f8766d"))
          #plot specifications
          fft6 <- fft6 + theme(legend.position = "none") +
            ylim(0, 5e-06)     
#ZT12
          #ggplot canvas
          fft12 <- ggplot(data.frame(x = c(51, 3200)), aes(x = x)) +
            scale_x_continuous(trans = "log10", breaks = c(100, 500, 1000, 3000), limits = c(51, 3200)) +
            xlab("Frequency (Hz)") +
            ylab("") +
            ggtitle("ZT12") +
            theme_minimal()
          #naam
          #individual plots naam
          for (i in 1:length(ZT12_naam$X)) {
            fft12 <- fft12 + stat_function(fun = fit_eq, geom = "line", size = 0.05, n = 3200,
                                     args = list(F0divm = ZT12_naam$F0divm[i], f0 = ZT12_naam$f0[i], Q = ZT12_naam$Q[i]),
                                     aes(colour = "#00bfc4", alpha = 0.3))
          }
          #mean line naam
          fft12 <- fft12 + stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                                   args = list(F0divm = median(ZT12_naam$F0divm), f0 = median(ZT12_naam$f0), Q = median(ZT12_naam$Q)),
                                   aes(colour = "#00bfc4"))
          #empty
          #individual plots empty
          for (i in 1:length(ZT12_empty$X)) {
            fft12 <- fft12 + stat_function(fun = fit_eq, geom = "line", size = 0.05, n = 3200,
                                     args = list(F0divm = ZT12_empty$F0divm[i], f0 = ZT12_empty$f0[i], Q = ZT12_empty$Q[i]),
                                     aes(colour = "#f8766d", alpha = 0.3))
          }
          #mean line empty
          fft12 <- fft12 + stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                                   args = list(F0divm = median(ZT12_empty$F0divm), f0 = median(ZT12_empty$f0), Q = median(ZT12_empty$Q)),
                                   aes(colour = "#f8766d"))
          #plot specifications
          fft12 <- fft12 + theme(legend.position = "none") +
            ylim(0, 5e-06)
#ZT18
          #ggplot canvas
          fft18 <- ggplot(data.frame(x = c(51, 3200)), aes(x = x)) +
            scale_x_continuous(trans = "log10", breaks = c(100, 500, 1000, 3000), limits = c(51, 3200)) +
            xlab("Frequency (Hz)") +
            ylab("") +
            ggtitle("ZT18") +
            theme_minimal()
          #naam
          #individual plots naam
          for (i in 1:length(ZT18_naam$X)) {
            fft18 <- fft18 + stat_function(fun = fit_eq, geom = "line", size = 0.05, n = 3200,
                                     args = list(F0divm = ZT18_naam$F0divm[i], f0 = ZT18_naam$f0[i], Q = ZT18_naam$Q[i]),
                                     aes(colour = "#00bfc4", alpha = 0.3))
          }
          #mean line naam
          fft18 <- fft18 + stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                                   args = list(F0divm = median(ZT18_naam$F0divm), f0 = median(ZT18_naam$f0), Q = median(ZT18_naam$Q)),
                                   aes(colour = "#00bfc4"))
          #empty
          #individual plots empty
          for (i in 1:length(ZT18_empty$X)) {
            fft18 <- fft18 + stat_function(fun = fit_eq, geom = "line", size = 0.05, n = 3200,
                                     args = list(F0divm = ZT18_empty$F0divm[i], f0 = ZT18_empty$f0[i], Q = ZT18_empty$Q[i]),
                                     aes(colour = "#f8766d", alpha = 0.3))
          }
          #mean line empty
          fft18 <- fft18 + stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                                   args = list(F0divm = median(ZT18_empty$F0divm), f0 = median(ZT18_empty$f0), Q = median(ZT18_empty$Q)),
                                   aes(colour = "#f8766d"))
          #plot specifications
          fft18 <- fft18 + theme(legend.position = "none") +
            ylim(0, 5e-06)
#box plots
#f0
          bp_f0 <- ggplot(df_25, aes(x=zeitgeber_time, y=f0)) +
            geom_boxplot(aes(color=genetic_condition),  outlier.alpha=0.3) +
            theme_minimal() +
            xlab("Zeitgeber Time") +
            ylab("Best Frequency (Hz)") +
            coord_cartesian(
              xlim = NULL,
              ylim = c(175, 450),
              expand = TRUE,
              default = FALSE,
              clip = "on"
            ) + 
            scale_colour_manual("", 
                                values = c("#f8766d", "#00bfc4"),
                                labels = c("*Naam* RNAi 25°C", "attP40 Control 25°C")) +
            theme(legend.text = element_markdown()) +
            theme(legend.position = "none") +
            stat_summary(fun = mean, shape = 20, size = 0.4, show.legend=FALSE,
                         aes(colour = genetic_condition), position = position_dodge(width = 0.75))
#kBT
          bp_kBT <- ggplot(df_25, aes(x=zeitgeber_time, y=kBT)) +
            geom_boxplot(aes(color=genetic_condition),  outlier.alpha=0.3) +
            theme_minimal() +
            xlab("Zeitgeber Time") +
            ylab("Power Gain") +
            coord_cartesian(
              xlim = NULL,
              ylim = c(0.5, 20),
              expand = TRUE,
              default = FALSE,
              clip = "on"
            ) + 
            scale_colour_manual("", 
                                values = c("#f8766d", "#00bfc4"),
                                labels = c("*Naam* RNAi 25°C", "attP40 Control 25°C")) +
            theme(legend.text = element_markdown()) +
            theme(legend.position = "left") +
            scale_y_log10() +
            stat_summary(fun = mean, shape = 20, size = 0.4, show.legend=FALSE,
                         aes(colour = genetic_condition), position = position_dodge(width = 0.75))
#Q
          bp_Q <- ggplot(df_25, aes(x=zeitgeber_time, y=Q)) +
            geom_boxplot(aes(color=genetic_condition),  outlier.alpha=0.3) +
            theme_minimal() +
            xlab("Zeitgeber Time") +
            ylab("Tuning Sharpness") +
            scale_colour_manual("", 
                                values = c("#f8766d", "#00bfc4"),
                                labels = c("*Naam* RNAi 25°C", "attP40 Control 25°C")) +
            theme(legend.text = element_markdown()) +
            theme(legend.position = "none") +
            scale_y_log10() +
            stat_summary(fun = mean, shape = 20, size = 0.4, show.legend=FALSE,
                         aes(colour = genetic_condition), position = position_dodge(width = 0.75))
#print
patchwork <- (fft0 | fft6 | fft12 | fft18)
patchwork2 <- (bp_kBT | bp_f0 | bp_Q)

patchwork
patchwork2
#save as vector
#ggsave(file="all_fly_fits_(n=963)_black.svg", path = "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment", plot = p, width = 10, height = 10)
