#load libraries
library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(ggpubr)
library(car)
library(rstatix)
library(Cairo)

#set working directory
setwd("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/")

#load master data frame
df <- read.csv("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/model_parameters_all_flies.csv")

#check format
str(df)

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

#recheck format
str(df)

#generate frequency table to verify balanced design
table(df$genetic_condition, df$zeitgeber_time)

#exclude negative f0, kBT, and Q values
#df <- filter(df, f0 < 430)
#df <- filter(df, kBT >= 0)
#df <- filter(df, Q >= 0)

#box plots with multiple groups, colour box plot by "genetic_condition"
  # plot energy gain ("kBT") by groups ("zeitgeber_time")
  ggboxplot(df, x = "zeitgeber_time", y = "kBT", color = "genetic_condition",
            palette = c("#3c9fc9", "#c4c4c4"))
  
  # plot tuning sharpness ("Q") by groups ("zeitgeber_time")
  ggboxplot(df, x = "zeitgeber_time", y = "Q", color = "genetic_condition",
            palette = c("#3c9fc9", "#c4c4c4"))
  
  # plot best frequency ("f0") by groups ("zeitgeber_time")
  ggboxplot(df, x = "zeitgeber_time", y = "f0", color = "genetic_condition",
            palette = c("#3c9fc9", "#c4c4c4"))

#two-way anovas - additive (for multiplicative replace '+' with '*' if synergistic effect suspected)
  #two-way anova for kBT
  aov2_kBT <- aov(kBT ~ zeitgeber_time * genetic_condition, data = df)
  summary(aov2_kBT)
  
  #two-way anova for Q
  aov2_Q <- aov(Q ~ zeitgeber_time * genetic_condition, data = df)
  summary(aov2_Q)
  
  #two-way anova for f0
  aov2_f0 <- aov(f0 ~ zeitgeber_time * genetic_condition, data = df)
  summary(aov2_f0)

#summary statistics
  #mean & standard deviation by groups (kBT)
  group_by(df, zeitgeber_time, genetic_condition) %>%
    summarise(
      count = n(),
      mean = mean(kBT, na.rm = TRUE),
      sd = sd(kBT, na.rm = TRUE)
    )
  
  #mean & standard deviation by groups (Q)
  group_by(df, zeitgeber_time, genetic_condition) %>%
    summarise(
      count = n(),
      mean = mean(Q, na.rm = TRUE),
      sd = sd(Q, na.rm = TRUE)
    )
  
  #mean & standard deviation by groups (f0)
  group_by(df, zeitgeber_time, genetic_condition) %>%
    summarise(
      count = n(),
      mean = mean(f0, na.rm = TRUE),
      sd = sd(f0, na.rm = TRUE)
    )

#pairwise comparisons (done for ZT only as genetic_condition already known to be significant)
  #tukey honest significant differences multiple pairwise comparisons (kBT)
  TukeyHSD(aov2_kBT, which = "zeitgeber_time")
  
  #tukey honest significant differences multiple pairwise comparisons (Q)
  TukeyHSD(aov2_Q, which = "zeitgeber_time")
  
  #tukey honest significant differences multiple pairwise comparisons (f0)
  TukeyHSD(aov2_f0, which = "zeitgeber_time")

#assumption testing (kBT)
  #1 homogeneity of variances
    #plot of residuals vs fitted values (consider removing outliers)
    plot(aov2_kBT, 1)
    #levene's test to check homogeneity of variances (should be non-significant)
    leveneTest(kBT ~ zeitgeber_time*genetic_condition, data = df)
  #2 normality
    #q-q plot
    plot(aov2_kBT, 2)
    #extract residuals
    aov_residuals_kBT <- residuals(object = aov2_kBT)
    #run shapiro-wilk test (should be non-significant)
    shapiro.test(x = aov_residuals_kBT)

#assumption testing (Q)
  #1 homogeneity of variances
    #plot of residuals vs fitted values (consider removing outliers)
    plot(aov2_Q, 1)
    #levene's test to check homogeneity of variances (should be non-significant)
    leveneTest(Q ~ zeitgeber_time*genetic_condition, data = df)
  #2 normality
    #q-q plot
    plot(aov2_Q, 2)
    #extract residuals
    aov_residuals_Q <- residuals(object = aov2_Q)
    #run shapiro-wilk test (should be non-significant)
    shapiro.test(x = aov_residuals_Q)
    
#assumption testing (f0)
  #1 homogeneity of variances
    #plot of residuals vs fitted values (consider removing outliers)
    plot(aov2_f0, 1)
    #levene's test to check homogeneity of variances (should be non-significant)
    leveneTest(f0 ~ zeitgeber_time*genetic_condition, data = df)
  #2 normality
    #q-q plot
    plot(aov2_f0, 2)
    #extract residuals
    aov_residuals_f0 <- residuals(object = aov2_f0)
    #run shapiro-wilk test (should be non-significant)
    shapiro.test(x = aov_residuals_f0)

#kruskal-wallis tests
kruskal.test(kBT ~ genetic_condition, data = df)
kruskal.test(Q ~ genetic_condition, data = df)
kruskal.test(f0 ~ genetic_condition, data = df)

#ideal test is non-parametrised multifactorial (multiple independents) multivariate (multiple dependents) analysis of variance (MANOVA)
#https://www.statology.org/three-way-anova-in-r/
#http://www.sthda.com/english/wiki/two-way-anova-test-in-r
#http://depts.washington.edu/acelab/proj/art/index.html
#r package for nonparametric tests file:///C:/Users/test/Downloads/feys.pdf
#harmonic oscillator model file:///C:/Users/test/Downloads/pnas.0405741102.pdf