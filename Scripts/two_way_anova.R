#load libraries
library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(ggpubr)
library(car)
library(rstatix)

#set working directory
setwd("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg")

#load files
ZT0_naam_parameters_30deg <- read.csv("30deg_ZT0_naam_parameters.csv")
ZT0_empty_parameters_30deg <- read.csv("30deg_ZT0_empty_parameters.csv")
ZT6_naam_parameters_30deg <- read.csv("30deg_ZT6_naam_parameters.csv")
ZT6_empty_parameters_30deg <- read.csv("30deg_ZT6_empty_parameters.csv")
ZT12_naam_parameters_30deg <- read.csv("30deg_ZT12_naam_parameters.csv")
ZT12_empty_parameters_30deg <- read.csv("30deg_ZT12_empty_parameters.csv")
ZT18_naam_parameters_30deg <- read.csv("30deg_ZT18_naam_parameters.csv")
ZT18_empty_parameters_30deg <- read.csv("30deg_ZT18_empty_parameters.csv")

#data frame formatting, f0 = best frequency, Q = tuning sharpness, kBT = energy gain
df1 <- ZT0_naam_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'naam', zeitgeiber_time = 0, f0 = f0, Q = Q, kBT = kBT)
df2 <- ZT0_empty_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'empty', zeitgeiber_time = 0, f0 = f0, Q = Q, kBT = kBT)
df3 <- ZT6_naam_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'naam', zeitgeiber_time = 6, f0 = f0, Q = Q, kBT = kBT)
df4 <- ZT6_empty_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'empty', zeitgeiber_time = 6, f0 = f0, Q = Q, kBT = kBT)
df5 <- ZT12_naam_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'naam', zeitgeiber_time = 12, f0 = f0, Q = Q, kBT = kBT)
df6 <- ZT12_empty_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'empty', zeitgeiber_time = 12, f0 = f0, Q = Q, kBT = kBT)
df7 <- ZT18_naam_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'naam', zeitgeiber_time = 18, f0 = f0, Q = Q, kBT = kBT)
df8 <- ZT18_empty_parameters_30deg %>% 
  transmute(temperature_condition = 30, genetic_condition = 'empty', zeitgeiber_time = 18, f0 = f0, Q = Q, kBT = kBT)

#final data frame
df <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

#check format
str(df)

#convert temperature_condition, genetic_condition & zeitgeiber_time as factors and recode levels
df$temperature_condition <- factor(df$temperature_condition,
                                   levels = c(25, 30),
                                   labels = c("25_deg", "30_deg"))

df$genetic_condition <- factor(df$genetic_condition,
                                   levels = c("naam", "empty"),
                                   labels = c("naam", "empty"))

df$zeitgeiber_time <- factor(df$zeitgeiber_time,
                                   levels = c(0, 6, 12, 18),
                                   labels = c("ZT0", "ZT6", "ZT12", "ZT18"))

#recheck format
str(df)

#generate frequency table to verify balanced design
table(df$genetic_condition, df$zeitgeiber_time)

#exclude negative f0, kBT, and Q values
#df <- filter(df, f0 >= 0)
#df <- filter(df, kBT >= 0)
#df <- filter(df, Q >= 0)

#box plots with multiple groups, colour box plot by "genetic_condition"
  # plot energy gain ("kBT") by groups ("zeitgeiber_time")
  ggplot(df, aes(x = zeitgeiber_time, y = kBT, fill = genetic_condition)) + geom_boxplot()
  
  # plot tuning sharpness ("Q") by groups ("zeitgeiber_time")
  ggplot(df, aes(x = zeitgeiber_time, y = Q, fill = genetic_condition)) + geom_boxplot()
  
  # plot best frequency ("f0") by groups ("zeitgeiber_time")
  ggplot(df, aes(x = zeitgeiber_time, y = f0, fill = genetic_condition)) + geom_boxplot()

#two-way anovas - additive (for multiplicative replace '+' with '*' if synergistic effect suspected)
  #two-way anova for kBT
  aov2_kBT <- aov(kBT ~ zeitgeiber_time * genetic_condition, data = df)
  summary(aov2_kBT)
  
  #two-way anova for Q
  aov2_Q <- aov(Q ~ zeitgeiber_time * genetic_condition, data = df)
  summary(aov2_Q)
  
  #two-way anova for f0
  aov2_f0 <- aov(f0 ~ zeitgeiber_time * genetic_condition, data = df)
  summary(aov2_f0)

#summary statistics
  #mean & standard deviation by groups (kBT)
  group_by(df, zeitgeiber_time, genetic_condition) %>%
    summarise(
      count = n(),
      mean = mean(kBT, na.rm = TRUE),
      sd = sd(kBT, na.rm = TRUE)
    )
  
  #mean & standard deviation by groups (Q)
  group_by(df, zeitgeiber_time, genetic_condition) %>%
    summarise(
      count = n(),
      mean = mean(Q, na.rm = TRUE),
      sd = sd(Q, na.rm = TRUE)
    )
  
  #mean & standard deviation by groups (f0)
  group_by(df, zeitgeiber_time, genetic_condition) %>%
    summarise(
      count = n(),
      mean = mean(f0, na.rm = TRUE),
      sd = sd(f0, na.rm = TRUE)
    )

#pairwise comparisons (done for ZT only as genetic_condition already known to be significant)
  #tukey honest significant differences multiple pairwise comparisons (kBT)
  TukeyHSD(aov2_kBT, which = "zeitgeiber_time")
  
  #tukey honest significant differences multiple pairwise comparisons (Q)
  TukeyHSD(aov2_Q, which = "zeitgeiber_time")
  
  #tukey honest significant differences multiple pairwise comparisons (f0)
  TukeyHSD(aov2_f0, which = "zeitgeiber_time")

#assumption testing (kBT)
  #1 homogeneity of variances
    #plot of residuals vs fitted values (consider removing outliers)
    plot(aov2_kBT, 1)
    #levene's test to check homogeneity of variances (should be non-significant)
    leveneTest(kBT ~ zeitgeiber_time*genetic_condition, data = df)
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
    leveneTest(Q ~ zeitgeiber_time*genetic_condition, data = df)
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
    leveneTest(f0 ~ zeitgeiber_time*genetic_condition, data = df)
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