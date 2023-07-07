#load libraries
{library(magrittr)
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
library(ggsignif)}

#functions
se <- function(x, na.rm = TRUE) {
  sd(x)/sqrt(length(x))
}


comparisons_list <- function(data,
                             x) {
  # creating a dataframe with just the columns of interest
  # make sure the grouping variable x is indeed a factor
  # has no unused levels
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x)
    ) %>%
    dplyr::mutate(.data = ., 
                  x = droplevels(as.factor(x)))
  
  grouplevels <- levels(data$x)
  g1_list <- combn(grouplevels, 2)[1, ]
  g2_list <- combn(grouplevels, 2)[2, ]
  
  comparisons_list <- lapply(
    1:length(g1_list),
    function(i) c(
      combn(grouplevels, 2)[2, i],
      combn(grouplevels, 2)[1, i]
    )
  )
  
  return(comparisons_list)
}


#load master data frame
df <- read.csv("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/model_parameters_all_flies.csv")
unformatted_df <- df

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

#means & line plots by temperature_condition, genetic condition, and zeitgeber_time
#f0_means
    f0_means <- df %>%
      group_by(genetic_condition, zeitgeber_time) %>%
      summarise(mean_f0 = mean(f0))
    
      p1 <- ggplot(f0_means, aes(x=zeitgeber_time, y=mean_f0, group=genetic_condition)) +
      geom_line(aes(color=genetic_condition)) +
      geom_point(aes(color=genetic_condition)) +
      ylim(280,375) +
      theme_minimal()
#kBT_means
    kBT_means <- df %>%
      group_by(genetic_condition, zeitgeber_time) %>%
      summarise(mean_kBT = mean(kBT))
    
      p2 <- ggplot(kBT_means, aes(x=zeitgeber_time, y=mean_kBT, group=genetic_condition)) +
      geom_line(aes(color=genetic_condition)) +
      geom_point(aes(color=genetic_condition)) +
      ylim(2,10) +
      theme_minimal()
#Q_means
    Q_means <- df %>%
      group_by(genetic_condition, zeitgeber_time) %>%
      summarise(mean_Q = mean(Q))
    
      p3 <- ggplot(Q_means, aes(x=zeitgeber_time, y=mean_Q, group=genetic_condition)) +
      geom_line(aes(color=genetic_condition)) +
      geom_point(aes(color=genetic_condition)) +
      ylim(1.5,3) +
      theme_minimal()

#box plots of dependent variables by groups ("zeitgeber_time")
#f0
      p4 <-  ggplot(df, aes(x=zeitgeber_time, y=f0)) +
      geom_boxplot(aes(color=genetic_condition),  outlier.alpha=0.3) +
      theme_minimal()
#kBT
      p5 <-  ggplot(df, aes(x=zeitgeber_time, y=kBT)) +
      geom_boxplot(aes(color=genetic_condition),  outlier.alpha=0.3) +
      theme_minimal()
#Q
      p6 <-  ggplot(df, aes(x=zeitgeber_time, y=Q)) +
      geom_boxplot(aes(color=genetic_condition),  outlier.alpha=0.3) +
      theme_minimal()
      
#means 4 levels
#f0
      f0_means_16 <- df %>%
        group_by(genetic_condition, zeitgeber_time, temperature_condition) %>%
        summarise(mean_f0 = mean(f0), sem=se(f0), n = length(genetic_condition))
      f0_means_16$genetic_temperature_condition <- paste(f0_means_16$genetic_condition, f0_means_16$temperature_condition)
      
     p7 <- ggplot(f0_means_16, aes(x=zeitgeber_time, y=mean_f0, group=genetic_temperature_condition)) +
        geom_line(aes(color=genetic_condition, alpha = temperature_condition)) +
        geom_point(aes(color=genetic_condition, alpha = temperature_condition)) +
        scale_alpha_discrete(range = c(0.3, 1)) +
        ylim(225,400) +
        theme_minimal()
#kBT
      kBT_means_16 <- df %>%
        group_by(genetic_condition, zeitgeber_time, temperature_condition) %>%
        summarise(mean_kBT = mean(kBT), sem=se(kBT), n = length(genetic_condition))
      kBT_means_16$genetic_temperature_condition <- paste(kBT_means_16$genetic_condition, kBT_means_16$temperature_condition)
      
      p8 <- ggplot(kBT_means_16, aes(x=zeitgeber_time, y=mean_kBT, group=genetic_temperature_condition)) +
        geom_line(aes(color=genetic_condition, alpha = temperature_condition)) +
        geom_point(aes(color=genetic_condition, alpha = temperature_condition)) +
        scale_alpha_discrete(range = c(0.3, 1)) +
        theme_minimal()
#Q
      Q_means_16 <- df %>%
        group_by(genetic_condition, zeitgeber_time, temperature_condition) %>%
        summarise(mean_Q = mean(Q), sem=se(Q), n = length(genetic_condition))
      Q_means_16$genetic_temperature_condition <- paste(Q_means_16$genetic_condition, Q_means_16$temperature_condition)
      
      p9 <- ggplot(Q_means_16, aes(x=zeitgeber_time, y=mean_Q, group=genetic_temperature_condition)) +
        geom_line(aes(color=genetic_condition, alpha = temperature_condition)) +
        geom_point(aes(color=genetic_condition, alpha = temperature_condition)) +
        scale_alpha_discrete(range = c(0.3, 1)) +
        theme_minimal()

      
#patchwork 1
(p4|p5|p6)/
(p1|p2|p3)
      
#patchwork 2
(p4|p5|p6)/
(p1|p2|p3)/
(p7|p8|p9)
      
#draft graphs
#smoothed line plot
    ggplot(unformatted_df, aes(x=zeitgeber_time, y=f0)) +
      geom_smooth(aes(color=genetic_condition)) +
      theme_minimal() +
      xlim(0, 24)
#rsquared
    ggplot(unformatted_df, aes(x=zeitgeber_time, y=stiffness)) +
      geom_smooth(aes(color=genetic_condition)) +
      theme_minimal()

    
#box plots of dependent variables by groups ("zeitgeber_time")
#f0
    df$genetic_temperature_condition <- paste(df$genetic_condition, df$temperature_condition)
    df$genetic_temperature_zeitgeber_condition <- paste(df$genetic_temperature_condition, df$zeitgeber_time)
    
    px <- ggplot(df, aes(x=zeitgeber_time, y=f0, color = genetic_temperature_condition,
                                group = interaction(zeitgeber_time, genetic_condition, temperature_condition))) +
      geom_boxplot(outlier.alpha=0.3) +
      scale_colour_manual(values = c("#94dfe1", "#00bfc4", "#fcbfbb", "#f8766d")) +
      scale_y_log10() +
      theme_minimal()
#rsquared
    df$genetic_temperature_condition <- paste(df$genetic_condition, df$temperature_condition)
    
    pxi <- ggplot(df, aes(x=zeitgeber_time, y=r.squared, color = genetic_temperature_condition,
                        group = interaction(zeitgeber_time, genetic_condition, temperature_condition))) +
      geom_boxplot(outlier.alpha=0.3) +
      scale_colour_manual("", 
                          values = c("#94dfe1", "#00bfc4", "#fcbfbb", "#f8766d"),
                          labels = c("attP40 Control 25°C",
                                     "attP40 Control 30°C",
                                     "*Naam* RNAi 25°C",
                                     "*Naam* RNAi 30°C")) +
      ylim(0.8, 1) +
      xlab("Zeitgeber Time") +
      ylab("R²") +
      theme_minimal() +
      theme(axis.title.y=element_text(face="italic")) +
      theme(legend.text = element_markdown()) +
      stat_summary(fun = mean, shape = 20, size = 0.4, position = position_dodge2(width = 0.75), show.legend=FALSE)
#kBT
    g1 <- ggplot(df, aes(x=zeitgeber_time, y=kBT, color = genetic_temperature_condition,
                          group = interaction(zeitgeber_time, genetic_condition, temperature_condition))) +
      geom_boxplot(outlier.alpha=0.3) +
      scale_colour_manual("", 
                          values = c("#94dfe1", "#00bfc4", "#fcbfbb", "#f8766d"),
                          labels = c("attP40 Control 25°C",
                                     "attP40 Control 30°C",
                                     "*Naam* RNAi 25°C",
                                     "*Naam* RNAi 30°C")) +
      xlab("Zeitgeber Time") +
      ylab("Power Gain") +
      theme_minimal() +
      scale_y_log10() +
      coord_cartesian(
        xlim = NULL,
        #ylim = c(0.5, 30),
        expand = TRUE,
        default = FALSE,
        clip = "on"
      ) +
      theme(legend.text = element_markdown()) +
      theme(legend.position = "left") +
      stat_summary(fun = mean, shape = 20, size = 0.4, position = position_dodge2(width = 0.75), show.legend=FALSE)
#f0
    g2 <- ggplot(df, aes(x=zeitgeber_time, y=f0, color = genetic_temperature_condition,
                         group = interaction(zeitgeber_time, genetic_condition, temperature_condition))) +
      geom_boxplot(outlier.alpha=0.3) +
      scale_colour_manual("", 
                          values = c("#94dfe1", "#00bfc4", "#fcbfbb", "#f8766d"),
                          labels = c("attP40 Control 25°C",
                                     "attP40 Control 30°C",
                                     "*Naam* RNAi 25°C",
                                     "*Naam* RNAi 30°C")) +
      xlab("Zeitgeber Time") +
      ylab("Best Frequency (Hz)") +
      theme_minimal() +
      theme(legend.text = element_markdown()) +
      coord_cartesian(
        xlim = NULL,
        ylim = c(150, 500),
        expand = TRUE,
        default = FALSE,
        clip = "on"
      ) +
      theme(legend.position = "none") +
      stat_summary(fun = mean, shape = 20, size = 0.4, position = position_dodge2(width = 0.75), show.legend=FALSE)
#Q
    g3 <- ggplot(df, aes(x=zeitgeber_time, y=Q, color = genetic_temperature_condition,
                         group = interaction(zeitgeber_time, genetic_condition, temperature_condition))) +
      geom_boxplot(outlier.alpha=0.3) +
      scale_colour_manual("", 
                          values = c("#94dfe1", "#00bfc4", "#fcbfbb", "#f8766d"),
                          labels = c("attP40 Control 25°C",
                                     "attP40 Control 30°C",
                                     "*Naam* RNAi 25°C",
                                     "*Naam* RNAi 30°C")) +
      xlab("Zeitgeber Time") +
      ylab("Tuning Sharpness") +
      theme_minimal() +
      theme(legend.text = element_markdown()) +
      coord_cartesian(
        xlim = NULL,
        ylim = c(0.5, 15),
        expand = TRUE,
        default = FALSE,
        clip = "on"
      ) +
      scale_y_log10() +
      theme(legend.position = "none") +
      stat_summary(fun = mean, shape = 20, size = 0.4, position = position_dodge2(width = 0.75), show.legend=FALSE)
r2over1 <- filter(df, r.squared <= 0.8)
patchwork3 <- (g1 | g2 | g3)
patchwork3

#kBT

p7 <- ggplot(kBT_means_16, aes(x=zeitgeber_time, y=mean_kBT, group=genetic_temperature_condition)) +
  geom_line(aes(colour = genetic_temperature_condition), size=.5) +
  geom_point(aes(colour = genetic_temperature_condition), size=3) +
  geom_errorbar(aes(ymin=mean_kBT-sem, ymax=mean_kBT+sem, color=genetic_temperature_condition), width=.2, size=.5) +
  geom_signif(comparisons = comp.list,
              step_increase = 0.1, test = "wilcox.test", map_signif_level = TRUE) +
  theme_minimal() +
  scale_colour_manual("", 
                      values = c("#94dfe1", "#00bfc4", "#fcbfbb", "#f8766d"),
                      labels = c("attP40 Control 25°C",
                                 "attP40 Control 30°C",
                                 "*Naam* RNAi 25°C",
                                 "*Naam* RNAi 30°C")) +
  xlab("") +
  ylab("Power Gain") +
  theme(legend.text = element_markdown()) +
  theme(legend.position = "left")

#f0
p8 <- ggplot(f0_means_16, aes(x=zeitgeber_time, y=mean_f0, group=genetic_temperature_condition)) +
  geom_line(aes(colour = genetic_temperature_condition), size=.5) +
  geom_point(aes(colour = genetic_temperature_condition), size=3) +
  geom_errorbar(aes(ymin=mean_f0-sem, ymax=mean_f0+sem, color=genetic_temperature_condition), width=.2, size=.5) +
  theme_minimal() +
  scale_colour_manual("", 
                      values = c("#94dfe1", "#00bfc4", "#fcbfbb", "#f8766d"),
                      labels = c("attP40 Control 25°C",
                                 "attP40 Control 30°C",
                                 "*Naam* RNAi 25°C",
                                 "*Naam* RNAi 30°C")) +
  xlab("") +
  ylab("Best Frequency (Hz)") +
  theme(legend.text = element_markdown()) +
  theme(legend.position = "none")

#Q
p9 <- ggplot(Q_means_16, aes(x=zeitgeber_time, y=mean_Q, group=genetic_temperature_condition)) +
  geom_line(aes(colour = genetic_temperature_condition), size=.5) +
  geom_point(aes(colour = genetic_temperature_condition), size=3) +
  geom_errorbar(aes(ymin=mean_Q-sem, ymax=mean_Q+sem, color=genetic_temperature_condition), width=.2, size=.5) +
  theme_minimal() +
  scale_colour_manual("", 
                      values = c("#94dfe1", "#00bfc4", "#fcbfbb", "#f8766d"),
                      labels = c("attP40 Control 25°C",
                                 "attP40 Control 30°C",
                                 "*Naam* RNAi 25°C",
                                 "*Naam* RNAi 30°C")) +
  xlab("") +
  ylab("Tuning Sharpness") +
  theme(legend.text = element_markdown()) +
  theme(legend.position = "none")

#draft
#comp.list <- comparisons_list(data = df, x = genetic_temperature_zeitgeber_condition)

ggplot(df, aes(x=genetic_temperature_zeitgeber_condition, y=kBT)) +
  geom_boxplot(aes(colour = genetic_temperature_condition),outlier.alpha=0.3) +
  geom_signif(comparisons = comp.list, step_increase = 0.1, test = "wilcox.test", map_signif_level = TRUE) +
  xlab("") +
  ylab("Power Gain") +
  theme_minimal() +
  coord_cartesian(
    xlim = NULL,
    #ylim = c(0.5, 30),
    expand = TRUE,
    default = FALSE,
    clip = "on"
  ) +
  theme(legend.position = "left") +
  theme(axis.text.x = element_text(angle=90)) +
  scale_colour_manual("", values = c("#94dfe1", "#00bfc4", "#fcbfbb", "#f8766d"))
  stat_summary(aes(colour = genetic_temperature_condition), fun = mean, shape = 20, size = 0.4, position = position_dodge2(width = 0.75), show.legend=FALSE)

patchwork4 <- (p7 | p8 | p9)
patchwork4

#means
comp.list <- comparisons_list(data = df, x = genetic_temperature_condition)
x.labs <- c("attP40 Control 25°C", "attP40 Control 30°C", "Naam RNAi 25°C", "Naam RNAi 30°C")

#kBT
i <- ggplot(df, aes(x=genetic_temperature_condition, y=kBT)) +
  geom_boxplot(aes(colour=zeitgeber_time)) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 1, color = "black", size = 0.2) +
  #stat_summary(aes(label=round(..y..,3)), fun = mean, geom="text", size=4, vjust = -0.2, hjust = 0.5, angle = 0, fontface = "bold") +
  geom_signif(comparisons = comp.list,
              step_increase = 0.1, test = "wilcox.test", map_signif_level = TRUE) +
  scale_colour_brewer("", palette = "BrBG") +
  theme_minimal() +
  scale_y_log10() +
  ylab("Power Gain") +
  xlab("") +
  theme(legend.position = "left") +
  scale_x_discrete(labels= x.labs) +
  theme(axis.text.x = element_text(angle=90)) +
  coord_cartesian(
    xlim = NULL,
    ylim = c(1e-01, 1e+04),
    expand = TRUE,
    default = FALSE,
    clip = "on")

#f0
ii <- ggplot(df, aes(x=genetic_temperature_condition, y=f0)) +
  geom_boxplot(aes(colour=zeitgeber_time)) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 1, color = "black", size = 0.2) +
  #stat_summary(aes(label=round(..y..,0)), fun = mean, geom="text", size=4, vjust = -0.2, hjust = 0.5, angle = 0, fontface = "bold") +
  geom_signif(comparisons = comp.list,
              step_increase = 0.1, test = "wilcox.test", map_signif_level = TRUE) +
  scale_colour_brewer("", palette = "BrBG") +
  theme_minimal() +
  #scale_y_log10() +
  ylab("Best Frequency (Hz)") +
  xlab("") +
  theme(legend.position = "none") +
  scale_x_discrete(labels= x.labs) +
  theme(axis.text.x = element_text(angle=90)) +
  coord_cartesian(
    xlim = NULL,
    #ylim = c(1e-01, 1e+04),
    expand = TRUE,
    default = FALSE,
    clip = "on")

#Q
iii <- ggplot(df, aes(x=genetic_temperature_condition, y=Q)) +
  geom_boxplot(aes(colour=zeitgeber_time)) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 1, color = "black", size = 0.2) +
  #stat_summary(aes(label=round(..y..,3)), fun = mean, geom="text", size=4, vjust = -0.2, hjust = 0.5, angle = 0, fontface = "bold") +
  geom_signif(comparisons = comp.list,
              step_increase = 0.1, test = "wilcox.test", map_signif_level = TRUE) +
  scale_colour_brewer("", palette = "BrBG") +
  theme_minimal() +
  scale_y_log10() +
  ylab("Tuning Sharpness") +
  xlab("") +
  theme(legend.position = "none") +
  scale_x_discrete(labels= x.labs) +
  theme(axis.text.x = element_text(angle=90)) +
  coord_cartesian(
    xlim = NULL,
    #ylim = c(1e-01, 1e+04),
    expand = TRUE,
    default = FALSE,
    clip = "on")

patchworkx <- (i|ii|iii)
patchworkx
#parametric 3-way anova
    #perform three-way ANOVA
    #model <- aov(f0 ~ temperature_condition * genetic_condition * zeitgeber_time, data=df)
    #view summary of three-way ANOVA
    #summary(model)

#nonparametric 3-way anova
    #aligned rank-transform of data using library(ARTool)
    m = art(f0 ~ temperature_condition * genetic_condition * zeitgeber_time, data=df) #linear mixed model syntax; see lme4::lmer
    #run anova on aligned rank-transformed data
    anova(m)
    #pairwise comparisons with holm-sidak correction
    art.con(m, "genetic_condition:zeitgeber_time", adjust="holm") %>%
      summary() %>%  # add significance stars to the output
      mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                           cutpoints = c(0, .001, .01, .05, .10, 1),
                           symbols = c("***", "**", "*", ".", " ")))
    #pairwise comparisons with holm-sidak correction
    art.con(m, "temperature_condition:genetic_condition:zeitgeber_time", adjust="holm") %>%
      summary() %>%  # add significance stars to the output
      mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                           cutpoints = c(0, .001, .01, .05, .10, 1),
                           symbols = c("***", "**", "*", ".", " ")))
    #pairwise comparisons with holm-sidak correction
    art.con(m, "temperature_condition:zeitgeber_time", adjust="holm") %>%
      summary() %>%  # add significance stars to the output
      mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                           cutpoints = c(0, .001, .01, .05, .10, 1),
                           symbols = c("***", "**", "*", ".", " ")))
    #test for differences of differences
    art.con(m, "genetic_condition:zeitgeber_time", interaction = TRUE)
    
#filter data
    df_filtered <- filter(df, genetic_condition == "empty")
    #nonparametric 3-way anova
    #aligned rank-transform of data using library(ARTool)
    m = art(Q ~ temperature_condition * zeitgeber_time, data=df_filtered) #linear mixed model syntax; see lme4::lmer
    #run anova on aligned rank-transformed data
    aovdf <- as.data.frame(anova(m))
    write.csv(aovdf, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/empty_Q_aov2.csv")
    #pairwise comparisons with holm-sidak correction
    pairwise <- art.con(m, "genetic_condition:temperature_condition", adjust="holm") %>%
      summary() %>%  # add significance stars to the output
      mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                           cutpoints = c(0, .001, .01, .05, .10, 1),
                           symbols = c("***", "**", "*", ".", " ")))
    pairwise <- as.data.frame(pairwise)
    #write.csv(pairwise, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/25deg_Q_holm-sidak_pairwise.csv")
    
#obtain pairwise comparisons by means not ranks
    m.linear = lm(kBT ~ genetic_condition * temperature_condition * zeitgeber_time, data=df)
    anova(m.linear)
    pairwise <- contrast(emmeans(m.linear, ~ genetic_condition:temperature_condition:zeitgeber_time), method = "pairwise")
    pairwise <- as.data.frame(pairwise)
    pairwise
    
    #write.csv(pairwise, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/temperature_genetic_tukey_f0.csv")
    
    df_filtered <- filter(df, df$genetic_condition == "empty")
    #nonparametric 3-way anova
    #aligned rank-transform of data using library(ARTool)
    m = art(kBT ~ genetic_condition * temperature_condition * zeitgeber_time, data=df) #linear mixed model syntax; see lme4::lmer
    #run anova on aligned rank-transformed data
    anova(m)
    #pairwise comparisons with holm-sidak correction
    pairwise <- art.con(m, "genetic_condition:temperature_condition:zeitgeber_time", adjust="holm") %>%
      summary() %>%  # add significance stars to the output
      mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                           cutpoints = c(0, .001, .01, .05, .10, 1),
                           symbols = c("***", "**", "*", ".", " ")))
    pairwise <- as.data.frame(pairwise)
    
write.csv(pairwise, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/kBT_pairwise_p_values.csv")
    