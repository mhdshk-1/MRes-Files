library(data.table)
library(ggplot2)

## functions

se <- function(x, na.rm = TRUE) {
  sd(x)/sqrt(length(x))
}

fit_eq <- function(F0divm, f0, x, Q) {
  ((F0divm)/sqrt((((2*f0*pi)^2-(x*2*pi)^2)^2)+((2*f0*pi)^2*(2*pi*x)^2/Q^2)))*2*pi*x
}

## read in data

grp01_rawdata <- file.choose()
grp02_rawdata <- file.choose()
grp03_rawdata <- file.choose()
grp04_rawdata <- file.choose()
grp05_rawdata <- file.choose()
grp06_rawdata <- file.choose()

## set working directory

setwd(dirname(grp01_rawdata))


## globals

title <- "Circadian free fluctuations - per01"
grp01 <- "ZT00"
grp02 <- "ZT06"
grp03 <- "ZT12"
grp04 <- "ZT18"
grp05 <- "ZT00FR"
grp06 <- "ZT06FR"
  
## coerce into data.table and join data

grp01_data <- read.table(grp01_rawdata, stringsAsFactors = FALSE, row.names = 1, header = TRUE)
grp01_data <- data.table(grp01_data, keep.rownames = TRUE)
grp01_data[, timepoint := grp01]
grp01_data[, temperature := "Low"]

grp02_data <- read.table(grp02_rawdata, stringsAsFactors = FALSE, row.names = 1, header = TRUE)
grp02_data <- data.table(grp02_data, keep.rownames = TRUE)
grp02_data[, timepoint := grp02]
grp02_data[, temperature := "High"]

grp03_data <- read.table(grp03_rawdata, stringsAsFactors = FALSE, row.names = 1, header = TRUE)
grp03_data <- data.table(grp03_data, keep.rownames = TRUE)
grp03_data[, timepoint := grp03]
grp03_data[, temperature := "High"]

grp04_data <- read.table(grp04_rawdata, stringsAsFactors = FALSE, row.names = 1, header = TRUE)
grp04_data <- data.table(grp04_data, keep.rownames = TRUE)
grp04_data[, timepoint := grp04]
grp04_data[, temperature := "Low"]

grp05_data <- read.table(grp05_rawdata, stringsAsFactors = FALSE, row.names = 1, header = TRUE)
grp05_data <- data.table(grp05_data, keep.rownames = TRUE)
grp05_data[, timepoint := grp05]
grp05_data[, temperature := "High"]
grp05_data <- grp05_data[-c(1,3), ]

grp06_data <- read.table(grp06_rawdata, stringsAsFactors = FALSE, row.names = 1, header = TRUE)
grp06_data <- data.table(grp06_data, keep.rownames = TRUE)
grp06_data[, timepoint := grp06]
grp06_data[, temperature := "High"]

data <- rbind(grp01_data, grp02_data, grp03_data, grp04_data, grp05_data, grp06_data)

## convert data into correct type

data$F0divm <- as.numeric(data$F0divm)
data$best_freq <- as.numeric(data$best_freq)
data$Q <- as.numeric(data$Q)
data$eff_mass <- as.numeric(data$eff_mass)
data$eff_stiffness <- as.numeric(data$eff_stiffness)
data$pred_stiffness <- as.numeric(data$pred_stiffness)
data$energy_injection <- as.numeric(data$energy_injection)
data$delta_kBT <- as.numeric(data$delta_kBT)

## summarise data

sum_data <- data[, .(mean_F0divm = mean(F0divm),
                     med_F0divm = median(F0divm),
                     se_F0divm = se(F0divm),
                     mean_delta_kBT = mean(delta_kBT),
                     med_delta_kBT = median(delta_kBT),
                     se_delta_kBT = se(delta_kBT),
                     mean_bestfreq = mean(best_freq),
                     med_bestfreq = median(best_freq),
                     se_bestfreq = se(best_freq),
                     mean_Q = mean(Q),
                     med_Q = median(Q),
                     se_Q = se(Q)),
                 by = timepoint]

## melt data for plotting

molten_data <- melt(data, id = c('rn', 'timepoint', 'temperature'), variable.name = 'parameter', value.name = 'value')

graph_data <- molten_data[parameter %in% c("best_freq", "Q", "delta_kBT")]
graph_data$timepoint <- factor(graph_data$timepoint, c(grp01, grp02, grp03, grp04, grp05, grp06))

p <- ggplot(graph_data, aes(timepoint, value, fill = timepoint)) +
  geom_boxplot() +
  scale_fill_manual("Timepoint",
                    labels = c(paste(grp01, " (n=", length(grp01_data$rn), ")", sep = ""),
                               paste(grp02, " (n=", length(grp02_data$rn), ")", sep = ""),
                               paste(grp03, " (n=", length(grp03_data$rn), ")", sep = ""),
                               paste(grp04, " (n=", length(grp04_data$rn), ")", sep = ""),
                               paste(grp05, " (n=", length(grp05_data$rn), ")", sep = ""),
                               paste(grp06, " (n=", length(grp06_data$rn), ")", sep = "")),
                    #values = c('#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')) +
                    values = c("#d94801", "#fdd0a2", "#8c2d04", "#fdae6b", "#fd8d3c", "#f16913")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  facet_wrap(~ parameter, nrow = 1, scales = "free") +
  ylab("") +
  xlab("Timepoint") +
  ggtitle(title)
  #ggsave(filename = paste(title, "_plotted_parameters.pdf", sep = ""), width = 24, height = 12, units = "cm") +
  #ggsave(filename = paste(title, "_plotted_parameters.png", sep = ""), width = 24, height = 12, units = "cm")

p

sum_data$timepoint <- factor(sum_data$timepoint, c(grp01, grp02, grp03, grp04, grp05, grp06))

o <- ggplot(data.frame(x = c(51, 3200)), aes(x)) +
  stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[1], f0 = sum_data$med_bestfreq[1], Q = sum_data$med_Q[1]), aes(colour = "1"), show.legend = TRUE) +
  stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[2], f0 = sum_data$med_bestfreq[2], Q = sum_data$med_Q[2]), aes(colour = "2"), show.legend = TRUE) +
  stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[3], f0 = sum_data$med_bestfreq[3], Q = sum_data$med_Q[3]), aes(colour = "3"), show.legend = TRUE) +
  stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[4], f0 = sum_data$med_bestfreq[4], Q = sum_data$med_Q[4]), aes(colour = "4"), show.legend = TRUE) +
  stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[5], f0 = sum_data$med_bestfreq[5], Q = sum_data$med_Q[5]), aes(colour = "5"), show.legend = TRUE) +
  stat_function(fun = fit_eq, geom = "line", size = 1, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[6], f0 = sum_data$med_bestfreq[6], Q = sum_data$med_Q[6]), aes(colour = "6"), show.legend = TRUE) +
  scale_colour_manual("Timepoint",
                      labels = c(paste(grp01, " (n=", length(grp01_data$rn), ")", sep = ""),
                                 paste(grp02, " (n=", length(grp02_data$rn), ")", sep = ""),
                                 paste(grp03, " (n=", length(grp03_data$rn), ")", sep = ""),
                                 paste(grp04, " (n=", length(grp04_data$rn), ")", sep = ""),
                                 paste(grp05, " (n=", length(grp05_data$rn), ")", sep = ""),
                                 paste(grp06, " (n=", length(grp06_data$rn), ")", sep = "")),
                      #values = c('#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')) +
                      values = c("#d94801", "#fdd0a2", "#8c2d04", "#fdae6b", "#fd8d3c", "#f16913")) +
  
  scale_x_continuous(trans = "log10", breaks = c(100, 200, 300, 400, 500, 600,700, 800, 900, 1000, 2000, 3000), limits = c(51, 3200)) +
  scale_y_continuous(limits = c(0,1.2e-05)) +
  ylab("Velocity (m/s)") +
  xlab("Frequency (Hz)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "right") +
  ggtitle(title)

sumplot <-  gridExtra::arrangeGrob(o, p, ncol = 1, nrow = 2)

ggsave(paste0("per01_free_fluctuations_med_fit.pdf"), plot = sumplot, width = 27, height = 27, units = "cm")


## timepoints

ZT00_plot <- ggplot(data.frame(x = c(51, 3200)), aes(x)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp01_data$F0divm[1], f0 = grp01_data$best_freq[1], Q = grp01_data$Q[1]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp01_data$F0divm[2], f0 = grp01_data$best_freq[2], Q = grp01_data$Q[2]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp01_data$F0divm[3], f0 = grp01_data$best_freq[3], Q = grp01_data$Q[3]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp01_data$F0divm[4], f0 = grp01_data$best_freq[4], Q = grp01_data$Q[4]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp01_data$F0divm[5], f0 = grp01_data$best_freq[5], Q = grp01_data$Q[5]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp01_data$F0divm[6], f0 = grp01_data$best_freq[6], Q = grp01_data$Q[6]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp01_data$F0divm[7], f0 = grp01_data$best_freq[7], Q = grp01_data$Q[7]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp01_data$F0divm[8], f0 = grp01_data$best_freq[8], Q = grp01_data$Q[8]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp01_data$F0divm[9], f0 = grp01_data$best_freq[9], Q = grp01_data$Q[9]), aes(colour = "1", alpha = 0.5)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 1.5, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[1], f0 = sum_data$med_bestfreq[1], Q = sum_data$med_Q[1]), aes(colour = "1"), show.legend = TRUE) +
  
  #scale_colour_manual("Timepoint", values = c('#fa9fb5')) +
  scale_colour_manual("Timepoint", values = c("#d94801")) +
  scale_x_continuous(trans = "log10", breaks = c(100, 200, 300, 400, 500, 600,700, 800, 900, 1000, 2000, 3000), limits = c(51, 3200)) +
  scale_y_continuous(limits = c(0,1.2e-05)) +
  ylab("Velocity (m/s)") +
  xlab("Frequency (Hz)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("per01 ZT00")

ZT00_plot

ZT06_plot <- ggplot(data.frame(x = c(51, 3200)), aes(x)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp02_data$F0divm[1], f0 = grp02_data$best_freq[1], Q = grp02_data$Q[1]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp02_data$F0divm[2], f0 = grp02_data$best_freq[2], Q = grp02_data$Q[2]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp02_data$F0divm[3], f0 = grp02_data$best_freq[3], Q = grp02_data$Q[3]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp02_data$F0divm[4], f0 = grp02_data$best_freq[4], Q = grp02_data$Q[4]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp02_data$F0divm[5], f0 = grp02_data$best_freq[5], Q = grp02_data$Q[5]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp02_data$F0divm[6], f0 = grp02_data$best_freq[6], Q = grp02_data$Q[6]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp02_data$F0divm[7], f0 = grp02_data$best_freq[7], Q = grp02_data$Q[7]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp02_data$F0divm[8], f0 = grp02_data$best_freq[8], Q = grp02_data$Q[8]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp02_data$F0divm[9], f0 = grp02_data$best_freq[9], Q = grp02_data$Q[9]), aes(colour = "1", alpha = 0.5)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 1.5, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[2], f0 = sum_data$med_bestfreq[2], Q = sum_data$med_Q[2]), aes(colour = "1"), show.legend = TRUE) +
  
  #scale_colour_manual("Timepoint", values = c('#f768a1')) +
  scale_colour_manual("Timepoint", values = c("#fdd0a2")) +
  scale_x_continuous(trans = "log10", breaks = c(100, 200, 300, 400, 500, 600,700, 800, 900, 1000, 2000, 3000), limits = c(51, 3200)) +
  scale_y_continuous(limits = c(0,1.2e-05)) +
  ylab("Velocity (m/s)") +
  xlab("Frequency (Hz)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("per01 ZT06")

ZT06_plot

ZT12_plot <- ggplot(data.frame(x = c(51, 3200)), aes(x)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[1], f0 = grp03_data$best_freq[1], Q = grp03_data$Q[1]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[2], f0 = grp03_data$best_freq[2], Q = grp03_data$Q[2]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[3], f0 = grp03_data$best_freq[3], Q = grp03_data$Q[3]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[4], f0 = grp03_data$best_freq[4], Q = grp03_data$Q[4]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[5], f0 = grp03_data$best_freq[5], Q = grp03_data$Q[5]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[6], f0 = grp03_data$best_freq[6], Q = grp03_data$Q[6]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[7], f0 = grp03_data$best_freq[7], Q = grp03_data$Q[7]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[8], f0 = grp03_data$best_freq[8], Q = grp03_data$Q[8]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[9], f0 = grp03_data$best_freq[9], Q = grp03_data$Q[9]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[10], f0 = grp03_data$best_freq[10], Q = grp03_data$Q[10]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[11], f0 = grp03_data$best_freq[11], Q = grp03_data$Q[11]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp03_data$F0divm[12], f0 = grp03_data$best_freq[12], Q = grp03_data$Q[12]), aes(colour = "1", alpha = 0.5)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 1.5, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[3], f0 = sum_data$med_bestfreq[3], Q = sum_data$med_Q[3]), aes(colour = "1"), show.legend = TRUE) +
  
  #scale_colour_manual("Timepoint", values = c('#dd3497')) +
  scale_colour_manual("Timepoint", values = c("#8c2d04")) +
  scale_x_continuous(trans = "log10", breaks = c(100, 200, 300, 400, 500, 600,700, 800, 900, 1000, 2000, 3000), limits = c(51, 3200)) +
  scale_y_continuous(limits = c(0,1.2e-05)) +
  ylab("Velocity (m/s)") +
  xlab("Frequency (Hz)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("per01 ZT12")

ZT12_plot

ZT18_plot <- ggplot(data.frame(x = c(51, 3200)), aes(x)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp04_data$F0divm[1], f0 = grp04_data$best_freq[1], Q = grp04_data$Q[1]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp04_data$F0divm[2], f0 = grp04_data$best_freq[2], Q = grp04_data$Q[2]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp04_data$F0divm[3], f0 = grp04_data$best_freq[3], Q = grp04_data$Q[3]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp04_data$F0divm[4], f0 = grp04_data$best_freq[4], Q = grp04_data$Q[4]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp04_data$F0divm[5], f0 = grp04_data$best_freq[5], Q = grp04_data$Q[5]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp04_data$F0divm[6], f0 = grp04_data$best_freq[6], Q = grp04_data$Q[6]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp04_data$F0divm[7], f0 = grp04_data$best_freq[7], Q = grp04_data$Q[7]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp04_data$F0divm[8], f0 = grp04_data$best_freq[8], Q = grp04_data$Q[8]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp04_data$F0divm[9], f0 = grp04_data$best_freq[9], Q = grp04_data$Q[9]), aes(colour = "1", alpha = 0.5)) +

  stat_function(fun = fit_eq, geom = "line", size = 1.5, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[4], f0 = sum_data$med_bestfreq[4], Q = sum_data$med_Q[4]), aes(colour = "1"), show.legend = TRUE) +
  
  #scale_colour_manual("Timepoint", values = c('#ae017e')) +
  scale_colour_manual("Timepoint", values = c("#fdae6b")) +
  scale_x_continuous(trans = "log10", breaks = c(100, 200, 300, 400, 500, 600,700, 800, 900, 1000, 2000, 3000), limits = c(51, 3200)) +
  scale_y_continuous(limits = c(0,1.2e-05)) +
  ylab("Velocity (m/s)") +
  xlab("Frequency (Hz)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("per01 ZT18")

ZT18_plot

ZT00FR_plot <- ggplot(data.frame(x = c(51, 3200)), aes(x)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp05_data$F0divm[1], f0 = grp05_data$best_freq[1], Q = grp05_data$Q[1]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp05_data$F0divm[2], f0 = grp05_data$best_freq[2], Q = grp05_data$Q[2]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp05_data$F0divm[3], f0 = grp05_data$best_freq[3], Q = grp05_data$Q[3]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp05_data$F0divm[4], f0 = grp05_data$best_freq[4], Q = grp05_data$Q[4]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp05_data$F0divm[5], f0 = grp05_data$best_freq[5], Q = grp05_data$Q[5]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp05_data$F0divm[6], f0 = grp05_data$best_freq[6], Q = grp05_data$Q[6]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp05_data$F0divm[7], f0 = grp05_data$best_freq[7], Q = grp05_data$Q[7]), aes(colour = "1", alpha = 0.5)) +
  # stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
  #               args = list(F0divm = grp05_data$F0divm[8], f0 = grp05_data$best_freq[8], Q = grp05_data$Q[8]), aes(colour = "1", alpha = 0.5)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 1.5, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[5], f0 = sum_data$med_bestfreq[5], Q = sum_data$med_Q[5]), aes(colour = "1"), show.legend = TRUE) +
  
  #scale_colour_manual("Timepoint", values = c('#7a0177')) +
  scale_colour_manual("Timepoint", values = c("#fd8d3c")) +
  scale_x_continuous(trans = "log10", breaks = c(100, 200, 300, 400, 500, 600,700, 800, 900, 1000, 2000, 3000), limits = c(51, 3200)) +
  scale_y_continuous(limits = c(0,1.2e-05)) +
  ylab("Velocity (m/s)") +
  xlab("Frequency (Hz)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("per01 ZT00FR")

ZT00FR_plot

ZT06FR_plot <- ggplot(data.frame(x = c(51, 3200)), aes(x)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp06_data$F0divm[1], f0 = grp06_data$best_freq[1], Q = grp06_data$Q[1]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp06_data$F0divm[2], f0 = grp06_data$best_freq[2], Q = grp06_data$Q[2]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp06_data$F0divm[3], f0 = grp06_data$best_freq[3], Q = grp06_data$Q[3]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp06_data$F0divm[4], f0 = grp06_data$best_freq[4], Q = grp06_data$Q[4]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp06_data$F0divm[5], f0 = grp06_data$best_freq[5], Q = grp06_data$Q[5]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp06_data$F0divm[6], f0 = grp06_data$best_freq[6], Q = grp06_data$Q[6]), aes(colour = "1", alpha = 0.5)) +
  stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
                args = list(F0divm = grp06_data$F0divm[7], f0 = grp06_data$best_freq[7], Q = grp06_data$Q[7]), aes(colour = "1", alpha = 0.5)) +
  # stat_function(fun = fit_eq, geom = "line", size = 0.25, n = 3200,
  #               args = list(F0divm = grp06_data$F0divm[8], f0 = grp06_data$best_freq[8], Q = grp06_data$Q[8]), aes(colour = "1", alpha = 0.5)) +
  
  stat_function(fun = fit_eq, geom = "line", size = 1.5, n = 3200,
                args = list(F0divm = sum_data$med_F0divm[6], f0 = sum_data$med_bestfreq[6], Q = sum_data$med_Q[6]), aes(colour = "1"), show.legend = TRUE) +
  
  #scale_colour_manual("Timepoint", values = c('#49006a')) +
  scale_colour_manual("Timepoint", values = c("#f16913")) +
  scale_x_continuous(trans = "log10", breaks = c(100, 200, 300, 400, 500, 600,700, 800, 900, 1000, 2000, 3000), limits = c(51, 3200)) +
  scale_y_continuous(limits = c(0,1.2e-05)) +
  ylab("Velocity (m/s)") +
  xlab("Frequency (Hz)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("per01 ZT06FR")

ZT06FR_plot

plot <- gridExtra::arrangeGrob(ZT00_plot, ZT06_plot, ZT12_plot, ZT18_plot, ZT00FR_plot, ZT06FR_plot, ncol = 3, nrow = 2)

ggsave(paste0("per01_free_fluctuations.pdf"), plot = plot, width = 48, height = 27, units = "cm")



## statistics test

model <- aov(delta_kBT ~ timepoint, data = data)
summary(model)

shapiro.test(data[timepoint == "ZT00"][, delta_kBT])

pairwise.t.test(data[, delta_kBT], data[, timepoint], p.adjust.method = "bonferroni")


write.table(tot_sum_data, paste(title, "outdata.txt", sep = ""), sep = "\t")


