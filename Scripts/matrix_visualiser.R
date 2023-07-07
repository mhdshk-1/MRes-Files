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
library(lattice)
library(viridisLite)
library(latticeExtra)
#read matrix
matrix_unformatted <- read.csv("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg_f0_p_matrix.csv")
#reassign first row to colnames
matrix <- matrix_unformatted[,-1]
rownames(matrix) <- matrix_unformatted[,1]
#convert data frame to matrix object
matrix.obj <- data.matrix(matrix, rownames.force = NA)
#direct matrix lattice plot using library(lattice)
levelplot(matrix.obj,
          aspect="iso", scales=list(x=list(rot=90)),
          main="", col.regions=magma(100),
          at=seq(0, 0.05, 0.01),
          colorkey=list(at=seq(0, 0.05, 0.01), 
                        labels=list(at=c(0.05, 0.04, 0.03, 0.02, 0.01), 
                                    labels=c("p = 0.05","" ,"" ,"" , "0.01"))),
          xlab="",
          ylab="")

#import value matrix
value_matrix_unformatted <- read.csv("C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/30deg_f0_matrix.csv")
#reassign first row to colnames
value_matrix <- value_matrix_unformatted[,-1]
rownames(value_matrix) <- value_matrix_unformatted[,1]
#convert data frame to matrix object
matrix.obj.2 <- data.matrix(value_matrix, rownames.force = NA)
#round to 3sf
matrix.obj.2 <- signif(matrix.obj.2,3)

#code for second matrix
myPanel <- function(x, y, z, ...) {
  panel.levelplot(x,y,z,...)
  panel.text(x, y,  matrix.obj.2[cbind(x,y)], cex = 0.7, col = "#000000") ## use handy matrix indexing
}

levelplot(matrix.obj,
          aspect="iso", scales=list(x=list(rot=90)),
          main="Best Frequency", col.regions=c("#fa4520", "#fc6e51", "#fa8a73", "#ffa694", "#fcc6bb"),
          #main="Power Gain", col.regions=c("#81a140", "#a3bf45", "#b5cf49", "#d2e190", "#edfab1"),
          #main="Tuning Sharpness", col.regions=c("#3791c4", "#5da6cf", "#70b5db", "#94cceb", "#c4eaff"),
          at=seq(0, 0.05, 0.01),
          colorkey=list(at=seq(0, 0.05, 0.01), 
                        labels=list(at=c(0.05, 0.04, 0.03, 0.02, 0.01), 
                                    labels=c("p = 0.05","" ,"" ,"" , "0.01"))),
          xlab="",
          ylab="",
          panel=myPanel)

          