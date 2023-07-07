library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(gridExtra)

# Set working directory - the one containing the txt-flies
setwd("Z:/Muhammad Shaikh/LDV Readings/FF Experiment/30deg/ZT6/Naam ASCII Plots")

#define PI
PI=3.1415926535897932384626433832795

# Import all txt files (NB- make sure there are no other txt files in the folder OR duplicates!)
temp <- list.files(path = "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/25deg/ZT18/Empty ASCII Plots",pattern = '.*\\.txt',
                       recursive = TRUE, ignore.case = TRUE, include.dirs = TRUE, full.names = TRUE)
temp
myfiles = lapply(temp, read.delim)

# Create metadata dataframe

meta_df <- data.frame(Flyname= c(temp))


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


# Create list variable for hosting regression outputs

coef_list <- list()

# loop through myfiles and save output to coef_list

for(i in 1:length(myfiles)){
  coef_list[[i]]=nls(ms~((F0divm)/sqrt((((2*f0*PI)^2-(Hz*2*PI)^2)^2)+((2*f0*PI)^2*(2*PI*Hz)^2/Q^2)))*2*PI*Hz, data=myfiles[[i]], start = list(F0divm = 0.0040, f0 = 355, Q = 1.6),
                     control = nls.control(warnOnly = TRUE), lower = list(F0divm = 0, f0 = 0, Q = 0),algorithm="port") 
} 

# Pull f0, Q and F0divm from coef_list

df <- t(sapply(coef_list,coefficients))

df <- as.data.frame(df)

#Calculate RSS, TSS and R-squared

for (i in 1:length(coef_list)) {
  
  df$tss[[i]] <- sum((fitted.values(coef_list[[i]])-(mean(fitted.values(coef_list[[i]]))))^2)
  df$rss[[i]] <- deviance(coef_list[[i]])
  df$r.squared[[i]] <- 1-(df$rss[[i]]/df$tss[[i]])

}


# Calculate KBT and other parameters - Masse and Steifheit

T=293
w0=(df$f0)*(2*PI)
kB=1.38*10^-23

#Masse=((2*((1.38*10^(-23))*T))*w0)/((F0divm^2)*Q)
#Steifheit=Masse*w0^2

df$Masse=((2*((1.38*10^(-23))*T))*w0)/((df$F0divm^2)*df$Q)
df$Steifheit=df$Masse*w0^2

# Calculate KBT and other parameters - proportionality factors and kBt

prop1=206.5e-12
df$Ks=prop1*(df$f0)^2


df$dE=((df$Ks/df$Steifheit)-1)*kB*T
df$kBT=((df$Ks/(df$Steifheit))-1)

# Loop scatterplots

plotlist <- function(myfiles){
  ggplot(myfiles, aes(x = Hz, y = ms)) +
    geom_point() + ggtitle(df$r.squared)
}

# Merge plotlist with
plots <- lapply(myfiles, plotlist)

#print(plots)
#g <- do.call("grid.arrange", c(plots, ncol = 3))

#Bring up a single plot for closer inspection - use function #plotfunc(p)

plotfunc <- function(p)plot(myfiles[[p]]$Hz, myfiles[[p]]$ms)

#Select the appropriate graph and press on the outliers you wish to remove. Press ESC to end.

outliers <- identify(x = myfiles[[45]]$Hz, y = myfiles[[45]]$ms, tolerance = 0.25)


# DELETE outliers from selected in the previous step
myfiles[45] <-lapply(myfiles[45], function(x) x[-c(outliers),])

#code to deal with save error: "unimplemented type 'list' in 'EncodeElement'"
df <- apply(df,2,as.character)

# save as CSV for use in Sigmaplot (import through Excel - then copy paste)
write.csv(df, "C:/Users/test/Documents/MRes Neuroscience/Research Project/Muhammad Shaikh/LDV Readings/FF Experiment/25deg/25deg_ZT18_empty_parameters.csv")

