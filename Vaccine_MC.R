# Uncomment and run these if you don't have the packages installed
#install.packages('boot')
#install.packages('ggplot2')

# Load required packages
library(boot)
library(ggplot2)
library(reshape2)

# Read in data 
setwd("~/Desktop") # Set this to where you put the data file.
datums_vaccine = read.table("Vaccine_Data.txt", header = TRUE)
datums_vaccine$diff = datums_vaccine$post - datums_vaccine$pre

num_sims <- 100000

# Initialize matrices/data.frames
datums <- data.frame(matrix(ncol = 3, nrow = 3))
rownames(datums) <- c("treat_autism", "treat_control","autism_control")
colnames(datums) <- c("mean", "ci","condition")
datums[1,3] = "treat_autism"
datums[2,3] = "treat_control"
datums[3,3] = "autism_control"

sims <- data.frame(matrix(NA, nrow = num_sims, ncol=6))
colnames(sims) <- c("treat_diff", "control_diff","autism_diff",
                    "window_size","min","max")
treat_autism <- data.frame(matrix(NA, nrow = num_sims, ncol=4))
colnames(treat_autism) <- c("binary", "window_size","min",
                            "max")
treat_control <- data.frame(matrix(NA, nrow = num_sims, ncol=4))
colnames(treat_control) <- c("binary", "window_size","min",
                             "max")
autism_control <- data.frame(matrix(NA, nrow = num_sims, ncol=4))
colnames(autism_control) <- c("binary", "window_size","min",
                              "max")

# Run simulations
for (i_sims in 1:num_sims) {
  
  # Get window size
  windows = c(seq(.25,1.25,.25))
  window = windows[sample(1:length(windows), 1)]
  
  # Make sure window doesn't exceed distribution
  flip = sample(0:1,1)
  
  if (flip) { 
    limit = sample((10 + (window*10)):60, 1, replace=F)
    max = limit/10
    min = max - window
  } else {
    limit = sample(10:(60 - (window*10)), 1, replace=F)
    min = limit/10
    max = min + window
  }
  
  # Cull data not in window
  datums_check = datums_vaccine[datums_vaccine$pre>=min & datums_vaccine$pre<=max,] 
  
  # Get mean of conditions in sample
  control = datums_check[datums_check$condition=='control',]
  autism = datums_check[datums_check$condition=='autism',]
  treatment = datums_check[datums_check$condition=='treatment',]
  
  sims[i_sims,1] = mean(treatment$diff)
  sims[i_sims,2] = mean(control$diff)
  sims[i_sims,3] = mean(autism$diff)
  sims[i_sims,4] = window
  sims[i_sims,5] = min
  sims[i_sims,6] = max
  
  # Code which conditions are bigger, 
  # ignoring cases when 1 of 2 compared conditions have 0 values
  if (nrow(treatment) > 0 & nrow(autism) > 0) {
    treat_autism[i_sims,2] = window
    treat_autism[i_sims,3] = min
    treat_autism[i_sims,4] = max
    if (mean(treatment$diff) > mean(autism$diff))
      treat_autism[i_sims,1] = 1
    else 
      treat_autism[i_sims,1] = 0
  }
  
  if (nrow(treatment) > 0 & nrow(control) > 0) {
    treat_control[i_sims,2] = window
    treat_control[i_sims,3] = min
    treat_control[i_sims,4] = max
    if (mean(treatment$diff) > mean(control$diff))
      treat_control[i_sims,1] = 1
    else treat_control[i_sims,1] = 0
  }
  
  if (nrow(autism) > 0 & nrow(control) > 0) {
    autism_control[i_sims,2] = window
    autism_control[i_sims,3] = min
    autism_control[i_sims,4] = max
    if (mean(autism$diff) > mean(control$diff))
      autism_control[i_sims,1] = 1
    else autism_control[i_sims,1] = 0
  }
}

# Remove columns with missing values (when 1 of 2 compared conditions had 0 values)
treat_autism = treat_autism[complete.cases(treat_autism),]
treat_control = treat_control[complete.cases(treat_control),]
autism_control = autism_control[complete.cases(autism_control),]

##### 

# Get overall means
datums[1,1] = mean(treat_autism[,1])
datums[2,1] = mean(treat_control[,1])
datums[3,1] = mean(autism_control[,1])

# Bootstrap Confidence Intervals
b <- boot(treat_autism[,1], function(u,i) mean(u[i]), R = 1000)
ci = boot.ci(b, type = "norm")
datums[1,2] = mean(treat_autism[,1]) - ci$norm[2]

b <- boot(treat_control[,1], function(u,i) mean(u[i]), R = 1000)
ci = boot.ci(b, type = "norm")
datums[2,2] = mean(treat_control[,1]) - ci$norm[2]

b <- boot(autism_control[,1], function(u,i) mean(u[i]), R = 1000)
ci = boot.ci(b, type = "norm")
datums[3,2] = mean(autism_control[,1]) - ci$norm[2]

# Plot overall means
ggplot(datums, aes(x=condition, y=mean)) +
  geom_errorbar(width=.1, aes(ymin=mean-ci, ymax=mean+ci)) +
  geom_point(shape=21, size=4, fill="white")

