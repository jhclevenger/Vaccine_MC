# Uncomment and run these if you don't have the packages installed
#install.packages('boot')
#install.packages('ggplot2')

# Load required packages
library(boot)
library(ggplot2)

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

sims <- matrix(NA, nrow = num_sims, ncol=3)
treat_autism <- matrix(NA, nrow = num_sims, ncol=1)
treat_control <- matrix(NA, nrow = num_sims, ncol=1)
autism_control <- matrix(NA, nrow = num_sims, ncol=1)

# Run simulations
for (i_sims in 1:num_sims) {
  
  # Get a number between 1 and 6, in .1 increments for window minimum
  limits = sample(10:60, 1, replace=F)
  min = min(limits)/10
  
  # Set window max as value .25 to 1.25 bigger than minimum (in increments of .25)
  max = min + c(.25,.5,.75)[sample(1:3, 1)]
  
  # Cull data to only be in window
  datums_check = datums_vaccine[datums_vaccine$pre>=min & datums_vaccine$pre<=max,] 
  
  # Get Mean of Conditions in Sample
  control = datums_check[datums_check$condition=='control',]
  autism = datums_check[datums_check$condition=='autism',]
  treatment = datums_check[datums_check$condition=='treatment',]
  
  sims[i_sims,1] = mean(treatment$diff)
  sims[i_sims,2] = mean(control$diff)
  sims[i_sims,3] = mean(autism$diff)
  
  # Code which conditions are bigger, 
  # ignoring cases when 1 of 2 compared conditions have 0 values
  if (nrow(treatment) > 0 & nrow(autism) > 0) {
    if (mean(treatment$diff) > mean(autism$diff))
      treat_autism[i_sims,1] = 1
    else treat_autism[i_sims,1] = 0
    }
  
  if (nrow(treatment) > 0 & nrow(control) > 0) {
    if (mean(treatment$diff) > mean(control$diff))
      treat_control[i_sims,1] = 1
    else treat_control[i_sims,1] = 0
  }
  
  if (nrow(autism) > 0 & nrow(control) > 0) {
    if (mean(autism$diff) > mean(control$diff))
      autism_control[i_sims,1] = 1
    else autism_control[i_sims,1] = 0
  }
}

# Remove missing values (when 1 of 2 compared conditions had 0 values)
treat_autism <- treat_autism[!is.na(treat_autism)]
treat_control <- treat_control[!is.na(treat_control)]
autism_control <- autism_control[!is.na(autism_control)]

# Get overall means
datums[1,1] = mean(treat_autism)
datums[2,1] = mean(treat_control)
datums[3,1] = mean(autism_control)

# Bootstrap Confidence Intervals
b <- boot(treat_autism, function(u,i) mean(u[i]), R = 1000)
ci = boot.ci(b, type = "norm")
datums[1,2] = mean(treat_autism) - ci$norm[2]

b <- boot(treat_control, function(u,i) mean(u[i]), R = 1000)
ci = boot.ci(b, type = "norm")
datums[2,2] = mean(treat_control) - ci$norm[2]

b <- boot(autism_control, function(u,i) mean(u[i]), R = 1000)
ci = boot.ci(b, type = "norm")
datums[3,2] = mean(autism_control) - ci$norm[2]

# Plot
ggplot(datums, aes(x=condition, y=mean)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=mean-ci, ymax=mean+ci)) +
  geom_point(shape=21, size=3, fill="white")