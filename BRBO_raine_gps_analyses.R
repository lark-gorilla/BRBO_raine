## Import BRBO raine tracking and so some exploratory analyses. Link tracked birds to
## diet samples and run HMM to identify foraging areas

#install.packages("RHmm", repos="http://R-Forge.R-project.org")

setwd("~/grive/phd/analyses/BRBO_raine")

library(RHmm)

dat<-read.csv("~/grive/phd/fieldwork/Raine_Dec_2014/data/tracking_results/Raine_tracking_trips_clean.csv")

head(dat)

