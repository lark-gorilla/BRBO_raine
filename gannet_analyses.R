#### gannet analyses for G M-C ####
# Compile data
# run HMM
# run dive analyses
# trip desciption table (mean + sd):
#Max. distance to colony (km)
#Foraging path length (km)
#Foraging trip duration (h)
#Speed (km h-1) 
#Flying time (h) 
#Resting time (h) 
#Dive duration (s)
#Dives per hour of trip

rm(list=ls())
setwd("~/research/gannets")

library(ggplot2)
library(geosphere)
library(RHmm)
library(adehabitatLT)

d_2012<-list.files("data/FS Data 2012")
d_2012<-d_2012[-grep("xls", d_2012)]

dat_2012<-NULL
for(i in d_2012)
{
  d1<-read.csv(paste("data/FS Data 2012/", i, sep=""), h=F)
  d1$ID<-substr(i, 1, nchar(j)-4)
  dat_2012<-rbind(dat_2012, d1)
}


d_2014<-list.files("data/fs data 2014")
d_2014<-d_2014[-grep("xlsx", d_2014)]
i
dat_2014<-NULL
for(i in d_2014)
{
  d_2014_l2<-list.files(paste("data/fs data 2014/", i, sep=""))
  d_2014_l2<-d_2014_l2[grep("csv", d_2014_l2)]
  d_2014_l2<-d_2014_l2[grep("GPS", d_2014_l2)]
  
  l2_bind<-NULL
  for(j in d_2014_l2)
  {
  d1<-read.csv(paste("data/fs data 2014/", i, "/", j, sep=""), h=F)
  d1$ID<-substr(j, 1, nchar(j)-4)
  l2_bind<-rbind(l2_bind, d1)
  print(j)
  }
 
  dat_2014<-rbind(dat_2014, l2_bind)
}
#kill points in N hemisphere, calibration??
dat_2014<-dat_2014[dat_2014$V7=="S",]


d_2016<-list.files("data/FS Data 2016")
d_2016<-d_2016[grep("csv", d_2016)]

dat_2016<-NULL
for(i in d_2016)
{
  d1<-read.csv(paste("data/FS Data 2016/", i, sep=""), h=F)
  d1$ID<-substr(i, 1, nchar(j)-4)
  dat_2016<-rbind(dat_2016, d1)
}


# data in, now to compile and format into master need: ID, DateTime, Lat, Long, year

dfor2012<-data.frame(Date=as.Date(dat_2012$V14, format="%d.%m.%Y"), 
                      Time=dat_2012$V16, Latitude=dat_2012$V7, 
                     Longitude=dat_2012$V6, ID=dat_2012$ID, year=2012)
# 2012 datetime still in UTC??
                                  
dfor2014<-data.frame(Date=as.Date(dat_2014$V4, format="%Y/%m/%d"), 
                     Time=substr(dat_2014$V5, 1, 8), Latitude=-dat_2014$V6, 
                     Longitude=dat_2014$V8, ID=dat_2014$ID, year=2014)

dfor2016<-data.frame(Date=as.Date(dat_2016$V4, format="%Y/%m/%d"), 
                     Time=substr(dat_2016$V5, 1, 8), Latitude=-dat_2016$V6, 
                     Longitude=dat_2016$V8, ID=dat_2016$ID, year=2016)

dat<-rbind(dfor2012, dfor2014, dfor2016)

write.csv(dat, "gannet_data_12_14_16_compiled.csv", row.names=F, quote=F)

dat<-read.csv("gannet_data_12_14_16_compiled.csv", h=T)

#kill crafty lat, long=0 points in 2012 data
dat<-dat[dat$Latitude!=0,]

dat$DateTime <- as.POSIXct(strptime(paste(dat$Date, dat$Time, sep=" "), "%Y-%m-%d %H:%M:%S"))
dat$TrackTime <- as.double(dat$DateTime)


require(sp)
require(rgdal)
require(maps)
require(mapdata)
require(maptools)

##Tripsplit

col_nz<-data.frame(Latitude=-40.557560, Longitude=173.025148)

#### Trip split ####
source("~/grive/phd/scripts/MIBA_scripts_revised/TripSplit_revised_MMLINUX.r")

birds <-unique(dat$ID)
table(dat$ID)

for(i in birds[45:59])
{
  Temp <- dat[dat$ID==i,]
  
  rm(Trip)
  
  Trip <- tripSplit(Track=Temp, Colony=col_nz, InnerBuff=0.1, ReturnBuff=0.15, Duration=0.25, plotit=T)
  
  if(which(birds==i) == 45) {Trips <- Trip} else
    Trips <- spRbind(Trips,Trip)
  
  print(Sys.time())

  #readline("")
}

write.csv(Trips@data, "gannet_dat_tripsplit_5.csv", quote=F, row.names=F)


# now read em all back in

dat<-rbind(
  read.csv("gannet_dat_tripsplit_1.csv", h=T),
  read.csv("gannet_dat_tripsplit_2.csv", h=T),
  read.csv("gannet_dat_tripsplit_3.csv", h=T),
  read.csv("gannet_dat_tripsplit_4.csv", h=T),
  read.csv("gannet_dat_tripsplit_5.csv", h=T))

write.csv(dat, "gannet_dat_tripsplit_full.csv", quote=F, row.names=F) # write full tripsplit dataset

dat$DateTime <- as.POSIXct(dat$DateTime, "%Y-%m-%d %H:%M:%S")

plot(Latitude~Longitude, dat, cex=0.5, pch=3) # v slow
points(Latitude~Longitude, dat[dat$trip_id==-1,], col=2, cex=0.5, pch=3)

nrow(dat)
nrow(dat[dat$trip_id==-1,])

dat<-dat[dat$trip_id!=-1,] # remove -1 points

#dive locations (gap>9 seconds)
dat$TT_diff<-NA
for(i in unique(dat$ID)){
  dat[dat$ID==i,]$TT_diff<-c(NA, diff(dat[dat$ID==i,]$TrackTime, lag=1))
}

summary(dat$TT_diff)
table(dat$TT_diff)

ID_tdif<-aggregate(TT_diff~ID, dat, mean, na.rm=T) # see if some birds were sampled at > 2 sec interval 

long_int_IDz<-ID_tdif[ID_tdif$TT_diff>3,]$ID # birds that are sampled at an interval > 3 secs

dat<-dat[!dat$ID %in% long_int_IDz,] #remove em

table(dat$TT_diff)

points(Latitude~Longitude, dat[dat$TT_diff>120,], col=3, cex=0.5, pch=3) # where are the > 120 gaps??

dat$dive<-0
dat[dat$TT_diff%in% (9:120),]$dive<-1 #saying dives occur from a min of 9 sec up to 2 mins

write.csv(dat, "gannet_dat_tripsplit_dives.csv", quote=F, row.names=F) # write full tripsplit dataset

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONVERT TRIPS OBJECT TO TRAJECTORY OBJECT TO CALCULATE TURNING ANGLES ETC.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# clean up the data a bit

dat<-na.omit(dat)

dat$trip_id<-as.character(dat$trip_id)

dat<-dat[dat$TT_diff!=0,]
### 1. project_data

library(geosphere)

mid_point<-data.frame(centroid(cbind(dat$Longitude, dat$Latitude)))
final_out.Wgs <- SpatialPoints(data.frame(dat$Longitude, dat$Latitude), proj4string=CRS("+proj=longlat"))
DgProj <- CRS(paste("+proj=laea +lon_0=", mid_point$lon, " +lat_0=", mid_point$lat, sep=""))
final_out.Projected <- spTransform(final_out.Wgs, CRS=DgProj)
#now we have projected coords for WHOLE dataset (rather than cols split)

### 3. Convert to LTRAJ
library(adehabitatLT)

trajectories <- as.ltraj(xy=data.frame(final_out.Projected@coords[,1],
                                       final_out.Projected@coords[,2]), date=as.POSIXct(dat$TrackTime, origin="1970/01/01", tz="GMT"), id=dat$trip_id, typeII = TRUE)   


tt<-summary.ltraj(trajectories)
trips<-tt$id
head(trajectories[[1]])
#plot(trajectories)

## I havent run these data data cleaning loops - think they only affect the odd point

### 4. Enumerate bogus data [>3 hr time lapse]
#nonsense<-data.frame(trips, oddlocs=0)
#for (t in 1:length(trips)){
#x<-trajectories[[t]]
#nonsense$oddlocs[nonsense$trips==trips[t]]<-dim(x[x$dt>10000,])[1]
#}
#nonsense

### 5. Eliminate bogus data [>3 hr time lapse]
### removes lines with a long time interval and NA for angle

#for (t in 1:length(trips)){
#x<-trajectories[[t]]
#x<-x[!(x$dt>10000),]
#x<-x[!is.na(x$rel.angle),]
#trajectories[[t]]<-x
#}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIT SINGLE HMM TO ALL TRIPS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#triplist<-list()					## causes error message in viterbi: "REAL() can only be applied to a 'numeric', not a 'list'
triplist<-data.frame()
for (t in 1:length(trips)){
  uil<-trajectories[[t]]
  uil<-uil[,c(6:7,10)]
  uil$speed<-uil$dist/uil$dt
  uil$dist<-NULL
  uil$dt<-NULL
  uil[is.na(uil)]<-0
  uil$ID<-trips[[t]]
  #triplist[[t]] <- uil
  triplist <- rbind(triplist,uil)
}

hmm.2<-HMMFit(triplist[,1:2],nStates=3)		## fits a hidden markov model with 3 states - foraging and commuting and sleeping?
states<-viterbi(hmm.2,triplist[,1:2])		## extracts the predicted states for each location fix using Viterbi's algorith from the HMM fit
triplist$state2<-states$states

dat$hmm_all_trips<-0
for(i in trips)
{
  dat[dat$trip_id==i,]$hmm_all_trips<-triplist[triplist$ID==i,]$state2
} ## loop to match up correct trips in triplist and final_out



########## plot separation between foraging and commuting ##########
plot(speed~rel.angle, data=triplist, type='p',col=triplist$state, pch=triplist$state)
## Nice plot :)

  
  