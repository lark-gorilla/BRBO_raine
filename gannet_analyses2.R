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

#start R in unix with R --max-ppsize 500000

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

# Extra data from Gabriel

extrA<-list.files("data/extra_data")
extrA<-extrA[grep("csv", extrA)]

dat_ext<-NULL
for(i in extrA)
{
  d1<-read.csv(paste("data/extra_data/", i, sep=""), h=F)
  d1$ID<-substr(i, 1, nchar(i)-4)
  dat_ext<-rbind(dat_ext, d1)
}

# data in, now to compile and format into master need: ID, DateTime, Lat, Long, year

ext_clean<-data.frame(Date=as.Date(dat_ext$V4, format="%Y/%m/%d"), 
                      Time=substr(dat_ext$V5, 1, 8), Latitude=as.numeric(paste("-", dat_ext$V6, sep="")), 
                     Longitude=dat_ext$V8, ID=dat_ext$ID, year=substr(dat_ext$V4, 1,4))
# remove badpoints
ext_clean<-ext_clean[ext_clean$Longitude>170,]

plot(Latitude~Longitude, ext_clean, pch=16, cex=0.5, col=ID)
                                  
dfor2014<-data.frame(Date=as.Date(dat_2014$V4, format="%Y/%m/%d"), 
                     Time=substr(dat_2014$V5, 1, 8), Latitude=-dat_2014$V6, 
                     Longitude=dat_2014$V8, ID=dat_2014$ID, year=2014)

dfor2016<-data.frame(Date=as.Date(dat_2016$V4, format="%Y/%m/%d"), 
                     Time=substr(dat_2016$V5, 1, 8), Latitude=-dat_2016$V6, 
                     Longitude=dat_2016$V8, ID=dat_2016$ID, year=2016)

dat<-rbind(dfor2012, dfor2014, dfor2016)

write.csv(dat, "gannet_data_12_14_16_compiled.csv", row.names=F, quote=F)

dat<-read.csv("gannet_data_12_14_16_compiled.csv", h=T)
dat$Date=as.Date(dat$Date, format="%Y-%m-%d")
ext_clean$year<-as.character(ext_clean$year)

dat<-rbind(dat, ext_clean)
dat_ord<-dat[order(dat$ID, dat$year),]

write.csv(dat_ord, "gannet_data_12_14_16_update_compiled.csv", row.names=F, quote=F)


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

birds<-names(sort(table(dat$ID)))

for(i in birds[2:length(birds)])
{
  Temp <- dat[dat$ID==i,]
  
  rm(Trip)
  
  Trip <- tripSplit(Track=Temp, Colony=col_nz, InnerBuff=0.1, ReturnBuff=0.15, Duration=0.25, plotit=F)
  
  write.csv(Trip@data, paste("temp/",i, ".csv", sep="" ), quote=F, row.names=F)
  #if(which(birds==i) == 1) {Trips <- Trip} else
  #  Trips <- spRbind(Trips,Trip)
  
  #print(Sys.time())

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

dat<-na.omit(dat)

dat$temp<-c(1, diff(dat$TrackTime))
dat<-dat[dat$temp!=0,] # remove duplicate points
dat$temp<-NULL

dat$DateTime <- as.POSIXct(dat$DateTime, "%Y-%m-%d %H:%M:%S")

#plot(Latitude~Longitude, dat, cex=0.5, pch=3) # v slow
#points(Latitude~Longitude, dat[dat$trip_id==-1,], col=2, cex=0.5, pch=3)

nrow(dat)
nrow(dat[dat$trip_id==-1,])

dat<-dat[dat$trip_id!=-1,] # remove -1 points

# Read in extra data
#dat<-read.csv("Gabriel_extra_data_tripsplit.csv", h=T)
# -1 already removed.

#dive locations (gap>9 seconds)
dat$TT_diff<-0
for(i in unique(dat$trip_id)){
  dat[dat$trip_id==i,]$TT_diff<-c(0, diff(dat[dat$trip_id==i,]$TrackTime, lag=1))
}

summary(dat$TT_diff)
table(dat$TT_diff)

d1<-data.frame(aggregate(TT_diff~trip_id, dat, mean), 
           tmin=aggregate(TT_diff~trip_id, dat, function(x){min(x[x>0])})[,2],
           tmax=aggregate(TT_diff~trip_id, dat, max )[,2],
           len=aggregate(TT_diff~trip_id, dat, function(x){length(x)})[,2],
           dmin=aggregate(ColDist~trip_id, dat, min, na.rm=T)[,2],
           dmax=aggregate(ColDist~trip_id, dat, max, na.rm=T)[,2])

# for extra dat
dat<-dat[dat$trip_id %in% d1[d1$tmin==1,]$trip_id,]
badz<-c("G46_C_2016-01-07 09-101", 
        "G55_D_2016-01-07 09-101")

dat<-dat[!dat$trip_id %in% badz,]

#points(Latitude~Longitude, dat[dat$TT_diff>120,], col=3, cex=0.5, pch=3) # where are the > 120 gaps??

dat<-dat[dat$TT_diff!=21419,] # kill bad point
dat<-dat[dat$TT_diff!=47440,]

badz<-c("11012012_tag1540_gps.txt2", 
        "G105_GPS & Alt 7_export_2015-01-05 17-561")

dat<-dat[!dat$trip_id %in% badz,]

also_badz<-d1[d1$tmin>2,]$trip_id # birds sampled at more than 2 second interval
dat<-dat[!dat$trip_id %in% also_badz,]

#now rerun d1

d1<-data.frame(aggregate(TT_diff~trip_id, dat, mean), 
             
               tmin=aggregate(TT_diff~trip_id, dat, function(x){min(x[x>0])})[,2],
               tmax=aggregate(TT_diff~trip_id, dat, max )[,2],
               len=aggregate(TT_diff~trip_id, dat, function(x){length(x)})[,2],
               dmin=aggregate(ColDist~trip_id, dat, min, na.rm=T)[,2],
               dmax=aggregate(ColDist~trip_id, dat, max, na.rm=T)[,2])

dat$year<-0

dat[grep("2012", dat$ID), ]$year<-2012
dat[grep("2014", dat$ID), ]$year<-2014
dat[grep("2015", dat$ID), ]$year<-2015
dat[grep("2016", dat$ID), ]$year<-2016

dat$dive<-0

#dat[ dat$year=="2012" & dat$TT_diff%in% (3:8),]$dive<-1 #2012 is every 2 secs so bit different
#dat[ dat$year!="2012" & dat$TT_diff%in% (2:8),]$dive<-1

dat[dat$TT_diff%in% (3:8),]$dive<-1

dat$Vdive<-0
dat$Udive<-0

dat[dat$TT_diff%in% (3:9),]$Vdive<-1 #2012 is every 2 secs so bit different
dat[dat$TT_diff%in% (10:20),]$Udive<-1



write.csv(dat, "gannet_dat_tripsplit_dives_CORRECTED.csv", quote=F, row.names=F) # write full tripsplit dataset

# cant figure out the HMM stuff for now so do descriptor table


trip_distances<-data.frame(tripID=unique(dat$trip_id), Returns="na",
                           max_dist=0, 
                           tot_length=0, 
                           tot_time=0, 
                           foraging_time=0, flying_time=0, resting_time=0,
                           dive_duration=0, dive_duration_sd=0,
                           dives_per_hour=0,Vdives_per_hour=0,
                           Udives_per_hour=0)  	### create data frame for each trip

trip_distances$Returns<-as.character(trip_distances$Returns)


for (i in trip_distances$trip){
  trip_distances[trip_distances$trip==i,3]<-max(dat[dat$trip_id==i,]$ColDist)/1000
  trip_distances[trip_distances$trip==i,5]<-(max(dat[dat$trip_id==i,]$TrackTime)-min(dat[dat$trip_id==i,]$TrackTime))/3600
  #trip_distances[trip_distances$trip==i,5]<-max(dat[dat$trip_id==i,]$ColDist)/1000
  ## Calculate distances from one point to the next and total trip distance
  x=dat[dat$trip_id==i,]
  x$Dist=0
  x$Dist[1]<-x$ColDist[1]/1000				### distance to first point is assumed a straight line from the nest/colony
  for (p in 2:dim(x)[1]){
    p1<-c(x$Longitude[p-1],x$Latitude[p-1])
    p2<-c(x$Longitude[p],x$Latitude[p])
    #x$Dist[p]<-pointDistance(p1,p2, lonlat=T, allpairs=FALSE)/1000			### no longer works in geosphere
    x$Dist[p]<-distMeeus(p1,p2)/1000						### great circle distance according to Meeus, converted to km
    
  }
  trip_distances[trip_distances$trip==i,4]<-sum(x$Dist)+(x$ColDist[p]/1000)	## total trip distance is the sum of all steps plus the dist from the nest of the last location - for non return trips this will be an underestimate
  trip_distances[trip_distances$trip==i,2]<-as.character(unique(dat[dat$trip_id==i,]$Returns))
  trip_distances[trip_distances$trip==i,9]<-mean(dat[dat$trip_id==i & dat$dive==1,]$TT_diff)
  trip_distances[trip_distances$trip==i,10]<-sd(dat[dat$trip_id==i & dat$dive==1,]$TT_diff)
  trip_distances[trip_distances$trip==i,11]<-(sum(dat[dat$trip_id==i,]$dive))/trip_distances[trip_distances$trip==i,5]
  trip_distances[trip_distances$trip==i,12]<-(sum(dat[dat$trip_id==i,]$Vdive))/trip_distances[trip_distances$trip==i,5]
  trip_distances[trip_distances$trip==i,13]<-(sum(dat[dat$trip_id==i,]$Udive))/trip_distances[trip_distances$trip==i,5]
  }

write.csv(trip_distances, "gannet_dat_trip_summary.csv", quote=F, row.names=F) # write full tripsplit dataset

## Have now completed HMM analyses so can update trip table with HMM results

setwd("~/research/gannets")

trip_distances<-read.csv("gannet_dat_trip_summary.csv", h=T)

hmmdat<-rbind(read.csv("FINALdata2012.csv", h=T),
              read.csv("FINALdata2014.csv", h=T),
              read.csv("FINALdata2015.csv", h=T),
              read.csv("FINALdata2016.csv", h=T))

for(i in unique(trip_distances$tripID))
{
trip_distances[trip_distances$tripID==i,6]<-(nrow(hmmdat[hmmdat$states==2 & hmmdat$ID==as.character(i),])*2)/3600
trip_distances[trip_distances$tripID==i,7]<-(nrow(hmmdat[hmmdat$states==3 & hmmdat$ID==as.character(i),])*2)/3600
trip_distances[trip_distances$tripID==i,8]<-(nrow(hmmdat[hmmdat$states==1 & hmmdat$ID==as.character(i),])*2)/3600
print(i)
}

write.csv(trip_distances, "gannet_dat_trip_summary_hmm.csv", quote=F, row.names=F) # write full tripsplit dataset



# read in tripdistances and correct hmm results with 30 second interval model

trip_distances<-read.csv("gannet_dat_trip_summary_hmm.csv",h=T)
hmmdat<-read.csv("trial30sec.csv", h=T)

for(i in unique(trip_distances$tripID))
{
  trip_distances[trip_distances$tripID==i,6]<-(nrow(hmmdat[hmmdat$state_moveHMM==2 & hmmdat$trip_id==as.character(i),])*30)/3600
  trip_distances[trip_distances$tripID==i,7]<-(nrow(hmmdat[hmmdat$state_moveHMM==3 & hmmdat$trip_id==as.character(i),])*30)/3600
  trip_distances[trip_distances$tripID==i,8]<-(nrow(hmmdat[hmmdat$state_moveHMM==1 & hmmdat$trip_id==as.character(i),])*30)/3600
  print(i)
}

write.csv(trip_distances, "gannet_dat_trip_summary_hmm30sec.csv", quote=F, row.names=F) # write full tripsplit dataset


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONVERT TRIPS OBJECT TO TRAJECTORY OBJECT TO CALCULATE TURNING ANGLES ETC.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# clean up the data a bit

#dat<-na.omit(dat)

dat<-read.csv("gannet_dat_tripsplit_dives_CORRECTED.csv", h=T) 

dat$trip_id<-as.character(dat$trip_id)

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

#triplist$ID<-as.character(triplist$ID)

#triplist$birdID<-substr(triplist$ID, 1, (nchar(triplist$ID)-1))

#now run on triplist

triplist$year<-0

triplist[grep("2012", triplist$ID), ]$year<-2012
triplist[grep("2014", triplist$ID), ]$year<-2014
triplist[grep("2015", triplist$ID), ]$year<-2015
triplist[grep("2016", triplist$ID), ]$year<-2016

#try to fit for just 2012

hmm.2<-HMMFit(triplist[triplist$year==2012,1:2],nStates=3)
states<-viterbi(hmm.2, triplist[triplist$year==2012,1:2])

triplist$state3<-NA

triplist[triplist$year==2012,]$state3<-states$states

d2012<-dat[dat$year==2012,]
d2012$HMM3<-triplist[triplist$year==2012,]$state3

write.csv(d2012, "gannet_dat_2012_hmm.csv", quote=F, row.names=F) # write full tripsplit dataset



triplist$state3<-5
for ( i in unique(triplist$year))
{

hmm.2<-HMMFit(triplist[triplist$year==i,1:2],nStates=3)
states<-viterbi(hmm.2, triplist[triplist$year==i,1:2])

triplist[triplist$year==i,]$state3<-states$states
print(i)
}

library(ggplot2)

p<-ggplot(triplist, aes(x=rel.angle, y=speed, colour=state3, shape=state3))
p+geom_point()+facet_wrap(~birdID)


dat$hmm_all_trips<-0
for(i in unique(triplist$birdID))
{
  dat[dat$trip_id==i,]$hmm_all_trips<-triplist[triplist$ID==i,]$state2
} ## loop to match up correct trips in triplist and final_out



########## plot separation between foraging and commuting ##########
plot(speed~rel.angle, data=triplist, type='p',col=triplist$state, pch=triplist$state)
## Nice plot :)

### Trial with moveHMM package

library(moveHMM)
options(digits=12)
rm(list=ls())

setwd("~/research/gannets")
datfull<-read.csv("gannet_dat_tripsplit_dives_CORRECTED.csv", h=T) 

#dat$trip_id<-as.character(dat$trip_id)
dat<-data.frame(y=datfull$Latitude, x=datfull$Longitude, ID=datfull$trip_id, year=datfull$year)

data<-prepData(dat, type="LL")

nrow(data[data$step==0,])/nrow(data)

# force all data to 2 second interval $$%%
sec1dat<- which(dat$year!="2012")
sec2dat<-seq(sec1dat[2], sec1dat[length(sec1dat)], 2)

dat2sec<-dat[c(which(dat$year=="2012"), sec2dat),]

data2sec<-prepData(dat2sec, type="LL")

nrow(data2sec[data2sec$step==0,])/nrow(data2sec)
# ################################### $$%%



#trial to remove 0 step from 2 sec data
temp<-data.frame(data2sec)
temp<-temp[temp$step>0,]
temp<-temp[-which(is.na(temp$ID)),]
unique(temp$ID)

tempdata<-prepData(data.frame(x=temp$x, y=temp$y,
                    ID=temp$ID, year=temp$year), type="LL")

mu0 <- c(0.0001,0.01,0.03)
sigma0 <- c(0.0001,0.01,0.01)
#zeromass0 <- c(0.01,0.0001,0.0001)
stepPar0 <- c(mu0,sigma0)
angleMean0 <- c(0,pi,0)
kappa0 <- c(2,1,2)
anglePar0 <- c(angleMean0,kappa0)

temp12<- fitHMM(data=tempdata[tempdata$year=="2012",],nbStates=3,stepPar0=stepPar0,
                anglePar0=anglePar0)

#####

#data2012<-data2012[data2012$step>0,]
#data2012<-data2012[!is.na(data2012$ID),]

plot(data2012)

##experimental 4 parameter
mu0 <- c(0.0001,0.01, 0.03, 0.03)
sigma0 <- c(0.0001,0.01,0.03, 0.01)
zeromass0 <- c(0.01,0.01, 0.0001, 0.0001)
stepPar0 <- c(mu0,sigma0, zeromass0)
angleMean0 <- c(0,pi,pi, 0)
kappa0 <- c(2,1,1, 2)
anglePar0 <- c(angleMean0,kappa0)

# parameters set up for 2 second interval data
# CURRENT runs with:2012, 2014, 2015
mu0 <- c(0.0001,0.01,0.03)
sigma0 <- c(0.0001,0.01,0.03)
zeromass0 <- c(0.01,0.0001,0.0001)
stepPar0 <- c(mu0,sigma0, zeromass0)
angleMean0 <- c(0,pi,0)
kappa0 <- c(2,1,2)
anglePar0 <- c(angleMean0,kappa0)

m3zm <- fitHMM(data=data2sec[data2sec$year=="2012",],nbStates=3,stepPar0=stepPar0,
             anglePar0=anglePar0)

out<-data2sec[data2sec$year=="2012",]
states<-viterbi(m3zm)
out$states<-states
write.csv(out, "FINALdata2012.csv", quote=F, row.names=F)

### 2014
m3zm14 <- fitHMM(data=data2sec[data2sec$year=="2014",],nbStates=3,
                 stepPar0=stepPar0, anglePar0=anglePar0)

statesb<-viterbi(m3zm14)
data2014<-data2sec[data2sec$year=="2014",]
data2014$states<-statesb
write.csv(data2014, "FINALdata2014.csv", quote=F, row.names=F)
########

######### 2015 ###########
for(i in unique(data2sec[data2sec$year=="2016",]$ID))
{
t1<-Sys.time()  
m3c <- fitHMM(data=data2sec[data2sec$ID== i,] ,nbStates=3,stepPar0=stepPar0,
              anglePar0=anglePar0)
print(i)
print(Sys.time()-t1)
print(nrow(data2sec[data2sec$ID== i,]))
}

#data2015sec<-data2sec[data2sec$ID!="G112_GPS & Alt 4_export_2015-01-09 06-512",]

m3c <- fitHMM(data=data2sec[data2sec$year=="2015",] ,nbStates=3,stepPar0=stepPar0,
              anglePar0=anglePar0)

statesc<-viterbi(m3c)

data2015<-data2sec[data2sec$year=="2015",]
data2015$states<-statesc
write.csv(data2015, "FINALdata2015.csv", quote=F, row.names=F)
##########

### 2016

mu0 <- c(0.0001,0.01,0.03)
sigma0 <- c(0.0001,0.01,0.01)
zeromass0 <- c(0.1,0.01,0.0001)
stepPar0 <- c(mu0,sigma0, zeromass0)
angleMean0 <- c(0,pi,0)
kappa0 <- c(2,1,2)
anglePar0 <- c(angleMean0,kappa0)

m3zm16 <- fitHMM(data=data2sec[data2sec$year=="2016",] ,nbStates=3,stepPar0=stepPar0,
              anglePar0=anglePar0)

statesd<-viterbi(m3zm16)
data2016<-data2sec[data2sec$year=="2016",]
data2016$states<-statesd
write.csv(data2016, "FINALdata2016.csv", quote=F, row.names=F)
########


dat[grep("2016", dat$ID), ]$state_moveHMM<-statesd


### model results area.

### 2014 ###
#Value of the maximum log-likelihood: 426133.834515 

#Step length parameters:
#  ----------------------
#  state 1          state 2           state 3
#mean      0.001060388544887 0.00711020469720 2.68409810635e-02
#sd        0.000215839250878 0.00551673856052 6.75550875692e-03
#zero-mass 0.589771673342178 0.02209703681139 1.23515377248e-09

#Turning angle parameters:
#------------------------
#  state 1         state 2           state 3
#mean          0.0105822818827 -0.274061749418 7.26172819971e-04
#concentration 0.9602575853541  0.238148394780 1.22134428844e+01

#Regression coeffs for the transition probabilities:
#  --------------------------------------------------
#  1 -> 2         1 -> 3         2 -> 1        2 -> 3
#intercept -2.72375710942 -62.0367238171 0.779706028724 -1.5353731961
#3 -> 1         3 -> 2
#intercept -66.1500209392 -4.07350518541

#Transition probability matrix:
#  -----------------------------
#  [,1]            [,2]              [,3]
#[1,] 9.38414026883e-01 0.0615859731167 1.07198241607e-27
#[2,] 6.42137382988e-01 0.2944461824439 6.34164345677e-02
#[3,] 1.83688701942e-29 0.0167328798580 9.83267120142e-01

#Initial distribution:
#  --------------------
#  [1] 0.0748181011654 0.0807992405701 0.8443826582645

#### 2015
#Value of the maximum log-likelihood: 1341965.0508 

#Step length parameters:
#  ----------------------
#  state 1           state 2           state 3
#mean      0.00563747270088 0.001201348671739 2.56695545871e-02
#sd        0.00467913747166 0.000422816580647 8.55647659535e-03
#zero-mass 0.56154092673888 0.217167334264157 3.37514223617e-07

#Turning angle parameters:
#  ------------------------
#  state 1         state 2          state 3
#mean          -0.0205061664933 0.0386428514213 0.00302798909852
#concentration  1.6894858051066 0.3646081544795 7.33681859339482

#Regression coeffs for the transition probabilities:
#  --------------------------------------------------
#  1 -> 2         1 -> 3         2 -> 1         2 -> 3
#intercept -1.38743146147 -3.60368341209 -2.94392692004 -54.8119051114
#3 -> 1         3 -> 2
#intercept -4.40222729552 -136.234542792

#Transition probability matrix:
#  -----------------------------
#  [,1]              [,2]              [,3]
#[1,] 0.7831226729741 1.95558172501e-01 2.13191545253e-02
#[2,] 0.0500243284140 9.49975671586e-01 1.49006272569e-24
#[3,] 0.0121017779855 6.74220610603e-60 9.87898222014e-01

#Initial distribution:
#  --------------------
#  [1] 0.0142118490407 0.2183602627886 0.7674278881707

#### 2012 #####
#Value of the maximum log-likelihood: 642099.289486 
#
#Step length parameters:
#  ----------------------
#  state 1           state 2           state 3
#mean      8.67790263643e-04 1.59362073798e-02 3.32131383924e-02
#sd        6.14139804991e-04 7.44058174462e-03 1.12715236953e-02
#zero-mass 4.57035638849e-05 2.89663909885e-09 1.59314957015e-09
#
#Turning angle parameters:
#  ------------------------
#  state 1          state 2           state 3
#mean          -0.00789793324085 -0.0152422869147 -0.00606858397492
#concentration  1.21442775811726  2.0770794283524  9.07597072194251
#
#Regression coeffs for the transition probabilities:
#  --------------------------------------------------
#  1 -> 2         1 -> 3        2 -> 1         2 -> 3
#intercept -5.61173862568 -22.7622352717 -4.1625050017 -2.59027765674
#3 -> 1         3 -> 2
#intercept -20.6253671852 -2.70408483836
#
#Transition probability matrix:
#  -----------------------------
#  [,1]             [,2]              [,3]
#[1,] 9.96358598477e-01 0.00364140139339 1.29688808065e-10
#[2,] 1.42756011938e-02 0.91695360028346 6.87707985228e-02
#[3,] 1.03366613101e-09 0.06273274888688 9.37267250079e-01
#
#Initial distribution:
#  --------------------
#  [1] 8.70160792864e-08 3.43080997796e-10 9.99999912641e-01





# parameters set up for 1 second interval 2014,15,16 data
#mu0 <- c(0.0001,0.001,0.01)
#sigma0 <- c(0.0001,0.001,0.01)
#zeromass0 <- c(0.000001,0.0000001,0.0000000001)
#stepPar0 <- c(mu0,sigma0, zeromass0)
#angleMean0 <- c(0,pi,0)
#kappa0 <- c(1,1,1)
#anglePar0 <- c(angleMean0,kappa0)

## Attempt a 2 state model
# parameters set up for 2 second interval data
# CURRENT runs with:2012, 2014, 2015, 2016
mu0 <- c(0.01,0.03)
sigma0 <- c(0.01,0.03)
zeromass0 <- c(0.01,0.0001)
stepPar0 <- c(mu0,sigma0, zeromass0)
angleMean0 <- c(pi,0)
kappa0 <- c(1,2)
anglePar0 <- c(angleMean0,kappa0)

m3zm4 <- fitHMM(data=data2sec[data2sec$year=="2012",],nbStates=2,stepPar0=stepPar0,
                anglePar0=anglePar0)

out<-data2sec[data2sec$year=="2012",]
states<-viterbi(m3zm)
out$states<-states
write.csv(out, "new2sec2012.csv", quote=F, row.names=F)

### 2014
m3zm14 <- fitHMM(data=data2sec[data2sec$year=="2014",],nbStates=2,
                 stepPar0=stepPar0, anglePar0=anglePar0)

statesb<-viterbi(m3zm14)
data2014<-data2sec[data2sec$year=="2014",]
data2014$states<-statesb
write.csv(data2014, "FINALdata2014.csv", quote=F, row.names=F)
########

######### 2015 ###########
for(i in unique(data2sec[data2sec$year=="2016",]$ID))
{
  t1<-Sys.time()  
  m3c <- fitHMM(data=data2sec[data2sec$ID== i,] ,nbStates=3,stepPar0=stepPar0,
                anglePar0=anglePar0)
  print(i)
  print(Sys.time()-t1)
  print(nrow(data2sec[data2sec$ID== i,]))
}

#data2015sec<-data2sec[data2sec$ID!="G112_GPS & Alt 4_export_2015-01-09 06-512",]

m3c <- fitHMM(data=data2sec[data2sec$year=="2015",] ,nbStates=3,stepPar0=stepPar0,
              anglePar0=anglePar0)

statesc<-viterbi(m3c)

data2015<-data2sec[data2sec$year=="2015",]
data2015$states<-statesc
write.csv(data2015, "FINALdata2015.csv", quote=F, row.names=F)
##########

### 2016
m3zm16 <- fitHMM(data=data2sec[data2sec$year=="2016",] ,nbStates=3,stepPar0=stepPar0,
                 anglePar0=anglePar0)

statesd<-viterbi(m3zm16)
data2016<-data2sec[data2sec$year=="2016",]
data2016$states<-statesd
write.csv(data2016, "FINALdata2016.csv", quote=F, row.names=F)
########


dat[grep("2016", dat$ID), ]$state_moveHMM<-statesd


# need to work out what is messing up the 'export_2015' trips
#data2015<-prepData(dat[ grep("export_2015" , dat$ID), ], type="LL", coordNames=c("Longitude", "Latitude"))
#m3c <- fitHMM(data=data2015 ,nbStates=3,stepPar0=stepPar0,
#              anglePar0=anglePar0)
#statesc<-viterbi(m3c)
#dat[grep("export_2015", dat$ID), ]$state_moveHMM<-statesc

data2016<-prepData(dat[ grep("2016", dat$ID), ], type="LL", coordNames=c("Longitude", "Latitude"))

data2016<-data2016[data2016$step>0,]
data2016<-data2016[!is.na(data2016$ID),]
                          
m3d <- fitHMM(data=data2016 ,nbStates=3,stepPar0=stepPar0,
              anglePar0=anglePar0)

statesd<-viterbi(m3d)

dat[grep("2016", dat$ID), ]$state_moveHMM<-statesd


options(digits=12)
rm(list=ls())

setwd("~/research/gannets")
datfull<-read.csv("gannet_dat_tripsplit_dives_CORRECTED.csv", h=T) 

#force all to 2 second
sec1dat<- which(datfull$year!="2012")
sec2dat<-seq(sec1dat[2], sec1dat[length(sec1dat)], 2)

dat2sec<-datfull[c(which(datfull$year=="2012"), sec2dat),]

# In the same way as above we now resample to longer intervals by hacking out
# some data. First try with 30 second interval on a trip by trip basis

dat30sec<-NULL
for ( i in unique(dat2sec$trip_id))
{
  d1<-dat2sec[dat2sec$trip_id==i,]
  d2<-d1[seq(1,nrow(d1), 15),]
  dat30sec<-rbind(dat30sec, d2)
  print(i)
}

#try model on 30 sec interval data
library(moveHMM)

tempdata<-prepData(data.frame(x=dat30sec$Longitude, y=dat30sec$Latitude,
                              ID=dat30sec$trip_id, year=dat30sec$year), type="LL")

mu0 <- c(0.01,0.1,0.3)
sigma0 <- c(0.01,0.1,0.3)
zeromass0 <- c(0.1,0.01,0.01)
stepPar0 <- c(mu0,sigma0, zeromass0)
angleMean0 <- c(0,pi,0)
kappa0 <- c(2,1,2)
anglePar0 <- c(angleMean0,kappa0)

temp30<- fitHMM(data=tempdata, nbStates=3,stepPar0=stepPar0,
                anglePar0=anglePar0)

states30<-viterbi(temp30)

dat30sec$state_moveHMM<-states30

dat30sec$TT_diff<-NULL
dat30sec$dive<-NULL

write.csv(dat30sec, "trial30sec.csv", row.names=F, quote=F)



## set the poor computer off resampling the data to 30 second interval
## this approach doesnt work.. too slow

source("~/research/seabird_analyses/Heron_Island/heron_analyses_r_GPS/Resample.r")

## set up to not interpolate between points > 1 hr apart

tripz2012<-unique(dat2sec[dat2sec$year=="2012",]$trip_id)
tripz2014<-unique(dat2sec[dat2sec$year=="2014",]$trip_id)
tripz2015<-unique(dat2sec[dat2sec$year=="2015",]$trip_id)
tripz2016<-unique(dat2sec[dat2sec$year=="2016",]$trip_id)



results<-NULL
for(i in tripz2014)
{
  Track<-dat2sec[dat2sec$trip_id == i,]
  
  #Track<-Track[-which(duplicated(Track$TrackTime)==TRUE),]
  #print(paste(length(which(duplicated(Track$TrackTime)==TRUE)), "duplicates removed", sep=" "))
  
  resample_output<-resample(Track, timeStep=0.0166666666667)  ## timeStep set for
                                                              ## 1 min intervals
  
  #readline("ok")
  results<-rbind(results,resample_output)
  print(i)
}

#write.csv(results, "data2012_resampled1min.csv", quote=F, row.names=F)
#write.csv(results, "data2014_resampled1min.csv", quote=F, row.names=F)


plot(Latitude~Longitude, results, pch=16, cex=0.4, col=Bird_ID)
map("worldHires", add=T, col=3)


results$DateGMT <- as.Date(as.POSIXlt(results$TrackTime, origin="1970-01-01", "GMT"))
results$TimeGMT <- format((as.POSIXlt(results$TrackTime, origin="1970-01-01", "GMT")), "%H:%M:%S")

results$DateTime2 <- paste(results$DateGMT, results$TimeGMT, sep= " ")
results$DateTime2 <- as.POSIXct(strptime(results$DateTime2, "%Y-%m-%d %H:%M:%S"), "GMT")
results$TrackTime2 <- as.double(results$DateTime2)

## 




  