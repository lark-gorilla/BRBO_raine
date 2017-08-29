## Import BRBO raine tracking and so some exploratory analyses. Link tracked birds to
## diet samples and run HMM to identify foraging areas

#install.packages("RHmm", repos="http://R-Forge.R-project.org")

setwd("~/grive/phd/analyses/BRBO_raine")

library(ggplot2)
library(geosphere)
library(RHmm)
library(adehabitatLT)

dat<-read.csv("~/grive/phd/fieldwork/Raine_Dec_2014/data/tracking_results/Raine_tracking_trips_clean.csv")

q<-ggplot(aes(Longitude, Latitude), data=dat)
q+geom_point()+geom_point(data=dat[dat$trip_id=="-1",], colour="red")

dat<-dat[dat$trip_id!="-1",]             
### 1. project_data

mid_point<-data.frame(centroid(cbind(dat$Longitude, dat$Latitude)))
final_out.Wgs <- SpatialPoints(data.frame(dat$Longitude, dat$Latitude), proj4string=CRS("+proj=longlat"))
DgProj <- CRS(paste("+proj=laea +lon_0=", mid_point$lon, " +lat_0=", mid_point$lat, sep=""))
final_out.Projected <- spTransform(final_out.Wgs, CRS=DgProj)
#now we have projected coords for WHOLE dataset (rather than cols split)

plot(final_out.Projected)

### 3. Convert to LTRAJ
dat$trip_id<-as.character(dat$trip_id) #for as.ltraj the -1 trip_id is still considered if trip_id is a factor

trajectories <- as.ltraj(xy=data.frame(final_out.Projected@coords[,1],
                                       final_out.Projected@coords[,2]), date=as.POSIXct(dat$TrackTime, origin="1970/01/01", tz="GMT"), id=dat$trip_id, typeII = TRUE)   


tt<-summary.ltraj(trajectories)
trips<-tt$id
head(trajectories[[1]])
plot(trajectories)

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

hmm.3<-HMMFit(triplist[,1:2],nStates=3)		## fits a hidden markov model with 3 states - foraging and commuting and sleeping?
states3<-viterbi(hmm.3,triplist[,1:2])		## extracts the predicted states for each location fix using Viterbi's algorith from the HMM fit
triplist$state3<-states3$states

hmm.2<-HMMFit(triplist[,1:2],nStates=2)		## fits a hidden markov model with 3 states - foraging and commuting
states2<-viterbi(hmm.2,triplist[,1:2])		## extracts the predicted states for each location fix using Viterbi's algorith from the HMM fit
triplist$state2<-states2$states

dat$hmm_all_trips2<-0
dat$hmm_all_trips3<-0
for(i in trips)
{
  dat[dat$trip_id==i,]$hmm_all_trips2<-triplist[triplist$ID==i,]$state2
  dat[dat$trip_id==i,]$hmm_all_trips3<-triplist[triplist$ID==i,]$state3
} ## loop to match up correct trips in triplist and final_out


########## plot separation between foraging and commuting ##########
plot(speed~rel.angle, data=triplist, type='p',col=triplist$state2, pch=triplist$state2)

plot(speed~rel.angle, data=triplist, type='p',col=triplist$state3, pch=triplist$state3)

#### write data out

write.csv(dat, "BRBO_raine_hmm.csv", quote=F, row.names=F)

Trips.Wgs <- SpatialPoints(data.frame(dat$Longitude, dat$Latitude), proj4string=CRS("+proj=longlat"))

Trips.Wgs<- SpatialPointsDataFrame(Trips.Wgs, data = data.frame(
  trip_id=dat$trip_id, hmm_state=dat$hmm_all_trips2))
# export with only 2 state hmm as more logical
writeOGR(Trips.Wgs, layer="BRBO_raine_HMMstate", dsn="GIS", driver="ESRI Shapefile", verbose=TRUE, overwrite=T)
# kml for online viewing, kill trip_id field to keep file <5mb for map upload
Trips.Wgs$trip_id<-NULL
setwd("~/grive/phd/analyses/BRBO_raine/GIS")
writeOGR(Trips.Wgs, "BRBO_raine_HMMstate.kml", "GIS", driver="KML", overwrite_layer=T)

## creating trip summary table ##
library(geosphere)

dat<-read.csv("~/grive/phd/analyses/BRBO_raine/BRBO_raine_hmm.csv", h=T)

trip_distances<-data.frame(tripID=unique(dat$trip_id), Returns="na",
                           max_dist=0, 
                           tot_length=0, 
                           tot_time=0, 
                           foraging_time=0, flying_time=0, resting_time=0,
                           dive_duration=0, dive_duration_sd=0,
                           dives_per_hour=0)  	### create data frame for each trip

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
}


for(i in unique(trip_distances$tripID))
{
  trip_distances[trip_distances$tripID==i,6]<-(nrow(dat[dat$hmm_all_trips3==2 & dat$trip_id==as.character(i),]))/60
  trip_distances[trip_distances$tripID==i,7]<-(nrow(dat[dat$hmm_all_trips3==3 & dat$trip_id==as.character(i),]))/60
  trip_distances[trip_distances$tripID==i,8]<-(nrow(dat[dat$hmm_all_trips3==1 & dat$trip_id==as.character(i),]))/60
  print(i)
}

write.csv(trip_distances, "~/grive/phd/analyses/BRBO_raine/BRBO_raine_summary.csv", quote=F, row.names=F) # write full tripsplit dataset

# Add mean dist to col that foraging occured
trip_distances<-read.csv("~/grive/phd/analyses/BRBO_raine/BRBO_raine_summary.csv", h=T)

trip_distances$ave_dcol_foraging<-0

for(i in trip_distances$tripID)
{
  trip_distances[trip_distances$tripID==i,]$ave_dcol_foraging<-
    mean(dat[dat$trip_id==i & dat$hmm_all_trips3==2,]$ColDist)
}

write.csv(trip_distances, "~/grive/phd/analyses/BRBO_raine/BRBO_raine_summary.csv", quote=F, row.names=F) # write full tripsplit dataset

