rm(list=ls())
setwd("~/grive/phd/analyses/BRBO_raine")
library(ggplot2)

regurg_assn<-read.csv("BRBO_regurg_times_assigned.csv", h=T)

sel_trax<-read.csv("BRBO_diet_anal_trax_forage_binned.csv", h=T)
sel_trax$DateTime<-as.POSIXct(strptime(sel_trax$DateTime, "%Y-%m-%d %H:%M:%S"), "GMT")

# for each gps position calc the time since diet sample collected


timez<-data.frame(trip_id=aggregate(data=sel_trax, DateTime~trip_id, FUN=min)[,1],
                  start=aggregate(data=sel_trax, DateTime~trip_id, FUN=min)[,2],
                  end=aggregate(data=sel_trax, DateTime~trip_id, FUN=max)[,2], diet_time=c("23:00",
                  "21:30", "21:16", "23:40", "20:13", "21:10"))

timez$dietDateTime<-as.POSIXct(strptime(paste(substr(timez$start, 1,10), 
                              timez$diet_time), "%Y-%m-%d %H:%M"), "GMT")

sel_trax$t_from_dsample<-0
for(i in 1:nrow(sel_trax))
{
 sel_trax[i,]$t_from_dsample=
   as.numeric(timez[timez$trip_id==sel_trax[i,]$trip_id,]$dietDateTime-
   sel_trax[i,]$DateTime, unit="hours")
}

sel_trax$S_count<-0
sel_trax$FF_count<-0

# Digest code   T squid   T flyfish  
#     1           0-4 hr    0-2 hr         
#     2           4-6 hr    2-4 hr
#     3           6-8 hr    4-6 hr
#     4           8+ hr     6+ hr

# revised times using CIs
# Digest code   T squid   T flyfish  
#     1           0-3.9 hr      0-2.5 hr (2)        
#     2           2.5-7.7 hr    1.4-4.8 hr
#     3           7.5-11.6 hr   2.7-7.3 hr
#     4           10+ hr        5.5+ hr

# revised times using CIs
# Digest code   T squid   T flyfish  
#     1           0-3.8 hr      0-2.3 hr (2)        
#     2           2.2-7.7 hr    1.4-4.6 hr
#     3           6.4**-14 hr   2.8-6.8 hr   ** should be 6.7 and 11.4 but accomodate
#     4           9+ hr        5.7+ hr

# might also be interesting to link prey size/weight to areas
for(i in 1:nrow(regurg_assn))
{
  if(regurg_assn[i,]$Species=="FF" & regurg_assn[i,]$Digestion.code==1){capture_bin<-c(0,2.3)}
  if(regurg_assn[i,]$Species=="FF" & regurg_assn[i,]$Digestion.code==2){capture_bin<-c(1.4,4.6)}
  if(regurg_assn[i,]$Species=="FF" & regurg_assn[i,]$Digestion.code==3){capture_bin<-c(2.8,6.8)}
  if(regurg_assn[i,]$Species=="FF" & regurg_assn[i,]$Digestion.code==4){capture_bin<-c(5.7,24)}
  
  if(regurg_assn[i,]$Species=="S" & regurg_assn[i,]$Digestion.code==1){capture_bin<-c(0,3.8)}
  if(regurg_assn[i,]$Species=="S" & regurg_assn[i,]$Digestion.code==2){capture_bin<-c(2.2,7.7)}
  if(regurg_assn[i,]$Species=="S" & regurg_assn[i,]$Digestion.code==3){capture_bin<-c(6.4,14)}
  if(regurg_assn[i,]$Species=="S" & regurg_assn[i,]$Digestion.code==4){capture_bin<-c(9,24)}

  
  if(regurg_assn[i,]$Species=="S"){
    sel_trax[sel_trax$trip_id==regurg_assn[i,]$trip_id & 
    sel_trax$t_from_dsample>= capture_bin[1] &
    sel_trax$t_from_dsample<= capture_bin[2] & 
    sel_trax$hmm_all_trips3!=3,]$S_count=
      sel_trax[sel_trax$trip_id==regurg_assn[i,]$trip_id & 
      sel_trax$t_from_dsample>= capture_bin[1] &
      sel_trax$t_from_dsample<= capture_bin[2] & 
      sel_trax$hmm_all_trips3!=3,]$S_count+1}
 
  if(regurg_assn[i,]$Species=="FF"){
    sel_trax[sel_trax$trip_id==regurg_assn[i,]$trip_id & 
    sel_trax$t_from_dsample>= capture_bin[1] &
    sel_trax$t_from_dsample<= capture_bin[2] & 
    sel_trax$hmm_all_trips3!=3,]$FF_count=
      sel_trax[sel_trax$trip_id==regurg_assn[i,]$trip_id & 
      sel_trax$t_from_dsample>= capture_bin[1] &
      sel_trax$t_from_dsample<= capture_bin[2] & 
      sel_trax$hmm_all_trips3!=3,]$FF_count+1}
  
  if(nrow(sel_trax[sel_trax$trip_id==regurg_assn[i,]$trip_id & 
             sel_trax$t_from_dsample>= capture_bin[1] &
             sel_trax$t_from_dsample<= capture_bin[2] & 
             sel_trax$hmm_all_trips3!=3,])==0){print(paste(i, "'s capture bin does not fit into tracked data"))}
  
  print(i)
}

# ok so for the moment there are a couple of issues basically all to
# do with linking our regression timings to the foraging tracks and
# general mis-alighnment. 1 thing to look at digestion level 4 prey in the 
# stomach on a trip that is too short, for bird 04_12_14_23Z_M this is
# certainly the case as it only had a max of 4 hrs from when sampled
# for diet? Surely these came from the previous days trip? if so they
# should be removed from the regression analyses.. need to get intel
# from Gab. The above loop works, it removes any samples that are too
# old (or young...hmmm) to fit in the tracking, some of these are real,
# some artifacts of bad lining up. even so write out and have a look

write.csv(sel_trax, "BRBO_diet_anal_trax_forage_applied2.csv", quote=F, row.names=F)