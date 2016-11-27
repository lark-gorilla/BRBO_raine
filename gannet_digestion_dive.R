# Use gannet prey digestion times to bin dive locations into 
# likely prey capture areas based on dive timestamp


rm(list=ls())
setwd("~/research/gannets")

## need to subsample full dataset for only those with regurg data
# then kill unneeded columns to reduce file size
datfull<-read.csv("gannet_dat_tripsplit_dives_CORRECTED.csv", h=T)

# First we ammend dive column as per gabriel's suggestions
# Now its 3:8 seconds not 2:8
table(datfull$dive)
datfull$dive<-0
datfull[ datfull$TT_diff%in% (3:8),]$dive<-1
table(datfull$dive)

sel_trax1415<-datfull[with(datfull, trip_id== 'G34_GPS&Alt_export_2014-12-23 07-501'| 
                         trip_id == 'G49_GPS & Alt4_export_2014-12-25 08-321'| trip_id== 'G57_GPS_Alt 4_export_2014-12-27 06-591'|
                         trip_id == 'G57_GPS_Alt 4_export_2014-12-27 06-592'| trip_id =='G57_GPS_Alt 4_export_2014-12-27 06-593'| 
                         trip_id== 'G79_GPS & Alt 3_export_2015-01-01 06-551'| 
                         trip_id == 'G80_GPS & Alt 4_export_2015-01-01 07-231'| trip_id== 'G89_GPS & Alt 3_export_2015-01-03 07-541'|
                         trip_id == 'G95_GPS & Alt 3_export_2015-01-04 08-271'| trip_id =='G95_GPS & Alt 3_export_2015-01-04 08-272'| 
                         trip_id== 'G102_GPS & Alt 4_export_2015-01-05 08-571'| trip_id=='G102_GPS & Alt 4_export_2015-01-05 08-572'|  
                         trip_id == 'G103_GPS & Alt 2_export_2015-01-05 09-211'| trip_id== 'G105_GPS & Alt 7_export_2015-01-05 10-551'|
                         trip_id == 'G108_GPS & Alt 3_export_2015-01-07 07-481'| trip_id =='G112_GPS & Alt 4_export_2015-01-09 06-511'| 
                         trip_id=='G112_GPS & Alt 4_export_2015-01-09 06-512'|
                         trip_id == 'G11_2015-12-28 14-48.csv1'| trip_id== 'G11_2015-12-28 14-48.csv2'|
                         trip_id == 'G12_2015-12-28 15-04.csv1'| trip_id =='G12_2015-12-28 15-04.csv2'|
                         trip_id == 'G22_A_2015-12-29 15-59.csv1'| trip_id== 'G22_A_2015-12-29 15-59.csv2'|trip_id=='G22_A_2015-12-29 15-59.csv3'|
                         trip_id == 'G27_A_2015-12-30 05-05.csv1'| trip_id =='G27_A_2015-12-30 05-05.csv2'|trip_id=='G27_A_2015-12-30 05-05.csv3'|
                         trip_id == 'G36_A_2015-12-31 09-33.csv1'| trip_id =='G36_A_2015-12-31 09-33.csv2' |trip_id=='G34_A_2015-12-31 07-04.csv1' 
                         ), ]

sel_trax<-rbind(datfull[datfull$year==2012,],
                sel_trax1415,
                datfull[datfull$year==2016,])
# take all of 2012 and 2016 and selected 2014 and 2015 
datfull<-NULL

## remove non-returning trips
sel_trax<-sel_trax[sel_trax$Returns!='N',]

# If trip 2 is large one then remove trip 1
sel_trax<-sel_trax[with(sel_trax, trip_id!='10012012_tag1541_gps.txt1'& trip_id!='G102_GPS & Alt 4_export_2015-01-05 08-571'&
                          trip_id!= 'G22_A_2015-12-29 15-59.csv1'& trip_id!= 'G27_A_2015-12-30 05-05.csv1'),]
#remove unneeded columns
sel_trax$X<-NULL
sel_trax$Y<-NULL
sel_trax$Returns<-NULL
sel_trax$ColDist<-NULL
sel_trax$TT_diff<-NULL
sel_trax$Date<-NULL
sel_trax$Time<-NULL

write.csv(sel_trax, "sel_trax_regurg_dive_3_8_sec_loc.csv", quote=F, row.names=F)

# when viewed in QGIS there seem to be many 'dives' that take place in 
# open water when according to the hmm the bird was flying, these could be
# correct or a loss of signal for another reason.
# will try to clean up using HMM: for i dive location give nearest
# HMM point; if HMM point = flying then remove dive point.

# Will implement the above approach if first stab without doesnt work

# Update: Gabrials' suggestion to use 3:8 secs as the dive interval kinda
# clears up the above issue

sel_trax$DateTime<-as.POSIXct(strptime(sel_trax$DateTime, "%Y-%m-%d %H:%M:%S"), "GMT")
sel_trax$trip_id<-factor(sel_trax$trip_id) # remove unused factors 
sel_trax$ID<-factor(sel_trax$ID) # remove unused factors 

# if aggregetae by ID (track ID) then for 3 trips that have a short 2nd trips(.5 hr)
# will have these times added on
timez<-data.frame(ID=aggregate(data=sel_trax, DateTime~ID, FUN=min)[,1],
                  start=aggregate(data=sel_trax, DateTime~ID, FUN=min)[,2],
                  end=aggregate(data=sel_trax, DateTime~ID, FUN=max)[,2])
timez$duration_hrs<-difftime(timez$end, timez$start, units="hours")
timez

#read in regurg data

regurg<-read.csv("gannet_regurg_times_combined.csv", h=T)
# ok they match
match( timez$ID, unique(regurg$ID))
# add effective diet sample time
raw_dat<-read.csv('gannet_data_12_14_16_compiled.csv',h=T)
raw_dat<-raw_dat[raw_dat$ID %in% timez$ID,]
raw_dat$DateTime<-as.POSIXct(strptime(paste(raw_dat$Date, raw_dat$Time), "%Y-%m-%d %H:%M:%S"), "GMT")
timez$raw_end=aggregate(data=raw_dat, DateTime~ID, FUN=max)[,2]
# ok atm raw end looks too late for most - ask gab
timez$dietDateTime<-as.POSIXct(strptime(timez$end, "%Y-%m-%d %H:%M:%S"), "GMT")
# for moment use end time of track as diet sample time

# for each dive position calc the time since diet sample collected
#subset to sel_trax just dives

#set both to character to stop factor conflicts
regurg$ID<-as.character(regurg$ID)

dive_trax<-sel_trax[sel_trax$dive==1,]
dive_trax$ID<-as.character(dive_trax$ID)


dive_trax$t_from_dsample<-0
for(i in 1:nrow(dive_trax))
{
  dive_trax[i,]$t_from_dsample=
    as.numeric(timez[timez$ID==dive_trax[i,]$ID,]$dietDateTime-
    dive_trax[i,]$DateTime,  unit="hours")
}

dive_trax$n_Anchovy<-0
dive_trax$n_Pilchard<-0
dive_trax$n_Saury<-0
dive_trax$n_JackMackrel<-0
dive_trax$n_Squid<-0
dive_trax$n_Garfish<-0
dive_trax$n_YellowEyeMullet<-0

# B and YJ ?

# Digest code   Fish   
#     1           0-1 hr         
#     2           1-2 hr    
#     3           2-4 hr    
#     4           4-6 hr
#     5           >6 hr

# need to redefine digestion codes to match data

qplot(data=dive_trax, x=t_from_dsample)+facet_wrap(~trip_id)

# might also be interesting to link prey size/weight to areas
regurg_trial<-regurg
regurg_trial$t1<-0 # 0-1,1-2,2-4,4-6
regurg_trial$t2<-0 # 0-2,2-3,3-5,5-7
regurg_trial$t3<-0 # 0-2,2-4,4-6,6-9
regurg_trial$t4<-0 # 0-2,2-5,5-8,8-11
regurg_trial$t5<-0

for(i in 1:nrow(regurg_trial))
    {
  if(regurg_trial[i,]$Digestion.code==1){capture_bin<-c(0,2)}
  if(regurg_trial[i,]$Digestion.code==2){capture_bin<-c(2,5)}
  if(regurg_trial[i,]$Digestion.code==3){capture_bin<-c(5,8)}
  if(regurg_trial[i,]$Digestion.code==4){capture_bin<-c(8,11)}
  
  regurg_trial[i,]$t3<-
          nrow(dive_trax[dive_trax$ID==regurg_trial[i,]$ID & 
          dive_trax$t_from_dsample>= capture_bin[1] &
          dive_trax$t_from_dsample<= capture_bin[2],])
    }
  

regurg3<-rbind(regurg2, regurg2)
regurg3$tdiff_both<-c(regurg2$tdiff_last, regurg2$tdiff_first)

m1<-lm(Digestion.code~tdiff_both, data=regurg3[regurg3$Species=="FF",])
library(lme4)

m1<-lmer(Digestion.code~tdiff_both+(1|trip_id), data=regurg3[regurg3$Species=="FF",])
summary(m1)
plot(m1)

d1<-data.frame(pred=predict(m1, newdata=data.frame(tdiff_both=seq(2.5,8,0.2)), type="response", re.form=~0),
               tdiff_both=seq(2.5,8,0.2))


p<-ggplot(data=regurg3[regurg3$Species=="FF",], aes(y=Digestion.code, x=tdiff_both))
p+geom_jitter(aes(colour=trip_id), height=0.2, width=0)+geom_line(data=d1, aes(x=tdiff_both, y=pred))

# absences explained.
# G11_2015 has second part of foraging trip after long rest ok only 3 points
# G41 is not in sel_trax, 
# neither is 9012012_tag1542_gp.txt or 10012012_tag714_gp.txt
# or 13012012_tag714_gp.txt



# might also be interesting to link prey size/weight to areas
for(i in 1:nrow(regurg))
{

  if(regurg[i,]$Digestion.code==1){capture_bin<-c(0,1)}
  if(regurg[i,]$Digestion.code==2){capture_bin<-c(1,2)}
  if(regurg[i,]$Digestion.code==3){capture_bin<-c(2,4)}
  if(regurg[i,]$Digestion.code==4){capture_bin<-c(4,6)}
  
  if(regurg[i,]$Species=="Anchovy"){
    dive_trax[dive_trax$ID==regurg[i,]$ID & 
               dive_trax$t_from_dsample>= capture_bin[1] &
               dive_trax$t_from_dsample<= capture_bin[2]
              ,]$n_Anchovy=
      dive_trax[dive_trax$ID==regurg[i,]$ID & 
                 dive_trax$t_from_dsample>= capture_bin[1] &
                 dive_trax$t_from_dsample<= capture_bin[2]
              ,]$n_Anchovy+1}
  
  if(regurg[i,]$Species=="Pilchard"){
    dive_trax[dive_trax$ID==regurg[i,]$ID & 
                dive_trax$t_from_dsample>= capture_bin[1] &
                dive_trax$t_from_dsample<= capture_bin[2]
              ,]$n_Pilchard=
      dive_trax[dive_trax$ID==regurg[i,]$ID & 
                  dive_trax$t_from_dsample>= capture_bin[1] &
                  dive_trax$t_from_dsample<= capture_bin[2]
                ,]$n_Pilchard+1}
  
  if(regurg[i,]$Species=="Saury"){
    dive_trax[dive_trax$ID==regurg[i,]$ID & 
                dive_trax$t_from_dsample>= capture_bin[1] &
                dive_trax$t_from_dsample<= capture_bin[2]
              ,]$n_Saury=
      dive_trax[dive_trax$ID==regurg[i,]$ID & 
                  dive_trax$t_from_dsample>= capture_bin[1] &
                  dive_trax$t_from_dsample<= capture_bin[2]
                ,]$n_Saury+1}
  
  if(regurg[i,]$Species=="Jack Mackrel"){
    dive_trax[dive_trax$ID==regurg[i,]$ID & 
                dive_trax$t_from_dsample>= capture_bin[1] &
                dive_trax$t_from_dsample<= capture_bin[2]
              ,]$n_JackMackrel=
      dive_trax[dive_trax$ID==regurg[i,]$ID & 
                  dive_trax$t_from_dsample>= capture_bin[1] &
                  dive_trax$t_from_dsample<= capture_bin[2]
                ,]$n_JackMackrel+1}
  
  if(regurg[i,]$Species=="Squid"){
    dive_trax[dive_trax$ID==regurg[i,]$ID & 
                dive_trax$t_from_dsample>= capture_bin[1] &
                dive_trax$t_from_dsample<= capture_bin[2]
              ,]$n_Squid=
      dive_trax[dive_trax$ID==regurg[i,]$ID & 
                  dive_trax$t_from_dsample>= capture_bin[1] &
                  dive_trax$t_from_dsample<= capture_bin[2]
                ,]$n_Squid+1}
  
  if(regurg[i,]$Species=="Garfish"){
    dive_trax[dive_trax$ID==regurg[i,]$ID & 
                dive_trax$t_from_dsample>= capture_bin[1] &
                dive_trax$t_from_dsample<= capture_bin[2]
              ,]$n_Garfish=
      dive_trax[dive_trax$ID==regurg[i,]$ID & 
                  dive_trax$t_from_dsample>= capture_bin[1] &
                  dive_trax$t_from_dsample<= capture_bin[2]
                ,]$n_Garfish+1}
  
  if(regurg[i,]$Species=="Yellow Eye Mullet"){
    dive_trax[dive_trax$ID==regurg[i,]$ID & 
                dive_trax$t_from_dsample>= capture_bin[1] &
                dive_trax$t_from_dsample<= capture_bin[2]
              ,]$n_YellowEyeMullet=
      dive_trax[dive_trax$ID==regurg[i,]$ID & 
                  dive_trax$t_from_dsample>= capture_bin[1] &
                  dive_trax$t_from_dsample<= capture_bin[2]
                ,]$n_YellowEyeMullet+1}
  
  
  if(nrow(dive_trax[dive_trax$ID==regurg[i,]$ID & 
                   dive_trax$t_from_dsample>= capture_bin[1] &
                   dive_trax$t_from_dsample<= capture_bin[2] 
                   ,])==0){print(paste(i, "'s capture bin does not fit into tracked data"))}
  
  print(i)
}

write.csv(dive_trax, "dive_trax_1.csv", quote=F, row.names=F)


