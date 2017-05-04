rm(list=ls())
setwd("~/grive/phd/analyses/BRBO_raine")
library(ggplot2)

# attempt to apply Gabriel's digestion foraging idea to BRBO data

# first we isolate the tracks (with hmm results), for the 6 birds that regurgitated
# on return of a GPS. As tracks have multiple days and trips on them we need to isolate
# the final trip that each bird made before they were captured as this will relate to
# the prey captured.

tracking<-read.csv("BRBO_raine_hmm.csv", h=T)

unique(tracking$trip_id)

sel_trax<-tracking[with(tracking, trip_id== '04_12_14_23Z_M3'| 
                        trip_id == '04_12_14_17Z_F4'| trip_id== '05_12_14_11Z_F5'|
                        trip_id == '04_12_14_26Z_M5'| trip_id =='07_12_14_35Z_F1'| 
                        trip_id =='04_12_14_01Z_M4'), ]

qplot(data=sel_trax, x=Longitude, y=Latitude, colour=trip_id, 
      shape=factor(hmm_all_trips3) , geom="point")

qplot(data=sel_trax, x=Longitude, y=Latitude, colour=factor(hmm_all_trips3), 
     geom="point", facets=~trip_id)

sel_trax$DateTime<-as.POSIXct(strptime(sel_trax$DateTime, "%Y-%m-%d %H:%M:%S"), "GMT")


timez<-data.frame(trip_id=aggregate(data=sel_trax, DateTime~trip_id, FUN=min)[,1],
                  start=aggregate(data=sel_trax, DateTime~trip_id, FUN=min)[,2],
end=aggregate(data=sel_trax, DateTime~trip_id, FUN=max)[,2], diet_time=c("23:00",
              "21:30", "21:16", "23:40", "20:13", "21:10"))

# My views are supported by a combination of authors that talk about food transit in sulid: Davies (1958) suggests that fish takes between 2-6 hs of digestion, whereas Laugksch & Duffy 1986 (attached).
# The authors propose that within:
  
#a) 1 h of capture birds have left 70% of meals in the stomach

#b) 2 h of capture birds have left 60% of meals in the stomach

#c) 3 h of capture birds have left 50% of meals in the stomach

#You have 4 digestion codes (D1, D2, D3 and D4) from undigested to digested…  and following these authors we can group them as follows:
#D1 and D2 = 0-2.0 h of capture
#D3= 2.1h-3.0  of capture
#D4=3.1h – 6h of capture

timez

# ok so problem we have is these digestion timings are far too
# short, most birds returned to colony (fed chick?) then we
# got sample so food had already been in stomach for several hours

# time hack to force all trips on same day

sel_trax$DateTimeHack<-as.POSIXct(strptime(paste("2000-01-01", sel_trax$Time), "%Y-%m-%d %H:%M:%S"), "GMT")


qplot(data=sel_trax, x=Longitude, y=Latitude, colour=factor(hmm_all_trips3), 
      geom="point", facets=~trip_id)

qplot(data=sel_trax, x=Time, y=ColDist, colour=factor(hmm_all_trips3), 
      geom="point", facets=~trip_id)

qplot(data=sel_trax, x=Time, y=factor(hmm_all_trips3), colour=factor(hmm_all_trips3), 
      geom="point", facets=~trip_id)

qplot(data=sel_trax, x=DateTimeHack, y=as.numeric(hmm_all_trips3), colour=hmm_all_trips3,
geom=c("point", "line"), facets=~trip_id)+scale_x_datetime(date_breaks="1 hour", date_labels="%H")

qplot(data=sel_trax[sel_trax$trip_id==timez$trip_id[1],], x=DateTimeHack, y=as.numeric(hmm_all_trips3), colour=hmm_all_trips3,
      geom=c("point", "line"))+scale_x_datetime(date_breaks="1 hour", date_labels="%H %M")

#ok so not very eloquent code to classify foraging bins windows
# when selecting we gotta be pragmatic, we cannot, using digestion
# levels, pull out unique foraging bouts below the temporal
# resolution of hours.
# The below loop alocates points into foraging/non-foragaing bouts,
# basically tidies up hmm results

sel_trax$foragebin=0

tripz<-timez$trip_id

for(j in tripz)
{
plot(hmm_all_trips3~DateTimeHack, data=sel_trax[sel_trax$trip_id==j,], type="b", col=hmm_all_trips3)

f1z<-identify(x=sel_trax[sel_trax$trip_id==j,]$DateTimeHack, y=sel_trax[sel_trax$trip_id==j,]$hmm_all_trips3)

for_seq<-NULL
for(i in 1:length(f1z)){
  if(i %in% c(2,4,6,8,10,12)){next}
  s1<-seq(f1z[i], f1z[i+1])
  for_seq<-c(for_seq, s1)
  }

Sys.sleep(1)
sel_trax[sel_trax$trip_id==j,]$foragebin[for_seq]<-1

plot(hmm_all_trips3~DateTimeHack, data=sel_trax[sel_trax$trip_id==j,], type="b", col=foragebin+1, main=j)
Sys.sleep(1)
}

qplot(data=sel_trax, x=DateTimeHack, y=foragebin, colour=factor(foragebin),
      geom=c("point"), facets=~trip_id)+scale_x_datetime(date_breaks="1 hour", date_labels="%H")

write.csv(sel_trax, "BRBO_diet_anal_trax_forage_binned.csv", quote=F, row.names=F)

#cool ok so now we can do a time since last forage
timez$trip_id<-factor(timez$trip_id) # removes old unused factor levels

timez$last_feed<-timez$start[1]
timez$first_feed<-timez$start[1]
for (i in 1:nrow(timez))
{timez[i,]$last_feed<-max(sel_trax[sel_trax$trip_id==timez$trip_id[i] & sel_trax$foragebin==1,]$DateTime)
timez[i,]$first_feed<-min(sel_trax[sel_trax$trip_id==timez$trip_id[i] & sel_trax$foragebin==1,]$DateTime)
}
timez$tdiff_last<-as.POSIXct(strptime(paste(as.Date(timez$last_feed), timez$diet_time), "%Y-%m-%d %H:%M"), "GMT")-timez$last_feed
timez$tdiff_first<-as.POSIXct(strptime(paste(as.Date(timez$first_feed), timez$diet_time), "%Y-%m-%d %H:%M"), "GMT")-timez$first_feed



regurg<-read.csv("Regurgitations Data_ BB_GMC_MARK.csv", h=T)

#unique(sel_trax$trip_id)
#substr(unique(sel_trax$trip_id),10,11)
#unique(regurg$Sample.ID)

regurg<-regurg[!is.na(regurg$trip_id),]

# so basically were gonna set up regressions for squid and 
# flying fish as these are the only ones there is enough data for

regurg2<-regurg[regurg$Species %in% c("S", "FF"),]

m_seq<-match(regurg2$trip_id, timez$trip_id)
#could use match?

regurg2$tdiff_last<-timez$tdiff_last[1]
regurg2$tdiff_first<-timez$tdiff_last[1]
regurg2$tdiff_last<-unlist(lapply(m_seq, FUN=function(x){timez[x,]$tdiff_last}))
regurg2$tdiff_first<-unlist(lapply(m_seq, FUN=function(x){timez[x,]$tdiff_first}))

p<-ggplot(data=regurg2[regurg2$Species=="FF",], aes(y=tdiff_first, x=Digestion.code))
p+geom_point()            

# ok so this is kinda the wrong plot, we basically have a range of
# digestion codes for each tdiff, but we also have min and max tdiff
# so can effectively say prey was caught between those times

write.csv(regurg2, "BRBO_regurg_times.csv", quote=F, row.names=F)

# so issue is that digestion codes can be effected by big lumps
# of small fish if all in a lump even if caught at same time
# also adult will likely feed chick some food before we caught 
# this could come from any point in the foraging trip
# so we'll do regression for dig code~time where time for 
# each code is the min and max, hopefully over lots of catches 
# something will even out?

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

#

regurg_assn<-read.csv("BRBO_regurg_times_assigned.csv", h=T)
library(lme4)

p<-ggplot(data=regurg_assn[regurg_assn$Species=="FF",], aes(y=Digestion.code, x=tdiff_assn))
p+geom_jitter(aes(colour=trip_id))

m1<-lmer(Digestion.code~tdiff_assn+(1|trip_id), data=regurg_assn[regurg_assn$Species=="FF",])
sum(resid(m1, type="pearson")^2)/df.residual(m1)
summary(m1)
plot(m1)

d1<-data.frame(pred=predict(m1, newdata=data.frame(tdiff_assn=seq(2.5,8,0.2)), type="response", re.form=~0),
               tdiff_assn=seq(2.5,8,0.2))
p<-ggplot(data=regurg_assn[regurg_assn$Species=="FF",], aes(y=Digestion.code, x=tdiff_assn))
p+geom_jitter(aes(colour=trip_id), height=0.2, width=0)+geom_line(data=d1, aes(x=tdiff_assn, y=pred))

m2<-lmer(Digestion.code~tdiff_assn+(1|trip_id), data=regurg_assn[regurg_assn$Species=="S",])
summary(m2)
plot(m2)
# maybe mixed model not good as unbalanced samples
m2a<-lm(Digestion.code~tdiff_assn, data=regurg_assn[regurg_assn$Species=="S",])

d1<-data.frame(pred=predict(m2, newdata=data.frame(tdiff_assn=seq(3,14,0.2)), type="response", re.form=~0),
               tdiff_assn=seq(3,14,0.2))
d2<-data.frame(pred=predict(m2a, newdata=data.frame(tdiff_assn=seq(3,14,0.2)), type="response"),
               tdiff_assn=seq(3,14,0.2))
p<-ggplot(data=regurg_assn[regurg_assn$Species=="S",], aes(y=Digestion.code, x=tdiff_assn))
p+geom_jitter(aes(colour=trip_id), height=0.2, width=0)+geom_line(data=d1, aes(x=tdiff_assn, y=pred))+
  geom_line(data=d2, aes(x=tdiff_assn, y=pred, colour="red"))

#ok right approach, maybe asymptotic curve or better sort data?

# hmm maybe if we combined prey types into one regression it would be simplar
p<-ggplot(data=regurg_assn, aes(y=Digestion.code, x=tdiff_assn))
p+geom_jitter(aes(colour=trip_id), height=0.2, width=0)

p<-ggplot(data=regurg_assn, aes(y=Digestion.code, x=tdiff_assn))
p+geom_jitter(aes(colour=Species), height=0.2, width=0)

# no sp differences
m3<-lmer(Digestion.code~tdiff_assn+(1|trip_id), data=regurg_assn)
sum(resid(m3, type="pearson")^2)/df.residual(m3)
summary(m3)

d1<-data.frame(pred=predict(m3, newdata=data.frame(tdiff_assn=seq(0,14,0.2)), type="response", re.form=~0),
               tdiff_assn=seq(0,14,0.2))
p<-ggplot(data=regurg_assn, aes(y=Digestion.code, x=tdiff_assn))
p+geom_jitter(aes(colour=trip_id), height=0.2, width=0)+geom_line(data=d1, aes(x=tdiff_assn, y=pred))+
  scale_x_continuous(breaks=0:14)+scale_y_continuous(breaks=0:4)

m3a<-lmer(Digestion.code~tdiff_assn+Species+(1|trip_id), data=regurg_assn)
sum(resid(m3a, type="pearson")^2)/df.residual(m3a)
summary(m3a)

nd<-data.frame(tdiff_assn=seq(0,14,0.2), Species=c(rep("S", 71), rep("FF", 71)))
d1<-data.frame(pred=predict(m3a, newdata=nd, type="response", re.form=~0),nd)
              
p<-ggplot(data=regurg_assn, aes(y=Digestion.code, x=tdiff_assn))
p+geom_jitter(aes(colour=trip_id), height=0.2, width=0)+geom_line(data=d1, aes(x=tdiff_assn, y=pred, colour=Species))+
  scale_x_continuous(breaks=0:14)+scale_y_continuous(breaks=0:4)

# different sp curves
m3b<-lmer(Digestion.code~tdiff_assn*Species+(1|trip_id), data=regurg_assn)
sum(resid(m3b, type="pearson")^2)/df.residual(m3b)
summary(m3b)

nd<-data.frame(tdiff_assn=seq(0,14,0.2), Species=c(rep("S", 71), rep("FF", 71)))
d1<-data.frame(pred=predict(m3b, newdata=nd, type="response", re.form=~0),nd)

p<-ggplot(data=regurg_assn, aes(y=Digestion.code, x=tdiff_assn))
p+geom_jitter(aes(colour=trip_id), height=0.2, width=0)+geom_line(data=d1, aes(x=tdiff_assn, y=pred, colour=Species))+
  scale_x_continuous(breaks=0:14)+scale_y_continuous(breaks=0:4)

# same sp curves qith quadratic
m3c<-lmer(Digestion.code~poly(tdiff_assn,2)+Species+(1|trip_id), data=regurg_assn)
sum(resid(m3c, type="pearson")^2)/df.residual(m3c)
summary(m3c) # nah

# predct m3a random effects

nd<-expand.grid(tdiff_assn=seq(0,14,0.2), Species=c("S","FF"), trip_id=levels(regurg_assn$trip_id))
d1<-data.frame(pred=predict(m3b, newdata=nd, type="response", re.form=~(1|trip_id)), nd)

p<-ggplot(data=regurg_assn, aes(y=Digestion.code, x=tdiff_assn))
p+geom_jitter(aes(colour=trip_id), height=0.2, width=0)+geom_line(data=d1, aes(x=tdiff_assn, y=pred, colour=trip_id, linetype=Species))+
  scale_x_continuous(breaks=0:14)+scale_y_continuous(breaks=0:4)


sum(resid(m3, type="pearson")^2)/df.residual(m3)
sum(resid(m3a, type="pearson")^2)/df.residual(m3a)
sum(resid(m3b, type="pearson")^2)/df.residual(m3b)

AIC(m3, m3a, m3b)
library(MuMIn)
r.squaredGLMM(m3)
r.squaredGLMM(m3a)
r.squaredGLMM(m3b)
#best model is m3a
summary(m3a)

# write out model m3a predictions
nd<-data.frame(tdiff_assn=seq(0,14,0.2), Species=c(rep("S", 71), rep("FF", 71)))
d1<-data.frame(pred=predict(m3a, newdata=nd, type="response", re.form=~0),nd)

write.csv(d1, "BRBO_m3a_model_predictions.csv", quote=F, row.names=F)
# simplified regression times
# Digest code   T squid   T flyfish  
#     1           0-4 hr    0-2 hr         
#     2           4-6 hr    2-4 hr
#     3           6-8 hr    4-6 hr
#     4           8+ hr     6+ hr

# trial with unweighted data
regurg_assn$uniqueID<-paste(regurg_assn$trip_id, regurg_assn$Species, regurg_assn$tdiff_assn)
unique_regurg_assn<-regurg_assn[-which(duplicated(regurg_assn$uniqueID)),]

m3b<-lmer(Digestion.code~tdiff_assn*Species+(1|trip_id), data=unique_regurg_assn)
sum(resid(m3b, type="pearson")^2)/df.residual(m3b)
summary(m3b)


nd<-expand.grid(tdiff_assn=seq(0,14,0.2), Species=c("S","FF"), trip_id=levels(regurg_assn$trip_id))
d1<-data.frame(pred=predict(m3b, newdata=nd, type="response", re.form=~(1|trip_id)), nd)

p<-ggplot(data=unique_regurg_assn, aes(y=Digestion.code, x=tdiff_assn))
p+geom_point(aes(colour=trip_id, shape=Species))+geom_line(data=d1, aes(x=tdiff_assn, y=pred, colour=trip_id, linetype=Species))+
  scale_x_continuous(breaks=0:14)+scale_y_continuous(breaks=0:4)

#looks good better as non random effect

m4b<-lm(Digestion.code~tdiff_assn*Species, data=unique_regurg_assn)
sum(resid(m4b, type="pearson")^2)/df.residual(m4b)
summary(m4b)
AIC(m4b, m3b)

nd<-expand.grid(tdiff_assn=seq(0,14,0.2), Species=c("S","FF"))
d1<-data.frame(pred=predict(m4b, newdata=nd, type="response"), nd)

p<-ggplot(data=unique_regurg_assn, aes(y=Digestion.code, x=tdiff_assn))
p+geom_point(aes(colour=trip_id, shape=Species))+geom_line(data=d1, aes(x=tdiff_assn, y=pred, linetype=Species))+
  scale_x_continuous(breaks=0:14)+scale_y_continuous(breaks=0:4)

# ok that was using the old tdiff_assn, which is arbitrary when considering individual smaples
#write.csv(unique_regurg_assn, "BRBO_regurg_times_assigned_unique.csv", quote=F, row.names=F)
# have corrected tdiffs(new column) for removing dig class 4

unique_regurg_assn<-read.csv("BRBO_regurg_times_assigned_unique.csv",h=T)

m4c<-lm(Digestion.code~tdiff_assn_no4+Species, 
        data=unique_regurg_assn[unique_regurg_assn$Digestion.code<4,])
sum(resid(m4c, type="pearson")^2)/df.residual(m4c)
summary(m4c)
AIC(m4b, m4c)

nd<-expand.grid(tdiff_assn_no4=seq(0,14,0.2), Species=c("S","FF"))
d1<-data.frame(pred=predict(m4c, newdata=nd, interval="confidence",type="response"), nd)

p<-ggplot(data=d1, aes(x=tdiff_assn_no4))
p+geom_point(data=unique_regurg_assn, aes(y=Digestion.code, x=tdiff_assn_no4,colour=trip_id, shape=Species))+
  geom_line(data=d1, aes(x=tdiff_assn_no4, y=pred.fit, linetype=Species))+
  geom_ribbon(data=d1, aes(x=tdiff_assn_no4, ymin=pred.lwr, ymax=pred.upr, fill=Species), alpha=0.5)+
  scale_x_continuous(breaks=0:14)+scale_y_continuous(breaks=0:4)

# looks good, try forccing the intercept through 0 and keep the dig code 4's in

m4d<-lm(Digestion.code~-1+tdiff_assn_no4:Species, 
        data=unique_regurg_assn)
#sum(resid(m4d, type="pearson")^2)/df.residual(m4d)
summary(m4d)
AIC(m4d, m4c)
lsmeans(m4d, spec="Species")
anova(m4d)

nd<-expand.grid(tdiff_assn_no4=seq(0,14,0.2), Species=c("S","FF"))
d1<-data.frame(pred=predict(m4d, newdata=nd, interval="confidence",type="response"), nd)

p<-ggplot(data=d1, aes(x=tdiff_assn_no4))
p+geom_point(data=unique_regurg_assn, aes(y=Digestion.code, x=tdiff_assn_no4,colour=trip_id, shape=Species))+
  geom_line(data=d1, aes(x=tdiff_assn_no4, y=pred.fit, linetype=Species))+
  geom_ribbon(data=d1, aes(x=tdiff_assn_no4, ymin=pred.lwr, ymax=pred.upr, fill=Species), alpha=0.5)+
  scale_x_continuous(breaks=0:14)+scale_y_continuous(breaks=0:4)

# cool, pretty happy. So we use a zero intercept, with interaction term on data

nd<-expand.grid(tdiff_assn_no4=seq(0,14,0.1), Species=c("S","FF"))
d1<-data.frame(pred=predict(m4d, newdata=nd, interval="confidence",type="response"), nd)

write.csv(d1, "BRBO_m4d_model_predictions.csv", quote=F, row.names=F)
# simplified regression times
# Digest code   T squid   T flyfish  
#     1           0-3 hr    0-1.7 hr (2)        
#     2           3-6 hr    1.7-3.5 hr
#     3           6-9 hr    3.5-5.2 hr
#     4           9-12 hr   5.2-7 hr

# another attempt

regurg_assn<-read.csv("BRBO_regurg_times_assigned.csv", h=T)
library(lme4)
library(lmerTest)
tdiff_assn
m4d<-lmer(Digestion.code~-1+tdiff_assn:Species+(1|trip_id), 
        data=regurg_assn)
#sum(resid(m4d, type="pearson")^2)/df.residual(m4d)
summary(m4d)
AIC(m4d, m4c)
lsmeansLT(m4d, test.effs="tdiff_assn:Species")
anova(m4d)

nd<-expand.grid(tdiff_assn_no4=seq(0,14,0.2), Species=c("S","FF"))
d1<-data.frame(pred=predict(m4d, newdata=nd, interval="confidence",type="response"), nd)

p<-ggplot(data=d1, aes(x=tdiff_assn_no4))
p+geom_point(data=unique_regurg_assn, aes(y=Digestion.code, x=tdiff_assn_no4,colour=trip_id, shape=Species))+
  geom_line(data=d1, aes(x=tdiff_assn_no4, y=pred.fit, linetype=Species))+
  geom_ribbon(data=d1, aes(x=tdiff_assn_no4, ymin=pred.lwr, ymax=pred.upr, fill=Species), alpha=0.5)+
  scale_x_continuous(breaks=0:14)+scale_y_continuous(breaks=0:4)
