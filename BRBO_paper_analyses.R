### 09/01/17 Alan Road, Manchester
### Analyses for paper results and graphics

# Test the effect of Sex and chick age on tracking metrics

setwd("~/grive/phd/analyses/BRBO_raine")

dat<-read.csv("BRBO_raine_summary.csv", h=T)

dat$Date<-substr(dat$DateTime, 1,8)

head(dat)

#remove non returnning trips
dat<-dat[dat$Returns!="N",]

hist(dat$max_dist)
hist(dat$tot_length)
hist(dat$tot_time)
hist(dat$foraging_perc)
hist(dat$flying_perc)
hist(dat$resting_perc)


library(ggplot2)
# check AM/PM differences in trips
dat$ampm<-"AM"
dat[dat$hour>12,]$ampm<-"PM"
coef(summary(lm(max_dist~ampm, data=dat)))
anova(lm(max_dist~ampm, data=dat))

qplot(data=dat, y=max_dist, x=factor(hour), geom="boxplot")
qplot(data=dat, y=max_dist, x=ampm, geom="boxplot")

qplot(data=dat, y=max_dist, x=ampm, colour=Sex,  geom="boxplot")
qplot(data=dat, x=hour, geom="histogram")+facet_grid(Sex~.)

coef(summary(lm(hour~Sex, data=dat)))
anova(lm(hour~Sex, data=dat))



qplot(data=dat, y=max_dist, x=Sex, geom="boxplot")
qplot(data=dat, y=max_dist, x=trackID, geom="boxplot")
qplot(data=dat, y=max_dist, x=cktar)+geom_smooth(method="lm")
qplot(data=dat, y=max_dist, x=ckweight)+geom_smooth(method="lm")
qplot(data=dat, y=max_dist, x=ckcond)+geom_smooth(method="lm")
qplot(data=dat, y=max_dist, x=adweight)+geom_smooth(method="lm")
qplot(data=dat, y=max_dist, x=adtar)+geom_smooth(method="lm")
qplot(data=dat, y=max_dist, x=Date, geom="boxplot")

cor(dat[c(1,7,21,9:13)])
# chick metrics correlated use condition

m1<-lm(max_dist~Sex*adweight+ckcond+trackID+Date, data=dat)
summary(m1) # some collinearity, test seperate

summary(lm(max_dist~trackID, data=dat))
anova(lm(max_dist~trackID, data=dat)) # no effect
#Df Sum Sq Mean Sq F value Pr(>F)
#trackID   17  76540  4502.4  1.4548 0.1628
#Residuals 40 123794  3094.9  

m1<-lm(max_dist~Sex*adweight+ckcond+Date, data=dat)
summary(m1) 
anova(m1)
#Df Sum Sq Mean Sq F value   Pr(>F)   
#Sex           1  25188 25188.0  8.2193 0.006185 **
#  adweight      1    776   776.4  0.2533 0.617080   
#ckcond        1  17575 17574.7  5.7349 0.020670 * 
#  Date          6  11026  1837.6  0.5997 0.729084   
#Sex:adweight  1   1737  1737.4  0.5669 0.455235   
#Residuals    47 144032  3064.5        

m1<-lm(max_dist~Sex+ckcond, data=dat)
summary(m1) # selected model coefficients

#MAX DIST
coef(summary(lm(max_dist~Sex, data=dat)))
anova(lm(max_dist~Sex, data=dat))
coef(summary(lm(max_dist~adweight, data=dat)))
anova(lm(max_dist~adweight, data=dat))
# ck cond has outliers from big chicks. remove.
coef(summary(lm(max_dist~ckcond, data=dat[dat$ckcond<2,])))
anova(lm(max_dist~ckcond, data=dat[dat$ckcond<2,]))
coef(summary(lm(max_dist~Date, data=dat)))
anova(lm(max_dist~Date, data=dat))
coef(summary(lm(max_dist~trackID, data=dat)))
anova(lm(max_dist~trackID, data=dat))

#TOT LENGTH
coef(summary(lm(tot_length~Sex, data=dat)))
anova(lm(tot_length~Sex, data=dat))
coef(summary(lm(tot_length~adweight, data=dat)))
anova(lm(tot_length~adweight, data=dat))
coef(summary(lm(tot_length~ckcond, data=dat[dat$ckcond<2,])))
anova(lm(tot_length~ckcond, data=dat[dat$ckcond<2,]))
coef(summary(lm(tot_length~Date, data=dat)))
anova(lm(tot_length~Date, data=dat))
coef(summary(lm(tot_length~trackID, data=dat)))
anova(lm(tot_length~trackID, data=dat))

#TOT TIME
coef(summary(lm(tot_time~Sex, data=dat)))
anova(lm(tot_time~Sex, data=dat))
coef(summary(lm(tot_time~adweight, data=dat)))
anova(lm(tot_time~adweight, data=dat))
coef(summary(lm(tot_time~ckcond, data=dat[dat$ckcond<2,])))
anova(lm(tot_time~ckcond, data=dat[dat$ckcond<2,]))
coef(summary(lm(tot_time~Date, data=dat)))
anova(lm(tot_time~Date, data=dat))
coef(summary(lm(tot_time~trackID, data=dat)))
anova(lm(tot_time~trackID, data=dat))

#foraging_time
coef(summary(lm(foraging_time~Sex, data=dat)))
anova(lm(foraging_time~Sex, data=dat))
coef(summary(lm(foraging_time~adweight, data=dat)))
anova(lm(foraging_time~adweight, data=dat))
coef(summary(lm(foraging_time~ckcond, data=dat[dat$ckcond<2,])))
anova(lm(foraging_time~ckcond, data=dat[dat$ckcond<2,]))
coef(summary(lm(foraging_time~Date, data=dat)))
anova(lm(foraging_time~Date, data=dat))
coef(summary(lm(foraging_time~trackID, data=dat)))
anova(lm(foraging_time~trackID, data=dat))

#flying_time
coef(summary(lm(flying_time~Sex, data=dat)))
anova(lm(flying_time~Sex, data=dat))
coef(summary(lm(flying_time~adweight, data=dat)))
anova(lm(flying_time~adweight, data=dat))
coef(summary(lm(flying_time~ckcond, data=dat[dat$ckcond<2,])))
anova(lm(flying_time~ckcond, data=dat[dat$ckcond<2,]))
coef(summary(lm(flying_time~Date, data=dat)))
anova(lm(flying_time~Date, data=dat))
coef(summary(lm(flying_time~trackID, data=dat)))
anova(lm(flying_time~trackID, data=dat))

#resting_time
coef(summary(lm(sqrt(resting_time)~Sex, data=dat)))
anova(lm(sqrt(resting_time)~Sex, data=dat))
coef(summary(lm(sqrt(resting_time)~adweight, data=dat)))
anova(lm(sqrt(resting_time)~adweight, data=dat))
coef(summary(lm(sqrt(resting_time)~ckcond, data=dat[dat$ckcond<2,])))
anova(lm(sqrt(resting_time)~ckcond, data=dat[dat$ckcond<2,]))
coef(summary(lm(sqrt(resting_time)~Date, data=dat)))
anova(lm(sqrt(resting_time)~Date, data=dat))
coef(summary(lm(sqrt(resting_time)~trackID, data=dat)))
anova(lm(sqrt(resting_time)~trackID, data=dat))

lsmeans(lm(max_dist~Sex, data=dat), "Sex")
anova(lm(max_dist~Sex, data=dat))
lsmeans(lm(tot_length~Sex, data=dat), "Sex")
anova(lm(tot_length~Sex, data=dat))
lsmeans(lm(tot_time~Sex, data=dat), "Sex")
anova(lm(tot_time~Sex, data=dat))
lsmeans(lm(foraging_time~Sex, data=dat), "Sex")
anova(lm(foraging_time~Sex, data=dat))
lsmeans(lm(flying_time~Sex, data=dat), "Sex")
anova(lm(flying_time~Sex, data=dat))
lsmeans(lm(sqrt(resting_time)~Sex, data=dat), "Sex", transform="response")
anova(lm(sqrt(resting_time)~Sex, data=dat))

mean(dat$max_dist);sd(dat$max_dist)
mean(dat$tot_length);sd(dat$tot_length)
mean(dat$tot_time);sd(dat$tot_time)
mean(dat$foraging_time);sd(dat$foraging_time)
mean(dat$flying_time);sd(dat$flying_time)
mean(dat$resting_time);sd(dat$resting_time)

p<-ggplot(data=dat, aes(x=hour, fill=Sex))
p+geom_histogram(position="dodge")+scale_x_continuous(breaks=5:18)

# differences in adult morphometrics
setwd("~/grive/phd/analyses/BRBO_raine")

dat<-read.csv("BRBO_adult_meta.csv", h=T)

aggregate(Weight~Sex, dat, mean)
aggregate(Weight~Sex, dat, sd)
qplot(data=dat, x=Sex, y=Weight, geom="boxplot")
t.test(Weight~Sex, dat)


aggregate(Tarsus~Sex, dat, mean)
aggregate(Tarsus~Sex, dat, sd)
qplot(data=dat, x=Sex, y=Tarsus, geom="boxplot")
t.test(Tarsus~Sex, dat)


aggregate(CulmenLength~Sex, dat, mean)
aggregate(CulmenLength~Sex, dat, sd)
qplot(data=dat, x=Sex, y=CulmenLength, geom="boxplot")
t.test(CulmenLength~Sex, dat)

aggregate(CulmenHeight~Sex, dat, mean)
aggregate(CulmenHeight~Sex, dat, sd)
qplot(data=dat, x=Sex, y=CulmenHeight, geom="boxplot")
t.test(CulmenHeight~Sex, dat)

aggregate(WingLength~Sex, dat, mean)
aggregate(WingLength~Sex, dat, sd)
qplot(data=dat, x=Sex, y=WingLength, geom="boxplot")
t.test(WingLength~Sex, dat)

# Prey capture location analysis
rm(list=ls())
dat<-read.csv("BRBO_diet_anal_trax_forage_applied2_forageonly.csv", h=T)

#dat<-read.csv("BRBO_raine_hmm.csv", h=T)
#dat<-dat[dat$Returns!="N",]

table(dat[dat$S_count>0,]$hmm_all_trips3) #377 
table(dat$hmm_all_trips3) #658
(100/658)*(658-377)

# time of capture
dat$DateTime<-as.POSIXct(strptime(dat$DateTime, "%Y-%m-%d %H:%M:%S"), "GMT")
library(lubridate)
dat$hour<-hour(dat$DateTime)

d1<-rbind(data.frame(dat[dat$FF_count>0,], sp="FF"),
          data.frame(dat[dat$S_count>0,], sp="SQ"))
plot(Latitude~Longitude, d1, col=sp)

qplot(data=d1,y=hour, x=sp, geom="boxplot" )

mean(d1[d1$hour>12 & d1$sp=="FF",]$hour) # we remove morning outliers
sd(d1[d1$hour>12 & d1$sp=="FF",]$hour)

mean(d1[d1$hour>12 & d1$sp=="SQ",]$hour)
sd(d1[d1$hour>12 & d1$sp=="SQ",]$hour)

summary(lm(hour~sp, data=d1[d1$hour>12,]))
anova(lm(hour~sp, data=d1[d1$hour>12,]))

# Pull in spatial data

library(raster)
library(rgdal)

r1<-raster("/home/mark/grive/phd/sourced_data/env_data/ausbath_09_v4/w001001.adf")

sp<-SpatialPoints(cbind(d1$Longitude, d1$Latitude))
r1<-crop(r1, extent(sp))

reef<-readOGR( layer="GBR_DRY_REEF", dsn="/home/mark/grive/phd/sourced_data/env_data/GBRMPA_Data Export/GBR_DRY_REEF.shp", verbose=TRUE)
plot(r1)
plot(reef, add=TRUE)

rera<-rasterize(reef, r1, field=1,background=NA)
d_reef<-distance(rera)

cora<-rasterize(data.frame(x=144.0353, y=-11.5907), r1, field=1,background=NA)
d_col<-distance(cora)

d1$bathy<-extract(r1,cbind(d1$Longitude, d1$Latitude))
d1$d_col<-extract(d_col,cbind(d1$Longitude, d1$Latitude))
d1$d_reef<-extract(d_reef,cbind(d1$Longitude, d1$Latitude))

d1$bathy<-abs(d1$bathy)
d1$d_col<-d1$d_col/1000
d1$d_reef<-d1$d_reef/1000

qplot(data=d1, y=bathy, x=sp, geom="boxplot")
mean(d1[d1$sp=="FF",]$bathy, na.rm=T) 
sd(d1[ d1$sp=="FF",]$bathy, na.rm=T)
mean(d1[ d1$sp=="SQ",]$bathy, na.rm=T)
sd(d1[ d1$sp=="SQ",]$bathy, na.rm=T)
hist(d1$bathy)
summary(lm(bathy~sp, data=d1))
anova(lm(bathy~sp, data=d1))

qplot(data=d1, y=d_col, x=sp, geom="boxplot")
mean(d1[d1$sp=="FF",]$d_col, na.rm=T) 
sd(d1[ d1$sp=="FF",]$d_col, na.rm=T)
mean(d1[ d1$sp=="SQ",]$d_col, na.rm=T)
sd(d1[ d1$sp=="SQ",]$d_col, na.rm=T)
hist(d1$d_col)
summary(lm(d_col~sp, data=d1))
anova(lm(d_col~sp, data=d1))

qplot(data=d1, y=d_reef, x=sp, geom="boxplot")
mean(d1[d1$sp=="FF",]$d_reef, na.rm=T) 
sd(d1[ d1$sp=="FF",]$d_reef, na.rm=T)
mean(d1[ d1$sp=="SQ",]$d_reef, na.rm=T)
sd(d1[ d1$sp=="SQ",]$d_reef, na.rm=T)
hist(d1$d_reef)
summary(lm(d_reef~sp, data=d1))
anova(lm(d_reef~sp, data=d1))

mean(d1[d1$hour>12,]$hour, na.rm=T)
sd(d1[d1$hour>12,]$hour, na.rm=T)
mean(d1$bathy, na.rm=T) ;sd(d1$bathy, na.rm=T)
mean(d1$d_col, na.rm=T) ;sd(d1$d_col, na.rm=T)
mean(d1$d_reef, na.rm=T) ;sd(d1$d_reef, na.rm=T)

# other summary stuff for capt. loc

aggregate(S_count~TrackID, data=d1, sum)
aggregate(FF_count~TrackID, data=d1, sum)

# for checking full hmm dataset findings against capture location
anova(lm(d_reef~Sex, data=d1[d1$hmm_all_trips3==2,]))
anova(lm(d_col~Sex, data=d1[d1$hmm_all_trips3==2,]))
anova(lm(bathy~Sex, data=d1[d1$hmm_all_trips3==2,]))

# foraging bouts and dist from colony bs Sex
dat<-read.csv("BRBO_raine_hmm.csv", h=T)
dat<-dat[dat$Returns!="N",]

out<-NULL
for(i in unique(dat$trip_id))
{
t1<-dat[dat$trip_id==i,]$hmm_all_trips3

t2<-data.frame(trip_id=i, Sex=unique(dat[dat$trip_id==i,]$Sex),
mean_bout=mean(diff(which(t1==3))[which(diff(which(t1==3))>1)]),
sd_bout=sd(diff(which(t1==3))[which(diff(which(t1==3))>1)]))
out<-rbind(out, t2)
print(i)
}

qplot(data=out[out$mean_bout<50,], y=mean_bout, x=Sex, geom="boxplot")

summary(lm(mean_bout~Sex, data=out[out$mean_bout<50,]))
anova(lm(mean_bout~Sex, data=out[out$mean_bout<50,]))
# same bout length

qplot(data=dat[dat$hmm_all_trips3==2,],
      y=ColDist, x=Sex, geom="boxplot")

# check if AM/PM returning birds contained more flying fish/squid

setwd("~/grive/phd/analyses/BRBO_raine")

dat<-read.csv("BRBO_raine_summary.csv", h=T)

dat$Date<-substr(dat$DateTime, 1,8)

#remove non returnning trips
dat<-dat[dat$Returns!="N",]

diet<-read.csv("Regurgitations Data_ BB_GMC_MARK.csv", h=T)

dat$end_time<-dat$hour+dat$tot_time
hist(dat$end_time)

substr(dat[dat$end_time<13,]$tripID, 10,14)
# oh it ok as we only have the 6 gps and diet birds to
# test and only 1 was an AM bird.
