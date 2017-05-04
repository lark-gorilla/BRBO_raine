### 09/01/17 Alan Road, Manchester
### Analyses for paper results and graphics

# Test the effect of Sex and chick age on tracking metrics

setwd("~/grive/phd/analyses/BRBO_raine")

dat<-read.csv("BRBO_raine_summary.csv", h=T)

dat$Date<-substr(dat$DateTime, 1,8)

head(dat)

#remove non returnning trips !!!!!!!
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

summary(lm(ColDist~Sex, data=dat[dat$hmm_all_trips3==2,]))
anova(lm(ColDist~Sex, data=dat[dat$hmm_all_trips3==2,]))

library(lme4)
summary(lmer(ColDist~Sex+(1|TrackID), data=dat[dat$hmm_all_trips3==2,]))
drop1(lmer(ColDist~Sex+(1|TrackID), data=dat[dat$hmm_all_trips3==2,]), test="Chisq")
#Single term deletions
#Model:
#  ColDist ~ Sex + (1 | TrackID)
#Df    AIC    LRT Pr(Chi)  
#<none>    143967                 
#Sex     1 143970 4.8979 0.02689 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# check if AM/PM returning birds contained more flying fish/squid

setwd("~/grive/phd/analyses/BRBO_raine")

dat<-read.csv("BRBO_raine_summary.csv", h=T)

dat$Date<-substr(dat$DateTime, 1,8)

#remove non returnning trips
dat<-dat[dat$Returns!="N",]


# make some forging location kernels
dat<-read.csv("~/grive/phd/analyses/BRBO_raine/BRBO_raine_hmm.csv", h=T)

dat<-dat[dat$Returns!="N",]

source("~/grive/phd/scripts/github_MIBA/batchUD.R")


for(j in c(99,75,50,25))
{
  #heron_old$ID<-j
  dat$ID<-j
  UD_out<-batchUD(dat[with(dat, hmm_all_trips3!=3 & Sex=='F'),],
                  Scale = 5, UDLev = j)
  
  if(j==99){all_UDs<-UD_out}else{all_UDs<-spRbind(all_UDs, UD_out)}
  
  plot(all_UDs, border=factor(all_UDs$id))
}

all_UDs <- spTransform(all_UDs, CRS=CRS("+proj=longlat +ellps=WGS84"))
#I updated Lubuntu and now proj is handed correctly so can deal with WGS!! :)

plot(all_UDs, border=factor(all_UDs$id), lwd=2)

setwd("~/grive/phd/analyses/BRBO_raine")
writeOGR(all_UDs, layer="BRBO_female_forgrest5km", dsn="GIS", driver="ESRI Shapefile", verbose=TRUE, overwrite=T)

#birdlife approach
dat$ID<-dat$trip_id
UD_male<-batchUD(dat[with(dat, hmm_all_trips3==2 & Sex=='M'),],
                Scale = 5, UDLev = 50)
UD_male <- spTransform(UD_male, CRS=CRS("+proj=longlat +ellps=WGS84"))

r1<-raster("/home/mark/grive/phd/sourced_data/env_data/ausbath_09_v4/w001001.adf")

extent=(c(143, 145, -10, -13))

r1<-crop(r1, extent(c(143, 145, -13, -10)))

Male_udcount<-rasterize(UD_male, r1, field=1, fun='count')

writeRaster(Male_udcount, "~/grive/phd/analyses/BRBO_raine/GIS/maleUDcount.tif", overwrite=T)

dat$ID<-dat$trip_id
UD_female<-batchUD(dat[with(dat, hmm_all_trips3==2 & Sex=='F'),],
                 Scale = 5, UDLev = 50)
UD_female <- spTransform(UD_female, CRS=CRS("+proj=longlat +ellps=WGS84"))


Female_udcount<-rasterize(UD_female, r1, field=1, fun='count')
writeRaster(Female_udcount, "~/grive/phd/analyses/BRBO_raine/GIS/femaleUDcount.tif", overwrite=T)

# do hour of day foraging

setwd("~/grive/phd/analyses/BRBO_raine")

dat<-read.csv("~/grive/phd/analyses/BRBO_raine/BRBO_raine_hmm.csv", h=T)

dat<-dat[dat$Returns!="N",]

dat$Hr<-as.numeric(substr(dat$Time, 1,2))

d_batlarge<-aggregate(trip_id~Hr+Sex, dat, FUN=function(x){length(unique(x))})

d_batlarge$trip_id_perc<-0
d_batlarge[d_batlarge$Sex=="F",]$trip_id_perc<-
  round((d_batlarge[d_batlarge$Sex=="F",]$trip_id/length(unique(dat[dat$Sex=="F",]$trip_id)))*100)
d_batlarge[d_batlarge$Sex=="M",]$trip_id_perc<-
  round((d_batlarge[d_batlarge$Sex=="M",]$trip_id/length(unique(dat[dat$Sex=="M",]$trip_id)))*100)


d2<-aggregate(Hr~trip_id+Sex, dat, FUN=function(x){min(x)})
d3<-aggregate(Hr~trip_id+Sex, dat, FUN=function(x){max(x)})

d_sten<-d_batlarge
out<-NULL
for(i in 1:nrow(d_sten)){
  out<-rbind(out, data.frame(Sex=d_sten[i,]$Sex,Hr=d_sten[i,]$Hr,
            varib="n_start", count=seq(0, nrow(d2[d2$Sex==d_sten[i,]$Sex & d2$Hr==d_sten[i,]$Hr,]))),
            data.frame(Sex=d_sten[i,]$Sex,Hr=d_sten[i,]$Hr,
            varib="n_end", count=seq(0, nrow(d3[d3$Sex==d_sten[i,]$Sex & d3$Hr==d_sten[i,]$Hr,]))))
  
   print(i)
  }
d_sten<-out

d_sten<-d_sten[d_sten$count!=0,]

dat$t1<-strptime(dat$Time ,"%H:%M:%S", "GMT")
d_batlarge$t1<-strptime(paste(d_batlarge$Hr, ":00:00", sep="") ,"%H:%M:%S", "GMT")
d_sten$t1<-strptime(paste(d_sten$Hr, ":00:00", sep="") ,"%H:%M:%S", "GMT")

d_st<-d_sten[d_sten$varib=="n_start",]
d_st$t2<-strptime(paste(d_st$Hr-1, ":45:00", sep="") ,"%H:%M:%S", "GMT")

d_ed<-d_sten[d_sten$varib=="n_end",]
d_ed$t2<-strptime(paste(d_ed$Hr, ":15:00", sep="") ,"%H:%M:%S", "GMT")

g<-ggplot()
g+geom_col(data=d_batlarge, aes(x=t1, y=trip_id_perc))+
  geom_density(data=dat[dat$hmm_all_trips3==2,], aes(x=t1, (..scaled..)*100), linetype=1, position="identity")+
  geom_point(data=d_st, aes(x=t2, y=count*3), shape=1, size=2)+
  geom_point(data=d_ed, aes(x=t2, y=count*3), shape=2, size=2)+
  scale_x_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H"),
                   limits=c(as.POSIXct(strptime("04:30:00" ,"%H:%M:%S", "GMT"), "GMT"),
                  as.POSIXct(strptime("20:45:00" ,"%H:%M:%S", "GMT"), "GMT")))+
    theme_classic()+
  facet_grid(Sex~.)

gf<-g+geom_col(data=d_batlarge[d_batlarge$Sex=="F",], aes(x=t1, y=trip_id_perc))+
  geom_density(data=dat[dat$hmm_all_trips3==2 & dat$Sex=="F",], aes(x=t1, (..scaled..)*100), linetype=1, position="identity", trim=T)+
  geom_point(data=d_st[d_st$Sex=="F",], aes(x=t2, y=count*3.5), shape=16, size=3)+
  geom_point(data=d_ed[d_ed$Sex=="F",], aes(x=t2, y=count*3.5), shape=17, size=3)+
  scale_x_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H"),
                   limits=c(as.POSIXct(strptime("04:30:00" ,"%H:%M:%S", "GMT"), "GMT"),
                            as.POSIXct(strptime("20:30:00" ,"%H:%M:%S", "GMT"), "GMT")))+
  ylab("Trips at-sea (%)")+xlab("Hour")+
  theme_classic()+geom_text(aes(x=as.POSIXct(strptime("04:30:00" ,"%H:%M:%S", "GMT"), "GMT"), y=95, label="a"), size=12)+
  theme(axis.title.y = element_text(size = rel(1.8), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
  theme(axis.text.x = element_text( size=13))+
  theme(axis.text.y = element_text( size=13))

gm<-g+geom_col(data=d_batlarge[d_batlarge$Sex=="M",], aes(x=t1, y=trip_id_perc))+
  geom_density(data=dat[dat$hmm_all_trips3==2 & dat$Sex=="M",], aes(x=t1, (..scaled..)*100), linetype=1, position="identity", trim=T)+
  geom_point(data=d_st[d_st$Sex=="M",], aes(x=t2, y=count*3.5), shape=16, size=3)+
  geom_point(data=d_ed[d_ed$Sex=="M",], aes(x=t2, y=count*3.5), shape=17, size=3)+
  scale_x_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H"),
                   limits=c(as.POSIXct(strptime("04:30:00" ,"%H:%M:%S", "GMT"), "GMT"),
                            as.POSIXct(strptime("20:30:00" ,"%H:%M:%S", "GMT"), "GMT")))+
  ylab("Trips at-sea (%)")+xlab("Hour")+
  theme_classic()+geom_text(aes(x=as.POSIXct(strptime("04:30:00" ,"%H:%M:%S", "GMT"), "GMT"), y=95, label="b"), size=12)+
  theme(axis.title.y = element_text(size = rel(1.8), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.8), angle = 00))+
  theme(axis.text.x = element_text( size=13))+
  theme(axis.text.y = element_text( size=13))

library(gridExtra)
jpeg("~/grive/phd/writeup/Raine_BRBO/activityplots3.jpg", width =11.69 , height =8.27 , units ="in", res =300)
#A4 size
grid.arrange(gf, gm, ncol=1, nrow=2)
dev.off()

# do some stats

qplot(data=d_batlarge[d_batlarge$Hr<13,], x=Sex, y=trip_id_perc, geom="boxplot")

hist(d_batlarge[d_batlarge$Hr<13,]$trip_id_perc) # use glm?

summary(lm(trip_id_perc~Sex, data=d_batlarge[d_batlarge$Hr<13,]))

glm1<-glm(cbind(trip_id_perc, 100-trip_id_perc)~Sex, data=d_batlarge[d_batlarge$Hr<13,], family="quasibinomial")
sum((resid(glm1, type="pearson")^2))/df.residual(glm1)
drop1(glm1, test="LRT")

qplot(data=d_batlarge[d_batlarge$Hr>12,], x=Sex, y=trip_id_perc, geom="boxplot")

glm2<-glm(cbind(trip_id_perc, 100-trip_id_perc)~Sex, data=d_batlarge[d_batlarge$Hr>12,], family="quasibinomial")
sum((resid(glm2, type="pearson")^2))/df.residual(glm2)
drop1(glm2, test="LRT")


glm1<-glm(cbind(trip_id_perc, 100-trip_id_perc)~Sex, data=d_batlarge[d_batlarge$Hr %in% seq(8,12),], family="binomial")
sum((resid(glm1, type="pearson")^2))/df.residual(glm1)
drop1(glm1, test="LRT")

qplot(data=d_batlarge[d_batlarge$Hr>12,], x=Sex, y=trip_id_perc, geom="boxplot")

glm2<-glm(cbind(trip_id_perc, 100-trip_id_perc)~Sex, data=d_batlarge[d_batlarge$Hr %in% seq(13,17),], family="binomial")
sum((resid(glm2, type="pearson")^2))/df.residual(glm2)
drop1(glm2, test="LRT")

# lets do stats within each sex instead of comparing M vs F
d_batlarge$AMPM<-"AM"
d_batlarge[d_batlarge$Hr>12,]$AMPM<-"PM"

qplot(data=d_batlarge, x=AMPM, y=trip_id_perc, colour=Sex, geom="boxplot")

glm1<-glm(cbind(trip_id_perc, 100-trip_id_perc)~AMPM,
          data=d_batlarge[d_batlarge$Sex=="F",], family="quasibinomial")
sum((resid(glm1, type="pearson")^2))/df.residual(glm1)
summary(glm1)
drop1(glm1, test="F") # F most appropriate for quasi
lsmeans(glm1, specs="AMPM", transform="response")


glm1<-glm(cbind(trip_id_perc, 100-trip_id_perc)~AMPM,
          data=d_batlarge[d_batlarge$Sex=="M",], family="quasibinomial")
sum((resid(glm1, type="pearson")^2))/df.residual(glm1)
summary(glm1)
drop1(glm1, test="F") # F most appropriate for quasi
lsmeans(glm1, specs="AMPM", transform="response")

mean(d_batlarge[d_batlarge$Sex=="M" &
                  d_batlarge$AMPM=="AM",]$trip_id_perc)
sd(d_batlarge[d_batlarge$Sex=="M" &
                  d_batlarge$AMPM=="AM",]$trip_id_perc)
mean(d_batlarge[d_batlarge$Sex=="M" &
                  d_batlarge$AMPM=="PM",]$trip_id_perc)
sd(d_batlarge[d_batlarge$Sex=="M" &
                d_batlarge$AMPM=="PM",]$trip_id_perc)

mean(d_batlarge[d_batlarge$Sex=="F" &
                  d_batlarge$AMPM=="AM",]$trip_id_perc)
sd(d_batlarge[d_batlarge$Sex=="F" &
                d_batlarge$AMPM=="AM",]$trip_id_perc)
mean(d_batlarge[d_batlarge$Sex=="F" &
                  d_batlarge$AMPM=="PM",]$trip_id_perc)
sd(d_batlarge[d_batlarge$Sex=="F" &
                d_batlarge$AMPM=="PM",]$trip_id_perc)

# departure and arrival time

summary(lm(Hr~Sex, data=d2))
drop1(lm(Hr~Sex, data=d2), test="F")

summary(lm(Hr~Sex, data=d3))
drop1(lm(Hr~Sex, data=d3), test="F")

mean(d2[d2$Sex=="M",]$Hr)
sd(d2[d2$Sex=="M",]$Hr)
mean(d2[d2$Sex=="F",]$Hr)
sd(d2[d2$Sex=="F",]$Hr)

mean(d3[d3$Sex=="M",]$Hr)
sd(d3[d3$Sex=="M",]$Hr)
mean(d3[d3$Sex=="F",]$Hr)
sd(d3[d3$Sex=="F",]$Hr)

# foraging time
dat$AMPM<-"AM"
dat[dat$Hr>12,]$AMPM<-"PM"

dat2<-aggregate(hmm_all_trips2~Hr+AMPM+Sex, dat[dat$hmm_all_trips3==2,], FUN=function(x){length(x)})

qplot(data=dat2, y=hmm_all_trips2, x=AMPM, colour=Sex, geom="boxplot")

summary(lm(hmm_all_trips2~AMPM, data=dat2[dat2$Sex=="F",]))
drop1(lm(hmm_all_trips2~AMPM, data=dat2[dat2$Sex=="F",]), test="F")

summary(lm(hmm_all_trips2~AMPM, data=dat2[dat2$Sex=="M",]))
drop1(lm(hmm_all_trips2~AMPM, data=dat2[dat2$Sex=="M",]), test="F")

mean(dat2[dat2$AMPM=="AM" & dat2$Sex=="F",]$hmm_all_trips2)
sd(dat2[dat2$AMPM=="AM" & dat2$Sex=="F",]$hmm_all_trips2)
mean(dat2[dat2$AMPM=="PM" & dat2$Sex=="F",]$hmm_all_trips2)
sd(dat2[dat2$AMPM=="PM" & dat2$Sex=="F",]$hmm_all_trips2)

mean(dat2[dat2$AMPM=="AM" & dat2$Sex=="M",]$hmm_all_trips2)
sd(dat2[dat2$AMPM=="AM" & dat2$Sex=="M",]$hmm_all_trips2)
mean(dat2[dat2$AMPM=="PM" & dat2$Sex=="M",]$hmm_all_trips2)
sd(dat2[dat2$AMPM=="PM" & dat2$Sex=="M",]$hmm_all_trips2)






### time each species spends at nest ###


setwd("~/grive/phd/analyses/BRBO_raine")

dat<-read.csv("~/grive/phd/analyses/BRBO_raine/BRBO_raine_hmm.csv", h=T)

dat<-dat[dat$Returns!="N",]

dat$Hr<-as.numeric(substr(dat$Time, 1,2))

dat$DateTime<-as.POSIXct(strptime(dat$DateTime, "%Y-%m-%d %H:%M:%S"), "GMT")

c_times<-NULL
for( i in unique(dat$TrackID))
 {
 out<-data.frame(dat[dat$TrackID==i,][which(diff(dat[dat$TrackID==i,]$DateTime)>2000),],
                  t_c_diff=diff(dat[dat$TrackID==i,]$DateTime)[which(diff(dat[dat$TrackID==i,]$DateTime)>2000)])
 c_times<-rbind(c_times, out)

 print(i)
 }

c_times$t_c_diff2<-as.numeric(c_times$t_c_diff/3600)

library(ggplot2)
g<-ggplot(data=c_times, aes(x=t_c_diff2, colour=Sex))
g+geom_histogram(position="dodge", binwidth=1)
# ok so there are male day trips an both do overnighters,
# Males do long overnighters and into day

g<-ggplot(data=c_times[c_times$ Hr>16,], aes(x=t_c_diff2, colour=Sex))
g+geom_histogram(position="dodge", binwidth=1)

g<-ggplot(data=c_times, aes(x=t_c_diff2, colour=Sex))
g+geom_histogram(position="dodge", binwidth=1)+facet_wrap(~TrackID)


g<-ggplot(data=c_times[c_times$t_c_diff2>8,], aes(y=as.numeric(t_c_diff2), x=Sex))
g+geom_boxplot(position="dodge")

# not significant, check other variables: Date, chick cond, Ad weight, not indiv bird as lmer
#write.csv(c_times, "BRBO_time_at_col.csv", quote=F, row.names=F)
# read back in with vlookedup fields
rm(list=ls())
c_times<-read.csv("BRBO_time_at_col.csv", h=T)
library(lme4)
library(lmerTest) # gives p values (credibly?) and makes anova 
# give F values and Pr(>F) for lmers

#summary(lm(t_c_diff2~Sex, c_times[c_times$t_c_diff2>8,]))

lmer1<-lmer(t_c_diff2~Sex+(1|ID), c_times[c_times$t_c_diff2>8,])
summary(lmer1)
anova(lmer1)
lsmeansLT(lmer1, test.effs="Sex")

###
qplot(data=c_times[c_times$t_c_diff2>8,], y=t_c_diff2, x=adweight)

lmer1<-lmer(t_c_diff2~adweight+(1|ID), c_times[c_times$t_c_diff2>8,])
summary(lmer1)
anova(lmer1)
lsmeansLT(lmer1, test.effs="adweight")

###
qplot(data=c_times[c_times$t_c_diff2>8,], y=t_c_diff2, x=Date)

lmer1<-lmer(t_c_diff2~Date+(1|ID), c_times[c_times$t_c_diff2>8,])
summary(lmer1)
anova(lmer1)
lsmeansLT(lmer1, test.effs="Date")

###
qplot(data=c_times[c_times$t_c_diff2>8,], y=t_c_diff2, x=ckcond)

lmer1<-lmer(t_c_diff2~ckcond+(1|ID), c_times[c_times$t_c_diff2>8,])
summary(lmer1)
anova(lmer1)
lsmeansLT(lmer1, test.effs="ckcond")

# One more check of amount of prey (numbers) brought back (after chick feed)

d1<-read.csv("regurgitations_with_timestamp.csv", h=T)

d2<-aggregate(Species~Sample.ID+Sex, d1[d1$Species=="FF",], FUN=function(x){length(x)})

mean(d2[d2$Sex=="female",]$Species)
sd(d2[d2$Sex=="female",]$Species)

mean(d2[d2$Sex=="male",]$Species)
sd(d2[d2$Sex=="male",]$Species)

d2<-aggregate(Species~Sample.ID+Sex, d1[d1$Species=="S",], FUN=function(x){length(x)})

mean(d2[d2$Sex=="female",]$Species)
sd(d2[d2$Sex=="female",]$Species)

mean(d2[d2$Sex=="male",]$Species)
sd(d2[d2$Sex=="male",]$Species)
