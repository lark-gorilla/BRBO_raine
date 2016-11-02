# Read in HMM gannet data and look for mid trip 'long' resting period
# to split foraging trip into adult and chick feeding sections


rm(list=ls())
setwd("~/research/gannets")

## might be needed to link 2 sec hmm data to the original, but it might not!
## heres code incase, will need moveHMM

datfull<-read.csv("gannet_dat_tripsplit_dives_CORRECTED.csv", h=T)

#dat$trip_id<-as.character(dat$trip_id)
dat<-data.frame(y=datfull$Latitude, x=datfull$Longitude, ID=datfull$trip_id,
                year=datfull$year, DateTime=datfull$DateTime, Returns=datfull$Returns )
library(moveHMM)
data<-prepData(dat, type="LL")

nrow(data[data$step==0,])/nrow(data)

# force all data to 2 second interval $$%%
sec1dat<- which(dat$year!="2012")
sec2dat<-seq(sec1dat[2], sec1dat[length(sec1dat)], 2)

dat2sec<-dat[c(which(dat$year=="2012"), sec2dat),]

data2sec<-prepData(dat2sec, type="LL")

nrow(data2sec[data2sec$step==0,])/nrow(data2sec)


d2012<-read.csv("FINALdata2012.csv", h=T)
d2014<-read.csv("FINALdata2014.csv", h=T)
d2015<-read.csv("FINALdata2015.csv", h=T)
d2016<-read.csv("FINALdata2016.csv", h=T)

d2012<-cbind(d2012, data2sec[ data2sec$year=="2012", 7:8])
d2014<-cbind(d2014, data2sec[ data2sec$year=="2014", 7:8])
d2015<-cbind(d2015, data2sec[ data2sec$year=="2015", 7:8])
d2016<-cbind(d2016, data2sec[ data2sec$year=="2016", 7:8])


# first we isolate the tracks (with hmm results), for the birds that regurgitated
# on return of a GPS. We use metadata to manually select trips. 
# there are some birds with multiple trips.. need to investigate, for mo we'll take em all


unique(d2012$ID)# all from 2012
unique(d2012$ID)# all from 2016

unique(d2014$ID)

sel_trax14<-d2014[with(d2014, ID== 'G34_GPS&Alt_export_2014-12-23 07-501'| 
                       ID == 'G49_GPS & Alt4_export_2014-12-25 08-321'| ID== 'G57_GPS_Alt 4_export_2014-12-27 06-591'|
                       ID == 'G57_GPS_Alt 4_export_2014-12-27 06-592'| ID =='G57_GPS_Alt 4_export_2014-12-27 06-593' 
                       ), ]

sel_trax15<-d2015[with(d2015, ID== 'G79_GPS & Alt 3_export_2015-01-01 06-551'| 
                         ID == 'G80_GPS & Alt 4_export_2015-01-01 07-231'| ID== 'G89_GPS & Alt 3_export_2015-01-03 07-541'|
                         ID == 'G95_GPS & Alt 3_export_2015-01-04 08-271'| ID =='G95_GPS & Alt 3_export_2015-01-04 08-272'| 
                         ID== 'G102_GPS & Alt 4_export_2015-01-05 08-571'| ID=='G102_GPS & Alt 4_export_2015-01-05 08-572'|  
                         ID == 'G103_GPS & Alt 2_export_2015-01-05 09-211'| ID== 'G105_GPS & Alt 7_export_2015-01-05 10-551'|
                         ID == 'G108_GPS & Alt 3_export_2015-01-07 07-481'| ID =='G112_GPS & Alt 4_export_2015-01-09 06-511'| ID=='G112_GPS & Alt 4_export_2015-01-09 06-512'|
                       ID == 'G11_2015-12-28 14-48.csv1'| ID== 'G11_2015-12-28 14-48.csv2'|
                         ID == 'G12_2015-12-28 15-04.csv1'| ID =='G12_2015-12-28 15-04.csv2'|
                         ID == 'G22_A_2015-12-29 15-59.csv1'| ID== 'G22_A_2015-12-29 15-59.csv2'|ID=='G22_A_2015-12-29 15-59.csv3'|
                         ID == 'G27_A_2015-12-30 05-05.csv1'| ID =='G27_A_2015-12-30 05-05.csv2'|ID=='G27_A_2015-12-30 05-05.csv3'|
                         ID == 'G36_A_2015-12-31 09-33.csv1'| ID =='G36_A_2015-12-31 09-33.csv2'
                         ), ]


sel_trax<-rbind(d2012, sel_trax14 ,sel_trax15, d2016)

d2012<-NULL;d2014<-NULL;d2015<-NULL;d2016<-NULL


## remove non-returning trips
sel_trax<-sel_trax[sel_trax$Returns!='N',]

sel_trax$trax<-substr(sel_trax$ID, 1, (nchar(as.character(sel_trax$ID))-1))
trax<-unique(substr(sel_trax$ID, 1, (nchar(as.character(sel_trax$ID))-1)))
# for trax that have multiple trips, select the best (last)
library(ggplot2)

for(i in trax)
    {
    print(qplot(x=x, y=y, data=sel_trax[sel_trax$trax==i,], colour=ID, geom="point", main=i)+geom_point(x=173.02522, y=-40.55754, colour="orange", size=2))
    print(i)
    readline("hi")
    }
    
table(sel_trax$ID)

# If trip 2 is large one then remove trip 1

sel_trax<-sel_trax[with(sel_trax, ID!='10012012_tag1541_gps.txt1'& ID!='G102_GPS & Alt 4_export_2015-01-05 08-571'&
                       ID!= 'G22_A_2015-12-29 15-59.csv1' ),]

# actually lets remove all 2nd (or smaller) trips
sel_trax<-sel_trax[with(sel_trax, ID!='G57_GPS_Alt 4_export_2014-12-27 06-592'& ID!='G36_A_2015-12-31 09-33.csv2'&
                          ID!= 'G95_GPS & Alt 3_export_2015-01-04 08-272' & ID!='G27_A_2015-12-30 05-05.csv1'),]


write.csv(sel_trax, "select_trax_allyrs.csv", quote=F, row.names=F)

sel_trax<-read.csv("select_trax_allyrs.csv", h=T)
## had a look, still dont really like the hmm results for all years.. could try one on this subset of data?

pd<-prepData(data.frame(x=sel_trax$x, y=sel_trax$y,
                        ID=sel_trax$ID), type="LL")

#264396 G95_GPS & Alt 3_export_2015-01-04 08-271 1.973462 0.8766083 173.3836
pd<-pd[pd$step!=1.973462,]

mu0 <- c(0.0001,0.01,0.03)
sigma0 <- c(0.0001,0.01,0.03)
zeromass0 <- c(0.01,0.0001,0.0001)
stepPar0 <- c(mu0,sigma0, zeromass0)
angleMean0 <- c(0,pi,0)
kappa0 <- c(2,1,2)
anglePar0 <- c(angleMean0,kappa0)

sel_trax$hmm_trial<-0
for(i in unique(sel_trax$ID))
{
stepPar0 <- c(mu0,sigma0, zeromass0)
if(min(pd[pd$ID==i,]$step, na.rm = T)!=0){stepPar0<-stepPar0[1:6]}  
m3zm <- fitHMM(data=pd[pd$ID==i,] ,nbStates=3,stepPar0=stepPar0,anglePar0=anglePar0)

states<-viterbi(m3zm)
sel_trax[sel_trax$ID==i,]$hmm_trial<-states
print(i)
print(m3zm)
}

write.csv(sel_trax, "select_trax_allyrs_hmmtrial.csv", quote=F, row.names=F)

#seems to work forsome tracks but other just get it wrong..
# trial 30 second interval

dat30sec<-NULL
for ( i in unique(sel_trax$ID))
{
  d1<-sel_trax[sel_trax$ID==i,]
  d2<-d1[seq(1,nrow(d1), 15),]
  dat30sec<-rbind(dat30sec, d2)
  print(i)
}

tempdata<-prepData(data.frame(x=dat30sec$x, y=dat30sec$y,
                              ID=dat30sec$ID), type="LL")

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

dat30sec$hmm_30sec<-states30


write.csv(dat30sec, "select_trax_allyrs_hmmtrial30sec.csv", row.names=F, quote=F)

# maybe this 30 second interval is the way to go.
dat30sec$DateTime<-as.POSIXct(strptime(dat30sec$DateTime, "%Y-%m-%d %H:%M:%S"), "GMT")

dat30sec$ID<-factor(dat30sec$ID) # remove unused factors 


timez<-data.frame(trip_id=aggregate(data=dat30sec, DateTime~ID, FUN=min)[,1],
                  start=aggregate(data=dat30sec, DateTime~ID, FUN=min)[,2],
                  end=aggregate(data=dat30sec, DateTime~ID, FUN=max)[,2])
timez$duration_hrs<-timez$end-timez$start
# see if we can find this mid trip resting period
library(ggplot2)
library(gridExtra)

for(i in unique(dat30sec$ID))
{
  p1<-qplot(x=x, y=y, data=dat30sec[dat30sec$ID==i,], colour=factor(hmm_30sec), geom="point", main=i)
  p2<-qplot(x=DateTime, y=hmm_30sec,data=dat30sec[dat30sec$ID==i,],  geom="line")+
    geom_point(data=dat30sec[dat30sec$ID==i,],aes(x=DateTime, y=hmm_30sec,colour=factor(hmm_30sec)))
  print(grid.arrange(p1, p2, nrow=2))
  print(i)
  readline("L")
}


library(ggplot2)

qplot(data=d2015[d2015$ID=="G102_GPS & Alt 4_export_2015-01-05 08-571",],
      x=seq(1:nrow(d2015[d2015$ID=="G102_GPS & Alt 4_export_2015-01-05 08-571",])), y=states,
            geom="line")
      




