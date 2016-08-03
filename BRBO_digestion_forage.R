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
#D3= 2.1h-3.0 h of capture
#D4=3.1h – 6h of capture




            