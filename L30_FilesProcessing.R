
#v2.2 FINAL METHOD  ===== > uses TfGM proportional areas approach. See  docs & 
# the 2 databases for more reference 

rm(list=ls())
library("readstata13")
library(dplyr)

area_hw = read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/Areas_Highways.csv',header=T, as.is=T)
area_pt = read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/Areas_PT.csv',header=T, as.is=T)
area_vdm = read.csv('C:/temp/Manchester_Traffic_data/2-L2_L3_level/Areas_VDM.csv',header=T, as.is=T)   

#### PRE-CHECK: read unprocessed car OD traffic (morning/off-peak/afternoon rates already adjusted)
car0 <-read.csv('C:/temp/Manchester_Traffic_data/0-L0_level/0_CarOD.csv',header=T,as.is = T)
colnames(car0)
head(car0)

#total Driv+Pass demand for G.M. region: 3.8M (people)
#check commuter demand before processing: subset for commuters (UserClass==1)
car0comm <- subset(x = car0,UserClass==1)
sel= (car0comm$TimeID==2)
sel1 = (car0comm$TimeID !=2)
sum(car0comm$DemandN[sel]) * 6 + sum(car0comm$DemandN[sel1])   
#commuters demand for whole day~ 1.82 M (people/trips)

head(car0comm)
tail(car0comm)
rm(car0,car0comm, sel, sel1)

################################################
#         OPTIONAL CHECK : start with L1 car file, previous to geogr. conversion
################################################
car1 <-read.csv('C:/temp/Manchester_Traffic_data/1-Filter95/L1_Car_95_v1.csv',header=T,as.is = T)
colnames(car1)
head(car1)
car1[ is.na(car1) ] = 0
sum(car1$DemandN)       #demand ~ 3.64 M (95%)
rm(car1)                #just checking 95% demand before DB processing

##################### NORMAL PROCESS #########################
#
############# CAR TRAFFIC: L3 GENERATION FROM L2 (Car traffic processing)
carfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2Car_MSOA.rds'
car <- readRDS(carfile)

nrow(car)
colnames(car)  # [1] "Origin"      "Destination" "MSOAOrig"    "AreaOrig"    "DDriv"       "DPass"       "MSOADest"   
               # [8] "AreaDest"


##add Area orig/dest column
car = inner_join(car, area_hw[c("X2013HighwayZone", "AreaHighway")],  by=c("Origin" = "X2013HighwayZone") )
car = inner_join(car, area_hw[c("X2013HighwayZone", "AreaHighway")],  by=c("Destination" = "X2013HighwayZone") )
colnames(car)

colnames(car)[c(9,10)]= c('AreaHighwayOrig','AreaHighwayDest')

############# GLOBAL METHOD (split demand proportional to area MSOA/area zone)

#method for driver figures
car$xDemand <- car$DDriv *  car$AreaOrig / car$AreaHighwayOrig  
car$yDemand <-  car$AreaDest  / car$AreaHighwayDest
car$xyDemand <- car$xDemand * car$yDemand

carDriver <-aggregate(car$xyDemand,by=list(car$MSOAOrig,car$MSOADest), FUN=sum,na.rm=T)
colnames(carDriver) <- c('MSOAOrig','MSOADest','DemandDriver')
sum(carDriver$DemandDriver)  #checking demand is unchanged: ~3.268 M
carDriver$DemandDriver <- round(carDriver$DemandDriver, 0)


#same for passenger
car$xDemand <- car$DPass *  car$AreaOrig / car$AreaHighwayOrig  
car$xyDemand <- car$xDemand * car$yDemand

carPass <-aggregate(car$xyDemand,by=list(car$MSOAOrig,car$MSOADest), FUN=sum,na.rm=T)
colnames(carPass) <- c('MSOAOrig','MSOADest','DemandPassenger')
sum(carPass$DemandPassenger)  #checking demand is unchanged ~1.757 M
carPass$DemandPassenger <- round(carPass$DemandPassenger, 0)

# car global demand
car <- cbind(carDriver,DemandPassenger=carPass$DemandPassenger, mode=3)


car <- car[car$DemandDriver+ car$DemandPassenger!=0,]

saveRDS(car,'./L3/L3_car.rds')
rm(carDriver, carPass, area_hw)

############# L3 GENERATION FROM L2 (PUBLIC TRANSPORT TRAFFIC)

ptfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_PT_MSOA.Rds'
pt <- readRDS(ptfile)    #reads L2_PT_MSOA.Rds
colnames(pt)

##add PT orig/dest Area column
pt = inner_join(pt, area_pt[c("X2013PTZone", "AreaPT")],  by=c("Origin" = "X2013PTZone") )
pt = inner_join(pt, area_pt[c("X2013PTZone", "AreaPT")],  by=c("Destination" = "X2013PTZone") )
colnames(pt)

colnames(pt)[c(8,9)]= c('AreaPTOrig','AreaPTDest')


##################GLOBAL method: allocate AreaOrig first, then AreaDest
pt$xDemand <- pt$DemandOD *  pt$AreaOrig / pt$AreaPTOrig  
pt$yDemand <-  pt$AreaDest  / pt$AreaPTDest
pt$xyDemand <- pt$xDemand * pt$yDemand

pt <-aggregate(pt$xyDemand,by=list(pt$MSOAOrig,pt$MSOADest), FUN=sum, na.rm=T)
colnames(pt) <- c('MSOAOrig','MSOADest','DemandT')
sum(pt$DemandT)  #checking demand is unchanged at ~775K trips
pt$DemandT <- round(pt$DemandT, 0)
pt <- cbind(pt, mode=2)
pt <- pt[pt$DemandT!=0,]

saveRDS(pt,'./L3/L3_pt.rds')
rm(pt, area_pt)


############# WALKING:  L3 GENERATION FROM L2 (WALK + CYCLING TRAFFIC)

#read gm.od3 to get distances
# gm.od3 <- readRDS('./L4/gm.od1.rds')     #flows file w. fast route distances (48425)  
# summary(gm.od3$dist)

#add cols: dist-slope
walkfile <- 'C:/temp/Manchester_Traffic_data/2-L2_L3_level/L2_WC_MSOA.Rds'
wc <- readRDS(walkfile)  #reads L2_WC_MSOA.Rds

###############################
###############################
###############################




wc[is.na(wc)] =  0
sum(wc$CycleGM)     #predicted total cyclists ~150 K


wc <- wc[wc$FootGM!=0 | wc$CycleGM!=0,]
wc <- cbind(wc,mode=1)

saveRDS(wc,'./L3/L3_wc.Rds')


#######################  AGGREGATING TOTAL DEMAND  for Gr. MANCHESTER in file gm.od
#
#  
#######################  NEXT PHASE: combine  traffic modes & join to Census flows/centroids

rm(list=ls())   #clean previous vars

path <- './L3/'
wc <- readRDS(paste0(path,'L3_wc.Rds'))   # wc <- readRDS(paste0(path,'L3_wc_meandistance.Rds'))
pt <- readRDS(paste0(path,'L3_pt.Rds'))
car <- readRDS(paste0(path,'L3_car.Rds'))

#reshape for rbind
colnames(wc) <- c("MSOAOrig","MSOADest","FootGM", "CycleGM", "mode") 
colnames(pt) <- c("MSOAOrig","MSOADest","BusGM", "mode")
colnames(car) <- c("MSOAOrig","MSOADest","CarDriver","CarPassenger", "mode")

#eliminates mode column
wc <-wc[,-5]
pt <-pt[,-4]
car <-car[,-5]

#create gm.od by joining car->pt->wc, then rounds flows  
gm.od <-left_join(car, pt, by=c('MSOAOrig', 'MSOADest'))
gm.od <-left_join(gm.od, wc, by=c('MSOAOrig', 'MSOADest'))
gm.od[,c("CarDriver","CarPassenger","BusGM","FootGM","CycleGM")] <- round(gm.od[,c("CarDriver","CarPassenger","BusGM","FootGM","CycleGM")],0)   
gm.od[is.na(gm.od)] <-0

#get totals
gm.od$AllGM <- gm.od$CarDriver + gm.od$CarPassenger + gm.od$BusGM + gm.od$FootGM + gm.od$CycleGM

#### gm.od: all trips (from Greater Manchester -> any UK point + from any UK point->Greater Manchester)
#### this means >100K  flows (way more than Robin's file)


#centroids file for filtering G.M. MSOAs
pathGM <- '../pct-data/greater-manchester/'
c <-readRDS(file.path(pathGM,'c.Rds'))
c.df <-c@data
colnames(c.df)

#filtering for MSOAOrig & MSOADest ONLY in G.M.
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOAOrig'='geo_code'))
gm.od <-inner_join(gm.od, c.df[,1:2], by=c('MSOADest'='geo_code'))
sum(gm.od$AllGM)   #inner G.M. demand=7.48 M
rm(c.df)

drops =c("geo_label.x","geo_label.y")
gm.od <-gm.od[ , !names(gm.od) %in% drops]   
colnames(gm.od)[1:2]<-c('msoa1','msoa2')
gm.od <- gm.od[,c(1:2,8,3:7)]
saveRDS(gm.od, './L4/gm.od.rds')

#only execute if not run before
#source('L31_addDistances.R')   #add distances to flows using stplanr (latest, from github)
rm(list=ls())


