
#########################################
#PREDICTION BASED ON REAL DISTANCES between MSOAs part of VDMs 
#########################################

# pathGM <- '../pct-data/greater-manchester/'
# c <-readRDS(file.path(pathGM,'c.Rds'))
# c.df <-c@data
# colnames(c.df)
# 
# #filtering wc:  MSOAOrig & MSOADest ONLY in G.M.
# wc <-inner_join(wc, c.df[,1:2], by=c('MSOAOrig'='geo_code'))
# wc <-inner_join(wc, c.df[,1:2], by=c('MSOADest'='geo_code'))
# sum(wc$DemandOD)   #inner G.M. demand=7.48 M
# rm(c, c.df)

wc = readRDS('./L3/L3_wcdist.Rds')
#wc =wc[wc$dist<30000, ]   #assume nobody walks/cycle>30Km


###### adjustment algorithm
demandPre = sum(wc$DemandOD)
wc$dist = wc$dist/1000    #convert to Km
wc$distmean = wc$distmean/1000    #convert to Km

#add areas
wc = inner_join(wc, area_vdm[c("VDMZone", "AreaVDM")],  by=c("Origin" = "VDMZone") )
wc = inner_join(wc, area_vdm[c("VDMZone", "AreaVDM")],  by=c("Destination" = "VDMZone") )
colnames(wc)
wc = dplyr::rename(.data = wc, AreaVDMOrig = AreaVDM.x,
                   AreaVDMDest  = AreaVDM.y  )

#estimate demand by converted area size + by proximity
wc$Demandold= wc$DemandOD
#wc$DemandOD1 = 0.7 * wc$DemandOD * exp(-1.1 * wc$dist)   #old approach I improvised
wc$DemandOD1.old = 0.85 * wc$DemandOD
wc$DemandOD2.old = 0.15 * wc$DemandOD

wc$DemandOD1 = 0.85 * wc$DemandOD * 
    exp(2.3431 + (-1.7623 * wc$dist) + (0.14173 * wc$dist^2) + (-0.0035273 * wc$dist^3))/  (1+ exp(2.3431 + (-1.7623 * wc$dist) + (0.14173 * wc$dist^2) + (-0.0035273 * wc$dist^3)))

wc$DemandOD2 = 0.15 * wc$DemandOD * (wc$AreaOrig * wc$AreaDest)  / (wc$AreaVDMOrig*wc$AreaVDMDest)

#demandPost =sum(wc$DemandOD)    #variation after double estimate

wc.agg = aggregate(wc[,c('DemandOD1','DemandOD2'),], by=list(wc$Origin, wc$Destination), FUN=sum, na.rm=T)   #this could be aggr. by MSOAOrig-MSOADest
names(wc.agg) = c('Origin', 'Destination', 'DemandAfter1','DemandAfter2')

wc= inner_join(wc, wc.agg, by=c("Origin" = "Origin", "Destination" = "Destination"))
#wc= inner_join(wc, wc.agg, by=c("MSOAOrig" = "MSOAOrig", "MSOADest" = "MSOADest")  )
wc$DemandOD1 = wc$DemandOD1 * wc$DemandOD1.old / wc$DemandAfter1
wc$DemandOD2 = wc$DemandOD2 * wc$DemandOD2.old / wc$DemandAfter2

wc$DemandOD = wc$DemandOD1 + wc$DemandOD2

#checks
wc = arrange(wc, -wc$DemandOD)
td=wc[wc$DemandOD>100,]  #few people travel long distances
max(td$dist)

#adjust demand
# afterDemand = sum(wc$DemandOD)
# wc.agg = aggregate(wc[,'DemandOD'], by=list(wc$Origin, wc$Destination), FUN=sum, na.rm=T)
# names(wc.agg)=c('Origin','Destination', 'DemandTotal')

# wc = inner_join(wc, wc.agg, by=c("Origin" = "Origin", "Destination" = "Destination"))
# wc$DemandOD = wc$DemandOD/wc$DemandTotal

# wc$DemandOD =wc$DemandOD * preDemand /afterDemand
# wc$CycleGM =wc$CycleGM * preDemand /afterDemand

#read wu03 (Census)
wu03.gm = readRDS('./L4/wu03.gm.rds')      # Census flows GM

#add Census (+12 cols)
zones= read.csv('./L4/Census_zones.csv')
wc <-left_join(wc, wu03.gm, by=c('MSOAOrig'='msoa1','MSOADest'='msoa2'))
wc = left_join(wc, zones[, c('geo_code','cycperc','cycwalk')], by=c("MSOAOrig"="geo_code"))
wc[is.na(wc)] = 0
rm(wu03.gm)


sel10minus= ( (wc$Bicycle + wc$On.foot) <=10) |(wc$Bicycle== 0)  | (wc$On.foot== 0)
sel10plus=  ! sel10minus
sum(sel10plus)       #flows w. Census data

#delete OD flows w/o a distance
wc= wc[! is.na(wc$dist),]  #typically ALL have distance
wc$CycleGM = 0



for (i in c(1, 2))   {
    
    # dist ranges for prediction
    if (i==1) {sel = wc$DemandOD !=0 & sel10minus
    }   else { sel = wc$DemandOD !=0 & sel10plus }        #apply only to flows with 'potential' cyclists
    
    sel1 = sel & (wc$dist>= 0) & (wc$dist< 3)   ; sel1factor = 0.025
    sel2 = sel & (wc$dist>= 3) & ( wc$dist <  6)     ; sel2factor = 0.339
    sel3 = sel & (wc$dist>= 6) &  (wc$dist <  10)    ; sel3factor = 1.30
    sel4 = sel & (wc$dist>= 10) & (wc$dist <  15)  
    sel5 = sel & (wc$dist>= 15)
    
    
    if (i==1)   {  #flows w. insufficient Census data
        
        #values as per Anna's table 20-Oct-2015 (replace w. Census MSOA-level cycling%)
        # wc$CycleGM[sel1] = wc$DemandOD[sel1] *   0.0703 * 0.32
        # wc$CycleGM[sel2] = wc$DemandOD[sel2] *   0.298 * 0.85
        # wc$CycleGM[sel3] = wc$DemandOD[sel3] *   0.495 * 1.05
        # wc$CycleGM[sel4] = wc$DemandOD[sel4] *   0.92
        # wc$CycleGM[sel5] = wc$DemandOD[sel5] *   1
        
        wc$CycleGM[sel1] = wc$DemandOD[sel1] *   wc$cycwalk[sel1] * 0.32
        wc$CycleGM[sel2] = wc$DemandOD[sel2] *   wc$cycwalk[sel2] * 0.85
        wc$CycleGM[sel3] = wc$DemandOD[sel3] *   wc$cycwalk[sel3] * 1.05
        wc$CycleGM[sel4] = wc$DemandOD[sel4] *   0.92
        wc$CycleGM[sel5] = wc$DemandOD[sel5] *   1
        
        
        
    } else  {   #flows w. enough Census data
        
        wc$CycleGM[sel1] = 0.25 * wc$DemandOD[sel1] * wc$Bicycle[sel1]/(wc$Bicycle[sel1]+wc$On.foot[sel1])      # 0.025/(1+0.025)
        wc$CycleGM[sel2] = 0.339 * wc$DemandOD[sel2] * wc$Bicycle[sel2]/(wc$Bicycle[sel2]+ wc$On.foot[sel2])        # 0.339/ (1+ 0.339)   
        wc$CycleGM[sel3] = 1.3  * wc$DemandOD[sel3] * wc$Bicycle[sel3]/(wc$Bicycle[sel3] + wc$On.foot[sel3]) 
        wc$CycleGM[sel4] = 0.92  * wc$DemandOD[sel4] *  wc$Bicycle[sel4]/(wc$Bicycle[sel4] + wc$On.foot[sel4])       # 92% of total
        wc$CycleGM[sel5] = 1* wc$DemandOD[sel5] *   wc$Bicycle[sel5]/(wc$Bicycle[sel5] + wc$On.foot[sel5]  )                 # =1 => all people cycling
    }
    
}

sum(wc$DemandOD)
sum(wc$CycleGM)

dropcols = grep(pattern = 'sel',x = ls())
rm(list=ls()[dropcols])

# flag 'wrong' flows where cycling demand comes out higher than total demand
sum(wc$CycleGM > wc$DemandOD)
wc$CycleGM[wc$CycleGM > wc$DemandOD]= wc$DemandOD[wc$CycleGM > wc$DemandOD]    #no negative totals for w+c

wc$FootGM = wc$DemandOD - wc$CycleGM
sum(wc$FootGM==0)
sum(wc$FootGM)

#aggregate cycling / walking per MSOA
wc <-aggregate(wc[ , c('FootGM','CycleGM')] ,by=list(wc$MSOAOrig,wc$MSOADest), FUN=sum, na.rm=T)
colnames(wc) <- c('MSOAOrig','MSOADest','FootGM', 'CycleGM')

wc[,c("FootGM","CycleGM")] = round(wc[,c("FootGM","CycleGM")], 0)

#check & roundings
sum(wc$FootGM)  
sum(wc$CycleGM)

wc=arrange(wc, -CycleGM)

