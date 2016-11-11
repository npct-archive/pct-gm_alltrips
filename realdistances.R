
#PREDICT BASED ON REAL DISTANCES
pathGM <- '../pct-data/greater-manchester/'
c <-readRDS(file.path(pathGM,'c.Rds'))
c.df <-c@data
colnames(c.df)

#filtering wc:  MSOAOrig & MSOADest ONLY in G.M.
wc <-inner_join(wc, c.df[,1:2], by=c('MSOAOrig'='geo_code'))
wc <-inner_join(wc, c.df[,1:2], by=c('MSOADest'='geo_code'))
sum(wc$DemandOD)   #inner G.M. demand=7.48 M
rm(c, c.df)

#read wu03 (Census)
wu03.gm = readRDS('./L4/wu03.gm.rds')      # Census flows GM

#add Census (+12 cols)
wc <-left_join(wc, wu03.gm, by=c('MSOAOrig'='msoa1','MSOADest'='msoa2'))
wc[is.na(wc)] = 0
rm(wu03.gm)

###adjust dist mean & run prediction on wc dataset
sel = (wc$Origin == wc$Destination)   #inner w/c flows
sum(sel)
wc$distmean = wc$distmean/1000    #convert to Km
wc$distmean[sel] = 0.90 * wc$distmean[sel]    #adjust for inner flows
wc$distmean[!sel] = 0.6 * wc$distmean[!sel]    #adjust for outer flows

sel10minus= ( (wc$Bicycle + wc$On.foot) <=10) |(wc$Bicycle== 0)  | (wc$On.foot== 0)
sel10plus=  ! sel10minus
sum(sel10plus)       #flows w. Census data

#delete OD flows w/o a distance
wc= wc[! is.na(wc$distmean),]  #typically ALL have distance
wc$CycleGM = 0



for (i in c(1, 2))   {
    
    # distmean ranges for prediction
    if (i==1) {sel = wc$DemandOD !=0 & sel10minus
    }   else { sel = wc$DemandOD !=0 & sel10plus }        #apply only to flows with 'potential' cyclists
    
    sel1 = sel & (wc$distmean>= 0) & (wc$distmean< 3)   ; sel1factor = 0.025
    sel2 = sel & (wc$distmean>= 3) & ( wc$distmean <  6)     ; sel2factor = 0.339
    sel3 = sel & (wc$distmean>= 6) &  (wc$distmean <  10)    ; sel3factor = 1.30
    sel4 = sel & (wc$distmean>= 10) & (wc$distmean <  15)  
    sel5 = sel & (wc$distmean>= 15)
    
    
    if (i==1)   {  #flows w. insufficient Census data
        
        #values as per Anna's table 20-Oct-2015 (replace w. Census MSOA-level cycling%)
        wc$CycleGM[sel1] = wc$DemandOD[sel1] *   0.0703 * 0.32   
        wc$CycleGM[sel2] = wc$DemandOD[sel2] *   0.298 * 0.85    
        wc$CycleGM[sel3] = wc$DemandOD[sel3] *   0.495 * 1.05
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


wc$CycleGM[wc$CycleGM > wc$FootGM]= wc$FootGM    #no negative totals for w+c
wc$FootGM = wc$DemandOD - wc$CycleGM
sum(wc$FootGM==0)

wc = inner_join(wc, area_vdm[c("VDMZone", "AreaVDM")],  by=c("Origin" = "VDMZone") )
wc = inner_join(wc, area_vdm[c("VDMZone", "AreaVDM")],  by=c("Destination" = "VDMZone") )
colnames(wc)
wc = dplyr::rename(.data = wc, AreaVDMOrig = AreaVDM.x,
                   AreaVDMDest  = AreaVDM.y  )


wc$xCycle <- wc$CycleGM *  wc$AreaOrig / wc$AreaVDMOrig
wc$yCycle <-  wc$AreaDest  / wc$AreaVDMDest
wc$xyCycle <- wc$xCycle * wc$yCycle
sum(wc$xyCycle)    

wc$xFoot <- wc$FootGM *  wc$AreaOrig / wc$AreaVDMOrig
wc$yFoot <-  wc$AreaDest  / wc$AreaVDMDest
wc$xyFoot <- wc$xFoot * wc$yFoot
sum(wc$xyFoot)

#aggregate cycling / walking per MSOA
wc1 <-aggregate(wc$xyFoot ,by=list(wc$MSOAOrig,wc$MSOADest), FUN=sum, na.rm=T)
colnames(wc1) <- c('MSOAOrig','MSOADest','FootGM')
wc2 <-aggregate(wc$xyCycle ,by=list(wc$MSOAOrig,wc$MSOADest), FUN=sum, na.rm=T)
colnames(wc2) <- c('MSOAOrig','MSOADest','CycleGM')


wc= cbind(wc1,CycleGM=wc2$CycleGM)
wc$CycleGM <- trunc(wc$CycleGM)

wc[,c("FootGM","CycleGM")] = round(wc[,c("FootGM","CycleGM")], 0)

#check & roundings
sum(wc$FootGM)  
sum(wc$CycleGM)

