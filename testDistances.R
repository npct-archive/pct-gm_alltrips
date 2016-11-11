
#calculates real distance-mean distance, between OD pairs / MSOA pairs

library(stplanr)

#calc. no. nodes, aggregate by MSOA pairs
wc$nodes=1
wc.agg.msoa = aggregate(wc$nodes, by=list(wc$MSOAOrig , wc$MSOADest), FUN=sum,na.rm=T)
names(wc.agg.msoa)=c('MSOAOrig','MSOADest', 'nodes')
wc.agg.msoa1= stplanr::onewayid(wc.agg.msoa, attrib= c(3))
wc.agg.msoa1= data.frame(wc.agg.msoa1)

del.rows = grep(pattern ='W',x = wc.agg.msoa1$MSOAOrig,value = F )
wc.agg.msoa1=wc.agg.msoa1[-del.rows, ]

del.rows = grep(pattern ='W',x = wc.agg.msoa1$MSOADest,value = F )
wc.agg.msoa1=wc.agg.msoa1[-del.rows, ]



#read ukmsoas 
pathGM <- '../pct-bigdata/'  #before w/o: -NC
ukmsoas = readRDS(file.path(pathGM,'ukmsoas-scenarios.Rds'))

#get distances for all MSOA pairs (aggregated)
wc.agg.msoa2 <- stplanr::od2line(wc.agg.msoa1, ukmsoas)
# wc.dist =stplanr::line2route(wc.agg.msoa2, route_fun = route_cyclestreet, 
#                              base_url = "http://pct.cyclestreets.net", plan = "fastest")
wc.dist$id = paste(wc.agg.msoa2@data$MSOAOrig, wc.agg.msoa2@data$MSOADest)
# saveRDS(wc.dist,'./L4/wc.dist.Rds')


######  Read distances obtained from CYCLESTREET
wc.dist = readRDS('./L4/wc.dist.Rds')
wc.dist = wc.dist@data[, c('id', 'length')]
wc.dist$length[is.na(wc.dist$length)] = 0        #?

# add id1,2 for distance transfer
wc.agg.msoa  = cbind(id1=paste(wc.agg.msoa$MSOAOrig , wc.agg.msoa$MSOADest), 
                       id2=paste(wc.agg.msoa$MSOADest , wc.agg.msoa$MSOAOrig), 
                       wc.agg.msoa)

# transfer distances to MSOA pairs (disaggregated)
wc.agg.msoa$dist = 0    #add distance column to collect values
wc.agg.msoa$id1 = as.character(wc.agg.msoa$id1)
wc.agg.msoa$id2 = as.character(wc.agg.msoa$id2)
wc.agg.msoa= left_join(wc.agg.msoa, wc.dist, by=c('id1'='id') )


sel = ! is.na(wc.agg.msoa$length)      # pairs w. distance
sum(sel)
wc.agg.msoa$dist[sel] = wc.agg.msoa$length[sel]
wc.agg.msoa$length =NULL

#rest of distances
wc.agg.msoa= left_join(wc.agg.msoa, wc.dist, by=c('id2'='id') )
sel = ! is.na(wc.agg.msoa$length)      # pairs w. distance
sum(sel)
wc.agg.msoa$dist[sel] = wc.agg.msoa$length[sel]
wc.agg.msoa$length =NULL

#check no distances missing
sum(is.na(wc.agg.msoa$dist) )


######## ADD distance to original wc OD pairs
wc$nodes =NULL
wc = inner_join(wc, wc.agg.msoa[,c("MSOAOrig","MSOADest","nodes", "dist")],
                by=c("MSOAOrig" ="MSOAOrig", "MSOADest" = "MSOADest"))

#calculate distance mean per O-D pair
wc.agg.od = aggregate(wc$dist,by=list(wc$Origin, wc$Destination), FUN=mean,na.rm=T)
names(wc.agg.od)=c('Origin','Destination', 'distmean')
summary(wc.agg.od$distmean)

#add distmean to wc flows
wc = inner_join(wc, wc.agg.od, by=c("Origin" = "Origin", "Destination" = "Destination"))
rm(wc.agg.od)

rm(wc.agg.msoa, wc.agg.msoa1,wc.dist)
