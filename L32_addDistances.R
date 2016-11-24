
rm(list=ls())
library(stplanr)
library(dplyr)

gm.od <- readRDS('./L4/gm.od.rds')

#  NO NEED to ELIMINATE CENTROIDS before onewayid
gm.od1= stplanr::onewayid(gm.od, attrib= c(3:8))

pathGM <- '../pct-data/greater-manchester/'  #before w/o: -NC
c <-readRDS(file.path(pathGM,'c.Rds'))   

gm.od1 = data.frame(gm.od1)
l <- stplanr::od2line(gm.od1,c)
saveRDS(l, './L4/l_oneway.Rds')  # flows one way w. fast route distances/slopes 

#use PCT-Cyclestreet server to calculate distances OR read pre-calculated
rf = readRDS('../pct-bigdata/rf_gm1.rds')
rq = readRDS('../pct-bigdata/rq_gm1.rds')

#check all lines are there
gm.od1$id = paste(gm.od1$msoa1, gm.od1$msoa2)
(sum(gm.od1$id %in% rf$id) == nrow(gm.od1))   #must be  TRUE

#if routes NOT generated:   
# rf = line2route(l=l, route_fun = route_cyclestreet, base_url = "http://pct.cyclestreets.net", plan = "fastest")
# rf$id = paste(l$msoa1, l$msoa2)
# saveRDS(rf, '../pct-bigdata/rf_gm1.rds')
# 
# rq = line2route(l=l, route_fun = route_cyclestreet, base_url = "http://pct.cyclestreets.net", plan = "quietest")
# rq$id = paste(l$msoa1, l$msoa2)
# saveRDS(rq, '../pct-bigdata/rq_gm1.rds')

ldist= cbind(l@data, rf@data)    #rf 15+1 columns now
ldist$length[is.na(ldist$length)]  = 0  #inner MSOA flows distances=0

######## ADDING to gm.od: DISTANCE-ELEVATION
rm(gm.od1, rf)

gm.od=cbind(id1=paste(gm.od$msoa1 , gm.od$msoa2), id2=paste(gm.od$msoa2 , gm.od$msoa1),
            dist=0, slope=0, gm.od)
gm.od$id1 = as.character(gm.od$id1)
gm.od$id2 = as.character(gm.od$id2)
ldist$id =paste(ldist$msoa1 , ldist$msoa2)


gm.od= left_join(gm.od, ldist[, c(9:22)], by=c('id1'='id') )
sel = ! is.na(gm.od$length) &  ! is.na(gm.od$av_incline)
gm.od$dist[sel] = gm.od$length[sel]
gm.od$slope[sel] = gm.od$av_incline[sel]


gm.od= left_join(gm.od[ , c(1:12)], ldist[, c(9:22)], by=c('id2'='id') )
sel = ! is.na(gm.od$length) &  ! is.na(gm.od$av_incline)
gm.od$dist[sel] = gm.od$length[sel]
gm.od$slope[sel] = gm.od$av_incline[sel]

sum(is.na(gm.od$dist))   # must be 0 ( all distances/slopes rebuilt)
sum(is.na(gm.od$slope))   

gm.od = gm.od[,c(3:12)]
saveRDS(gm.od, './L4/gm.od1.Rds')  # flows w. fast route distances/slopes 


