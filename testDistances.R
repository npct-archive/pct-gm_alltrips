
library(stplanr)

wc.agg.msoa = aggregate(wc$dist, by=list(wc$MSOAOrig , wc$MSOADest), FUN=sum,na.rm=T)
names(wc.agg.msoa)=c('MSOAOrig','MSOADest', 'distsum')
wc.agg.msoa1= stplanr::onewayid(wc.agg.msoa, attrib= c(3))
wc.agg.msoa1= data.frame(wc.agg.msoa1)

del.rows = grep(pattern ='W',x = wc.agg.msoa1$MSOAOrig,value = F )
wc.agg.msoa1=wc.agg.msoa1[-del.rows, ]

del.rows = grep(pattern ='W',x = wc.agg.msoa1$MSOADest,value = F )
wc.agg.msoa1=wc.agg.msoa1[-del.rows, ]

#read ukmsoas 
pathGM <- '../pct-bigdata/'  #before w/o: -NC
ukmsoas = readRDS(file.path(pathGM,'ukmsoas-scenarios.Rds'))


wc.agg.msoa2 <- stplanr::od2line(wc.agg.msoa1, ukmsoas)
wc.dist =stplanr::line2route(wc.agg.msoa2, route_fun = route_cyclestreet, 
                             base_url = "http://pct.cyclestreets.net", plan = "fastest")
