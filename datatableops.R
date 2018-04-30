library(data.table)

a<-Sys.time()

a1 <-dplyr::filter(seeds_ab, as.character(seeds_ab$MonitoringPlace) == "Hawdon")
t <-tidyr::spread(a1, MonitoringPlace,CumSum_Year)
t <-tidyquant::as_xts(t, date_col = DateCollected)
dygraph(t, group = "trees", main = "All beech species combined")
b<-Sys.time()
b-a

a<-Sys.time()
#a2<-as.data.table(seeds_ab)
a3<-subset(a2, MonitoringPlace == "Hawdon")
a3<-dcast(a3,DateCollected~MonitoringPlace, value.var='CumSum_Year')
t1<-as.xts.data.table(a3)
dygraph(t1, group = "trees", main = "All beech species combined")
b<-Sys.time()
b-a

