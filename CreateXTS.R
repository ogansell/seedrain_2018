#Create clean xts object

seeds1<-dplyr::filter(seeds,SpeciesName == "All Beech")#Create one species to test xts

seeds1$MonitoringPlace<-as.character(seeds1$MonitoringPlace)

seeds1$MonitoringPlace<-gsub("[[:space:]]", "", seeds1$MonitoringPlace)
seeds1$MonitoringPlace<-gsub(",", "", seeds1$MonitoringPlace)
seeds1$MonitoringPlace<-gsub("-", "", seeds1$MonitoringPlace)

t <-tidyr::spread(seeds1, MonitoringPlace,CumSum_Year)

t[is.na(t)] <- 0

t <-t[,c(1:10)]

t <-tidyquant::as_xts(t, date_col = DateCollected)

dygraph(t, group = "trees")

%>%
  
  
  dyOptions(dygraph, stackedGraph = TRUE)
