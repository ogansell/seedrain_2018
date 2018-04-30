
library(padr)

seeds_ab <-dplyr::filter(seeds,SpeciesName == "All Beech")
seeds_ab$SpeciesName <- NULL

seeds_ab<-thicken(seeds_ab, interval = "quarter")
seeds_ab<- pad(seeds_ab,interval = 'quarter', by = 'DateCollected_quarter',group = 'MonitoringPlace')
seeds_ab$DateCollected1<-ifelse(is.na(seeds_ab$DateCollected),as.character(seeds_ab$DateCollected_quarter),as.character(seeds_ab$DateCollected))
seeds_ab$DateCollected<-as.Date(seeds_ab$DateCollected1)
seeds_ab$CumSum_Year[is.na(seeds_ab$CumSum_Year)] <- 0
seeds_ab<-seeds_ab[,c(1:3)]

# al <-dplyr::filter(seeds_ab, MonitoringPlace == "Arthur")
# al1<- pad(seeds_ab,interval = 'week', by = 'DateCollected_week',group = 'MonitoringPlace')
t <-tidyr::spread(seeds_ab, MonitoringPlace,CumSum_Year)
#t[is.na(t)] <- 0
#t[t == 0] <- 1
#t[is.na(t)] <- 0
#t<-t%>% padr::pad(t$DateCollected, interval = "month", group = t[, 2:ncol(t)])
t <-tidyquant::as_xts(t, date_col = DateCollected)
#t[t == 0] <- NA
dygraph(t, group = "trees", main = "All beech species combined")%>%
  #dyLegend(show = "always", labelsSeparateLines = TRUE, labelsDiv = "legendDivID")%>%
  dyAxis("y", label = "Cumulative sum seeds/m<sup>2</sup>")%>%
  dyUnzoom()%>%
  dyOptions(drawPoints = TRUE)%>%
  dyOptions(connectSeparatedPoints = TRUE)
  #dyRangeSelector(dateWindow = c(paste0(mondate::mondate(Sys.Date()) - 6), paste0(Sys.Date())))
