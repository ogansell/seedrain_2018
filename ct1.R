
library(crosstalk)

seeds1$year<-as.factor(seeds1$year)

sd_all <-crosstalk::SharedData$new(seeds1, group = 'trees')



filter1 <-crosstalk::filter_select(id = "year", "Select a year", sd_all,~year, multiple = FALSE)
filter2 <-crosstalk::filter_select(id = "MonitoringPlace", "Select a place", sd_all,~MonitoringPlace, multiple = FALSE, allLevels = FALSE)

plotdf1 <- plot_ly(sd_all, x = ~DateCollected, y = ~CumSum_Year, color = ~SpeciesName, mode = "lines+markers")

bscols(filter1, filter2, plotdf1, widths = 12,12,12)





p2 <-plot_ly(sd_ryear2, x = ~DateCollected, y = ~CumSum_Year, type = 'scatter', mode = 'lines + markers',
             color = ~MonitoringPlace, colors = c(paste(as.character(ryearnow$col))), tooltip = "MonitoringPlace")%>%
  layout(showlegend = FALSE, font = list(family = "Archer", size = 14))

