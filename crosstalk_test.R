library(crosstalk)
library(plotly)

load("seeddata.RData")

head(seeds)

sd_all <-SharedData$new(seeds, group = 'trees')

sd_all_beech <- SharedData$new(seeds[seeds$SpeciesName == "All Beech",], group = "trees")

sd_red_beech <- SharedData$new(seeds[seeds$SpeciesName == "Red Beech",], group = "trees")

p1 <-plot_ly(sd_all_beech, x = ~DateCollected, y = ~CumSum_Year, type = 'scatter', mode = 'lines + markers',
             color = ~MonitoringPlace)
p2 <-plot_ly(sd_red_beech, x = ~DateCollected, y = ~CumSum_Year, type = 'scatter', mode = 'lines + markers',
             color = ~MonitoringPlace)

filter<- filter_select(id = "MonitoringPlace", "Select a Place", sd_all_beech,~MonitoringPlace, multiple = TRUE)


# gg1 <- ggplot(sd_red_beech) + 
#   geom_line(aes(DateCollected, CumSum_Year, group = MonitoringPlace)) + 
#   ggtitle("A plot with filter events")
# filter <- ggplotly(gg1, dynamicTicks = TRUE)


bscols(filter, p1,p2, widths = 12,12,12)
