
# output$plot1<-renderPlotly({
#   allbeech <-allbeech()
#   ab <-split(allbeech, allbeech$SpeciesName)
#   myplots<-lapply(ab,function(x)
#   p<-plot_ly(allbeech, x =~Date, color = ~MonitoringPlace )%>%
#     add_trace(y = ~CumSum_Year, type = "scatter", mode = "lines+markers",visible = TRUE,
#               legendgroup = paste0(allbeech$MonitoringPlace),showlegend = T)%>%
#     layout(title = ~SpeciesName,showlegend = T
#            ,xaxis = list (
#              rangeslider = list(type = "date"))
#            )
# )
# subplot(myplots, nrows = length(myplots),shareX = TRUE)
# })



# output$plot<-renderPlotly({
#   allbeech <-allbeech()
#   plot_ly(allbeech, x = ~Date, y = ~CumSum_Year, color = ~MonitoringPlace
#           , type = 'scatter', mode = 'lines') %>%
#     layout(title = ~SpeciesName
#            ,xaxis = list(
#       rangeslider = list(type = "date"))
#       )
#   })


#plotlyOutput('plot')

zp <- ggplot(data = seeds, aes(x = DateCollected, y = CumSum_Year, group = MonitoringPlace,
                               colour = as.factor(MonitoringPlace)))+ 
  geom_line()+ 
  geom_point()+
  facet_wrap(~SpeciesName,ncol = 1)+
  theme(strip.background = element_blank())+
  theme(legend.title = element_text())+
  labs(legend.title = "")
  
  
zp

zp<-qplot(DateCollected, CumSum_Year, data = seeds, geom = "line", group = MonitoringPlace) +
  facet_wrap(~SpeciesName, ncol = 1) 

zp <-ggplotly(zp)
zp

