library(lubridate)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  observeEvent(input$resetSites, {
    shinyjs::reset("Sites")
  })
  
  # observeEvent(
  #   if (nrow(input$Sites)>0){
  #     shinyjs::show("downloadbutton")
  #   } else {
  #     shinyjs::hide("downloadbutton")
  #   }
  # )
  
  observeEvent(input$Sites, {
    shinyjs::show("downloadbutton")
  })
  
  observeEvent(input$resetSites, {
    shinyjs::toggle("downloadbutton")
  })

  # observeEvent(input$Sites, {
  #   shinyjs::show("downloadbutton")
  # })
  
  observeEvent(input$resetSites, {
    shinyjs::hide("graphlegend")
  })
  
  observeEvent(input$Sites, {
    shinyjs::show("graphlegend")
  })
  
  observeEvent(input$resetSites, {
    shinyjs::hide("mainpanel")
  })
  
  observeEvent(input$Sites, {
    shinyjs::show("mainpanel")
  })
  
  library(tidyquant)
  library(tidyr)
  
  #One reactive event subsetting sites
  
  sites <-reactive({
    a <-dplyr::filter(seeds, as.character(seeds$MonitoringPlace) %in% input$Sites)
  })
  
  # #Reactive event with xts conversion
  # allbeech <- reactive({
  #   a <-dplyr::filter(seeds_ab, as.character(seeds_ab$MonitoringPlace) %in% input$Sites)
  #   t <-tidyr::spread(a, MonitoringPlace,CumSum_Year)
  #   t <-tidyquant::as_xts(t, date_col = DateCollected)
  # })
  # 
  # #Red beech reactive event with xts conversion
  # redbeech <- reactive({
  #   a <-dplyr::filter(seeds_rb, as.character(seeds_rb$MonitoringPlace) %in% input$Sites)
  #   t <-tidyr::spread(a, MonitoringPlace,CumSum_Year)
  #   t <-tidyquant::as_xts(t, date_col = DateCollected)
  # })
  # 
  # #Silver beech reactive event with xts conversion
  # silverbeech <- reactive({
  #   a <-dplyr::filter(seeds_sb, as.character(seeds_sb$MonitoringPlace) %in% input$Sites)
  #   t <-tidyr::spread(a, MonitoringPlace,CumSum_Year)
  #   t <-tidyquant::as_xts(t, date_col = DateCollected)
  # })
  # 
  # #Mountain beech reactive event with xts conversion
  # mountainbeech <- reactive({
  #   a <-dplyr::filter(seeds_mb, as.character(seeds_mb$MonitoringPlace) %in% input$Sites)
  #   t <-tidyr::spread(a, MonitoringPlace,CumSum_Year)
  #   t <-tidyquant::as_xts(t, date_col = DateCollected)
  # })
  # 
  # #Black beech reactive event with xts conversion
  # blackbeech <- reactive({
  #   a <-dplyr::filter(seeds_bb, as.character(seeds_bb$MonitoringPlace) %in% input$Sites)
  #   t <-tidyr::spread(a, MonitoringPlace,CumSum_Year)
  #   t <-tidyquant::as_xts(t, date_col = DateCollected)
  # })
  # 
  # #Black beech reactive event with xts conversion
  # hardbeech <- reactive({
  #   a <-dplyr::filter(seeds_hb, as.character(seeds_hb$MonitoringPlace) %in% input$Sites)
  #   t <-tidyr::spread(a, MonitoringPlace,CumSum_Year)
  #   t <-tidyquant::as_xts(t, date_col = DateCollected)
  # })
  
  #library(mondate)
  
  output$allbeech1 <- renderDygraph({
    sites <-sites()
    allbeech <-filter(sites, SpeciesName == "All Beech")
    allbeech$SpeciesName <-NULL
    allbeech <-tidyr::spread(allbeech, MonitoringPlace,CumSum_Year)
    allbeech <-tidyquant::as_xts(allbeech, date_col = DateCollected)
    #allbeech <-allbeech()
    dygraph(allbeech, group = "trees", main = "All beech species combined")%>%
      dyOptions(drawPoints = TRUE, pointSize = 5, strokeWidth = 3)%>%
      #dyRangeSelector(dateWindow = c(paste(today() - years(2)),paste(today())),retainDateWindow = FALSE)%>%
      dyLegend(show = "always", labelsSeparateLines = TRUE, labelsDiv = "legendDivID")%>%
      dyAxis("y", label = "Cumulative sum seeds/m<sup>2</sup>")%>%
      dyUnzoom()%>%
      dyOptions(connectSeparatedPoints = TRUE)%>%
      dyCrosshair(direction = "vertical")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      #dyShading(from = 500, to = max(allbeech), axis = "y") %>%
      dyCallbacks(drawCallback = dyRegister())
  })
  
  output$redbeech1 <- renderDygraph({
    sites <-sites()
    redbeech <-filter(sites, SpeciesName == "Red Beech")
    redbeech$SpeciesName <-NULL
    redbeech <-tidyr::spread(redbeech, MonitoringPlace,CumSum_Year)
    redbeech <-tidyquant::as_xts(redbeech, date_col = DateCollected)
    #redbeech <-redbeech()
    dygraph(redbeech , group = "trees", main = "Red beech")%>%
      dyLegend(dygraph, show = "follow")%>%
      dyAxis("y", label = "Cumulative sum seeds/m<sup>2</sup>")%>%
      dyOptions(drawPoints = TRUE, pointSize = 5, strokeWidth = 3)%>%
      dyOptions(connectSeparatedPoints = TRUE)%>%
      dyCrosshair(direction = "vertical")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyCallbacks(drawCallback = dyRegister())
  })

  output$silverbeech1 <- renderDygraph({
    sites <-sites()
    silverbeech <-filter(sites, SpeciesName == "Silver Beech")
    silverbeech$SpeciesName <-NULL
    silverbeech <-tidyr::spread(silverbeech, MonitoringPlace,CumSum_Year)
    silverbeech <-tidyquant::as_xts(silverbeech, date_col = DateCollected)
    #silverbeech <-silverbeech()
    dygraph(silverbeech, group = "trees", main = "Silver beech")%>%
      dyLegend(dygraph, show = "follow")%>%
      dyAxis("y", label = "Cumulative sum seeds/m<sup>2</sup>")%>%
      dyOptions(drawPoints = TRUE, pointSize = 5, strokeWidth = 3)%>%
      dyOptions(connectSeparatedPoints = TRUE)%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyCrosshair(direction = "vertical")%>%
      dyCallbacks(drawCallback = dyRegister())
  })

  output$mountainbeech1 <- renderDygraph({
    sites <-sites()
    mountainbeech <-filter(sites, SpeciesName == "Mountain Beech")
    mountainbeech$SpeciesName <-NULL
    mountainbeech <-tidyr::spread(mountainbeech, MonitoringPlace,CumSum_Year)
    mountainbeech <-tidyquant::as_xts(mountainbeech, date_col = DateCollected)
    #mountainbeech <-mountainbeech()
    dygraph(mountainbeech, group = "trees", main = "Mountain beech")%>%
      dyLegend(dygraph, show = "follow")%>%
      dyAxis("y", label = "Cumulative sum seeds/m<sup>2</sup>")%>%
      dyOptions(drawPoints = TRUE, pointSize = 5, strokeWidth = 3)%>%
      dyOptions(connectSeparatedPoints = TRUE)%>%
      dyCrosshair(direction = "vertical")%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))%>%
      dyCallbacks(drawCallback = dyRegister())
  })

  output$blackbeech1 <- renderDygraph({
    sites <-sites()
    blackbeech <-filter(sites, SpeciesName == "Black Beech")
    blackbeech$SpeciesName <-NULL
    blackbeech <-tidyr::spread(blackbeech, MonitoringPlace,CumSum_Year)
    blackbeech <-tidyquant::as_xts(blackbeech, date_col = DateCollected)    #blackbeech <-blackbeech()
    dygraph(blackbeech, group = "trees", main = "Black beech")%>%
      dyLegend(dygraph, show = "follow")%>%
      dyAxis("y", label = "Cumulative sum seeds/m<sup>2</sup>")%>%
      dyOptions(drawPoints = TRUE, pointSize = 5, strokeWidth = 3)%>%
      dyOptions(connectSeparatedPoints = TRUE)%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyCrosshair(direction = "vertical")%>%
      dyCallbacks(drawCallback = dyRegister())
  })

  output$hardbeech1 <- renderDygraph({
    sites <-sites()
    hardbeech <-filter(sites, SpeciesName == "Hard Beech")
    hardbeech$SpeciesName <-NULL
    hardbeech <-tidyr::spread(hardbeech, MonitoringPlace,CumSum_Year)
    hardbeech <-tidyquant::as_xts(hardbeech, date_col = DateCollected)
    #hardbeech <-hardbeech()
    dygraph(hardbeech, group = "trees", main = "Hard beech")%>%
      dyLegend(dygraph, show = "follow")%>%
      dyAxis("y", label = "Cumulative sum seeds/m<sup>2</sup>")%>%
      dyOptions(drawPoints = TRUE, pointSize = 5, strokeWidth = 3)%>%
      dyOptions(connectSeparatedPoints = TRUE)%>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyCrosshair(direction = "vertical")%>%
      dyCallbacks(drawCallback = dyRegister())
  })
  # 
  # # Render UI's based on only 1 reactive - Sites()
  output$rb<-renderUI({
    if(nrow(filter(sites(),SpeciesName == "Red Beech")>0)){
      dygraphOutput("redbeech1", height = "250px")
    } else NULL
  })

  output$sb<-renderUI({
    if(nrow(filter(sites(),SpeciesName == "Silver Beech")>0)){
      dygraphOutput("silverbeech1", height = "250px")
    } else NULL
  })

  output$hb<-renderUI({
    if(nrow(filter(sites(),SpeciesName == "Hard Beech")>0)){
      dygraphOutput("hardbeech1", height = "250px")
    } else NULL
  })
  # 
  output$bb<-renderUI({
    if(nrow(filter(sites(),SpeciesName == "Black Beech")>0)){
      dygraphOutput("blackbeech1", height = "250px")
    } else NULL
  })
  # 
  output$mb<-renderUI({
    if(nrow(filter(sites(),SpeciesName == "Mountain Beech")>0)){
      dygraphOutput("mountainbeech1", height = "250px")
    } else NULL
  })
  
  # Render UI's based on only reactives for each species
  # output$rb<-renderUI({
  #   if(nrow(redbeech())>0){
  #     dygraphOutput("redbeech1", height = "250px")
  #   } else NULL
  # })
  # 
  # output$sb<-renderUI({
  #   if(nrow(silverbeech())>0){
  #     dygraphOutput("silverbeech1", height = "250px")
  #   } else NULL
  # })
  # 
  # output$hb<-renderUI({
  #   if(nrow(hardbeech())>0){
  #     dygraphOutput("hardbeech1", height = "250px")
  #   } else NULL
  # })
  # 
  # output$bb<-renderUI({
  #   if(nrow(blackbeech())>0){
  #     dygraphOutput("blackbeech1", height = "250px")
  #   } else NULL
  # })
  # 
  # output$mb<-renderUI({
  #   if(nrow(mountainbeech())>0){
  #     dygraphOutput("mountainbeech1", height = "250px")
  #   } else NULL
  # })
  
  

  # #Render UI for download of graphs as they appear
   output$rbg<-renderUI({
     if(nrow(filter(sites(),SpeciesName == "Red Beech"))>0){
       dyDownload("redbeech1", "Red beech", asbutton = FALSE)
     } else NULL
   })
  
   output$sbg<-renderUI({
     if(nrow(filter(sites(),SpeciesName == "Silver Beech"))>0){
       dyDownload("silverbeech1", "Silver beech", asbutton = FALSE)
     } else NULL
   })
  
   output$hbg<-renderUI({
     if(nrow(filter(sites(),SpeciesName == "Hard Beech"))>0){
       dyDownload("hardbeech1", "Hard beech", asbutton = FALSE)
     } else NULL
   })
  
   output$bbg<-renderUI({
     if(nrow(filter(sites(),SpeciesName == "Black Beech"))>0){
       dyDownload("blackbeech1", "Black beech", asbutton = FALSE)
       } else NULL
   })
  
   output$mbg<-renderUI({
     if(nrow(filter(sites(),SpeciesName == "Mountain Beech"))>0){
       dyDownload("mountainbeech1", "Mountain beech", asbutton = FALSE)
    } else NULL
  })
  
  #Try dyDownloadGroup in a reactive ui
  # output$dydown<-renderUI({
  #     dyDownloadGroup("trees", "Download Plots:",
  #                     c("All beech species" = "allbeech1",
  #                       if(nrow(filter(sites(),SpeciesName == "Red Beech"))>0){
  #                        "Red beech" = "redbeech1"
  #                       } else NULL,
  #                       if(nrow(filter(sites(),SpeciesName == "Silver Beech"))>0){
  #                         "Silver beech" = "silverbeech1"
  #                       } else NULL,
  #                       if(nrow(filter(sites(),SpeciesName == "Hard Beech"))>0){
  #                         "Hard beech" = "hardbeech1"
  #                       } else NULL,
  #                       if(nrow(filter(sites(),SpeciesName == "Black Beech"))>0){
  #                         "Black beech" = "blackbeech1"
  #                       } else NULL,
  #                       if(nrow(filter(sites(),SpeciesName == "Mountain Beech"))>0){
  #                         "Mountain beech" = "mountainbeech1"
  #                       } else NULL
  #                       ))
  #                   })
  
  output$seedtable <-DT::renderDataTable({
      DT::datatable(seedtable, colnames = c("Monitoring Site", "Date Collected","Species Name", "Seeds m<sup>2</sup>",
                                            "Cumulative sum for year to date"),filter = "top",escape = FALSE, rownames = FALSE,
                    class = "stripeytable")
    
  })
  
})