
```{r map, echo=FALSE, warning=FALSE, message=FALSE}

 m <-readRDS("m.rds")
 y2016a <-readRDS("y2016a.rds")

 plotlist<-readRDS("plotlist.rds")

 output$map<-renderLeaflet({

   binpal <-colorBin(palette = "YlOrRd",
                   domain = y2016a$CumSum_Year,
                   bins = c(0,250,500,2000,10000), na.color = NA)

 m <- leaflet() %>%
   addProviderTiles("Esri.WorldImagery", group = "Satellite",options = tileOptions(max(5))) %>%   Add esri satellite imagery

   addTiles("http://tiles-a.data-cdn.linz.govt.nz/services;key=d27d21709f324848b2d1ffc5e2220036/tiles/v4/layer=767/EPSG:3857/{z}/{x}/{y}.png",group = "Topo",options = tileOptions(max(5))) %>% Add plain topo 50 imagery

   setView(lng = 174.923, lat = -41.268, zoom = 6)%>% set the view over NZ and the zoom level
    
   addCircleMarkers(data = y2016a,
     label = as.character(as.character(y2016a$MonitoringPlace)),
     labelOptions = labelOptions(noHide = F),options = popupOptions(maxWidth = 600, minWidth = 600),
     color = ~binpal(CumSum_Year),weight = 1, fillColor = ~binpal(CumSum_Year),
     fillOpacity = 0.90)%>%

   addCircleMarkers(data = sites_WGS_2016, popup = popupTable(sites_WGS_2016), label = as.character(sites_WGS_2016$MonitoringPlace), labelOptions = labelOptions(noHide = F))%>%

   addLayersControl(baseGroups = c("Satellite","Topo"), options = layersControlOptions(collapsed = FALSE))%>%
   addScaleBar()
   
    addLegend(pal = binpal, values = y2016a$MonitoringPlace, opacity = 1,title = paste('<b>Seed/m<sup>2<sup></b>'))

 m   #Print the map

   })

sidebarPanel(plotlist[2])

 mainPanel(leafletOutput("map"))

```

