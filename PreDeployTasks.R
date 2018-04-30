#Pre processing tasks for shiny app


require(dygraphs)
require(dplyr)
require(shiny)
require(tidyquant)
require(tidyr)
require(mondate)
require(shinyjs)
require(padr)
require(leaflet)
require(aspace)
require(mapview)
require(plotly)
require(spsurvey)
require(rgdal)
require(rgeos)
require(mapview)
require(raster)
require(base64)
require(data.table)

#Function for convert crs and then outputting a mapview map for easy viewing of spatial data generated in NZTM
tomap <- function(x){
  nztm <-"+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  prj.LatLong <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")#Set CRS to convert to WGS84
  proj4string(x) = CRS(nztm)
  x <-spTransform(x, prj.LatLong)#Convert CRS to WGS84
  mapview::mapview(x)
}

#Function to make it easy to assign NZTM CRS to spatial data and then convert to WGS for use in leaflet
coord <- function(x){
  nztm <-"+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  prj.LatLong <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")#Set CRS to convert to WGS84
  proj4string(x) = CRS(nztm)
  x <-spTransform(x, prj.LatLong)#Convert CRS to WGS84
}

nztm <-"+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#Load data and process for dygraphs

seeds <-readRDS("ViableAndTotalSumCombined_daily.rds")
seeds <-filter(seeds, Seeds == "All Seeds")
seedtable <-seeds[,c(1:4,8)]
seedtable <-na.omit(seedtable)
seedtable <-dplyr::filter(seedtable, SpeciesName != "All Beech")
seedtable$MonitoringPlace<-as.character(seedtable$MonitoringPlace)
seeds <-seeds[,c(1:3,8)]
seeds <-na.omit(seeds)#Remove missing values
seeds1<-seeds



#Format names for converting to xts later
seeds$MonitoringPlace<-as.character(seeds$MonitoringPlace)
seeds$MonitoringPlace<-gsub("[[:space:]]", "", seeds$MonitoringPlace)
seeds$MonitoringPlace<-gsub(",", "", seeds$MonitoringPlace)
seeds$MonitoringPlace<-gsub("-", "", seeds$MonitoringPlace)


#Split into dataframes for allbeech and each species

seeds_ab <-dplyr::filter(seeds,SpeciesName == "All Beech")
#seeds_ab$SpeciesName <- NULL
seeds_ab<-thicken(seeds_ab, interval = "quarter")
seeds_ab<- pad(seeds_ab,interval = 'quarter', by = 'DateCollected_quarter',group = 'MonitoringPlace')
seeds_ab$DateCollected1<-ifelse(is.na(seeds_ab$DateCollected),as.character(seeds_ab$DateCollected_quarter),as.character(seeds_ab$DateCollected))
seeds_ab$DateCollected<-as.Date(seeds_ab$DateCollected1)
seeds_ab$CumSum_Year[is.na(seeds_ab$CumSum_Year)] <- 0
seeds_ab$SpeciesName<-ifelse(is.na(seeds_ab$SpeciesName),"All Beech", seeds_ab$SpeciesName)
seeds_ab<-seeds_ab[,c(1:4)]

seeds_rb <-dplyr::filter(seeds,SpeciesName == "Red Beech")
#seeds_rb$SpeciesName <- NULL
seeds_rb<-thicken(seeds_rb, interval = "quarter")
seeds_rb<- pad(seeds_rb,interval = 'quarter', by = 'DateCollected_quarter',group = 'MonitoringPlace')
seeds_rb$DateCollected1<-ifelse(is.na(seeds_rb$DateCollected),as.character(seeds_rb$DateCollected_quarter),as.character(seeds_rb$DateCollected))
seeds_rb$DateCollected<-as.Date(seeds_rb$DateCollected1)
seeds_rb$CumSum_Year[is.na(seeds_rb$CumSum_Year)] <- 0
seeds_rb$SpeciesName<-ifelse(is.na(seeds_rb$SpeciesName),"Red Beech", seeds_rb$SpeciesName)
seeds_rb<-seeds_rb[,c(1:4)]

seeds_sb <-dplyr::filter(seeds,SpeciesName == "Silver Beech")
#seeds_sb$SpeciesName <- NULL
seeds_sb<-thicken(seeds_sb, interval = "quarter")
seeds_sb<- pad(seeds_sb,interval = 'quarter', by = 'DateCollected_quarter',group = 'MonitoringPlace')
seeds_sb$DateCollected1<-ifelse(is.na(seeds_sb$DateCollected),as.character(seeds_sb$DateCollected_quarter),as.character(seeds_sb$DateCollected))
seeds_sb$DateCollected<-as.Date(seeds_sb$DateCollected1)
seeds_sb$CumSum_Year[is.na(seeds_sb$CumSum_Year)] <- 0
seeds_sb$SpeciesName<-ifelse(is.na(seeds_sb$SpeciesName),"Silver Beech", seeds_sb$SpeciesName)
seeds_sb<-seeds_sb[,c(1:4)]

seeds_bb <-dplyr::filter(seeds,SpeciesName == "Black Beech")
#seeds_bb$SpeciesName <- NULL
seeds_bb<-thicken(seeds_bb, interval = "quarter")
seeds_bb<- pad(seeds_bb,interval = 'quarter', by = 'DateCollected_quarter',group = 'MonitoringPlace')
seeds_bb$DateCollected1<-ifelse(is.na(seeds_bb$DateCollected),as.character(seeds_bb$DateCollected_quarter),as.character(seeds_bb$DateCollected))
seeds_bb$DateCollected<-as.Date(seeds_bb$DateCollected1)
seeds_bb$CumSum_Year[is.na(seeds_bb$CumSum_Year)] <- 0
seeds_bb$SpeciesName<-ifelse(is.na(seeds_bb$SpeciesName),"Black Beech", seeds_bb$SpeciesName)
seeds_bb<-seeds_bb[,c(1:4)]

seeds_hb <-dplyr::filter(seeds,SpeciesName == "Hard Beech")
#seeds_hb$SpeciesName <- NULL
seeds_hb<-thicken(seeds_hb, interval = "quarter")
seeds_hb<- pad(seeds_hb,interval = 'quarter', by = 'DateCollected_quarter',group = 'MonitoringPlace')
seeds_hb$DateCollected1<-ifelse(is.na(seeds_hb$DateCollected),as.character(seeds_hb$DateCollected_quarter),as.character(seeds_hb$DateCollected))
seeds_hb$DateCollected<-as.Date(seeds_hb$DateCollected1)
seeds_hb$CumSum_Year[is.na(seeds_hb$CumSum_Year)] <- 0
seeds_hb$SpeciesName<-ifelse(is.na(seeds_hb$SpeciesName),"Hard Beech", seeds_hb$SpeciesName)
seeds_hb<-seeds_hb[,c(1:4)]

seeds_mb <-dplyr::filter(seeds,SpeciesName == "Mountain Beech")
#seeds_mb$SpeciesName <- NULL
seeds_mb<-thicken(seeds_mb, interval = "quarter")
seeds_mb<- pad(seeds_mb,interval = 'quarter', by = 'DateCollected_quarter',group = 'MonitoringPlace')
seeds_mb$DateCollected1<-ifelse(is.na(seeds_mb$DateCollected),as.character(seeds_mb$DateCollected_quarter),as.character(seeds_mb$DateCollected))
seeds_mb$DateCollected<-as.Date(seeds_mb$DateCollected1)
seeds_mb$CumSum_Year[is.na(seeds_mb$CumSum_Year)] <- 0
seeds_mb$SpeciesName<-ifelse(is.na(seeds_mb$SpeciesName),"Mountain Beech", seeds_mb$SpeciesName)
seeds_mb<-seeds_mb[,c(1:4)]

seeds <-rbind(seeds_ab, seeds_bb, seeds_hb, seeds_mb, seeds_rb, seeds_sb)

#Save rds files to be loaded when app runs

#seeds_ab1<-as.data.table(seeds_ab)
seedtable<-as.data.table(seedtable, keep.rownames = FALSE)
# seeds_bb<-as.data.table(seeds_bb)
# seeds_hb<-as.data.table(seeds_hb)

# seeddata<-save(seeds,seeds_ab, seeds_bb, seeds_hb, seeds_mb, seeds_nb, 
#                seeds_rb, seeds_sb,seedtable,seeds2,
#                file = "seeddata.RData", compress = FALSE)


write.csv(seedtable, file = "Seedfall_All_data.csv", sep = ",", row.names = FALSE )

seeddata<-save(seeds,seedtable,
               file = "seeddata.RData", compress = FALSE)

#Calculate mean centres for each location using raw data

r1 <-readRDS(file="rawdata_daily.rds")

#Calculate mean centre

r1$MonitoringPlace <-as.character(r1$MonitoringPlace)#Converting to character before subset means empty sites aren't subset
raw_sub <-r1

raw_sub$MonitoringPlace <-as.factor(raw_sub$MonitoringPlace)

rv_split <- split(raw_sub, raw_sub$MonitoringPlace)

sites_mc = NULL
for(i in 1:length(rv_split))
{
  nm = names(rv_split[i])
  df1 = as.data.frame(rv_split[[i]])
  df1 <-dplyr::select(df1,MonitoringPlace,StationID,Easting,Northing)
  df1<-dplyr::distinct(df1,MonitoringPlace,StationID,Easting,Northing)
  df1<-na.omit(df1)
  df1<-dplyr::select(df1, Easting, Northing)
  df1<-mean_centre(id=1, weights=NULL, points=df1)
  df1$MonitoringPlace <-as.character(nm)
  sites_mc <-rbind(sites_mc,df1)
}

colnames(sites_mc) <-c("id","Easting_NZTM","Northing_NZTM","MonitoringPlace")

seeds1<-dplyr::left_join(seeds1, sites_mc,"MonitoringPlace")
seeds1$year <-substr(as.character(seeds1$DateCollected), 1, 4)

y2016<-dplyr::filter(seeds1, as.character(year) == "2016")

ab <-split(y2016, y2016$MonitoringPlace)

#Create list of plot objects for using in leaflet popup
plotlist =list()

  for(i in 1:(length(ab)))
  {
    nm = names(ab[i])
    df1 = as.data.frame(ab[[i]])
    pltName <- as.character(df1$MonitoringPlace[1])
    #Set up plot layout
    #Set up margins
    m = list(
      l = 70,
      r = 50,
      b = 70,
      t = 30,
      pad = 6
    )
    #Specify font
    f1 <- list(
      family = "Archer",
      size = 18,
      color = "black"
    )
    #Format x axis
    a <- list(
      title = "Date",
      titlefont = f1,
      hoverinfo = "none"
    )
    
    #Format y axis
    b <- list(
      title = "Cumulative sum of seeds/m<sup>2</sup>",
      titlefont = f1,
      hoverinfo = "none"
    )
    #Format y axis
    c <- list(
      title = paste0(as.character(df1$MonitoringPlace[1]),as.character(df1$year[1])),
      titlefont = f1
    )
    
    plotdf1 <- plot_ly(df1, x = ~DateCollected, y = ~CumSum_Year, color = ~SpeciesName, mode = "lines+markers")%>%
      layout(title = paste(as.character(df1$MonitoringPlace[1]),as.character(df1$year[1])),
             titlefont = f1,
             xaxis = a, yaxis = b)
    #nm<-plotdf1
    plotlist[[pltName]] <- plotdf1
    # plotpath <-file.path(getwd(),paste("plotly_graphs/",nm,".html",sep = ""))#makes a dataframe of each plot
    # #plotpath <-file.path(paste(tempdir(),nm,"_.html", sep=""))#makes a dataframe of each plot
    # htmlwidgets::saveWidget(as.widget(plotdf1),file = plotpath, selfcontained = TRUE)#saves plot to file
    # base64::encode(plotdf1, "test.base64")
    
  }

saveRDS(plotlist,"plotlist.rds", compress = FALSE)

#Create dataframe of highest values for selected year
y2016a = NULL
  for (i in 1:(length(ab)))
  {
    nm <-names(ab[[i]])
    d = as.data.frame(ab[[i]])
    d<-d %>% group_by(SpeciesName)%>%top_n(1,CumSum_Year)
    y2016a <-rbind(y2016a,d)
  }

y2016a<-filter(y2016a, SpeciesName == "All Beech")

sp<-SpatialPoints(y2016a[6:7])

y2016a <-SpatialPointsDataFrame(sp,as.data.frame(y2016a))

y2016a <-coord(y2016a)

saveRDS(y2016a, "y2016a.rds", compress = FALSE)

#y2016a<-coord(y2016a)

binpal <-colorBin(palette = "YlOrRd", 
                  domain = y2016a$CumSum_Year, 
                  bins = c(0,250,500,2000,10000), na.color = NA)

m <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite",options = tileOptions(max(5))) %>%  # Add esri satellite imagery
  
  addTiles("http://tiles-a.data-cdn.linz.govt.nz/services;key=d27d21709f324848b2d1ffc5e2220036/tiles/v4/layer=767/EPSG:3857/{z}/{x}/{y}.png",group = "Topo",options = tileOptions(max(5))) %>% #Add plain topo 50 imagery
  
  setView(lng = 174.923, lat = -41.268, zoom = 6)%>% #set the view over NZ and the zoom level
  # #
  addCircleMarkers(data = coord(y2016a), 
    #                popup = paste(
    # '<iframe width="400" height="400" frameborder="0" scrolling="no" src="file:///C:/Users/ogansell/Documents/VersionControl/DataVisualisations/shiny_dygraphs/plotly_graphs/',as.character(y2016a$MonitoringPlace),'.html"></iframe>',sep=""),
    #popupIFrame(paste0("plotly_graphs/",as.character(y2016a$MonitoringPlace)), type = 'html'),
    label = as.character(as.character(y2016a$MonitoringPlace)), 
    labelOptions = labelOptions(noHide = F),options = popupOptions(maxWidth = 600, minWidth = 600),
    color = ~binpal(CumSum_Year),weight = 1, fillColor = ~binpal(CumSum_Year), 
    fillOpacity = 0.90)%>%
  
  #addCircleMarkers(data = sites_WGS_2016, popup = popupTable(sites_WGS_2016), label = as.character(sites_WGS_2016$MonitoringPlace), labelOptions = labelOptions(noHide = F))%>%
  
  addLayersControl(baseGroups = c("Satellite","Topo"), options = layersControlOptions(collapsed = FALSE))%>%
  addScaleBar()
  # 
  # addLegend(pal = binpal, values = y2016a$MonitoringPlace, opacity = 1,title = paste('<b>Seed/m<sup>2<sup></b>'))

m  # Print the map

saveRDS(m,"m.rds", compress = FALSE)




