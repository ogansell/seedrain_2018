#Reactive function to allow for the selection of multiple sites
output$choose_sites <- renderUI(function() {
  sites <- reactive({
    a <- subset(Comb_Sum, year == input$year)
    a <- droplevels(a)
    return(a)
  })
  s <-sites()
  selectInput("Site",h5("Select site:"), 
              choices = as.character(seeds$MonitoringPlace),multiple = TRUE,selected=FALSE)#add "multiple = TRUE" to enable selection of multiple years
})

#Reactive function to subset data when users select a site for graphs
places <- reactive({
  validate(
    need(input$Year != "", "Please select a year(s) to display the graph")
  )#Replaces error message associated with no year being selected
  
  a <- subset(seeds, MonitoringPlace == input$MonitoringPlace)#Add '& year == input$Year' once work out how to limit year selectinput
  
  b <- a[a$year %in% input$Year,]
  #b <- subset(a, year == input$Year)
  a <- NULL
  #a <- droplevels(a)
  b <- droplevels(b)
  #return(a)
  return(b)
})