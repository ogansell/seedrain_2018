

# Define UI for random distribution application 
shinyUI(
  
  fluidPage(
  
  useShinyjs(),
  
  includeCSS('http://www.doc.govt.nz/themes/2014/css/2014.min.css'),
  
  tags$head(tags$script(src = "dygraph-extra.js")),
  tags$head(tags$script(src = "iframeResizer.contentWindow.min")),
  tags$script('$.fn.dataTableExt.sErrMode = "throw";'),
  tags$head(dyExtraHead()),
  
  HTML("
      <style>
      .h1sub {
        color: #575757;
        text-align: center;
        font-weight: normal;
        display: none;
      }
    
    @media print {
      .h1sub {
        top: -500px;
        color: #575757;
          text-align: center;
        font-weight: normal;
        display: block;
      }

      .dygraph-rangesel-fgcanvas {
       display: none;
       }
      
      .no-print, .no-print *
      {
        display: none !important;
      }

      .nav.nav-tabs {
      display: none;!important;
      }

    }
    
    .dygraph-title {
      color: black;
      font-weight: normal;
      font-family: Archer;
    }
    .dygraph-axis-label {
      font-size: 11px;
      font-family: Archer;
    }
    
    .dygraph-label.dygraph-ylabel {
      font-size: 18px;
      font-family: Archer;
    }
    
    .dygraph-legend {
      font-weight: normal;
      font-size: 11px;
      font-family: Archer;
    }
    
    .shiny-output-error {
      visibility: hidden;
     }

    .shiny-output-error:before {
      visibility: hidden;
    }


    html {
        overflow: scroll;
        overflow-x: hidden;
          }

        ::-webkit-scrollbar {
          width: 0px;  /* remove scrollbar space */
          background: transparent;  /* optional: just make scrollbar invisible */
                              }
       /* optional: show position indicator in red */
       ::-webkit-scrollbar-thumb {
       background: #FF0000;
       }


      .btn, button, input[type='submit'], input[type='reset'], input[type='button'] {
    background: #008cb2;
       font-size: 100%;
       font-family: 'ClearSans',sans-serif;
       text-decoration: underline;
       padding: .6em 1.3em;
       color: #fff;
       text-decoration: none;
       -webkit-transition: color .4s ease;
       -o-transition: color .4s ease;
       -moz-transition: color .4s ease;
       transition: color .4s ease;
       border: none;
       height: 52px;
       cursor: pointer;
       border-radius:0px;
      }

    .btn:hover {
        background: #008cb2;
        text-decoration: underline;
    }

    .img-responsive {
    max-width: 100%;
       height: auto;
       }

    </style>
       
       <header>
<img src='http://www.doc.govt.nz//themes/2014/images/doc-logo-black-and-white.png' width='150' height='26' 
class='printonly printlogo' alt=''>
       
       <h1 class='printonly'> Seedfall data explorer</h1>
       </header>
       <h1 class='h1sub'>
       A place where you can make cool graphs
       </h1>"
    
  ),
  
  tabsetPanel(
    tabPanel(
      title = "Getting started",
      fluidRow(column(6,
             h2("About"),
             p("This application allows users to access and visualise information from DOC's seed rain monitoring programme. Seed numbers are monitored at 
            a range of sites throughout the country to help to predict whether a masting event will occur. Masting events in beech and podocarp forests can lead
            to explosions in rodent numbers, which in turn fuels eruptions in stoat numbers. If a mast is predicted and seed numbers confirm it then DOC may initiate predator 
            control to mitigate any impacts from predator irruptions. This applications aims to make it easy for interested parties to access this information to use for 
               planning and communicating the need for pest control."),
             h3("Using this app"),
             p("The following gives a brief description of the functionality of this application. If the description below don't make sense then refer to the animated 
               image in this page."), 
             h4("Graphs"),
             p("You can select which sites you want to display the information for in the dropdown menu. The graphs will show the seed rain data for the 
               entire timeseries of the data for each site. You can zoom in on a period of interest by clicking and dragging over the period. The graph will zoom in.
               You can reset the graph by clicking on the rezoom button. You can download the graph for all beech species below 'Download graphs:'. You can reset the site selection 
               by pushing the 'Reset sites' button."),
             h4("Tables"),
             p("You can also view the data under the 'Data Tables' tab. The tables allow you to select data from places or time periods of interest. You can download the entire dataset
               for all sites from the 'Download data' button.")), 
             column(6,img(src='seedraingif.gif', align = "center", class = "img-responsive")))
    ),
    tabPanel(title = "Visualise data",  
  
  fluidRow(column(3,
                  div(id = "Sites",selectInput("Sites",h5("Select seed rain monitoring sites:"), 
                                               choices = seeds$MonitoringPlace,selected=TRUE, multiple = TRUE),
                      
                      actionButton("resetSites", "Reset Site selection"),
                      
                      class="no-print"),
                  
                  br(),
                  
                  div(id = "downloadbutton",h5("Download graphs:"),
                      dyDownload("allbeech1", "All beech", asbutton = FALSE),
                      uiOutput("sbg"),
                      uiOutput("mbg"),
                      uiOutput("rbg"),
                      uiOutput("hbg"),
                      uiOutput("bbg"),
                      #uiOutput("dydown"),
                      class="no-print"),
              
                  
                  br(),
                  
                  div(id = "graphlegend",
                      h5("Monitoring sites"),
                      textOutput("legendDivID"))), 
           
           column(9,
                  div(id = "mainpanel",
                      dygraphOutput("allbeech1" , height = "350px"),
                      uiOutput("sb"),
                      uiOutput("mb"),
                      uiOutput("rb"),
                      uiOutput("hb"),
                      uiOutput("bb")
                  )))
    ),#Close tabPanel
    # tabPanel(title = "Interactive map",
    #          m
    #          ),#Close tabPanel
    tabPanel(title = "Data table",
             HTML('  
                  <button type="button">
                  <a href="Seedfall_All_data.csv" download="Seedfall_All_data.csv"><font color="white">Download data</font></a>
                  </button>
                  '),
             br(""),
             bscols(dataTableOutput("seedtable"))
             )#Close tabsetPanel
  
)))
