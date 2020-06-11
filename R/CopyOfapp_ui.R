#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui2 <- function(request) {
  devtools::source_url("https://raw.githubusercontent.com/DrMattG/ShinyNINA/master/Shinytheme_NINA.R")
  title <- tags$a(href='https://www.nina.no',
                  'Lynx harvest', target="_blank")
  #packages
  library(HydeNet)
  #install_github("rich-iannone/DiagrammeR") need development version
  #install_version("DiagrammeR", version = "1.0.1", repos = "http://cran.us.r-project.org")
  library(DiagrammeR)
  library(data.table)
  library(rvest)
  library(rgdal)
  library(leaflet)
  library(fuzzyjoin)
  library(readxl)
  library(tidyverse)
  library(plotly)
  library(shinydashboard)
  library(shinythemes)
  library(shiny)
  library(leaflet.extras)
  library(dashboardthemes)
  library(tidyverse)
  library(httr)
  library(rjstat)
  library(htmlwidgets)
  
  get_data()
  
  #Build the shiny App
  
  title <- tags$a(href='https://www.nina.no',
                  'Lynx harvest', target="_blank")
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    # Define UI for application 
    #UI
    dashboardPage(
      dashboardHeader(
        title = title, titleWidth=600),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Visit-us", icon = icon("send",lib='glyphicon'),
                   href = "https://www.nina.no")
        )
      ),
      dashboardBody(theme_nina,
                    tags$head(
                      tags$link(rel="stylesheet", type= "text/css", href="custom1.css")),
                    tabsetPanel(
                      id = "tabs",
                      tabPanel(
                        title = "Lynx data",
                        value = "page1",
                        fluidRow( box(title = "Recorded causes of mortality (will update with new data from Statistics Norway automatically)"
                                      ,status = "primary"
                                      ,solidHeader = TRUE
                                      ,collapsible = TRUE
                                      , selectInput("reason", "Choose the reported reason:",
                                                    c("Total"= "Total",
                                                      "Noxious" = "Animals felled as noxious",
                                                      "Self-defence" = "Animals felled as self-defence",
                                                      "Illegal" = "Animals felled illegally",
                                                      "Vehicle" = "Animals killed by motor car",
                                                      "Train" = "Animals killed by train",
                                                      "Other" = "Animals killed by other causes",
                                                      "Quota (Lynx hunting)" = "Animals felled under quota hunting"
                                                    ))
                                      ,plotlyOutput("mortality", height = "600px")),
                                  box(title = "Region"
                                      ,status = "primary"
                                      ,solidHeader = TRUE
                                      ,collapsible = TRUE
                                      ,selectInput("region", "Choose the region",
                                                   c( "Østfold, Akershus, Oslo"="Østfold, Akershus, Oslo",
                                                      "Buskerud, Vestfold, Telemark, Aust-Agder"="Buskerud, Vestfold, Telemark, Aust-Agder",
                                                      "Møre og Romsdal, Trøndelag"="Møre og Romsdal, Trøndelag",
                                                      "Hedmark"="Hedmark",
                                                      "Nordland"="Nordland",
                                                      "Oppland"="Oppland",
                                                      "Troms, Finmark"="Troms, Finmark",
                                                      "Vest-Agder, Rogaland,Hordaland, Sogn og Fjordane"="Vest-Agder, Rogaland,Hordaland, Sogn og Fjordane")
                                                   , multiple = FALSE)
                                      ,plotOutput("region", height = "600px"))),
                        
                        fluidRow( box(title = "Licenses issued (will update with new data from Statistics Norway automatically)"
                                      ,status = "primary"
                                      ,solidHeader = TRUE
                                      ,collapsible = TRUE
                                      ,plotlyOutput("license", height = "600px")),
                                  box(title = "Regional licenses"
                                      ,status = "primary"
                                      ,solidHeader = TRUE
                                      ,collapsible = TRUE
                                      ,selectInput("regionL", "Choose the region",
                                                   c( "Østfold, Akershus, Oslo"="Østfold, Akershus, Oslo",
                                                      "Buskerud, Vestfold, Telemark, Aust-Agder"="Buskerud, Vestfold, Telemark, Aust-Agder",
                                                      "Møre og Romsdal, Trøndelag"="Møre og Romsdal, Trøndelag",
                                                      "Hedmark"="Hedmark",
                                                      "Nordland"="Nordland",
                                                      "Oppland"="Oppland",
                                                      "Troms, Finmark"="Troms, Finmark",
                                                      "Vest-Agder, Rogaland,Hordaland, Sogn og Fjordane"="Vest-Agder, Rogaland,Hordaland, Sogn og Fjordane")
                                                   , multiple = FALSE)
                                      ,plotOutput("regionL", height = "600px")))),
                      
                      # tabPanel(
                      #   title = "Female-only model",
                      #   value = "page2",
                      #   # fluidRow( box(title = "Plot the model"
                      #   #               ,status = "primary"
                      #   #               ,solidHeader = TRUE
                      #   #               ,collapsible = TRUE
                      #   #               ,grVizOutput("picture")
                      #   # ),
                      #   fluidRow(box(title="FG"
                      #       ,status ="primary"
                      #       ,solidHeader = TRUE
                      #       ,collapsible = TRUE
                      #       ,selectInput("fg","Input last FG estimate", choices=seq(1,150,1), multiple = FALSE, selected = 50 )
                      #       # ,selectInput("fgsd","Input last FG estimate standard deviation", choices=seq(1,15,1), multiple = FALSE, selected = 2 )
                      #   )),
                      #   fluidRow(box(title="Select Harvest"
                      #                ,status ="primary"
                      #                ,solidHeader = TRUE
                      #                ,collapsible = TRUE
                      #                ,selectInput("quota1","Choose quota",choices=seq(0,150,10), multiple = TRUE, selected = 0 )
                      #                ,tableOutput("table"))
                      #                ,actionButton("go", "To run the model please click here"),
                      #            
                      #            box(title="Results"
                      #                ,status ="primary"
                      #                ,solidHeader = TRUE
                      #                ,collapsible = TRUE
                      #                ,plotOutput("plot1")))
                      # ),
                      tabPanel(
                        title = "'Two-sex - multi-stage' population model",
                        value = "page3",
                        # fluidRow( box(title = "Plot the model"
                        #               ,status = "primary"
                        #               ,solidHeader = TRUE
                        #               ,collapsible = TRUE
                        #               ,grVizOutput("picture")
                        # ),
                        fluidRow(box(title="FG"
                                     ,status ="primary"
                                     ,solidHeader = TRUE
                                     ,collapsible = TRUE
                                     ,selectInput("fg","Input last FG estimate", choices=seq(1,150,1), multiple = FALSE, selected = 50 )
                                     # ,selectInput("fgsd","Input last FG estimate standard deviation", choices=seq(1,15,1), multiple = FALSE, selected = 2 )
                        )),
                        fluidRow(box(title="Select Harvest"
                                     ,status ="primary"
                                     ,solidHeader = TRUE
                                     ,collapsible = TRUE
                                     ,selectInput("quota1","Choose quota",choices=seq(0,150,10), multiple = TRUE, selected = 0 )
                                     ,tableOutput("table"))
                                 ,actionButton("go", "To run the model please click here"),
                                 
                                 box(title="Results"
                                     ,status ="primary"
                                     ,solidHeader = TRUE
                                     ,collapsible = TRUE
                                     ,plotOutput("plot1")))
                      ),
                      tabPanel(
                        title="Map",
                        value="page4",
                        fluidRow(box(title= "Map of the regions (takes a few seconds to load)"
                                     ,status= "primary"
                                     ,solidHeader = TRUE
                                     ,collapsible = TRUE
                                     ,selectInput("year", "Aar",choices=sort(as.numeric(unique(plotdat$Aar))))
                                     ,leafletOutput("mymap")),
                                 tabBox(
                                   title = "Lynx map",
                                   # The id lets us use input$tabset1 on the server to find the current tab
                                   id = "tabset1", height = "250px",
                                   tabPanel("Map key", "Indication of location of harvested lynx (at a regional scale) per year"),
                                   tabPanel("Some text", "Some text")
                                 )
                        )
                      )
                    )
      )
    ))
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'HarvestGolem'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

