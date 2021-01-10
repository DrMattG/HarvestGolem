#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    title <- tags$a(href='https://www.nina.no',
                  'Lynx harvest Female only', target="_blank")
  #packages
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
  library(shinyWidgets)
  
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    # Define Use Interface (UI) for application 
    shinydashboard::dashboardPage(
      dashboardHeader(
        title = title, titleWidth=600),# end dashboardheader
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Visit-us", icon = icon("send",lib='glyphicon'),
                   href = "https://www.nina.no")
        ) # end sidebar
      ),# ebd dashboardSidebar
      dashboardBody(
        tags$head(
          tags$link(rel="stylesheet", type= "text/css", href="custom1.css")),
        tabsetPanel(
          id = "tabs",
          tabPanel(
            title="Introduction",
            value="page1",
            fluidRow(box(HTML("This is a prototype R Shiny Web Application to interact with lynx harvest prognositic models. 
             'Shiny Apps' are hosted locally by default but can be deployed on a server to allow multiple users and increased flexibility.
             The most advanced prototype uses a female-only model but we can update this model easily and integrate multi-stage demographic models in to the App. The App consists of five pages. 
                          <br>
                          <br> This Introduction page contains an interactive map of the Harvest Management Regions
                                 (you can hover over a Region on the map and it will tell you which region number it is)
                          <br>
                          <br> The next page (Historical data) has a plot of the number of licenses issued for Lynx Harvest (data comes directly from SSB using an API and therefore should update annually). 
                                 Below this is a plot of the proportion of lynx harvest that is made up of Female and Male Adults and Kittens. 
                                 (NB: Kittens here are defined as less than 1 year of age and Adults as greater than 1 year of age. These definitions can be changed if more detail is required). 
                                 Currently, data for this plot is held at NINA and needs to be updated manually (although the plot should update with the updated data) </li></ul> 
                          <br>       
                          <br> The model page hosts the female-only model and allows for user-input in setting a potential number of harvested females for the following season. 
                               The model can be run for the whole of Norway or selected Regions or combinations of Regions. In the case where a combination of Regions is selected then the Regional Targets for each Region in the selection are summed
                          <br>
                          <br> The fourth page hosts some additional features for the more advanced user. These include changing the timeseries used in the model and increasing or decreasing the number of iterations the model runs for
                          <br>
                          <br> The final page shows the full demographic model emulator which is in development to speed up the full demographic model
                             ") # HTML
                                   )), #end box and end fluidrow
            fluidRow(leafletOutput("plotMap")) #end fluidrow
          ), #end tabPanel
          tabPanel(
            title = "Historical data",
            value = "page2",
            fluidRow(plotOutput("plotQ")), #end fluidRow
            fluidRow(plotOutput("plotR"))),#end fluidRow
          tabPanel(
            title= "Model",
            value="page3",
            fluidRow(
              column(12, 
                     h4("Quick start"),
                     box(print("We have set defaults for the expected harvests so if you are happy with these
                     you can proceed to the selection of the model. You can use the 'sliders' to change these values 
                     (please ensure that the lowest number is in the first slider and the highest in the last slider). 
                     In the 'Choose a Model' dropdown menu you have a choice of all the regions
                     (click on 'Select All') or any combination of the harvest management regions. 
                               Once you have made your selection press 'Run model'."))), #end box end column
              
              
              column(8, 
                     h3("Input options"),
                     
                     sliderInput(inputId="min_h.levels", label = "What is the minimum female harvest for the following year", 
                                 value=15, min=0, max=100),
                     sliderInput(inputId="mid_h.levels", label = "What is the expected female harvest for the following year", 
                                 value=30, min=0, max=100),
                     sliderInput(inputId="max_h.levels", label = "What is the maximum female harvest  for the following year",
                                 value=45, min=0, max=100),
                     pickerInput("model","Choose a Model", choices=c("Region_1"= "1",
                                                                     "Region_2"= "2",
                                                                     "Region_3"= "3",
                                                                     "Region_4"= "4",
                                                                     "Region_5"= "5",
                                                                     "Region_6"= "6",
                                                                     "Region_7"= "7",
                                                                     "Region_8"= "8"), 
                                 options = list(`actions-box` = TRUE),
                                 multiple = T)
                     
                     ), #end Column
                            h4("Run model"),
              actionButton("Run.model","Run model", icon("paper-plane"), 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              downloadButton(
                outputId = "downloader",
                label = "Download PDF"
              ), #end download button
            
            
            column(12,
              
              headerPanel("Tabular summary"),
              tabsetPanel(
                tabPanel("Family group population",  tableOutput("table2")), 
                tabPanel("Total population",  tableOutput("table3")),
                tabPanel("Predictive",  tableOutput("table"))
               
              ), #end tabPanel
              headerPanel("Graphical summary"),
              tabsetPanel(     
                tabPanel("Family group population",  plotOutput("plot1")), #end tabPanel
                tabPanel("Total population",  plotOutput("plot2")),#end tabPanel
                tabPanel("Predictive",  plotOutput("plot3"))#end tabPanel
              )#end tabPanel
              
            ), #end column
            
            uiOutput("mcmcPlots")
            ##################
          )), #end fluidRow end tabpanel
          tabPanel(
            title="Advanced settings",
            value="page4",
            fluidRow(box(paste0("Here are some more advanced user inputs if required. We have set defaults so that the user can ignore this 
                                page entirely. Please be aware if you change these values you can end up with small variations in the output due to 
                                the stochastic nature of the model. Some illogical actions are possible with these advanced settings (e.g. you could set the burn in to a larger number than the iterations 
                                this will cause an error in the model and it will not run)."))), #end box end fluidRow
            sliderInput("startYear", 
                        label = "Start year:",
                        min = 1996, max = as.numeric(substr(Sys.time(), 1, 4))-1, value=c(1996),sep=""
                        ,step=1),
            
            sliderInput("endYear", 
                        label = "End year:",
                        min = 1997, max = as.numeric(substr(Sys.time(), 1, 4)), value=as.numeric(substr(Sys.time(), 1, 4)),sep=""
                        ,step=1),
            numericInput("n_its", 
                        label = "number of iterations:",
                        2500000,min=100, max=3000000),
            numericInput("n_chains", 
                         label = "number of chains:",
                         3,min=2, max=5),
            numericInput("burn_in", 
                         label = "Burn in:",
                         1500000,min=1, max=2500000),
            numericInput("n_thin", 
                         label = "Thining:",
                         2,min=1, max=10000)
          ) #end tabPanel
        ) #end tabSetPanel
      ) #end dashboardBody
    ) #end dashboardPage
    )#end tag list
}

# niter <- 2500000
# nthin <- 2
# nburn <- 1500000
# nchains <- 3

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