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
  library(fuzzyjoin)
  library(readxl)
  library(tidyverse)
  library(plotly)
  library(shinydashboard)
  library(shinythemes)
  library(shiny)
  library(dashboardthemes)
  library(tidyverse)
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
                   href = "https://rovdata.no/Gaupe.aspx")
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
            fluidRow(box(tags$div(class="header", checked=NA,
                                  tags$p("This is a Shiny App based on the female-only prognosis model developed by  Nilsen et al. 2011."),
                                  tags$a(href="https://www.nina.no/archive/nina/PppBasePdf/rapport/2011/774.pdf", "Access the report here"),
                                  tags$p("The prognosis model is a hierarchial state-space model coded in R and JAGs. The model is based on the 
                                         existing time series of annual lynx family group counts (i.e. breeding female with kittens) and observed harvest of 
                                         lynx. The model uses a Bayesian approach with Markov-Chain Monte Carlo simulations. This model can be applied to 
                                         both the national and regional levels."),
                                  
                                  tags$p("The App consists of four pages:"),
                                  tags$p("This is the Introduction page"),
                                  tags$p("The historical data page plots the historical data"),
                                  tags$p("The model page hosts the female-only prognosis model. The model can be run for the whole of Norway or selected Regions or combinations of Regions. 
                                         In the case where a combination of Regions is selected then the Regional Targets for each Region in the selection are summed."),
                                  tags$p("The final page hosts some additional features for the more advanced user. These include changing the timeseries used in the model and increasing or decreasing the number of iterations the model runs for."))
            ), #end box 
            height=8),  #end fluidrow
         
          ), #end tabPanel
          
          tabPanel(
            title= "Historiske data",
            value= "page2",
            fluidRow(plotlyOutput("National", width="60%", height="600")),#end of fluidrow
            fluidRow(
            box(width=6,textOutput("Legend")))
            
          ), #end of tabPanel
          
          tabPanel(
            title= "Prognosemodell",
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
                     h3("Velge tre aktuelle hunndyr-kvoter"),
                     
                     sliderInput(inputId="min_h.levels", label = "Laveste kvotealternativ", 
                                 value=15, min=0, max=100),
                     sliderInput(inputId="mid_h.levels", label = "Middels kvotealternativ", 
                                 value=30, min=0, max=100),
                     sliderInput(inputId="max_h.levels", label = "Høyeste kvotealternativ",
                                 value=45, min=0, max=100),
                     pickerInput("model","Velg region(er)", choices=c("Region_1"= "1",
                                                                     "Region_2"= "2",
                                                                     "Region_3"= "3",
                                                                     "Region_4"= "4",
                                                                     "Region_5"= "5",
                                                                     "Region_6"= "6",
                                                                     "Region_7"= "7",
                                                                     "Region_8"= "8"), 
                                 options = list(`actions-box` = TRUE,
                                                `Select All` = "nasjonal kvote"
                                                ),
                                 multiple = T)
                     
                     ), #end Column
                            h4("Run model"),
              actionButton("Run.model","Run model", icon("paper-plane"), 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              downloadButton(
                outputId = "downloader",
                label = "Download PDF"
              ), #end download button
              
              conditionalPanel(condition = "input.updateButton != 0",
                               valueBoxOutput("vbox")),
            
            
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
            title="Avanserte innstillinger",
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
                        15000,min=100, max=30000000),
            numericInput("n_chains", 
                         label = "number of chains:",
                         3,min=2, max=5),
            numericInput("burn_in", 
                         label = "Burn in:",
                         7500,min=1, max=25000000),
            numericInput("n_thin", 
                         label = "Thinning:",
                         2,min=1, max=1000)
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
#niter <- 1500000
#nthin <- 2
#nburn <- 750000
#nchains <- 3

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