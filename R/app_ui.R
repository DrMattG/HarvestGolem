#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  devtools::source_url("https://raw.githubusercontent.com/DrMattG/ShinyNINA/master/Shinytheme_NINA.R")
  title <- tags$a(href='https://www.nina.no',
                  'Lynx harvest Female only', target="_blank")
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    # Define UI for application 
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
      dashboardBody(dashboardthemes::shinyDashboardThemes(theme_nina),
                    tags$head(
                      tags$link(rel="stylesheet", type= "text/css", href="custom1.css")),
                    fluidPage(
                      fluidRow(
                        column(8, 
                               h3("Input options"),
                               sliderInput("startYear", 
                                           label = "Start year:",
                                           min = 1996, max = as.numeric(substr(Sys.time(), 1, 4))-2, value=c(1996),sep=""
                                           ,step=1),
                               
                               sliderInput("endYear", 
                                           label = "End year:",
                                           min = 1997, max = as.numeric(substr(Sys.time(), 1, 4))-1, value=c(2019),sep=""
                                           ,step=1),
                               sliderInput(inputId="min_h.levels", label = "Minimum Harvest level for following year", 
                                           value=15, min=0, max=100),
                               sliderInput(inputId="mid_h.levels", label = "Median Harvest level for following year", 
                                           value=30, min=0, max=100),
                               sliderInput(inputId="max_h.levels", label = "Maximum Harvest level for following year",
                                           value=45, min=0, max=100)),
                        
                        h4("Run model"),
                        actionButton("Run.model","Run model", icon("paper-plane"), 
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                      
                      mainPanel(
                        
                        headerPanel("Tabular summary"),
                        tabsetPanel(
                          tabPanel("Family group population",  tableOutput("table2")), 
                          tabPanel("Total population",  tableOutput("table3"))
                          #, 
                          # tabPanel("JAGS output",  dataTableOutput("table4")) 
                          
                        ),
                        headerPanel("Graphical summary"),
                        tabsetPanel(     
                          tabPanel("Family group population",  plotOutput("plot1")), 
                          tabPanel("Total population",  plotOutput("plot2")),
                          tabPanel("Predictive",  plotOutput("plot3"))
                        )
                        
                      ),
                      
                      uiOutput("mcmcPlots")
                      
    )
  )
    )
  )
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

