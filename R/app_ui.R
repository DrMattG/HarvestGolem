#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    title <- tags$a(href='https://www.nina.no',
                  'Hunngaupejakt', target="_blank")
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
                                  tags$p("Dette er en Shiny App basert på prognosemodellen utviklet av Nilsen et al. 2011."),
                                  tags$a(href="https://www.nina.no/archive/nina/PppBasePdf/rapport/2011/774.pdf", "Få tilgang til rapporten her"),
                                  tags$p("Prognosemodellen er en hierarkisk «state-space» modell kodet i R og JAGs. Modellen er basert på eksisterende tidsserier fra årlige tellinger av familiegrupper av gaupe (dvs. hunndyr i følge med unge(r)) og observasjoner fra gaupejakt. Modellen benytter seg av en Bayesiansk tilnærming med Markov-Chain Monte Carlo simuleringer. Modellen kan anvendes både på nasjonalt og regionalt nivå."),
                                  
                                  tags$p("Appen består av fire sider:"),
                                  tags$p("Dette er introduksjonssiden"),
                                  tags$p("Siden for historiske data visualiserer de historiske dataene vi har tilgjengelig"),
                                  tags$p("Prognosemodellen er tilgjengelig på modellsiden. Modellen kan kjøres for hele Norge, valgte Regioner eller en kombinasjon av Regioner. I de tilfellene hvor en kombinasjon av Regioner blir valgt, vil de Regionale bestandsmålene for hver av disse Regionene bli summert."),
                                  tags$p("Den siste siden inneholder noen tilleggsfunksjoner for mer avanserte brukere. Ved hjelp av tilleggsfunksjonene kan brukeren blant annet endre tidsserien som er brukt i modellen og øke eller redusere antall iterasjoner modellen skal kjøre."))
            ), #end box 
            height=8),  #end fluidrow
         
          ), #end tabPanel
          
          tabPanel(
            title= "Historiske data",
            value= "page2",
            pickerInput("histReg","Velg region(er)", choices=c("Region_1"= "1",
                                                             "Region_2"= "2",
                                                             "Region_3"= "3",
                                                             "Region_4"= "4",
                                                             "Region_5"= "5",
                                                             "Region_6"= "6",
                                                             "Region_7"= "7",
                                                             "Region_8"= "8"), 
                        options = list(`actions-box` = TRUE,
                                       `deselect-all-text` = "Opphev alle",
                                       `select-all-text` = "Nasjonal",
                                       `none-selected-text` = "ingenting valgt"
                        ),
                        multiple = T,
                        selected = c("Region_1"= "1",
                                             "Region_2"= "2",
                                             "Region_3"= "3",
                                             "Region_4"= "4",
                                             "Region_5"= "5",
                                             "Region_6"= "6",
                                             "Region_7"= "7",
                                             "Region_8"= "8")),
            fluidRow(plotlyOutput("National", width="60%", height="600")),#end of fluidrow
            fluidRow(
            textOutput("Legend"),
            tags$head(tags$style("#Legend{color: black;
                                 font-size: 25px;
                                 font-style: italic;
                                 }"
            )
            ))
            
          ), #end of tabPanel
          
          tabPanel(
            title= "Prognosemodell",
            value="page3",
            fluidRow(
              column(12, 
                     h4("Hurtigstart"),
                     box(print("Vi har satt startverdier for forventet jaktuttak, så dersom du er fornøyd med disse kan du fortsette til modellseleksjonen. Du kan bruke ‘glidebryteren’ til å endre disse verdiene (vennligst sørg for at det laveste tallet er i den første glidebryteren og det høyeste i den siste glidebryteren). I ‘Velg en Modell’ dropdown menyen kan du velge fritt blant alle regionene (klikk på ‘Velg Alle’) eller velg en kombinasjon av forvaltningsregioner. Når du har gjort dine valg, trykk ‘Kjør Modell’."))), #end box end column
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
                                                `deselect-all-text` = "Opphev alle",
                                                `select-all-text` = "Nasjonal kvote",
                                                `none-selected-text` = "ingenting valgt"
                                                ),
                                 multiple = T,
                                 selected = c("Region_1"= "1",
                                              "Region_2"= "2",
                                              "Region_3"= "3",
                                              "Region_4"= "4",
                                              "Region_5"= "5",
                                              "Region_6"= "6",
                                              "Region_7"= "7",
                                              "Region_8"= "8"))
                     
                     ), #end Column
                            h4("Kjør modell"),
              actionButton("Run.model","Kjør modell", icon("paper-plane"), 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              downloadButton(
                outputId = "report",
                label = "Last ned PDF"
              ), #end download button
              
              # conditionalPanel(condition = "input.updateButton != 0",
              #                  valueBoxOutput("vbox")),
              # 
            
            column(12,
              
              headerPanel("Oppsummeringstabell"),
              tabsetPanel(
                tabPanel("Familiegruppebestand",  tableOutput("table2")), 
                tabPanel("Hele bestanden",  tableOutput("table3")),
                tabPanel("Prediktiv",  tableOutput("table"))
                              ), #end tabPanel
              headerPanel("Grafisk oppsummering"),
              tabsetPanel(     
                tabPanel("Familiegruppebestand",  plotOutput("plot1")), #end tabPanel
                tabPanel("Hele bestanden",  plotOutput("plot2")),#end tabPanel
                tabPanel("Prediktiv",  plotOutput("plot3"))#end tabPanel
              )#end tabPanel
             
            ), #end column
            
            uiOutput("mcmcPlots")
            ##################
          )), #end fluidRow end tabpanel
          tabPanel(
            title="Avanserte innstillinger",
            value="page4",
            fluidRow(box(paste0("•	Her er noen flere avanserte inputs til brukeren dersom det er nødvendig. Vi har satt startverdier slik at brukeren trygt kan ignorere denne siden. Vennligst vær klar over at dersom du endrer disse verdiene så kan du ende opp med små variasjoner i output på grunn av stokastisitet i modellen. Det er noen ulogiske valg som er mulig å gjøre med disse avanserte innstillingene (f.eks. så kan du sette «burn-in» til et større tall enn iterasjonene og dette vil gi en feilmelding i modellen og modellen vil derfor ikke kjøre)."))), #end box end fluidRow
            sliderInput("startYear", 
                        label = "Startår:",
                        min = 1996, max = as.numeric(substr(Sys.time(), 1, 4))-1, value=c(1996),sep=""
                        ,step=1),
            
            sliderInput("endYear", 
                        label = "Sluttår:",
                        min = 1997, max = as.numeric(substr(Sys.time(), 1, 4)), value=as.numeric(substr(Sys.time(), 1, 4)),sep=""
                        ,step=1),
            numericInput("n_its", 
                        label = "Antall iterasjoner:",
                        15000,min=100, max=30000000),
            numericInput("n_chains", 
                         label = "Antall rekker:",
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