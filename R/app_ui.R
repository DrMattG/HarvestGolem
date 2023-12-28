# The application User-Interface
#
# @import shiny 
# @noRd
app_ui <- function() {
  app_version<-"2024_01"
  title <- tags$a(
    href = "https://www.nina.no",
    tags$span(
      "Hunngaupejakt ",
      tags$small(paste("Version ", app_version))
    ),
    target = "_blank"
  )
  
  tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(
        title = title, titleWidth = 600
      ),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Hunngaupejakt", tabName = "dashboard", icon = icon("tachometer-alt", verify_fa = FALSE)),
          shinydashboard::menuItem("RovData",
                                   icon = icon("send", lib = "glyphicon"),
                                   href = "https://rovdata.no/Gaupe.aspx"
          )
        )
      ),
      shinydashboard::dashboardBody(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom1.css")
        ),
        tabsetPanel(
          id = "tabs",
          tabPanel(
            title = "Introduksjon",
            value = "page1",
            fluidRow(
              shinydashboard::box(tags$div(
                class = "header", checked = NA,
                tags$p("Dette er en Shiny App basert på prognosemodellen utviklet av Nilsen et al. 2011."),
                tags$a(href = "https://www.nina.no/archive/nina/PppBasePdf/rapport/2011/774.pdf", "Få tilgang til rapporten her"),
                tags$p("Prognosemodellen er en hierarkisk «state-space» modell kodet i R og JAGs. Modellen er basert på eksisterende tidsserier fra årlige tellinger av familiegrupper av gaupe (dvs. hunndyr i følge med unge(r)) og observasjoner fra gaupejakt. Modellen benytter seg av en Bayesiansk tilnærming med Markov-Chain Monte Carlo simuleringer. Modellen kan anvendes både på nasjonalt og regionalt nivå."),
                tags$p("Appen består av fire sider:"),
                tags$p("Dette er introduksjonssiden"),
                tags$p("Siden for historiske data visualiserer de historiske dataene vi har tilgjengelig"),
                tags$p("Prognosemodellen er tilgjengelig på modellsiden. Modellen kan kjøres for hele Norge, valgte Regioner eller en kombinasjon av Regioner. I de tilfellene hvor en kombinasjon av Regioner blir valgt, vil de Regionale bestandsmålene for hver av disse Regionene bli summert.For å kjøre en mer robust (langvarig) modell, endre innstillingene på siste side."),
                tags$p("Den siste siden inneholder noen tilleggsfunksjoner for mer avanserte brukere. Ved hjelp av tilleggsfunksjonene kan brukeren blant annet endre tidsserien som er brukt i modellen og øke eller redusere antall iterasjoner modellen skal kjøre.")
              )),
              height = 8
            )
          ), # end tabPanel

          tabPanel(
            title = "Historiske data",
            value = "page2",
            shinyWidgets::pickerInput("histReg", "Velg region(er)",
              choices = list(
                Regions = c(
                  "Region_1" = "1",
                  "Region_2" = "2",
                  "Region_3" = "3",
                  "Region_4" = "4",
                  "Region_5" = "5",
                  "Region_6" = "6",
                  "Region_7" = "7",
                  "Region_8" = "8"
                ),
                Whole = c()
              ),
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "Opphev alle",
                `select-all-text` = "Nasjonal",
                `none-selected-text` = "ingenting valgt"
              ),
              multiple = T,
              selected = c(
                "Region_1" = "1",
                "Region_2" = "2",
                "Region_3" = "3",
                "Region_4" = "4",
                "Region_5" = "5",
                "Region_6" = "6",
                "Region_7" = "7",
                "Region_8" = "8"
              )
            ),
            fluidRow(plotly::plotlyOutput("National", width = "80%", height = "600")), # end of fluidrow
            fluidRow(
              textOutput("Legend"),
              tags$head(tags$style("#Legend{color: black;
                                   font-size: 25px;
                                   font-style: italic;
                                   }"))
            )
          ), # end of tabPanel

          tabPanel(
            title = "Prognosemodell",
            value = "page3",
            fluidRow(
              column(
                12,
                h4("Hurtigstart"),
                shinydashboard::box(print("Vi har satt startverdier for forventet jaktuttak, så dersom du er fornøyd med disse kan du fortsette til modellseleksjonen. Du kan bruke ‘glidebryteren’ til å endre disse verdiene (vennligst sørg for at det laveste tallet er i den første glidebryteren og det høyeste i den siste glidebryteren). I ‘Velg en Modell’ dropdown menyen kan du velge fritt blant alle regionene (klikk på ‘Velg Alle’) eller velg en kombinasjon av forvaltningsregioner. Når du har gjort dine valg, trykk ‘Kjør Modell’."))
              ), # end box end column
              column(
                8,
                h3("Velge tre aktuelle hunndyr-kvoter"),
                sliderInput(
                  inputId = "min_h.levels", label = "Laveste kvotealternativ",
                  value = 15, min = 0, max = 100
                ),
                sliderInput(
                  inputId = "mid_h.levels", label = "Middels kvotealternativ",
                  value = 30, min = 0, max = 100
                ),
                sliderInput(
                  inputId = "max_h.levels", label = "Høyeste kvotealternativ",
                  value = 45, min = 0, max = 100
                ),
               shinyWidgets::pickerInput("model", "Velg region(er)",
                  choices = c(
                    "Region_1" = "1",
                    "Region_2" = "2",
                    "Region_3" = "3",
                    "Region_4" = "4",
                    "Region_5" = "5",
                    "Region_6" = "6",
                    "Region_7" = "7",
                    "Region_8" = "8"
                  ),
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Opphev alle",
                    `select-all-text` = "Nasjonal kvote",
                    `none-selected-text` = "ingenting valgt"
                  ),
                  multiple = T,
                  selected = c(
                    "Region_1" = "1",
                    "Region_2" = "2",
                    "Region_3" = "3",
                    "Region_4" = "4",
                    "Region_5" = "5",
                    "Region_6" = "6",
                    "Region_7" = "7",
                    "Region_8" = "8"
                  )
                )
              ), # end Column
              h4("Kjør modell"),
              actionButton("Run.model", "Kjør modell", icon("paper-plane"),
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              shinybusy::add_busy_spinner(spin = "double-bounce"),
              capture::capture(
                selector = "body",
                filename = "all-page.png",
                icon("camera"), "Take screenshot of all page"
              ),
              column(
                12,
                headerPanel("Oppsummeringstabell"),
                tabsetPanel(
                  tabPanel("Prognose", DT::dataTableOutput("table2")),
                  tabPanel("Prognose to år fram i tid", DT::dataTableOutput("table"))
                ), # end tabPanel
                headerPanel("Grafisk oppsummering"),
                tabsetPanel(
                  tabPanel("Familiegruppebestand", plotly::plotlyOutput("plot1")), # end tabPanel
                  tabPanel("Prognose to år fram i tid", plotly::plotlyOutput("plot3")) # end tabPanel
                ) # end tabPanel
              ), # end column
              uiOutput("mcmcPlots")
            ) ##################
          ) # end fluidRow end tabpanel
          , tabPanel(
            title = "Avanserte innstillinger",
            value = "page4",
            fluidRow(shinydashboard::box(paste0("Her er noen mer avanserte innganger til brukeren om nødvendig. Vi har satt startverdier slik at brukeren trygt kan ignorere denne siden. For å kjøre modellen raskt kan du velge 'rask' nedenfor."))), # end box end fluidRow
            sliderInput("startYear",
              label = "Startår:",
              min = 1996, max = as.numeric(substr(Sys.time(), 1, 4)) - 1, value = c(1996), sep = "",
              step = 1
            ),
            sliderInput("endYear",
              label = "Sluttår:",
              min = 1997, max = max(HarvestGolem::Lynx_monitoring_data$Aar) + 1,
              value = max(HarvestGolem::Lynx_monitoring_data$Aar) + 1,
              sep = "",
              step = 1
            ),
            radioButtons("speed", "Rask eller robust modell",
              c("Rask", "Robust"),
              selected = "Rask"
            )
          ) # end tabPanel
        ) # end tabSetPanel
      ) # end dashboardBody
    ) # end dashboardPage
  ) # end tag list
}
