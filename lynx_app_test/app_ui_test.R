# The application User-Interface
#
# @import shiny
# @noRd
app_ui <- function() {
  app_version <- "2026_01_test"

  region_choices <- c(
    "Region_1" = "1",
    "Region_2" = "2",
    "Region_3" = "3",
    "Region_4" = "4",
    "Region_5" = "5",
    "Region_6" = "6",
    "Region_7" = "7",
    "Region_8" = "8"
  )

  all_regions <- unname(region_choices)

  picker_options <- list(
    `actions-box` = TRUE,
    `deselect-all-text` = "Opphev alle",
    `select-all-text` = "Velg alle",
    `none-selected-text` = "Ingenting valgt"
  )

  title <- tags$a(
    href = "https://www.nina.no",
    target = "_blank",
    tags$span(
      "Hunngaupejakt ",
      tags$small(paste("Version", app_version))
    )
  )

  tagList(
    tags$head(
      tags$title("Hunngaupejakt App"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom1.css"),
      tags$style(HTML("\n        .content-wrapper, .right-side { background-color: #f7f7f7; }\n        .box { border-top: 3px solid #337ab7; }\n        #Legend { color: black; font-size: 18px; font-style: italic; }\n        .run-button { margin-top: 15px; margin-bottom: 20px; }\n        .help-text-small { color: #555; font-size: 14px; }\n      "))
    ),

    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(
        title = title,
        titleWidth = 600
      ),

      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            "Hunngaupejakt",
            tabName = "dashboard",
            icon = icon("tachometer-alt", verify_fa = FALSE)
          ),
          shinydashboard::menuItem(
            "RovData",
            icon = icon("send", lib = "glyphicon"),
            href = "https://rovdata.no/Gaupe.aspx"
          )
        )
      ),

      shinydashboard::dashboardBody(
        tabsetPanel(
          id = "tabs",

          tabPanel(
            title = "Introduksjon",
            value = "page1",
            fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  width = 12,
                  title = "Om appen",
                  status = "primary",
                  solidHeader = TRUE,
                  tags$div(
                    class = "header",
                    tags$p("Dette er en Shiny App basert på prognosemodellen utviklet av Nilsen et al. 2011."),
                    tags$p(tags$a(
                      href = "https://www.nina.no/archive/nina/PppBasePdf/rapport/2011/774.pdf",
                      target = "_blank",
                      "Få tilgang til rapporten her"
                    )),
                    tags$p("Prognosemodellen er en hierarkisk state-space modell kodet i R og JAGS. Modellen er basert på eksisterende tidsserier fra årlige tellinger av familiegrupper av gaupe og observasjoner fra gaupejakt. Modellen benytter en Bayesiansk tilnærming med Markov-Chain Monte Carlo simuleringer."),
                    tags$p("Appen består av fire sider: introduksjon, historiske data, prognosemodell og avanserte innstillinger."),
                    tags$p("På modellsiden kan modellen kjøres for hele Norge, valgte regioner eller en kombinasjon av regioner. For kombinasjoner av regioner summeres de regionale bestandsmålene."),
                    tags$p("Den siste siden inneholder mer avanserte innstillinger. Standardverdiene kan trygt brukes dersom du ikke ønsker å endre modelloppsettet.")
                  )
                )
              )
            )
          ),

          tabPanel(
            title = "Historiske data",
            value = "page2",
            fluidRow(
              column(
                width = 4,
                shinydashboard::box(
                  width = 12,
                  title = "Velg regioner",
                  status = "primary",
                  solidHeader = TRUE,
                  shinyWidgets::pickerInput(
                    inputId = "histReg",
                    label = "Velg region(er)",
                    choices = region_choices,
                    options = modifyList(picker_options, list(`select-all-text` = "Nasjonal")),
                    multiple = TRUE,
                    selected = all_regions
                  )
                )
              ),
              column(
                width = 8,
                shinydashboard::box(
                  width = 12,
                  title = "Historisk utvikling",
                  status = "primary",
                  solidHeader = TRUE,
                  plotly::plotlyOutput("National", height = "600px"),
                  br(),
                  textOutput("Legend")
                )
              )
            )
          ),

          tabPanel(
            title = "Prognosemodell",
            value = "page3",
            fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  width = 12,
                  title = "Hurtigstart",
                  status = "primary",
                  solidHeader = TRUE,
                  tags$p("Vi har satt startverdier for forventet jaktuttak. Dersom du er fornøyd med disse, kan du fortsette til modellseleksjonen."),
                  tags$p("Bruk glidebryterne til å endre kvotealternativene. Sørg for at laveste kvote er lavest, middels kvote er i midten, og høyeste kvote er høyest."),
                  tags$p("Velg én eller flere regioner og trykk deretter ‘Kjør modell’.")
                )
              )
            ),

            fluidRow(
              column(
                width = 4,
                shinydashboard::box(
                  width = 12,
                  title = "Modellvalg",
                  status = "primary",
                  solidHeader = TRUE,
                  h4("Velg tre aktuelle hunndyr-kvoter"),
                  sliderInput("min_h.levels", "Laveste kvotealternativ", value = 15, min = 0, max = 100, step = 1),
                  sliderInput("mid_h.levels", "Middels kvotealternativ", value = 30, min = 0, max = 100, step = 1),
                  sliderInput("max_h.levels", "Høyeste kvotealternativ", value = 45, min = 0, max = 100, step = 1),
                  shinyWidgets::pickerInput(
                    inputId = "model",
                    label = "Velg region(er)",
                    choices = region_choices,
                    options = modifyList(picker_options, list(`select-all-text` = "Nasjonal kvote")),
                    multiple = TRUE,
                    selected = all_regions
                  ),
                  actionButton(
                    "Run.model",
                    "Kjør modell",
                    icon = icon("paper-plane"),
                    class = "btn-primary run-button"
                  ),
                  tags$p(class = "help-text-small", "Tips: bruk ‘Rask’ for testing og ‘Robust’ for endelige kjøringer.")
                )
              ),
              column(
                width = 8,
                shinydashboard::box(
                  width = 12,
                  title = "Oppsummeringstabeller",
                  status = "primary",
                  solidHeader = TRUE,
                  tabsetPanel(
                    tabPanel("Prognose", DT::dataTableOutput("table2")),
                    tabPanel("Prognose to år fram i tid", DT::dataTableOutput("table"))
                  )
                ),
                shinydashboard::box(
                  width = 12,
                  title = "Grafisk oppsummering",
                  status = "primary",
                  solidHeader = TRUE,
                  tabsetPanel(
                    tabPanel("Familiegruppebestand", plotly::plotlyOutput("plot1", height = "550px")),
                    tabPanel("Prognose to år fram i tid", plotly::plotlyOutput("plot3", height = "550px"))
                  )
                )
              )
            ),

            shinybusy::add_busy_spinner(spin = "double-bounce"),
            uiOutput("mcmcPlots")
          ),

          tabPanel(
            title = "Avanserte innstillinger",
            value = "page4",
            fluidRow(
              column(
                width = 12,
                shinydashboard::box(
                  width = 12,
                  title = "Avanserte innstillinger",
                  status = "primary",
                  solidHeader = TRUE,
                  tags$p("Her er noen mer avanserte innganger til brukeren om nødvendig. Standardverdiene kan trygt ignoreres. For testing kan du velge ‘Rask’; for endelige analyser bør du bruke ‘Robust’."),
                  sliderInput(
                    "startYear",
                    label = "Startår:",
                    min = 1996,
                    max = as.numeric(format(Sys.Date(), "%Y")) - 1,
                    value = 1996,
                    sep = "",
                    step = 1
                  ),
                  sliderInput(
                    "endYear",
                    label = "Sluttår:",
                    min = 1997,
                    max = max(HarvestGolem::Lynx_monitoring_data$Aar, na.rm = TRUE) + 1,
                    value = max(HarvestGolem::Lynx_monitoring_data$Aar, na.rm = TRUE) + 1,
                    sep = "",
                    step = 1
                  ),
                  radioButtons(
                    "speed",
                    "Rask eller robust modell",
                    choices = c("Rask", "Robust"),
                    selected = "Rask",
                    inline = TRUE
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
