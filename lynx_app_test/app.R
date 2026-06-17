library(shiny)

source("app_ui_test.R")
source("app_server_test.R")

ui <- app_ui()

server <- app_server

shinyApp(ui = ui, server = server)
