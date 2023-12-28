#' Run the Shiny Application
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  shinyApp(
      ui = app_ui, 
      server = app_server
    )
  }
