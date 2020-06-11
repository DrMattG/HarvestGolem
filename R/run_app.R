#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(X,
  ...
) {
  if(X=="female")
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(...)
  )
  else if(X=="full")
    with_golem_options(
      app = shinyApp(
        ui = app_ui2, 
        server = app_server2
      ), 
      golem_opts = list(...)
    )
  else
    print("Please enter either 'female' for the female-only model or 'full' for the two sex model")
    
}
