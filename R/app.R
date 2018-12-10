#' Launch the getSptialData web application
#'
#'
#' @return shiny application object
#'
#' @example \dontrun {webapp()}
#'
#' @import shiny
#' @export
#'


# wrapper for shiny::shinyApp()
gSD_app <- function() {
  shinyApp(ui = appUI, server = appServer)
}