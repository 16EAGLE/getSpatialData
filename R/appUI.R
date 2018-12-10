#' gSD webapp server
#'
#' @import shiny
#' @import shinyjs
#' @import shinydashboard
#' @import shinycssloaders

appUI <- dashboardPage(title = "getSpatialData | Query, preview, download and preprocess satellite imagery",
  dashboardHeader(title = "getSpatialData", disable = TRUE),
  dashboardSidebar(disable = TRUE  #,
    #sidebarMenu(
    #  menuItem("General settings", tabName = "general", icon = icon("sliders")),
    #  menuItem("Advanced settings", tabName = "advanced", icon = icon("magic"))
    #)
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(type="text/css", "body {padding-top: 50px;}")),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico"))
    
    #inser columns
    
  )
)