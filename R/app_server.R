#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # mod_meta_server("meta_ui_1")
  # mod_02navi_server("02navi_ui_1")
  mod_03indi_server("03indi_ui_1")
  mod_04alueprof_server("04alueprof_ui_1")
  mod_041zipcode_server("041zipcode_ui_1")
  mod_05etc_server("05etc_ui_1")
}
