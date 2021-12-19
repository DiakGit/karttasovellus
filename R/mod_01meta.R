#' meta UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import metathis
#' @importFrom magrittr %>%
#' @importFrom shiny NS tagList 
mod_01meta_ui <- function(id){
  ns <- NS(id)
  tagList(
   # EI KÄYTÖSSÄ
  )
}
    
#' meta Server Functions
#'
#' @noRd 
mod_01meta_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 # EI KÄYTÖSSÄ
  })
}
    
## To be copied in the UI
    
## To be copied in the server
# mod_meta_server("meta_ui_1")
