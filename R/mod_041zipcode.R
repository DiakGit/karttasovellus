#' 041zipcode UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_041zipcode_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$div(class = "container_1280",
             tags$div(class = "row",
                      tags$div(class = "col-lg-3 grey-background",
                               tags$h2(id = "indikaattorivertailu", "Postinumeroalueet"),
                               uiOutput(ns("output_region_selected")),
                               uiOutput(ns("output_regio_show_mode")),
                               uiOutput(ns("output_variable"))#,
                               # uiOutput(ns("output_leaflet"))#,
                               # uiOutput(ns("output_save_data_indicator"))
                      ),
                      tags$div(class = "col-lg-5",
                               leaflet::leafletOutput(ns("map_zip_plot"), width = "100%", height = "800px")
                      ),
                      tags$div(class = "col-lg-4",
                               uiOutput(ns("ui_plot_zip_bar"))
                      )
             ),
             tags$div(class = "row",
                      tags$div(class = "col-lg-12",
                               plotOutput(ns("timeseries_zip_plot"), 
                                          width = "100%", height = "900px")
                      )
             )),
    tags$hr()
  )
}
    
#' 041zipcode Server Functions
#'
#' @noRd 
mod_041zipcode_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #    _                 _ _         _   
    #   | |__   __ _ _ __ (_) | ____ _| |_ 
    #   | '_ \ / _` | '_ \| | |/ / _` | __|
    #   | | | | (_| | | | | |   < (_| | |_ 
    #   |_| |_|\__,_|_| |_|_|_|\_\__,_|\__|
    #                                      
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    ## hanikat ----
    
    output$output_region_selected <- renderUI({
      
      # indicator_df <- varlist_diak()
      opt_indicator <- c("yksi","kaksi")
      reg <- sf::st_drop_geometry(karttasovellus::regio_Postinumeroalueet) 
      opt_indicator <- reg$region_name
      names(opt_indicator) <- paste0(reg$region_name," (", reg$region_code, ")")
      
      tagList(
        selectInput(
          inputId = ns("value_region_selected"), 
          label = "Valitse postinumeroalue", 
          choices = opt_indicator, 
          selected = opt_indicator[101])
      )
    })
    

    output$output_regio_show_mode <- renderUI({
      
      opt_indicator <- c("kaikki tason alueet", 
                         "valittu alue ja sen naapurit")

      tagList(
        radioButtons(inputId = ns("value_regio_show_mode"), 
                     choices = opt_indicator, 
                     selected = opt_indicator[2],
                     label = "Kuvioissa näytettävät alueet", 
                     inline = FALSE)
      )
      
    })
    
    
    output$output_variable <- renderUI({
      
      # indicator_df <- varlist_diak()
      opt_indicator <- c('Kokonaislukema',
                         'Alimpaan tuloluokkaan kuuluvat taloudet',
                         'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
                         'Työttömät (% 15-64 -vuotiaista)'
      )
      
      tagList(
        selectInput(
          inputId = ns("value_variable"), 
          label = "Valitse muuttuja", 
          choices = opt_indicator, 
          selected = opt_indicator[1])
      )
    })
    
    # output$output_leaflet <- renderUI({
    #   
    #   # indicator_df <- varlist_diak()
    #   opt_indicator <- c(TRUE,FALSE)
    #   names(opt_indicator) <- c("vuorovaikutteinen","staattinen")
    #   
    #   tagList(
    #     radioButtons(
    #       inputId = ns("value_leaflet"), 
    #       label = "Kartan tyyppi", 
    #       choices = opt_indicator, 
    #       selected = opt_indicator[1],
    #       inline = FALSE)
    #   )
    # })

    
    # map_zip_plot
    # ui_plot_zip_bar
    # timeseries_zip_plot
    
    output$map_zip_plot <- leaflet::renderLeaflet({
      map_zipcodes(input_value_region_selected = input$value_region_selected,
                   input_value_regio_show_mode = input$value_regio_show_mode,
                   input_value_variable = input$value_variable,
                   leaflet = TRUE
                   )
    })
    
    output$ui_plot_zip_bar <- renderUI({
      # plot(cars)
      tagList(
        tags$hr()
      )
    })
    
    output$timeseries_zip_plot <- renderPlot({
      plot_zipcodes_line(
        input_value_region_selected = input$value_region_selected,
        input_value_regio_show_mode = input$value_regio_show_mode,
        input_value_variable = input$value_variable)
    })
    
    
    
 
  })
}
    
## To be copied in the UI

## To be copied in the server

