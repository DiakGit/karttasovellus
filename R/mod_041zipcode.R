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
                               tags$h2(id = "zipcode", "Postinumeroalueet"),
                               tags$p("Postinumeroalueittainen data näytetään aina kunnittain. Halutessasi voi valita myös kaikkien naapurikuntien datan mukaan."),
                               uiOutput(ns("output_region_selected")),
                               uiOutput(ns("output_regio_show_mode")),
                               uiOutput(ns("output_variable"))#,
                               # uiOutput(ns("output_leaflet"))#,
                      ),
                      tags$div(class = "col-lg-5",
                               leaflet::leafletOutput(ns("map_zip_plot"), width = "90%", height = "800px")
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
      
      reg <- sf::st_drop_geometry(karttasovellus::region_data)
      reg <- reg[reg$level == "Kunnat",]
      opt_indicator <- reg$region_code
      names(opt_indicator) <- reg$region_name
      
      tagList(
        selectInput(
          inputId = ns("value_region_selected"), 
          label = "Valitse kunta", 
          choices = opt_indicator, 
          selected = opt_indicator[1])
      )
    })
    

    output$output_regio_show_mode <- renderUI({
      
      opt_indicator <- c("valittu alue", 
                         "valittu alue ja sen naapurit")

      tagList(
        radioButtons(inputId = ns("value_regio_show_mode"), 
                     choices = opt_indicator, 
                     selected = opt_indicator[1],
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

    output$map_zip_plot <- leaflet::renderLeaflet({
      
      req(input$value_variable)
      req(input$value_region_selected)
      req(input$value_regio_show_mode)
      
      map_zipcodes(input_value_region_selected = input$value_region_selected,
                   input_value_regio_show_mode = input$value_regio_show_mode,
                   input_value_variable = input$value_variable,
                   leaflet = TRUE
                   )
    })
    
    
    output$bar_zip_plot <- renderPlot({
      plot_zipcodes_bar(
        input_value_region_selected = input$value_region_selected,
        input_value_regio_show_mode = input$value_regio_show_mode,
        input_value_variable = input$value_variable)
    })
    
    # output$ui_plot_zip_bar <- renderUI({
    #   # plot(cars)
    #   tagList(
    #     plotOutput(ns("bar_zip_plot"))
    #   )
    # })
    
    output$ui_plot_zip_bar <- renderUI({
      
      req(input$value_variable)
      req(input$value_region_selected)
      req(input$value_regio_show_mode)
      
      
      dat <- process_zipdata(varname = input$value_variable)

      if (input$value_regio_show_mode == "kaikki tason alueet"){
        dat <- dat
      } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
        naapurikoodit <- get_naapurikoodit_zip(regio_selected = input$value_region_selected)
        dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
      }  else if (input$value_regio_show_mode == "valittu alue"){
        naapurikoodit <- karttasovellus::region_data_zip[karttasovellus::region_data_zip$kuntanro == input$value_region_selected,]$region_code
        dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
      }
      
      Sys.sleep(1)
      rows <- nrow(dat)
      # rows <- 10
      bar_height = 300 + rows * 17

      tagList(
        div(style='height:820px; overflow-y: auto; overflow-x: hidden;',
            plotOutput(ns("bar_zip_plot"), width = "100%", height = bar_height)
        )
        
      )
      
    })
    
    
    output$timeseries_zip_plot <- renderPlot({
      
      req(input$value_variable)
      req(input$value_region_selected)
      req(input$value_regio_show_mode)
      
      plot_zipcodes_line(
        input_value_region_selected = input$value_region_selected,
        input_value_regio_show_mode = input$value_regio_show_mode,
        input_value_variable = input$value_variable)
    })
    
    
    
 
  })
}
    
# plot_zipcodes_line(
#   input_value_region_selected = 91,
#   input_value_regio_show_mode = "valittu alue ja naapurit",
#   input_value_variable = "Kokonaislukema")
