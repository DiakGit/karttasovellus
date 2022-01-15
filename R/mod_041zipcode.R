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
                               uiOutput(ns("output_regio_level")),
                               uiOutput(ns("output_region_selected")),
                               uiOutput(ns("output_regio_show_mode")),
                               uiOutput(ns("output_variable"))#,
                               # actionButton(ns("do"), "Click Me")
                               # uiOutput(ns("output_leaflet"))#,
                      ),
                      tags$div(class = "col-lg-5",
                               leaflet::leafletOutput(ns("map_zip_plot"), width = "90%", height = "800px")
                               # plotOutput(ns("map_zip_plot"), width = "90%", height = "800px")
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
             )
             ),
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
    
    
    output$output_regio_level <- renderUI({
      
      # req(input$value_variable)
      #varname <- input$value_variable[1]
      #indicator_df <- varlist_diak()
      
      #opt_indicator <- unique(indicator_df[indicator_df$var_class %in% input$value_variable_class & indicator_df$variable %in% varname,]$regio_level)
      #opt_indicator <- indicator_df[indicator_df$variable %in% varname,]$regio_level
      opt_indicator <- c("Hyvinvointialueet","Seutukunnat","Kunnat")
      opt_indicator <- factor(opt_indicator, levels = c("Hyvinvointialueet","Seutukunnat","Kunnat"))
      opt_indicator <- sort(opt_indicator)
      
      # if (input$sidebar_menu == "info"){
      #     tagList()
      # } else {
      tagList(
        radioButtons(inputId = ns("value_regio_level"), 
                     label = "Valitse aluetaso", inline = FALSE,
                     choices = opt_indicator, selected = "Kunnat")
      )
      # }
      
    })
    
    
    output$output_region_selected <- renderUI({
      
      req(input$value_regio_level)
      # req(input$value_variable)

      reg <- sf::st_drop_geometry(karttasovellus::region_data)
      reg <- reg[reg$level %in% input$value_regio_level,]
      opt_indicator <- reg$region_code
      names(opt_indicator) <- reg$region_name
      Sys.sleep(1)
      
      tagList(
        selectInput(
          inputId = ns("value_region_selected"), 
          label = "Valitse alue", 
          choices = opt_indicator, 
          selected = opt_indicator[1])
      )
    })
    

    # output$output_regio_show_mode <- renderUI({
    #   
    #   opt_indicator <- c("valittu alue", 
    #                      "valittu alue ja sen naapurit")
    # 
    #   tagList(
    #     radioButtons(inputId = ns("value_regio_show_mode"), 
    #                  choices = opt_indicator, 
    #                  selected = opt_indicator[1],
    #                  label = "Kuvioissa näytettävät alueet", 
    #                  inline = FALSE)
    #   )
    #   
    # })
    
    
    output$output_variable <- renderUI({
      
      # req(input$value_regio_level)
      # req(input$value_region_selected)
      
      
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
    # output$map_zip_plot <- renderPlot({
      
      req(input$value_variable)
      req(input$value_regio_level)
      req(input$value_region_selected)
      
      map_zipcodes(input_value_region_selected = input$value_region_selected,
                   input_value_regio_level = input$value_regio_level,
                   input_value_variable = input$value_variable,
                   # input_value_region_selected = 91,
                   # input_value_regio_level = "Kunnat",
                   # input_value_variable = "Kokonaislukema",
                   leaflet = TRUE
                   )
    })
    
    
    output$bar_zip_plot <- renderPlot({
      
      req(input$value_variable)
      req(input$value_region_selected)
      req(input$value_regio_level)
      
      plot_zipcodes_bar(
        input_value_region_selected = input$value_region_selected,
        input_value_regio_level = input$value_regio_level,
        input_value_variable = input$value_variable
        # input_value_region_selected = 91,
        # input_value_regio_level = "Kunnat",
        # input_value_variable = "Kokonaislukema"
        )
    })

    output$ui_plot_zip_bar <- renderUI({
      
      req(input$value_variable)
      req(input$value_region_selected)
      req(input$value_regio_level)
      dat <- process_zipdata(varname = input$value_variable)
      # dat <- process_zipdata(varname = "Kokonaislukema")

      zipcodes <- get_koodit_zip(regio_selected = input$value_region_selected,
                                 value_regio_level = input$value_regio_level)
      # zipcodes <- get_koodit_zip(regio_selected = 924,
      #                            value_regio_level = "Kunnat")
      dat <- dat %>% filter(aluekoodi %in% zipcodes)
      
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
      req(input$value_regio_level)
      
      plot_zipcodes_line(
        input_value_region_selected = input$value_region_selected,
        input_value_regio_level = input$value_regio_level,
        input_value_variable = input$value_variable
        # input_value_region_selected = 924,
        # input_value_regio_level = "Kunnat",
        # input_value_variable = "Kokonaislukema"
        )
    })
    
    
    
 
  })
}
    
# plot_zipcodes_line(
#   input_value_region_selected = 91,
#   input_value_regio_show_mode = "valittu alue ja naapurit",
#   input_value_variable = "Kokonaislukema")
