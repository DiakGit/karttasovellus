#' 041zipcode UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 


init_val <- 1
names(init_val) <- "Itä-Uudenmaan HVA"

mod_041zipcode_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$div(class = "container_1280",
             tags$div(class = "row",
                      tags$div(class = "col-lg-3 grey-background",
                               tags$h2(id = "zipcode", "Postinumeroalueet"),
                               tags$p("Postinumeroalueittainen data näytetään aina kunnittain. Halutessasi voi valita myös kaikkien naapurikuntien datan mukaan."),
                               radioButtons(inputId = ns("value_regio_level"), 
                                            label = "Valitse aluetaso", 
                                            inline = FALSE,
                                            choices = c("Hyvinvointialueet","Seutukunnat","Kunnat"), 
                                            selected = "Hyvinvointialueet"),
                               
                               # uiOutput(ns("output_regio_level")),
                               
                               selectInput(
                                 inputId = ns("value_region_selected"),
                                 label = "Valitse alue",
                                 choices = init_val,
                                 selected = init_val), 
                               # uiOutput(ns("output_region_selected")),
                               selectInput(
                                 inputId = ns("value_variable"), 
                                 label = "Valitse muuttuja", 
                                 choices = c('Kokonaislukema',
                                             'Alimpaan tuloluokkaan kuuluvat taloudet',
                                             'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
                                             'Työttömät'
                                 ), 
                                 selected = "Kokonaislukema"),
                               # uiOutput(ns("output_variable")),
                               actionButton(ns("button_zip"), "Päivitä kuvat", class="btn btn-primary", icon("fas fa-sync")),
                               radioButtons(ns("value_leaflet"), 
                                            tags$strong("Kartan tyyppi"), 
                                            choices = c("vuorovaikutteinen",
                                                        "staattinen")
                               ),
                               # uiOutput(ns("output_leaflet"))#,
                      ),
                      tags$div(class = "col-lg-4",
                               uiOutput(ns("ui_map_zip_plot"))
                      ),
                      tags$div(class = "col-lg-5",
                               uiOutput(ns("ui_plot_zip_bar"))
                      )
             ),
             tags$div(class = "row",
                      tags$div(class = "col-lg-12",
                               plotOutput(ns("timeseries_zip_plot"),
                                          width = "100%", height = "700px")
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

    observeEvent(input$value_regio_level, {
      # freezeReactiveValue(input, "value_regio_level")
      reg <- sf::st_drop_geometry(karttasovellus::region_data)
      reg <- reg[reg$level %in% input$value_regio_level,]
      # reg <- reg[reg$level %in% "Hyvinvointialueet",]
      opt_indicator <- reg$region_code
      names(opt_indicator) <- reg$region_name
      updateSelectInput(inputId = "value_region_selected", 
                         choices = opt_indicator,
                         selected = opt_indicator[1])
    })

    # observeEvent(input$value_region_selected, {
    #   # freezeReactiveValue(input, "value_regio_level")
    #   opt_indicator <- c('Kokonaislukema',
    #                      'Alimpaan tuloluokkaan kuuluvat taloudet',
    #                      'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
    #                      'Työttömät'
    #   )
    #   updateSelectInput(inputId = "value_variable", 
    #                     choices = opt_indicator,
    #                     selected = opt_indicator[1])
    # })

    # =============================================================================================
    # =============================================================================================
    # KUVAT
    
    plotReactiveMapLeaflet <- eventReactive({
      input$button_zip
    }, {
      map_zipcodes(input_value_region_selected = input$value_region_selected,
                   input_value_regio_level = input$value_regio_level,
                   input_value_variable = input$value_variable,
                   # input_value_region_selected = 91,
                   # input_value_regio_level = "Kunnat",
                   # input_value_variable = "Kokonaislukema",
                   leaflet = TRUE)
    }, ignoreNULL = FALSE)
    
    output$map_plot_leaflet <- leaflet::renderLeaflet({
      plotReactiveMapLeaflet()
    })
    
    plotReactiveMapStatic <- eventReactive({
      input$button_zip
    }, {
      map_zipcodes(input_value_region_selected = input$value_region_selected,
                   input_value_regio_level = input$value_regio_level,
                   input_value_variable = input$value_variable,
                   # input_value_region_selected = 91,
                   # input_value_regio_level = "Kunnat",
                   # input_value_variable = "Kokonaislukema",
                   leaflet = FALSE)
    }, ignoreNULL = FALSE)
    
    
    output$map_plot_static <- renderPlot({
      plotReactiveMapStatic()
    })
    
    output$ui_map_zip_plot <- renderUI({
      
      if (input$value_leaflet != "staattinen"){
        tag_list <- shinycssloaders::withSpinner(leaflet::leafletOutput(ns("map_plot_leaflet"), width = "90%", height = "820px"))
      } else {
        tag_list <- shinycssloaders::withSpinner(plotOutput(ns("map_plot_static"), height = "800px", width = "100%"))
      }
      tagList(
        tag_list
      )
    })
    

    funkBar <- eventReactive({
      input$button_zip
    }, {
      plot_zipcodes_bar(
        input_value_region_selected = input$value_region_selected,
        input_value_regio_level = input$value_regio_level,
        input_value_variable = input$value_variable
        # input_value_region_selected = 924,
        # input_value_regio_level = "Kunnat",
        # input_value_variable = "Kokonaislukema"
      )
    }, ignoreNULL = FALSE)
    
    output$bar_zip_plot <- renderPlot({
      funkBar()
    })
    

    plotZipReactive <- eventReactive({
      input$button_zip
    }, {
      dat <- process_zipdata(varname = input$value_variable)
      
      zipcodes <- get_koodit_zip(regio_selected = input$value_region_selected,
                                 value_regio_level = input$value_regio_level)
      dat <- dat %>% filter(aluekoodi %in% zipcodes)
      rows <- nrow(dat)
      bar_height = 300 + rows * 17
      
      tagList(
        div(style='height:820px; overflow-y: auto; overflow-x: hidden;',
            shinycssloaders::withSpinner(plotOutput(ns("bar_zip_plot"), width = "100%", height = bar_height))
        )
      )

    }, ignoreNULL = FALSE)
    
    
    
    output$ui_plot_zip_bar <- renderUI({
      plotZipReactive()
    })
    
    funkTimeSeries <- eventReactive({
      input$button_zip
    }, {
      plot_zipcodes_line(
        input_value_region_selected = input$value_region_selected,
        input_value_regio_level = input$value_regio_level,
        input_value_variable = input$value_variable
        # input_value_region_selected = 924,
        # input_value_regio_level = "Kunnat",
        # input_value_variable = "Kokonaislukema"
      )
    }, ignoreNULL = FALSE)
    
    output$timeseries_zip_plot <- renderPlot({
      funkTimeSeries()
    })

  })
}
