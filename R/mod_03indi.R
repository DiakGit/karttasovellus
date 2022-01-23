#' 03indi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @import shiny
#' @importFrom shiny NS tagList 

# indicator_df <- varlist_diak()
# opt_indicator <- unique(indicator_df$var_class)

mod_03indi_ui <- function(id){
  ns <- NS(id)
  tagList(
 tags$div(class = "container_1280",
         tags$div(class = "row",
                  tags$div(class = "col-lg-3 grey-background",
                                    tags$h2(id = "indikaattorivertailu", "Indikaattorivertailu"),
                             selectInput(
                               inputId = ns("value_variable_class"), 
                               label = "Valitse muuttujaluokka", 
                               choices = c('Summamuuttujat',
                                           'Inhimillinen huono-osaisuus',
                                           'Huono-osaisuuden taloudelliset yhteydet',
                                           'Huono-osaisuuden sosiaaliset seuraukset'), 
                               selected = "Summamuuttujat"
                             ),
                           selectInput(inputId = ns("value_variable"), 
                                       label = "Valitse muuttuja", 
                                       choices = "Huono-osaisuus yhteensä",
                                       selected = "Huono-osaisuus yhteensä", 
                                       multiple = FALSE),
                                    # uiOutput(ns("output_indicator")),
                           radioButtons(inputId = ns("value_regio_level"), 
                                        label = "Valitse aluetaso", 
                                        choices = "Hyvinvointialueet",
                                        selected = "Hyvinvointialueet"),
                                    # uiOutput(ns("output_regio_level")),
                           selectInput(inputId = ns("value_region_selected"), 
                                       label = "Valitse alue",
                                       choices = "Varsinais-Suomen HVA",
                                       selected = "Varsinais-Suomen HVA"
                           ),
                                    # uiOutput(ns("output_regio_select")),
                           radioButtons(inputId = ns("value_regio_show_mode"), 
                                        choices = "kaikki tason alueet", 
                                        selected = "kaikki tason alueet",
                                       # inline = FALSE,
                                        label = "Kuvioissa näytettävät alueet"
                                       ),
                                    # uiOutput(ns("output_regio_show_mode")),
                           actionButton(ns("button_ind"), "Päivitä kuvat"),
                                     radioButtons(ns("value_leaflet"), 
                                                  "Kartan tyyppi", 
                                                  choices = c("vuorovaikutteinen",
                                                              "staattinen"),
                                                  selected = "vuorovaikutteinen"
                                     )
                                    ),
                  tags$div(class = "col-lg-5",
                           uiOutput(ns("ui_map_plot"))
                           # shinycssloaders::withSpinner(leaflet::leafletOutput(ns("map_plot_leaflet"), width = "90%", height = "820px"))
                           # ui_map_plot

                           ),
                  tags$div(class = "col-lg-4",
                           uiOutput(ns("ui_plot_bar"))
                           # shinycssloaders::withSpinner(plotOutput(ns("rank_plot"), width = "100%", height = 800))
                  )
                  ),
tags$div(class = "row",
                tags$div(class = "col-lg-12",
                         shinycssloaders::withSpinner(
                           plotOutput(ns("timeseries_plot"), width = "100%", height = "900px")
                         )
)
),
tags$hr()
  ))
}
    
#' 03indi Server Functions
#' 
#' @import dplyr
#' @import shiny
#' @import tidyr
#' @import glue
#' @import ggplot2
#' @import hrbrthemes
#' @import sf
#' @import forcats
#' @import ggrepel
#' @import ineq
#' @import patchwork
#' 
#' @noRd 
mod_03indi_server <- function(id){
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

    observeEvent(input$value_variable_class, {
      # freezeReactiveValue(input, "value_variable")
      indicator_df <- varlist_diak()
      # järjestetään niin että ne indikaattorit joita useammalta tasolta tulee ekana
      opt_indicator <- indicator_df[indicator_df$var_class %in% input$value_variable_class,]$variable
      opt_indicator2 <- names(sort(table(opt_indicator), decreasing = TRUE))
      if (input$value_variable_class == "Summamuuttujat"){
        opt_indicator2 <- factor(opt_indicator2, levels = c("Huono-osaisuus yhteensä",
                                                            "Huono-osaisuuden sosiaaliset seuraukset", 
                                                            "Huono-osaisuuden taloudelliset yhteydet", 
                                                            "Inhimillinen huono-osaisuus"))
        opt_indicator2 <- sort(opt_indicator2)
      } else {
        opt_indicator2 <- factor(opt_indicator2)
      }
      updateSelectInput(inputId = "value_variable", choices = opt_indicator2, selected = opt_indicator2[1])
    })
    
    

    observeEvent(input$value_variable, {
      # freezeReactiveValue(input, "value_regio_level")
      varname <- input$value_variable[1]
      indicator_df <- varlist_diak()
      opt_indicator <- unique(indicator_df[indicator_df$var_class %in% input$value_variable_class & indicator_df$variable %in% varname,]$regio_level)
      opt_indicator <- indicator_df[indicator_df$variable %in% varname,]$regio_level
      opt_indicator <- factor(opt_indicator, levels = c("Hyvinvointialueet","Seutukunnat","Kunnat"))
      opt_indicator <- sort(opt_indicator)
      updateRadioButtons(inputId = "value_regio_level", choices = opt_indicator, selected = opt_indicator[1])
    })

    observeEvent(input$value_regio_level, {
      # freezeReactiveValue(input, "value_region_selected")
      region_data <- get_region_data()
      tmpdat <- region_data[region_data$level %in% input$value_regio_level,]
      opt_indicator <- tmpdat$region_name
      updateSelectInput(inputId = "value_region_selected", choices = opt_indicator, selected = opt_indicator[1]) 
    })

    observeEvent(c(input$value_variable,input$value_regio_level), {
      
      # freezeReactiveValue(input, "value_regio_show_mode")
      varlist <- varlist_diak()
      varlist_kunta <- varlist[varlist$regio_level == "Kunnat",]$variable
      
      if (input$value_regio_level != "Kunnat" & input$value_variable %in% varlist_kunta){
        opt_indicator <- c("kaikki tason alueet", 
                           "valittu alue ja sen naapurit", 
                           "valitun alueen kunnat")
      } else if (input$value_regio_level == "Kunnat") {
        opt_indicator <- c("kaikki tason alueet", 
                           "valittu alue ja sen naapurit")
      }
      updateRadioButtons(inputId = "value_regio_show_mode", choices = opt_indicator, selected = opt_indicator[1]) 
    })
    
    
    
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #    ___           _ _ _               _   _             _ _                _       _   
    #   |_ _|_ __   __| (_) | ____ _  __ _| |_| |_ ___  _ __(_) | ___   ___   _(_) ___ | |_ 
    #    | || '_ \ / _` | | |/ / _` |/ _` | __| __/ _ \| '__| | |/ / | | \ \ / / |/ _ \| __|
    #    | || | | | (_| | |   < (_| | (_| | |_| || (_) | |  | |   <| |_| |\ V /| | (_) | |_ 
    #   |___|_| |_|\__,_|_|_|\_\__,_|\__,_|\__|\__\___/|_|  |_|_|\_\\__,_| \_/ |_|\___/ \__|
    #                                                                                       
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

    ## indikaattorikuviot ----
    
    
    plotReactiveDat <- eventReactive({
      input$button_ind
    }, {
      dat <- process_data(input_value_regio_level = input$value_regio_level,
                          input_value_variable = input$value_variable)
      
      region_data <- get_region_data()
      naapurikoodit <- get_naapurikoodit(input_value_regio_level = input$value_regio_level,
                                         input_value_region_selected = input$value_region_selected)
      
      dat <- dat %>% 
        st_set_geometry(NULL) 
      
      if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
        dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
        # if (input$value_regio_level == "Kunnat"){
        #   dat$color <- ifelse(!dat$aluekoodi %in% naapurikoodit, FALSE, TRUE)
        # }
      } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
        
        dat <- create_municipalities_within_region(input_value_variable = input$value_variable, 
                                                   input_value_regio_level = input$value_regio_level,
                                                   input_value_region_selected = input$value_region_selected,
                                                   timeseries = FALSE)
      }
      return(dat)
    }, ignoreNULL = FALSE)
    
    
    output$ui_plot_bar <- renderUI({

      dat <- plotReactiveDat()
      rows <- nrow(dat)
      bar_height = 300 + rows * 17

      tagList(
        div(style='height:820px; overflow-y: auto; overflow-x: hidden;',
            shinycssloaders::withSpinner(plotOutput(ns("rank_plot"), width = "100%", height = bar_height))
        )
      )
    })
    
    plotReactiveRank <- eventReactive({
      input$button_ind
    }, {
      plot_rank_bar(input_value_regio_level = input$value_regio_level,
                      input_value_variable = input$value_variable,
                      input_value_region_selected = input$value_region_selected,
                      input_value_regio_show_mode = input$value_regio_show_mode)
    }, ignoreNULL = FALSE)


    output$rank_plot <- renderPlot({
      plotReactiveRank()
    })
        
    
    plotReactiveMapLeaflet <- eventReactive({
      input$button_ind
    }, {
      plot_map(input_value_regio_level = input$value_regio_level,
               input_value_variable = input$value_variable,
               input_value_region_selected = input$value_region_selected,
               input_value_regio_show_mode = input$value_regio_show_mode,
               leaflet = TRUE)
    }, ignoreNULL = FALSE)
    
    output$map_plot_leaflet <- leaflet::renderLeaflet({
      plotReactiveMapLeaflet()
    })
    
    plotReactiveMapStatic <- eventReactive({
      input$button_ind
    }, {
      plot_map(input_value_regio_level = input$value_regio_level,
               input_value_variable = input$value_variable,
               input_value_region_selected = input$value_region_selected,
               input_value_regio_show_mode = input$value_regio_show_mode,
               leaflet = FALSE)
    }, ignoreNULL = FALSE)
    

    output$map_plot_static <- renderPlot({
      plotReactiveMapStatic()
      })

      output$ui_map_plot <- renderUI({
        
        if (input$value_leaflet != "staattinen"){
          tag_list <- shinycssloaders::withSpinner(leaflet::leafletOutput(ns("map_plot_leaflet"), width = "90%", height = "820px"))
        } else {
          tag_list <- shinycssloaders::withSpinner(plotOutput(ns("map_plot_static"), height = "800px", width = "100%"))
        }
        tagList(
          tag_list
        )
      })
      
      funk <- eventReactive({
        input$button_ind
      }, {
        plot_timeseries(input_value_regio_level = input$value_regio_level,
                        input_value_variable = input$value_variable,
                        input_value_region_selected = input$value_region_selected,
                        input_value_regio_show_mode = input$value_regio_show_mode)
      }, ignoreNULL = FALSE)

      output$timeseries_plot <- renderPlot({
        funk()
      })
      
    # output$aputeksti <- renderText({
    #     
    #     txt <- react_value_region_profile()
    #     return(txt)
    # })
  })
}
    
