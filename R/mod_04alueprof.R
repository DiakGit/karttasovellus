#' 04alueprof UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_04alueprof_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(class = "container_1280",
             tags$div(class = "row grey-background",
                      tags$div(class = "col-lg-7",
                               tags$h2(id = "alueprofiili", "Alueprofiili"),
                               tags$p("Alueprofiilissa näet kaikki aineiston osoittimet luokan mukaan ryhmiteltynä. Kustakin osoittimesta näytetään valitun alueen lisäksi sen rajanaapurit sekä  osoittimen korkeimman ja matalimman arvon alueet. Alueet on järjestetty kunkin osoittimen kohdalla sijan mukaan."),
                               tags$p("Valitse ensin aluetaso, sitten alue ja paina lopuksi", tags$em("Luo alueprofiili"), "-painiketta. Alueprofiilin luominen kestää noin 30 sekuntia. Voit tallentaa profiilin laitteellesi")
                      ),
                      tags$div(class = "col-lg-5",
                               uiOutput(ns("output_regio_level_profile")),
                               uiOutput(ns("output_region_profile")),
                               uiOutput(ns("output_button_profile"))
                      )
                      ),
             tags$div(class = "row",
                      tags$div(class = "col-lg-12",
                               uiOutput(ns("region_profile_html"))
                      )
             )
             )
  )
}
    
#' 04alueprof Server Functions
#'
#' @import shinycssloaders
#' @import knitr
#'
#' @noRd 
mod_04alueprof_server <- function(id){
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
 
    output$output_regio_level_profile <- renderUI({
      
      opt_indicator <- sort(c("Hyvinvointialueet","Seutukunnat","Kunnat"))
      
      tagList(
        radioButtons(inputId = ns("value_regio_level_profile"), 
                     label = "Valitse aluetaso", inline = TRUE,
                     choices = opt_indicator, selected = "Hyvinvointialueet")
      )
    })
    
    output$output_region_profile <- renderUI({
      
      req(input$value_regio_level_profile)
      
      region_data <- get_region_data()
      tmpdat <- region_data[region_data$level %in% input$value_regio_level_profile,]
      choices <- tmpdat$region_name
      
      tagList(
        selectInput(inputId = ns("value_region_profile"), 
                    label = "Valitse alue",
                    choices = choices, 
                    selected = choices[1])
      )
    })
    
    
    output$output_button_profile <- renderUI({
      tagList(
        actionButton(ns("button"), 
                     label = "Luo alueprofiili", 
                     class="btn btn-outline-primary")
      )
    })
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #       _    _                             __ _ _ _ _ 
    #      / \  | |_   _  ___ _ __  _ __ ___  / _(_|_) (_)
    #     / _ \ | | | | |/ _ \ '_ \| '__/ _ \| |_| | | | |
    #    / ___ \| | |_| |  __/ |_) | | | (_) |  _| | | | |
    #   /_/   \_\_|\__,_|\___| .__/|_|  \___/|_| |_|_|_|_|
    #                        |_|                          
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    ## alueprofiilit ----

    # EVENTREACTIVE
    react_value_regio_level_profile <- eventReactive(input$button, {
      input$value_regio_level_profile
    })
    
    react_value_region_profile <- eventReactive(input$button, {
      input$value_region_profile
    })

    
    output$summamuuttuja_01 <- renderUI({
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()
      
      region_data <- get_region_data()
      region_data <- dplyr::filter(region_data, level %in% aluetaso1)
      naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
      
      tabdat <- create_alueprofiili_content(input_value_region_profile = aluename, 
                                            input_value_regio_level_profile = aluetaso1)
      
      # Summamuuttujat
      muuttujaluokka <- "Summamuuttujat"
      tabdat_tmp <- tabdat %>% 
        filter(var_class == muuttujaluokka)
      muuttujanimi <- unique(tabdat_tmp$muuttuja)
      
      for (ix in seq_along(muuttujanimi)){
        lista_tbl <- create_raw_tbl(dd1 = tabdat_tmp, muuttujanimi= muuttujanimi, varnro = ix)
        tbl_temp1 <- create_gt_tbl(lst_df = lista_tbl)
        assign(x = paste0("lista1_tbl", stringr::str_pad(ix, width = 2, pad = 0)), value = tbl_temp1)
      }
      
      output$aikasarja1_01 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[1], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[1]))
      output$aikasarja1_02 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[2], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[2]))
      output$aikasarja1_03 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[3], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[3]))
      output$aikasarja1_04 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[4], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[4]))
      
      output$kartta1_01 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[1], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[1]))
      output$kartta1_02 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[2], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[2]))
      output$kartta1_03 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[3], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[3]))
      output$kartta1_04 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[4], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[4]))
      
      tagList(
        fluidRow(tags$h5(muuttujanimi[1])),
        fluidRow(column(3,lista1_tbl01),column(5,withSpinner(plotOutput(ns("kartta1_01"),width = "100%"))),column(4,withSpinner(plotOutput(ns("aikasarja1_01"),width = "100%")))),
        fluidRow(tags$h5(muuttujanimi[2])),
        fluidRow(column(3,lista1_tbl02),column(5,plotOutput(ns("kartta1_02"),width = "100%")),column(4,plotOutput(ns("aikasarja1_02"),width = "100%"))),
        fluidRow(tags$h5(muuttujanimi[3])),
        fluidRow(column(3,lista1_tbl03),column(5,plotOutput(ns("kartta1_03"),width = "100%")),column(4,plotOutput(ns("aikasarja1_03"),width = "100%"))),
        fluidRow(tags$h5(muuttujanimi[4])),
        fluidRow(column(3,lista1_tbl04),column(5,plotOutput(ns("kartta1_04"),width = "100%")),column(4,plotOutput(ns("aikasarja1_04"),width = "100%")))
      )
    })
    
    output$inhimillinen_01 <- renderUI({
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()
      
      region_data <- get_region_data()
      region_data <- dplyr::filter(region_data, level %in% aluetaso1)
      naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
      
      tabdat <- create_alueprofiili_content(input_value_region_profile = aluename, 
                                            input_value_regio_level_profile = aluetaso1)
      
      muuttujaluokka <- "Inhimillinen huono-osaisuus"
      tabdat_tmp <- tabdat %>% 
        filter(var_class == muuttujaluokka)
      muuttujanimi <- unique(tabdat_tmp$muuttuja)
      
      
      for (ix in seq_along(muuttujanimi)){
        lista_tbl <- create_raw_tbl(dd1 = tabdat_tmp, muuttujanimi= muuttujanimi, varnro = ix)
        tbl_temp1 <- create_gt_tbl(lst_df = lista_tbl)
        assign(x = paste0("lista2_tbl", stringr::str_pad(ix, width = 2, pad = 0)), value = tbl_temp1)
      }
      
      output$aikasarja2_01 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[1], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[1]))
      output$aikasarja2_02 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[2], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[2]))
      output$aikasarja2_03 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[3], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[3]))
      output$aikasarja2_04 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[4], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[4]))
      output$aikasarja2_05 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[5], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[5]))
      output$aikasarja2_06 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[6], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[6]))
      if (aluetaso1 == "Hyvinvointialueet"){
        output$aikasarja2_07 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[7], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[7]))
        output$aikasarja2_08 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[8], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[8]))
        output$aikasarja2_09 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[9], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[9]))
        output$aikasarja2_10 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[10], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[10]))
        output$aikasarja2_11 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[11], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[11]))
      }
      
      output$kartta2_01 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[1], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[1]))
      output$kartta2_02 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[2], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[2]))
      output$kartta2_03 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[3], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[3]))
      output$kartta2_04 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[4], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[4]))
      output$kartta2_05 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[5], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[5]))
      output$kartta2_06 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[6], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[6]))
      if (aluetaso1 == "Hyvinvointialueet"){
        output$kartta2_07 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[7], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[7]))
        output$kartta2_08 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[8], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[8]))
        output$kartta2_09 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[9], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[9]))
        output$kartta2_10 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[10], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[10]))
        output$kartta2_11 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[11], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[11]))
      }
      
      if (aluetaso1 == "Hyvinvointialueet"){
        tagList(
          fluidRow(tags$h5(muuttujanimi[1])),
          fluidRow(column(3,lista2_tbl01),column(5,plotOutput(ns("kartta2_01"),width = "100%")),column(4,plotOutput(ns("aikasarja2_01"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[2])),
          fluidRow(column(3,lista2_tbl02),column(5,plotOutput(ns("kartta2_02"),width = "100%")),column(4,plotOutput(ns("aikasarja2_02"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[3])),
          fluidRow(column(3,lista2_tbl03),column(5,plotOutput(ns("kartta2_03"),width = "100%")),column(4,plotOutput(ns("aikasarja2_03"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[4])),
          fluidRow(column(3,lista2_tbl04),column(5,plotOutput(ns("kartta2_04"),width = "100%")),column(4,plotOutput(ns("aikasarja2_04"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[5])),
          fluidRow(column(3,lista2_tbl05),column(5,plotOutput(ns("kartta2_05"),width = "100%")),column(4,plotOutput(ns("aikasarja2_05"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[6])),
          fluidRow(column(3,lista2_tbl06),column(5,plotOutput(ns("kartta2_06"),width = "100%")),column(4,plotOutput(ns("aikasarja2_06"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[7])),
          fluidRow(column(3,lista2_tbl07),column(5,plotOutput(ns("kartta2_07"),width = "100%")),column(4,plotOutput(ns("aikasarja2_07"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[8])),
          fluidRow(column(3,lista2_tbl08),column(5,plotOutput(ns("kartta2_08"),width = "100%")),column(4,plotOutput(ns("aikasarja2_08"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[9])),
          fluidRow(column(3,lista2_tbl09),column(5,plotOutput(ns("kartta2_09"),width = "100%")),column(4,plotOutput(ns("aikasarja2_09"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[10])),
          fluidRow(column(3,lista2_tbl10),column(5,plotOutput(ns("kartta2_10"),width = "100%")),column(4,plotOutput(ns("aikasarja2_10"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[11])),
          fluidRow(column(3,lista2_tbl11),column(5,plotOutput(ns("kartta2_11"),width = "100%")),column(4,plotOutput(ns("aikasarja2_11"),width = "100%")))
        )
      } else {
        tagList(
          fluidRow(tags$h5(muuttujanimi[1])),
          fluidRow(column(3,lista2_tbl01),column(5,plotOutput(ns("kartta2_01"),width = "100%")),column(4,plotOutput(ns("aikasarja2_01"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[2])),
          fluidRow(column(3,lista2_tbl02),column(5,plotOutput(ns("kartta2_02"),width = "100%")),column(4,plotOutput(ns("aikasarja2_02"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[3])),
          fluidRow(column(3,lista2_tbl03),column(5,plotOutput(ns("kartta2_03"),width = "100%")),column(4,plotOutput(ns("aikasarja2_03"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[4])),
          fluidRow(column(3,lista2_tbl04),column(5,plotOutput(ns("kartta2_04"),width = "100%")),column(4,plotOutput(ns("aikasarja2_04"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[5])),
          fluidRow(column(3,lista2_tbl05),column(5,plotOutput(ns("kartta2_05"),width = "100%")),column(4,plotOutput(ns("aikasarja2_05"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[6])),
          fluidRow(column(3,lista2_tbl06),column(5,plotOutput(ns("kartta2_06"),width = "100%")),column(4,plotOutput(ns("aikasarja2_06"),width = "100%")))
        )
      }
    })
    
    output$sosiaalinen_01 <- renderUI({
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()
      
      region_data <- get_region_data()
      region_data <- dplyr::filter(region_data, level %in% aluetaso1)
      naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
      
      tabdat <- create_alueprofiili_content(input_value_region_profile = aluename, 
                                            input_value_regio_level_profile = aluetaso1)
      
      muuttujaluokka <- "Huono-osaisuuden sosiaaliset seuraukset"
      tabdat_tmp <- tabdat %>% 
        filter(var_class == muuttujaluokka)
      muuttujanimi <- unique(tabdat_tmp$muuttuja)
      
      for (ix in seq_along(muuttujanimi)){
        lista_tbl <- create_raw_tbl(dd1 = tabdat_tmp, muuttujanimi= muuttujanimi, varnro = ix)
        tbl_temp1 <- create_gt_tbl(lst_df = lista_tbl)
        assign(x = paste0("lista3_tbl", stringr::str_pad(ix, width = 2, pad = 0)), value = tbl_temp1)
      }
      
      output$aikasarja3_01 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[1], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[1]))
      output$aikasarja3_02 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[2], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[2]))
      output$aikasarja3_03 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[3], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[3]))
      output$aikasarja3_04 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[4], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[4]))
      if (aluetaso1 == "Hyvinvointialueet"){
        output$aikasarja3_05 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[5], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[5]))
        output$aikasarja3_06 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[6], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[6]))
      }
      
      output$kartta3_01 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[1], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[1]))
      output$kartta3_02 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[2], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[2]))
      output$kartta3_03 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[3], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[3]))
      output$kartta3_04 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[4], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[4]))
      if (aluetaso1 == "Hyvinvointialueet"){
        output$kartta3_05 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[5], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[5]))
        output$kartta3_06 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[6], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[6]))
      }
      
      if (aluetaso1 == "Hyvinvointialueet"){
        tagList(
          fluidRow(tags$h5(muuttujanimi[1])),
          fluidRow(column(3,lista3_tbl01),column(5,plotOutput(ns("kartta3_01"),width = "100%")),column(4,plotOutput(ns("aikasarja3_01"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[2])),
          fluidRow(column(3,lista3_tbl02),column(5,plotOutput(ns("kartta3_02"),width = "100%")),column(4,plotOutput(ns("aikasarja3_02"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[3])),
          fluidRow(column(3,lista3_tbl03),column(5,plotOutput(ns("kartta3_03"),width = "100%")),column(4,plotOutput(ns("aikasarja3_03"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[4])),
          fluidRow(column(3,lista3_tbl04),column(5,plotOutput(ns("kartta3_04"),width = "100%")),column(4,plotOutput(ns("aikasarja3_04"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[5])),
          fluidRow(column(3,lista3_tbl05),column(5,plotOutput(ns("kartta3_05"),width = "100%")),column(4,plotOutput(ns("aikasarja3_05"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[6])),
          fluidRow(column(3,lista3_tbl06),column(5,plotOutput(ns("kartta3_06"),width = "100%")),column(4,plotOutput(ns("aikasarja3_06"),width = "100%")))
        )
      } else {
        tagList(
          fluidRow(tags$h5(muuttujanimi[1])),
          fluidRow(column(3,lista3_tbl01),column(5,plotOutput(ns("kartta3_01"),width = "100%")),column(4,plotOutput(ns("aikasarja3_01"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[2])),
          fluidRow(column(3,lista3_tbl02),column(5,plotOutput(ns("kartta3_02"),width = "100%")),column(4,plotOutput(ns("aikasarja3_02"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[3])),
          fluidRow(column(3,lista3_tbl03),column(5,plotOutput(ns("kartta3_03"),width = "100%")),column(4,plotOutput(ns("aikasarja3_03"),width = "100%"))),
          fluidRow(tags$h5(muuttujanimi[4])),
          fluidRow(column(3,lista3_tbl04),column(5,plotOutput(ns("kartta3_04"),width = "100%")),column(4,plotOutput(ns("aikasarja3_04"),width = "100%")))
        )
      }
      
      
    })
    
    output$taloudellinen_01 <- renderUI({
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()
      
      region_data <- get_region_data()
      region_data <- dplyr::filter(region_data, level %in% aluetaso1)
      naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
      
      tabdat <- create_alueprofiili_content(input_value_region_profile = aluename, 
                                            input_value_regio_level_profile = aluetaso1)
      
      muuttujaluokka <- "Huono-osaisuuden taloudelliset yhteydet"
      tabdat_tmp <- tabdat %>% 
        filter(var_class == muuttujaluokka)
      muuttujanimi <- unique(tabdat_tmp$muuttuja)
      
      for (ix in seq_along(muuttujanimi)){
        lista_tbl <- create_raw_tbl(dd1 = tabdat_tmp, muuttujanimi= muuttujanimi, varnro = ix)
        tbl_temp1 <- create_gt_tbl(lst_df = lista_tbl)
        assign(x = paste0("lista4_tbl", stringr::str_pad(ix, width = 2, pad = 0)), value = tbl_temp1)
      }
      
      output$aikasarja4_01 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[1], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[1]))
      output$aikasarja4_02 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[2], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[2]))
      output$aikasarja4_03 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[3], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[3]))
      output$aikasarja4_04 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[4], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[4]))
      output$aikasarja4_05 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[5], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[5]))
      output$aikasarja4_06 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[6], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_aikasarja(varname = muuttujanimi[6]))
      
      output$kartta4_01 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[1], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[1]))
      output$kartta4_02 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[2], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[2]))
      output$kartta4_03 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[3], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[3]))
      output$kartta4_04 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[4], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[4]))
      output$kartta4_05 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[5], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[5]))
      output$kartta4_06 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[6], input_value_regio_level_profile = aluetaso1, input_value_region_profile = aluename, val_muuttujaluokka = muuttujaluokka)
      }, alt = create_alt_text_kartta(varname = muuttujanimi[6]))
      
      tagList(
        fluidRow(tags$h5(muuttujanimi[1])),
        fluidRow(column(3,lista4_tbl01),column(5,plotOutput(ns("kartta4_01"),width = "100%")),column(4,plotOutput(ns("aikasarja4_01"),width = "100%"))),
        fluidRow(tags$h5(muuttujanimi[2])),
        fluidRow(column(3,lista4_tbl02),column(5,plotOutput(ns("kartta4_02"),width = "100%")),column(4,plotOutput(ns("aikasarja4_02"),width = "100%"))),
        fluidRow(tags$h5(muuttujanimi[3])),
        fluidRow(column(3,lista4_tbl03),column(5,plotOutput(ns("kartta4_03"),width = "100%")),column(4,plotOutput(ns("aikasarja4_03"),width = "100%"))),
        fluidRow(tags$h5(muuttujanimi[4])),
        fluidRow(column(3,lista4_tbl04),column(5,plotOutput(ns("kartta4_04"),width = "100%")),column(4,plotOutput(ns("aikasarja4_04"),width = "100%"))),
        fluidRow(tags$h5(muuttujanimi[5])),
        fluidRow(column(3,lista4_tbl05),column(5,plotOutput(ns("kartta4_05"),width = "100%")),column(4,plotOutput(ns("aikasarja4_05"),width = "100%"))),
        fluidRow(tags$h5(muuttujanimi[6])),
        fluidRow(column(3,lista4_tbl06),column(5,plotOutput(ns("kartta4_06"),width = "100%")),column(4,plotOutput(ns("aikasarja4_06"),width = "100%")))
      )
    })
    
    
    output$zipcode_tables <- renderUI({

      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()

      reg_code <- karttasovellus::region_data %>%
        filter(level == aluetaso1,
               region_name == aluename) %>%
        pull(region_code)
      tags <- table_zipcodes(input_value_region_selected = reg_code,
                     input_value_regio_level = aluetaso1)
      tagList(
        tags
      )
    })
    
    output$zipcode_maps <- renderUI({
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()
      
      reg_code <- karttasovellus::region_data %>%
        filter(level == aluetaso1,
               region_name == aluename) %>%
        pull(region_code)
      
      output$map_zipcode_01 <- renderPlot({
        
        # 'Työttömät','Alimpaan tuloluokkaan kuuluvat täysi-ikäiset','Alimpaan tuloluokkaan kuuluvat taloudet','Kokonaislukema'
        
              map1 <- map_zipcodes(input_value_region_selected = reg_code,
                                       input_value_regio_level = aluetaso1,
                                       input_value_variable = "Kokonaislukema",
                                       leaflet = FALSE)
              map2 <- map_zipcodes(input_value_region_selected = reg_code,
                                   input_value_regio_level = aluetaso1,
                                   input_value_variable = "Työttömät",
                                   leaflet = FALSE)
              map3 <- map_zipcodes(input_value_region_selected = reg_code,
                                   input_value_regio_level = aluetaso1,
                                   input_value_variable = "Alimpaan tuloluokkaan kuuluvat täysi-ikäiset",
                                   leaflet = FALSE)
              map4 <- map_zipcodes(input_value_region_selected = reg_code,
                                   input_value_regio_level = aluetaso1,
                                   input_value_variable = "Alimpaan tuloluokkaan kuuluvat taloudet",
                                   leaflet = FALSE)
              patchwork::wrap_plots(list(map1,map2,map3,map4), ncol = 2)
              
      })
      
      tagList(
        withSpinner(plotOutput(ns("map_zipcode_01"),height = "1800px", width = "100%"))
      )
    })
    

    # output$zipcode_timeseries <- renderUI({
    #   
    #   aluename <- react_value_region_profile()
    #   aluetaso1 <- react_value_regio_level_profile()
    #   
    #   reg_code <- karttasovellus::region_data %>%
    #     filter(level == aluetaso1,
    #            region_name == aluename) %>%
    #     pull(region_code)
    #   
    #   output$lineplot_zipcode_01 <- renderPlot({
    #     
    #     # 'Työttömät','Alimpaan tuloluokkaan kuuluvat täysi-ikäiset','Alimpaan tuloluokkaan kuuluvat taloudet','Kokonaislukema'
    #     
    #     map1 <- plot_zipcodes_line(input_value_region_selected = reg_code,
    #                          input_value_regio_level = aluetaso1,
    #                          input_value_variable = "Kokonaislukema")
    #     map2 <- plot_zipcodes_line(input_value_region_selected = reg_code,
    #                          input_value_regio_level = aluetaso1,
    #                          input_value_variable = "Työttömät")
    #     map3 <- plot_zipcodes_line(input_value_region_selected = reg_code,
    #                          input_value_regio_level = aluetaso1,
    #                          input_value_variable = "Alimpaan tuloluokkaan kuuluvat täysi-ikäiset")
    #     map4 <- plot_zipcodes_line(input_value_region_selected = reg_code,
    #                          input_value_regio_level = aluetaso1,
    #                          input_value_variable = "Alimpaan tuloluokkaan kuuluvat taloudet")
    #     patchwork::wrap_plots(list(map1,map2,map3,map4), ncol = 2)
    #     
    #   })
    #   
    #   tagList(
    #     withSpinner(plotOutput(ns("lineplot_zipcode_01"),height = "1800px", width = "100%"))
    #   )
    # })
    
    
    
    ### profiili_html ----
    output$region_profile_html <- renderUI({
      
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()
      
      region_data <- get_region_data()
      region_data <- dplyr::filter(region_data, level %in% aluetaso1)
      naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
      tabdat <- create_alueprofiili_content(input_value_region_profile = aluename, 
                                            input_value_regio_level_profile = aluetaso1)
      
      tagList(
        fluidRow(column(width = 6,
                        tags$h3(glue("{aluename} ({aluetaso1})")),
                        tags$p("Analyysissä mukana naapurit: ", 
                               glue_collapse(unique(tabdat[tabdat$rooli == "naapuri",]$aluenimi), sep = ", ", last = " ja "))
        ),
        column(width = 6,
               withSpinner(uiOutput(ns("output_save_word")), proxy.height = "100px")
        )#,
        # column(width = 2,
        #        uiOutput(ns("output_save_data_profile"))
        # )
        ),
        tags$hr(),
        ## ##
        tags$h4("Summamuuttujat"),
        ## ## ##
        uiOutput(ns("summamuuttuja_01")),
        ## ## ##
        tags$h4("Inhimillinen huono-osaisuus"),
        ## ## ##
        uiOutput(ns("inhimillinen_01")),
        ## ## ##
        tags$h4("Huono-osaisuuden sosiaaliset seuraukset"),
        ## ## ##
        uiOutput(ns("sosiaalinen_01")),
        ## ## ##
        tags$h4("Huono-osaisuuden taloudelliset yhteydet"),
        ## ## ##
        uiOutput(ns("taloudellinen_01")),
        ## ## ##
        tags$h4("Postinumeroalueittainen tieto"),
        ## ## ##
        tags$h5("Taulukko"),
        uiOutput(ns("zipcode_tables")),
        # ## ## ##
        tags$h5("Kartat"),
        uiOutput(ns("zipcode_maps"))#,
        
        # # ## ## ##
        # # tags$h5("Aikasarjat"),
        # # uiOutput(ns("zipcode_timeseries"))
      )
      
    })
    
    output$report <- downloadHandler(
      
      filename = function() {
        file_name <- glue("alueprofiili_{react_value_region_profile()}_{tolower(react_value_regio_level_profile())}{input$value_report_format}")
        # file_name <-  glue("alueprofiili{input$value_report_format}")
        return(file_name)
      },
      content = function(file) {
        
        shiny::withProgress(
          message = paste0("Luodaan dokumenttia"),
          value = 0,
          {
            shiny::incProgress(1/10)
            Sys.sleep(1)
            
            aluename <- react_value_region_profile()
            shiny::incProgress(3/10)
            
            region_data <- get_region_data()
            region_data <- dplyr::filter(region_data, level %in% react_value_regio_level_profile())
            naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
            
            # klik <- list("id" = "Veteli")
            params <- list(region = aluename,
                           region_level = react_value_regio_level_profile(),
                           datetime = Sys.time(),
                           data = get_dat(),
                           data_aikasarja = get_dat_timeseries(),
                           spatdat = process_data_profile_doc(input_value_regio_level_profile = react_value_regio_level_profile()),
                           region_data = region_data,
                           naapurikoodit = naapurikoodit#,
                           # odt_kerroin = 1
            )
            
            tempReport <- file.path(tempdir(), "report.Rmd")
            # tempReport <- file.path("~/Downloads", "report.Rmd")
            # dea("./docs/report_template.Rmd", tempReport, overwrite = TRUE)
            # lns <- readLines("./docs/report_template.Rmd") 
            lns <- readLines(system.file("templates", "report_template.Rmd", package="karttasovellus"))
            
            if (input$value_report_format == ".docx"){
              # lns2 <- sub("korvaa_asiakirjamalli", "reference_docx: diak_karttasovellus.dotx", lns)
              # lns3 <- sub("korvaa_dokumenttimuoto", "word_document", lns2)
              lns3 <- lns
              file.copy(system.file("templates", "diak_karttasovellus.dotx", package="karttasovellus"),
                # "./docs/diak_karttasovellus.dotx",
                        tempdir(),
                        overwrite = TRUE)
              params[["fig_width_default"]] <- 12
              params[["fig_height_default"]] <- 10
              params[["doc_format"]] <- "docx"
              # params[["fig_dpi"]] <- 300
              # params[["out_width"]] <- "300px"
            } else {
              lns2 <- sub("reference_docx: diak_karttasovellus.dotx", "reference_odt: diak_karttasovellus.ott", lns)
              lns3 <- sub("word_document", "odt_document", lns2)
              file.copy(system.file("templates", "diak_karttasovellus.ott", package="karttasovellus"), #"./docs/diak_karttasovellus.ott",
                        tempdir(),
                        overwrite = TRUE)
              params[["fig_width_default"]] <- 12
              params[["fig_height_default"]] <- 10
              params[["doc_format"]] <- "odt"
              # params[["fig_dpi"]] <- 300
              # params[["out_width"]] <- "300px"
            }
            
            writeLines(lns3, tempReport)
            shiny::incProgress(5/10)
            rmarkdown::render(tempReport, 
                              output_file = file, 
                              params = params,
                              envir = new.env(parent = globalenv()
                              )
            )
            shiny::incProgress(7/10)
          })
      }
    )
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #    _____     _ _                            
    #   |_   _|_ _| | | ___ _ __  _ __  _   _ ___ 
    #     | |/ _` | | |/ _ \ '_ \| '_ \| | | / __|
    #     | | (_| | | |  __/ | | | | | | |_| \__ \
    #     |_|\__,_|_|_|\___|_| |_|_| |_|\__,_|___/
    #                                             
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    ## wordin tallennus ----
    
    output$output_save_word <- renderUI({
      
      # req(input$value_variable)
      tagList(
        downloadButton(ns("report"), "Tallenna alueprofiili laitteellesi!", class="btn btn-dark"),
        radioButtons(ns("value_report_format"),
                     "Valitse tallennettavan tiedoston tiedostomuoto",
                     choiceNames = list(#"vektorikuva (.pdf)",
                       "Word (.docx)",
                       "LibreOffice/OpenOffice/Google Docs (.odt)"),
                     choiceValues = list(#".pdf",
                       ".docx",
                       ".odt"),
                     inline = TRUE)
        
      )
    })
    
    ## datan tallennus ----
    ## 
    output$save_data_profile <- downloadHandler(
      
      filename = function() {
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        
        file_name <- glue("alueprofiili_data_{janitor::make_clean_names(aluename)}_{tolower(aluetaso1)}.csv")
        return(file_name)
      },
      content = function(file) {
        
        dat <- get_dat_timeseries()
        region_data <- get_region_data()
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        
        naapurikoodit_lst <- region_data[region_data$level %in% aluetaso1 & 
                                           region_data$region_name %in% aluename,"neigbours"]
        
        naapurikoodit <- naapurikoodit_lst %>% 
          unnest(cols = c(neigbours)) %>% 
          pull(neigbours)
        
        dat[dat$regio_level %in% aluetaso1 & dat$aluenimi %in% aluename ,] %>% 
          select(aika,aluenimi,var_class,variable,value) %>% 
          mutate(rooli = "valinta") -> tmpdat1
        dat[dat$regio_level %in% aluetaso1 & dat$aluekoodi %in% naapurikoodit ,] %>% 
          filter(!aluenimi %in% aluename) %>% 
          select(aika,aluenimi,var_class,variable,value) %>% 
          mutate(rooli = "naapuri") -> tmpdat2
        tmpdat <- bind_rows(tmpdat1,tmpdat2) 
        
        readr::write_excel_csv2(x = tmpdat, file = file)
      }
    )
    
    output$output_save_data_profile <- renderUI({
      
      # req(input$value_variable)
      tagList(
        downloadButton(ns("save_data_profile"), "Tallenna data csv-muodossa!", class="btn btn-dark")
      )
    })

  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
