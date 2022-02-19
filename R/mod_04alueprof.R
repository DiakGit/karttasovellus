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
                               tags$p("Alueprofiilissa näet kaikki aineiston osoittimet luokan mukaan ryhmiteltynä. Kustakin osoittimesta näytetään valitun alueen lisäksi sen rajanaapurit sekä osoittimen korkeimman ja matalimman arvon alueet. Alueet on järjestetty kunkin osoittimen kohdalla aseman mukaan."),
                               tags$p("Valitse ensin aluetaso, sitten alue ja paina lopuksi", tags$em("Luo alueprofiili"), "-painiketta. Alueprofiilin luominen kestää noin minuutin ajan. Voit tallentaa profiilin laitteellesi")
                      ),
                      tags$div(class = "col-lg-5",
                               uiOutput(ns("output_regio_level_profile")),
                               shinycssloaders::withSpinner(uiOutput(ns("output_region_profile"))),
                               tags$div(class = "row", 
                                        tags$div(class = "col-lg-6",
                                                 uiOutput(ns("output_button_profile"))),
                                        tags$div(class = "col-lg-6",
                                                 uiOutput(ns("output_save_word")))
                                        )
                      )
                      ),
             tags$div(class = "row",
                      tags$div(class = "col-lg-12",
                               uiOutput(ns("region_profile_html2"))
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
                     label = tags$strong("Valitse aluetaso"), 
                     inline = TRUE,
                     choices = opt_indicator, selected = "Hyvinvointialueet")
      )
    })
    
    output$output_region_profile <- renderUI({
      
      req(input$value_regio_level_profile)
      
      region_data <- get_region_data()
      tmpdat <- region_data[region_data$level %in% input$value_regio_level_profile,]
      choices <- stringr::str_sort(tmpdat$region_name, locale = "fi")
      
      tagList(
        selectInput(inputId = ns("value_region_profile"), 
                    label = tags$strong("Valitse alue"),
                    choices = choices, 
                    selected = choices[1])
      )
    })
    
    
    output$output_button_profile <- renderUI({
      tagList(
        actionButton(ns("button"), 
                     label = tags$strong("Luo alueprofiili sovellukseen"), 
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

    
    output$alueprofiili_html <- renderUI({
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()

      region_data <- get_region_data()
      region_data_base <- dplyr::filter(region_data, level %in% aluetaso1)
      naapurikoodit_base <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
      
      tabdat_base <- create_alueprofiili_content(input_value_region_profile = aluename, 
                                            input_value_regio_level_profile = aluetaso1)
      tabdat_base_ts <- create_alueprofiili_content(input_value_region_profile = aluename, 
                                            input_value_regio_level_profile = aluetaso1,
                                            aikasarja = TRUE)
      
      # Summamuuttujat
      muuttujaluokka <- c("Summamuuttujat",
                          "Inhimillinen huono-osaisuus",
                          "Huono-osaisuuden sosiaaliset seuraukset",
                          "Huono-osaisuuden taloudelliset yhteydet"
                          )
      
      # Eri aluetasoilla on eri muuttujat
      valid_variables <- tabdat_base %>% filter(aluenimi %in% aluename,
                        !is.na(arvo)) %>% pull(muuttuja)
      
      # ii <- 1
      lapply(seq_along(muuttujaluokka), function(ii) {
      tabdat_tmp <- tabdat_base %>% 
        filter(var_class == muuttujaluokka[ii], 
               muuttuja %in% valid_variables)
      muuttujanimi <- unique(tabdat_tmp$muuttuja)

      profile_plot_height <- paste0(400 + length(naapurikoodit_base)*20, "px")
      

      nro <- length(muuttujanimi)
      lapply(1:nro, function(i) {

        # jätä ne muuttujat näyttämättä, joilta tasolta ei yli kahta riviä (kuntatasolla puuttuu osa)
        if (nrow(tabdat_tmp[tabdat_tmp$muuttuja == muuttujanimi[i],]) > 2) {

        output[[paste0('taulu',ii,'_',i)]] <- function() {
          lista_tbl <- create_raw_tbl(dd1 = tabdat_tmp, 
                                      muuttujanimi= muuttujanimi, 
                                      varnro = i)
          kableExtra::kable_styling(knitr::kable(lista_tbl))
        }
        
        output[[paste0('kartta',ii,'_',i)]] <- renderPlot({
          alueprofiilikartta_html(val_muuttuja = muuttujanimi[i], 
                                     input_value_regio_level_profile = aluetaso1, 
                                     input_value_region_profile = aluename, 
                                     val_muuttujaluokka = muuttujaluokka[ii],
                                     tabdat = tabdat_base,
                                     region_data = region_data_base,
                                     naapurikoodit = naapurikoodit_base)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[i]))
        
        output[[paste0('aikasarja',ii,'_',i)]] <- renderPlot({
          alueprofiiliaikasarja_html(val_muuttuja1 = muuttujanimi[i], 
                                     input_value_regio_level_profile = aluetaso1, 
                                     input_value_region_profile = aluename, 
                                     val_muuttujaluokka = muuttujaluokka[ii],
                                     tabdat = tabdat_base_ts,
                                     region_data = region_data_base,
                                     naapurikoodit = naapurikoodit_base)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[i]))
        

        if (i == 1){
          h4_title <- muuttujaluokka[ii]
          h4_id <- janitor::make_clean_names(muuttujaluokka[ii])
          tag_lnk <- tags$a(class="nav-link", href="#alueprofiili", "Alueprofiilin alkuun")
        }  else {
          h4_title <- ""
          h4_id <- ""
          tag_lnk <- ""
        }
          tagList(
            fluidRow(tags$h4(h4_title, id = h4_id)),tag_lnk,
            fluidRow(tags$h5(muuttujanimi[i])),
            fluidRow(column(3,withSpinner(tableOutput(ns(paste0('taulu',ii,'_',i))))),
                     column(5,withSpinner(plotOutput(ns(paste0('kartta',ii,'_',i)), height = profile_plot_height, width = "100%"))),
                     column(4,withSpinner(plotOutput(ns(paste0('aikasarja',ii,'_',i)), height = profile_plot_height, width = "100%"))))
          )
        }
      })
      })
      })
    
    # Alueproofiilin postinumeroalueet

    output$zipcode_tables <- renderUI({

      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()

      load(system.file("data", "region_data.rda", package="karttasovellus"))
      reg_code <- region_data %>%
        filter(level == aluetaso1,
               region_name == aluename) %>%
        pull(region_code)
      tags <- table_zipcodes(input_value_region_selected = reg_code,
                     input_value_regio_level = aluetaso1, 
                     zipvars = c('Kokonaislukema',
                                 'Alimpaan tuloluokkaan kuuluvat taloudet',
                                 'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
                                 'Työttömät',
                                 'Peruskoulutuksen omaavat'))
      tagList(
        tags
      )
    })
    
    output$zipcode_dotplot <- renderUI({

        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        
        load(system.file("data", "region_data.rda", package="karttasovellus"))
        reg_code <- region_data %>%
          filter(level == aluetaso1,
                 region_name == aluename) %>%
          pull(region_code)
        
        # zipppiem määrä ->> kuvan korkeus
        naapurikoodit <- get_koodit_zip(regio_selected = reg_code,
                                        value_regio_level = aluetaso1)
        korkeus <- paste0(200 + length(naapurikoodit) * 14, "px")
        
        output$dot_plot_zip <- renderPlot({
          plot_zipcodes_dotplot_alueprofiili(input_value_region_selected = reg_code,
                                             input_value_regio_level = aluetaso1,
                                             zipvars = c('Kokonaislukema',
                                                         'Alimpaan tuloluokkaan kuuluvat taloudet',
                                                         'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
                                                         'Työttömät',
                                                         'Peruskoulutuksen omaavat'))
        })
        tagList(
          fluidRow(column(width = 12,
                          withSpinner(plotOutput(ns("dot_plot_zip"),height = korkeus, width = "100%")))
          )
        )
      })
    
    
    
    output$zipcode_maps <- renderUI({
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()
      
      load(system.file("data", "region_data.rda", package="karttasovellus"))
      reg_code <- region_data %>%
        filter(level == aluetaso1,
               region_name == aluename) %>%
        pull(region_code)
      
      output$map_zipcode_01 <- renderPlot({
        
        zipvars <- c('Kokonaislukema',
          'Alimpaan tuloluokkaan kuuluvat taloudet',
          'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
          'Työttömät',
          'Peruskoulutuksen omaavat'
          )
        maplista <- list()
        for (ii in seq_along(zipvars)){
          maplista[[ii]] <- map_zipcodes(input_value_region_selected = reg_code,
                                       input_value_regio_level = aluetaso1,
                                       input_value_variable = zipvars[ii],
                                       leaflet = FALSE, alueprofiili = TRUE)
      }
      patchwork::wrap_plots(maplista, ncol = 2)      
      })
      
      output$map_zipcode_leaflet <- renderLeaflet({
        map_zipcodes_alueprofiili(input_value_region_selected = reg_code, 
                                  input_value_regio_level = aluetaso1, 
                                  zipvars = c('Kokonaislukema',
                                              'Alimpaan tuloluokkaan kuuluvat taloudet',
                                              'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
                                              'Työttömät',
                                              'Peruskoulutuksen omaavat'))
      })
      
      tagList(
        fluidRow(column(width = 7,
                 withSpinner(plotOutput(ns("map_zipcode_01"),height = "1700px", width = "100%"))),
          column(width = 5,
                 withSpinner(leafletOutput(ns("map_zipcode_leaflet"),height = "1200px", width = "95%")))
      )
      )
    })
    
    ### profiili_html ----
    
    output$region_profile_html2 <- renderUI({
      
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()
      region_data <- get_region_data()
      reg_code <- region_data[region_data$level == aluetaso1 & region_data$region_name == aluename, ]$region_code
      
      url <- glue("https://diakgit.github.io/alueprofiilit/{tolower(aluetaso1)}-{janitor::make_clean_names(aluename)}-{reg_code}/index.html")
      
      tagit <- glue('
      <div class="embed-responsive embed-responsive-4by3" style="box-shadow: rgba(100, 100, 111, 0.2) 0px 7px 29px 0px;">
        <iframe class="embed-responsive-item" src={url} allowfullscreen style=""></iframe>
          </div>
      ')
      
      tagList(
        
        tags$html(HTML(tagit))
          
      )
    })
    
    
    output$region_profile_html <- renderUI({
      
      
      aluename <- react_value_region_profile()
      aluetaso1 <- react_value_regio_level_profile()
      
      region_data <- get_region_data()
      region_data <- dplyr::filter(region_data, level %in% aluetaso1)
      naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
      tabdat <- create_alueprofiili_content(input_value_region_profile = aluename, 
                                            input_value_regio_level_profile = aluetaso1)
      
      tagList(
        tags$div(style = "padding-top: 150px;"),
        tags$hr(),
        fluidRow(column(width = 6,
                        tags$h3(glue("{aluename} ({aluetaso1})")),
                        tags$p("Analyysissä mukana naapurit: ", 
                               glue_collapse(unique(tabdat[tabdat$rooli == "naapuri",]$aluenimi), sep = ", ", last = " ja "))
        ),
        column(width = 6#,
               # withSpinner(uiOutput(ns("output_save_word")), proxy.height = "100px")
        )
        ),
        tags$hr(),
        tags$strong("Alueprofiilin sisällysluettelo"),
        tags$li(class = "nav-item",
                tags$a(class="toc-alueprof", href="#summamuuttujat", "Summamuuttujat")
        ),
        tags$li(class = "nav-item",
                tags$a(class="toc-alueprof", href="#inhimillinen_huono_osaisuus", "Inhimillinen huono-osaisuus")
        ),
        tags$li(class = "nav-item",
                tags$a(class="toc-alueprof", href="#huono_osaisuuden_sosiaaliset_seuraukset", "Huono-osaisuuden sosiaaliset seuraukset")
        ),
        tags$li(class = "nav-item",
                tags$a(class="toc-alueprof", href="#huono_osaisuuden_taloudelliset_yhteydet", "Huono-osaisuuden taloudelliset yhteydet")
        ),
        # tags$li(class = "nav-item",
        #         tags$a(class="toc-alueprof", href="#postinumerotieto", "Postinumeroalueittainen tieto")
        # ),
        # tags$ul(
        #   tags$li(class = "nav-item",
        #           tags$a(class="toc-alueprof", href="#ziptbl", "Taulukko")
        #   ),
        #   tags$li(class = "nav-item",
        #           tags$a(class="toc-alueprof", href="#dotplot", "Pistekuvio")
        #   ),
        #   tags$li(class = "nav-item",
        #           tags$a(class="toc-alueprof", href="#zipmap", "Kartat")
        #   ),
        # ),
        ## ## ##
        uiOutput(ns("alueprofiili_html"))#,
        # # ## ## ##
        # tags$h4("Postinumeroalueittainen tieto", id = "postinumerotieto"),
        # tags$a(class="nav-link", href="#alueprofiili", "Alueprofiilin alkuun"),
        ## ## ##
        # tags$h5("Taulukko", id = "ziptbl"),
        # tags$a(class="nav-link", href="#alueprofiili", "Alueprofiilin alkuun"),
        # uiOutput(ns("zipcode_tables")),
        # ## ## ##
        # tags$h5("Pistekuvio", id = "dotplot"),
        # tags$a(class="nav-link", href="#alueprofiili", "Alueprofiilin alkuun"),
        # uiOutput(ns("zipcode_dotplot")),
        # # ## ## ##
        # tags$h5("Kartat", id = "zipmap"),
        # tags$a(class="nav-link", href="#alueprofiili", "Alueprofiilin alkuun"),
        # uiOutput(ns("zipcode_maps")),
        # tags$a(class="nav-link", href="#alueprofiili", "Alueprofiilin alkuun")
      )
      
    })
    
    # wordin_luominen ----
    output$report <- downloadHandler(
      
      filename = function() {
        file_name <- glue("alueprofiili_{react_value_region_profile()}_{tolower(react_value_regio_level_profile())}{input$value_report_format}")
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
            
            
            
            params <- list(region = aluename,
                           region_level = react_value_regio_level_profile(),
                           datetime = Sys.time())

            tempReport <- file.path(tempdir(), "report.Rmd")
            lns <- readLines(system.file("templates", "report_template.Rmd", package="karttasovellus"))
            
            if (input$value_report_format == ".docx"){
              lns3 <- lns
              file.copy(system.file("templates", "diak_karttasovellus.dotx", package="karttasovellus"),
                        tempdir(),
                        overwrite = TRUE)
              params[["fig_width_default"]] <- 12
              params[["fig_height_default"]] <- 10
              params[["doc_format"]] <- "docx"
            } else {
              lns2 <- sub("reference_docx: diak_karttasovellus.dotx", "reference_odt: diak_karttasovellus.ott", lns)
              lns3 <- sub("word_document", "odt_document", lns2)
              file.copy(system.file("templates", "diak_karttasovellus.ott", package="karttasovellus"), #"./docs/diak_karttasovellus.ott",
                        tempdir(),
                        overwrite = TRUE)
              params[["fig_width_default"]] <- 12
              params[["fig_height_default"]] <- 10
              params[["doc_format"]] <- "odt"
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
    ## 
    ## 

    output$report2 <- downloadHandler(
      
      filename = function() {
        file_name <- glue("alueprofiili_{tolower(janitor::make_clean_names(input$value_region_profile))}_{tolower(input$value_regio_level_profile)}{input$value_report_format}")
        return(file_name)
      },
      content = function(file) {
        download.file(glue("https://diakgit.github.io/alueprofiilit_docs/alueprofiili_{tolower(janitor::make_clean_names(input$value_region_profile))}_{tolower(input$value_regio_level_profile)}_{sub('\\\\.', '', input$value_report_format)}{input$value_report_format}"), 
                      destfile = file)
        
      }
    )
    
    output$output_save_word <- renderUI({
      
      tagList(
        downloadButton(ns("report2"), "Tallenna alueprofiili laitteellesi!", 
                       class="btn btn-dark"),
        radioButtons(ns("value_report_format"),
                     "Valitse tallennettavan tiedoston tiedostomuoto",
                     choiceNames = list(
                       "Word (.docx)",
                       "LibreOffice/OpenOffice/Google Docs (.odt)"),
                     choiceValues = list(
                       ".docx",
                       ".odt"),
                     inline = TRUE)
        
      )
    })
    
    # ## datan tallennus ----
    # ## 
    # output$save_data_profile <- downloadHandler(
    #   
    #   filename = function() {
    #     
    #     aluename <- react_value_region_profile()
    #     aluetaso1 <- react_value_regio_level_profile()
    #     
    #     file_name <- glue("alueprofiili_data_{janitor::make_clean_names(aluename)}_{tolower(aluetaso1)}.csv")
    #     return(file_name)
    #   },
    #   content = function(file) {
    #     
    #     dat <- get_dat_timeseries()
    #     region_data <- get_region_data()
    #     
    #     aluename <- react_value_region_profile()
    #     aluetaso1 <- react_value_regio_level_profile()
    #     
    #     naapurikoodit_lst <- region_data[region_data$level %in% aluetaso1 & 
    #                                        region_data$region_name %in% aluename,"neigbours"]
    #     
    #     naapurikoodit <- naapurikoodit_lst %>% 
    #       unnest(cols = c(neigbours)) %>% 
    #       pull(neigbours)
    #     
    #     dat[dat$regio_level %in% aluetaso1 & dat$aluenimi %in% aluename ,] %>% 
    #       select(aika,aluenimi,var_class,variable,value) %>% 
    #       mutate(rooli = "valinta") -> tmpdat1
    #     dat[dat$regio_level %in% aluetaso1 & dat$aluekoodi %in% naapurikoodit ,] %>% 
    #       filter(!aluenimi %in% aluename) %>% 
    #       select(aika,aluenimi,var_class,variable,value) %>% 
    #       mutate(rooli = "naapuri") -> tmpdat2
    #     tmpdat <- bind_rows(tmpdat1,tmpdat2) 
    #     
    #     readr::write_excel_csv2(x = tmpdat, file = file)
    #   }
    # )
    
    # output$output_save_data_profile <- renderUI({
    #   
    #   # req(input$value_variable)
    #   tagList(
    #     downloadButton(ns("save_data_profile"), "Tallenna data csv-muodossa!", class="btn btn-dark")
    #   )
    # })

  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
