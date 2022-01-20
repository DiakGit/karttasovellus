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
mod_03indi_ui <- function(id){
  ns <- NS(id)
  tagList(
 tags$div(class = "container_1280",
         tags$div(class = "row",
                  tags$div(class = "col-lg-3 grey-background",
                                    tags$h2(id = "indikaattorivertailu", "Indikaattorivertailu"),
                                    uiOutput(ns("output_indicator_class")),
                                    uiOutput(ns("output_indicator")),
                                    uiOutput(ns("output_regio_level")),
                                    uiOutput(ns("output_regio_select")),
                                    uiOutput(ns("output_regio_show_mode")),
                                     radioButtons(ns("value_leaflet"), 
                                                  "Kartan tyyppi", 
                                                  choices = c("vuorovaikutteinen",
                                                              "staattinen")
                                     ),
                                     actionButton(ns("button_ind"), "Piirrä kuvat")
                                    ),
                  tags$div(class = "col-lg-5",
                           plotOutput(ns("map_plot"), width = "100%", height = "800px")
                           ),
                  tags$div(class = "col-lg-4",
                           uiOutput(ns("ui_plot_bar"))
                  )
                  ),
tags$div(class = "row",
         tags$div(class = "col-lg-12",
                  plotOutput(ns("timeseries_plot"), width = "100%", height = "900px")
         )
)),
tags$hr()
  )
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
    
    output$output_indicator_class <- renderUI({
        
        indicator_df <- varlist_diak()
        opt_indicator <- unique(indicator_df$var_class)
        
        tagList(
            selectInput(
                inputId = ns("value_variable_class"), 
                label = "Valitse muuttujaluokka", 
                choices = opt_indicator, 
                selected = opt_indicator[1])
        )
    })
    
    
    output$output_indicator <- renderUI({

        indicator_df <- varlist_diak()
        # järjestetään niin että ne indikaattorit joita useammalta tasolta tulee ekana
        opt_indicator <- indicator_df[indicator_df$var_class %in% input$value_variable_class,]$variable
        opt_indicator2 <- names(sort(table(opt_indicator), decreasing = TRUE))
        
        if (input$value_variable_class == "Summamuuttujat"){
            opt_indicator2 <- factor(opt_indicator2, levels = c("Huono-osaisuus yhteensä",
                                                                "Huono-osaisuuden sosiaaliset seuraukset", 
                                                                "Huono-osaisuuden taloudelliset yhteydet", 
                                                                "Inhimillinen huono-osaisuus"))
        } else {
            opt_indicator2 <- factor(opt_indicator2)
        }
        
        tagList(
            selectInput(inputId = ns("value_variable"), 
                        label = "Valitse muuttuja", 
                        choices = opt_indicator2
                        ,selected = levels(opt_indicator2)[1],
                        multiple = FALSE)
        )
        # }
        
    })
    
    output$output_regio_level <- renderUI({
        
        varname <- input$value_variable[1]
        indicator_df <- varlist_diak()
        opt_indicator <- unique(indicator_df[indicator_df$var_class %in% input$value_variable_class & indicator_df$variable %in% varname,]$regio_level)
        opt_indicator <- indicator_df[indicator_df$variable %in% varname,]$regio_level
        opt_indicator <- factor(opt_indicator, levels = c("Hyvinvointialueet","Seutukunnat","Kunnat"))
        opt_indicator <- sort(opt_indicator)
        
        tagList(
            radioButtons(inputId = ns("value_regio_level"), 
                         label = "Valitse aluetaso", inline = FALSE,
                         choices = opt_indicator, selected = "Hyvinvointialueet")
        )
        # }
        
    })
    
    
    output$output_regio_select <- renderUI({
        
        region_data <- get_region_data()
        tmpdat <- region_data[region_data$level %in% input$value_regio_level,]
        choices <- tmpdat$region_name
        
        tagList(
            selectInput(inputId = ns("value_region_selected"), 
                        label = "Valitse alue",
                        choices = choices, 
                        selected = choices[1])
        )
    })
    
    
    output$output_regio_show_mode <- renderUI({
        
        varlist <- varlist_diak()
        varlist_kunta <- varlist[varlist$regio_level == "Kunnat",]$variable
        
        if (input$value_regio_level != "Kunnat" & input$value_variable %in% varlist_kunta){
            opt_indicator <- c("kaikki tason alueet", 
                               "valittu alue ja sen naapurit", 
                               "valitun alueen kunnat")
        } else {
            opt_indicator <- c("kaikki tason alueet", 
                               "valittu alue ja sen naapurit")
        }
        
        tagList(
            radioButtons(inputId = ns("value_regio_show_mode"), 
                         choices = opt_indicator, 
                         selected = opt_indicator[1],
                         label = "Kuvioissa näytettävät alueet", 
                         inline = FALSE)
        )
        
    })
    
  
    
    # EVENTREACTIVE
    react_value_variable <- eventReactive(input$button_ind, {
      input$value_variable
    })
    
    react_value_variable_class <- eventReactive(input$button_ind, {
      input$value_variable_class
    })
    
    react_value_regio_level <- eventReactive(input$button_ind, {
      input$value_regio_level
    })
    
    react_value_region_selected <- eventReactive(input$button_ind, {
      input$value_region_selected
    })
    
    react_value_leaflet <- eventReactive(input$button_ind, {
      input$value_leaflet
    })

    process_data <- reactive({
        
        dat <- get_dat()
        dat_1 <- dat[dat$regio_level %in% input$value_regio_level & dat$variable %in% input$value_variable,]
        
        if (input$value_regio_level == "Hyvinvointialueet"){
          reg <- karttasovellus::regio_Hyvinvointialueet
          } else if (input$value_regio_level == "Seutukunnat"){
            reg <- karttasovellus::regio_Seutukunnat
          } else if (input$value_regio_level == "Kunnat"){
            reg <- karttasovellus::regio_Kunnat
          }

        res <- left_join(reg, dat_1)  
        
        res <- res %>%
            select(-regio_level) %>%
            filter(!is.na(value)) %>%
            arrange(desc(value)) %>%
            mutate(value = round(value, 1)) %>%
            mutate(rank = 1:n())
        
        # res <- sf::st_transform(x = res, crs = "+proj=longlat +datum=WGS84")
        
        return(res)
    })
    
    create_municipalities_within_region <- function(varname = input$value_variable, 
                                                    regio_level = input$value_regio_level,
                                                    aluenimi = input$value_region_selected,
                                                    timeseries = TRUE){
        if (timeseries){
            dat <- get_dat_timeseries()
        } else {
            dat <- get_dat()
        }
        
        
        dat_1 <- dat[dat$regio_level %in% "Kunnat" & dat$variable %in% varname,]
        
        if (regio_level == "Hyvinvointialueet"){
            muni_key_subset <- geofi::municipality_key_2019 %>% 
              mutate(hyvinvointialue_name_fi = sub("hyvinvointialue", "HVA", hyvinvointialue_name_fi)) %>% 
                filter(hyvinvointialue_name_fi == aluenimi) %>% 
              select(name_fi)
        } else {
            muni_key_subset <- geofi::municipality_key_2019 %>% 
                filter(seutukunta_name_fi == aluenimi) %>% 
                select(name_fi) 
        }
        
        dat <- right_join(dat_1, muni_key_subset, by = c("aluenimi" = "name_fi")) %>% 
            mutate(color = TRUE, fill = value) %>% 
            select(-regio_level) %>%
            filter(!is.na(value)) %>%
            arrange(desc(value)) %>%
            mutate(value = round(value, 1)) %>%
            mutate(rank = 1:n())
        return(dat)
    }
    
    
    # get_naapurikoodit ----
    get_naapurikoodit <- function(region_data = region_data,
                                  regio_lvl = input$value_regio_level,
                                  regio_selected = input$value_region_selected){
        naapurikoodit_lst <- region_data[region_data$level %in% regio_lvl & 
                                             region_data$region_name %in% regio_selected,"neigbours"]
        
        naapurikoodit <- naapurikoodit_lst %>% unnest(cols = c(neigbours)) %>% pull(neigbours)
    }
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #    ___           _ _ _               _   _             _ _                _       _   
    #   |_ _|_ __   __| (_) | ____ _  __ _| |_| |_ ___  _ __(_) | ___   ___   _(_) ___ | |_ 
    #    | || '_ \ / _` | | |/ / _` |/ _` | __| __/ _ \| '__| | |/ / | | \ \ / / |/ _ \| __|
    #    | || | | | (_| | |   < (_| | (_| | |_| || (_) | |  | |   <| |_| |\ V /| | (_) | |_ 
    #   |___|_| |_|\__,_|_|_|\_\__,_|\__,_|\__|\__\___/|_|  |_|_|\_\\__,_| \_/ |_|\___/ \__|
    #                                                                                       
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

    ## indikaattorikuviot ----
    output$ui_plot_bar <- renderUI({
      
      Sys.sleep(1)
      if (input$value_regio_show_mode == "kaikki tason alueet"){
        if (grepl("Kunnat", input$value_regio_level)){
          bar_height = 4100
        } else if (grepl("Seutu", input$value_regio_level)){
          bar_height = 1600
        } else {
          bar_height = 800
        }
      } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
        bar_height = 800
      } else {
        bar_height = 500
      }

      tagList(
        div(style='height:820px; overflow-y: auto; overflow-x: hidden;',
            plotOutput(ns("rank_plot"), width = "100%", height = bar_height)
        )
      )
    })

    
    output$rank_plot <- renderPlot({

        dat <- process_data()
        
        region_data <- get_region_data()
        naapurikoodit <- get_naapurikoodit(region_data = region_data,
                                           regio_lvl = input$value_regio_level,
                                           regio_selected = input$value_region_selected)
        dat <- dat %>% #sf::st_transform(crs = 3067) %>% 
            mutate(color = ifelse(aluenimi %in% input$value_region_selected, TRUE, FALSE))
        
        dat <- process_data() %>% 
            st_set_geometry(NULL) %>% 
            select(rank,aluekoodi,aluenimi,value) %>% 
            mutate(aluenimi = factor(aluenimi), 
                   aluenimi = fct_reorder(aluenimi, -rank),
                   fill = value,
                   color = ifelse(aluenimi %in% input$value_region_selected, TRUE, FALSE))

        if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
            if (input$value_regio_level == "Kunnat"){
                dat$color <- ifelse(!dat$aluekoodi %in% naapurikoodit, FALSE, TRUE)
            }
        } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            
            dat <- create_municipalities_within_region(varname = input$value_variable, 
                                                       regio_level = input$value_regio_level,
                                                       aluenimi = input$value_region_selected,
                                                       timeseries = FALSE)
        }
        
        # luodaan alaotsikko
        if (input$value_regio_show_mode == "valitun alueen kunnat"){
          kuvan_subtitle <- glue("Kuvassa näytetään alueeseen {input$value_region_selected} tasolla {tolower(input$value_regio_level)} kuuluvat kunnat")
        } else {
          kuvan_subtitle <- glue("Aluetaso: {input$value_regio_level}")
        }
        
        ggplot(dat, aes(x = value, y = reorder(aluenimi, -rank))) + 
            theme_ipsum(base_family = "PT Sans",
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans",
                        grid_col = "white",
                        plot_title_face = "plain") +
            xlim(c(0,max(dat$value, na.rm = TRUE)*1.2)) +
            theme(legend.position = "right",
                  plot.title.position = "plot") +
            scale_color_manual(values = c("white","black")) + 
            geom_col(aes(color = color, fill = value), show.legend = FALSE, size = 1.1) +
          scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1) -> plot

            plot <- plot + geom_text(aes(label = value), 
                                     color = "black", 
                                     nudge_x = max(dat$value, na.rm = TRUE)*0.2, 
                                     family = "PT Sans")
            
        plot + scale_y_discrete(expand = expansion(add = 2)) +
            labs(x = NULL, y = NULL, fill = NULL) +
          labs(title = glue("{input$value_variable}"),
               subtitle = kuvan_subtitle,
               caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: THL (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"))

    }, alt = reactive({
        
        if (input$value_regio_show_mode == "kaikki tason alueet"){
            alt_teksti <- paste("Muuttujan", input$value_variable, 
                                "arvot aluetasolla",input$value_regio_level,
                                "esitetään pylväskuviossa pylvään pituutena ja täyttövärinä ja karttakuviossa alueen täyttövärinä.",
                                "Kuvioissa näytetään kaikki aluetason alueet ja korostettuna on",
                                input$value_region_selected
            )
        } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            
            region_data <- get_region_data()
            naapurikoodit <- get_naapurikoodit(region_data = region_data,
                                               regio_lvl = input$value_regio_level,
                                               regio_selected = input$value_region_selected)
            region_nms <- region_data[region_data$level %in% input$value_regio_level & region_data$region_code %in% naapurikoodit, ]$region_name
            alt_teksti <-         paste("Muuttujan", input$value_variable, 
                                        "arvot esitetään pylväskuviossa pylväiden pituutena ja pylvään pituutena ja karttakuviossa alueen värinä. Aluetasona näytetään",
                                        input$value_regio_level,
                                        "ja alueina näytetään ",
                                        glue_collapse(region_nms, sep = ", ", last = " ja "), "korostettuna", 
                                        input$value_region_selected
            )
            
        } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            
            dat <- create_municipalities_within_region(varname = input$value_variable, 
                                                       regio_level = input$value_regio_level,
                                                       aluenimi = input$value_region_selected,
                                                       timeseries = FALSE)
            
            alt_teksti <-         paste("Muuttujan ", input$value_variable, 
                                        "arvot esitetään pylväiden pituutena ja pylvään täyttövärinä ja karttakuviossa alueen värinä kuntatasolla käsittäen kunnat jotka kuuluvat alueeseen",
                                        input$value_region_selected, "tasolla", input$value_regio_level,
                                        "Kuntina näytetään",
                                        glue_collapse(unique(dat$aluenimi), sep = ", ", last = " ja ")
            )
            
        }
        }))
        
        output$map_plot <- renderPlot({
          
          dat <- process_data()
          region_data <- get_region_data()
          naapurikoodit <- get_naapurikoodit(region_data = region_data,
                                             regio_lvl = input$value_regio_level,
                                             regio_selected = input$value_region_selected)
          
          dat <- dat %>% 
            mutate(color = ifelse(aluenimi %in% input$value_region_selected, TRUE, FALSE))
          
          if (input$value_regio_show_mode == "kaikki tason alueet"){
            dat <- dat
          } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
          } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            
            dat <- create_municipalities_within_region(varname = input$value_variable, 
                                                       regio_level = input$value_regio_level,
                                                       aluenimi = input$value_region_selected,
                                                       timeseries = FALSE)
            
            reg <- karttasovellus::regio_Kunnat
            res <- left_join(reg, dat) %>% 
              filter(!is.na(aluenimi))
            
            dat <- res %>%
              filter(!is.na(value)) %>%
              arrange(desc(value)) %>%
              mutate(value = round(value, 1)) %>%
              mutate(rank = 1:n(), 
                     color = TRUE) 
          }
          
          # luodaan alaotsikko
          if (input$value_regio_show_mode == "valitun alueen kunnat"){
            kuvan_subtitle <- glue("Kuvassa näytetään alueeseen {input$value_region_selected} tasolla {tolower(input$value_regio_level)} kuuluvat kunnat")
          } else {
            kuvan_subtitle <- glue("Aluetaso: {input$value_regio_level}")
          }
          
          ggplot(data = dat, aes(fill = value)) +
            geom_sf(color = alpha("white", 1/3))  +
            geom_sf(aes(color = color), fill = NA, show.legend = FALSE)  +    
            scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1) +
            scale_color_manual(values = c(alpha("white", 1/3), "black")) +
            theme_ipsum(base_family = "PT Sans",
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans",
                        grid_col = "white",
                        plot_title_face = "plain") -> p
          
          if (input$value_regio_show_mode == "kaikki tason alueet"){
            if (input$value_regio_level != "Kunnat"){
              p + geom_sf_label(data = dat %>% filter(!aluenimi %in% input$value_region_selected), 
                                aes(label = value), 
                                family = "PT Sans", 
                                color = "black", 
                                fill = "white", 
                                size = 2.5) -> p
            }
            p <- p + geom_sf_label(data = dat %>% filter(aluenimi == input$value_region_selected),
                                   aes(label = paste(aluenimi, value)),
                                   fill = "white", color = "black", family = "PT Sans")
            
          } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            p + geom_sf_label(data = dat,
                              aes(label = paste(aluenimi, value)),
                              fill = "white", color = "black", family = "PT Sans") -> p
          } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            p + ggrepel::geom_label_repel(data = dat %>%
                                            sf::st_set_geometry(NULL) %>%
                                            bind_cols(dat %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                                          aes(label = paste0(aluenimi,"\n", value), x = X, y = Y),
                                          color = "black", fill = alpha("white", 2/3),
                                          family = "PT Sans", size = 3, lineheight = .8) -> p
            
          }
          p + theme(axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = c(0.1, 0.5),
                    plot.title.position = "plot") +
            labs(title = glue("{input$value_variable}"),
                 subtitle = kuvan_subtitle,
                 caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: THL (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"),
                 fill = paste0(add_line_break2(input$value_variable, 20), "\n(suhdeluku)"))
          
        }, alt = reactive({

          if (input$value_regio_show_mode == "kaikki tason alueet"){
            alt_teksti <- paste("Muuttujan", input$value_variable, 
                                "arvot aluetasolla",input$value_regio_level,
                                "esitetään pylväskuviossa pylvään pituutena ja täyttövärinä ja karttakuviossa alueen täyttövärinä.",
                                "Kuvioissa näytetään kaikki aluetason alueet ja korostettuna on",
                                input$value_region_selected
            )
          } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            
            region_data <- get_region_data()
            naapurikoodit <- get_naapurikoodit(region_data = region_data,
                                               regio_lvl = input$value_regio_level,
                                               regio_selected = input$value_region_selected)
            region_nms <- region_data[region_data$level %in% input$value_regio_level & region_data$region_code %in% naapurikoodit, ]$region_name
            alt_teksti <-         paste("Muuttujan", input$value_variable, 
                                        "arvot esitetään pylväskuviossa pylväiden pituutena ja pylvään pituutena ja karttakuviossa alueen värinä. Aluetasona näytetään",
                                        input$value_regio_level,
                                        "ja alueina näytetään ",
                                        glue_collapse(region_nms, sep = ", ", last = " ja "), "korostettuna", 
                                        input$value_region_selected
            )
            
          } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            
            dat <- create_municipalities_within_region(varname = input$value_variable, 
                                                       regio_level = input$value_regio_level,
                                                       aluenimi = input$value_region_selected,
                                                       timeseries = FALSE)
            
            alt_teksti <-         paste("Muuttujan ", input$value_variable, 
                                        "arvot esitetään pylväiden pituutena ja pylvään täyttövärinä ja karttakuviossa alueen värinä kuntatasolla käsittäen kunnat jotka kuuluvat alueeseen",
                                        input$value_region_selected, "tasolla", input$value_regio_level,
                                        "Kuntina näytetään",
                                        glue_collapse(unique(dat$aluenimi), sep = ", ", last = " ja ")
            )
            
          }
    }))
    
    ## aikasarja ----
    output$timeseries_plot <- renderPlot({

        dat <- get_dat_timeseries()
        region_data <- get_region_data()
        naapurikoodit_lst <- region_data[region_data$level %in% input$value_regio_level & 
                                             region_data$region_name %in% input$value_region_selected,"neigbours"]
        
        naapurikoodit <- naapurikoodit_lst %>% 
            unnest(cols = c(neigbours)) %>% 
            pull(neigbours)
        
        
        df <- dat[dat$variable == input$value_variable &
                      dat$regio_level == input$value_regio_level,] 
        
        df_gini <- karttasovellus::ineq_data
        df_gini <- df_gini[df_gini$variable == input$value_variable &
                  df_gini$regio_level == input$value_regio_level,]

        df2 <- df[df$aluekoodi %in% naapurikoodit,] %>% 
          filter(!aluenimi %in% input$value_region_selected)
        
        df_gini2 <- df_gini[df_gini$aluekoodi %in% naapurikoodit,]

        aika1 <- sort(unique(df$aika)) - 1
        aika2 <- sort(unique(df$aika)) + 1
        labels <- paste0(aika1,"-",aika2)
        
        ggplot() -> plot0
        
        df_selected_cs <- df %>% 
            filter(aika == max(aika, na.rm = TRUE),
                   aluenimi == input$value_region_selected)
        
        df_selected_ts <- df %>% 
            filter(aluenimi == input$value_region_selected)
        
        if (input$value_regio_show_mode == "kaikki tason alueet"){
            
            plot0 <- plot0 + 
                geom_line(data = df, aes(x = aika, y = value, color= aluenimi), show.legend = FALSE) +
                geom_line(data = df_selected_ts, aes(x = aika, y = value), color = "black") +
                geom_point(data = df_selected_ts, aes(x = aika, y =  value),
                           fill = "black", 
                           color = "white", shape = 21, size = 2.5, stroke = 1) +
                geom_text(data = df_selected_cs,
                          aes(x = aika, 
                              y = value, 
                              color= aluenimi, 
                              label = paste(aluenimi, round(value,1))),
                          color = "black", 
                          family = "PT Sans", 
                          nudge_x = .3)
            
            plot_gini <- ggplot() + 
              geom_line(data = df_gini, aes(x = aika, y = gini, color = aluenimi)) +
              ggrepel::geom_text_repel(data = df_gini %>% filter(aika == max(aika, na.rm = TRUE)),
                                       aes(x = aika, y = gini, color= aluenimi, label = paste(aluenimi, round(gini,2))), nudge_x = .2, family = "PT Sans")
            
        } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            
            plot0 <- plot0 + 
                geom_line(data = df2,aes(x = aika, y = value, color= aluenimi), show.legend = FALSE) +
                geom_line(data = df,
                          aes(x = aika, y = value, group = aluenimi),
                          color = "dim grey", alpha = .1) +
                geom_point(data = df2, 
                           aes(x = aika, y = value,fill= aluenimi), shape = 21, color = "white", stroke = 1, size = 2.5) +
                ggrepel::geom_text_repel(data = df2 %>% filter(aika == max(aika, na.rm = TRUE)),
                                         aes(x = aika, y = value, color= aluenimi, 
                                             label = paste(aluenimi, round(value,1))), 
                                         family = "PT Sans", nudge_x = .3) +
                geom_line(data = df_selected_ts, aes(x = aika, y = value), color = "black") +
                geom_point(data = df_selected_ts, aes(x = aika, y = value),
                           fill = "black", 
                           color = "white", shape = 21, size = 2.5, stroke = 1) +
                
                geom_text(data = df_selected_cs,
                          aes(x = aika, 
                              y = value, 
                              color= aluenimi, 
                              label = paste(aluenimi, round(value,1))),
                          color = "black", 
                          family = "PT Sans", 
                          nudge_x = .3)
            # gini
            plot_gini <- ggplot() + 
              geom_line(data = df_gini2, aes(x = aika, y = gini, color = aluenimi)) +
              geom_line(data = df_gini, aes(x = aika, y = gini, color = aluenimi), alpha = .1) +
              ggrepel::geom_text_repel(data = df_gini2 %>% filter(aika == max(aika, na.rm = TRUE)),
                                       aes(x = aika, y = gini, color= aluenimi, 
                                           label = paste(aluenimi, round(gini,1))), 
                                       family = "PT Sans", nudge_x = .3)

        } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            
            dat2 <- create_municipalities_within_region(varname = input$value_variable, 
                                                        regio_level = input$value_regio_level,
                                                        aluenimi = input$value_region_selected,
                                                        timeseries = TRUE)
            
            dat2_gini <- dat2 %>% group_by(aika) %>% 
                mutate(gini = round(ineq::Gini(value),2)) %>% 
                ungroup()
            
            plot0 <- plot0 + 
                geom_line(data = dat2,aes(x = aika, y = value, color= aluenimi), show.legend = FALSE) +
                geom_point(shape = 21, color = "white", stroke = 1, size = 2.5) +
                ggrepel::geom_text_repel(data = dat2 %>% filter(aika == max(aika, na.rm = TRUE)),
                                         aes(x = aika, y = value, color= aluenimi, label = paste(aluenimi, round(value,1))), nudge_x = .2, family = "PT Sans")
            
            df_gini2 <- df_gini[df_gini$aluenimi %in% input$value_region_selected,]

            plot_gini <- ggplot() + 
              geom_line(data = df_gini2, aes(x = aika, y = gini, color = aluenimi)) +
              geom_line(data = df_gini, aes(x = aika, y = gini, color = aluenimi), alpha = .1) +
              ggrepel::geom_text_repel(data = df_gini2 %>% filter(aika == max(aika, na.rm = TRUE)),
                                       aes(x = aika, y = gini, color= aluenimi, 
                                           label = paste(aluenimi, round(gini,1))), 
                                       family = "PT Sans", nudge_x = .3)
        }
        # luodaan alaotsikko
        if (input$value_regio_show_mode == "valitun alueen kunnat"){
            kuvan_subtitle <- glue("Kuvassa näytetään alueeseen {input$value_region_selected} tasolla {tolower(input$value_regio_level)} kuuluvat kunnat")
        } else {
            kuvan_subtitle <- glue("Aluetaso: {input$value_regio_level}")
        }
        
        plot0 +

            scale_x_continuous(breaks = sort(unique(df$aika)), labels = labels) +
          theme_ipsum(base_family = "PT Sans",
                      plot_title_family = "PT Sans",
                      subtitle_family = "PT Sans",
                      base_size = 14,
                      plot_title_face = "plain") +
            labs(fill = NULL,
                 x = NULL,
                 y = paste0(add_line_break2(input$value_variable, 50), "\n(suhdeluku)"),
                 title = glue("{input$value_variable}"),
                 subtitle = kuvan_subtitle
            )  +
            theme(legend.position = "none",
                  panel.grid.major.x = element_line(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.title.position = "plot",
                  plot.margin = unit(c(0,0,0,0), "mm")#,
                  ) -> plot1
        
        plot_gini <- plot_gini + scale_x_continuous(breaks = sort(unique(df$aika)), labels = labels) +
            theme_ipsum(base_family = "PT Sans",
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans",
                        base_size = 14,
                        plot_title_face = "plain") +
            labs(fill = NULL,
                 x = NULL,
                 subtitle = paste0("Indikaattorin ", add_line_break2(input$value_variable, 50), "\neriarvoisuuden kuntatason gini-kerroin aluetasolla: ", input$value_regio_level),
                 caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: THL (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"))  +
            theme(legend.position = "none",
                  panel.grid.major.x = element_line(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.title.position = "plot",
                  axis.title.x = element_blank(),
                  plot.margin = unit(c(10,0,0,0), "mm"),
                  axis.text.x = element_blank()
                  )
        
        patchwork::wrap_plots(plot1,plot_gini, ncol = 1, heights = c(1,1))
        
    }, alt = reactive({

        if (input$value_regio_show_mode == "kaikki tason alueet"){
            alt_teksti <- paste("Muuttujan", input$value_variable, 
                                "arvot aluetasolla",input$value_regio_level,
                                "esitetään vuositason aikasarjakuviona, jossa aikasarjan esimmäinen ajankohta on 2010 - 2012 ja viimeinen 2017 - 2019",
                                "Kuvioissa näytetään kaikki aluetason alueet ja korostettuna on",
                                input$value_region_selected
            )
        } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            
            region_data <- get_region_data()
            naapurikoodit <- get_naapurikoodit(region_data = region_data,
                                               regio_lvl = input$value_regio_level,
                                               regio_selected = input$value_region_selected)
            region_nms <- region_data[region_data$level %in% input$value_regio_level & region_data$region_code %in% naapurikoodit, ]$region_name
            alt_teksti <-         paste("Muuttujan", input$value_variable, 
                                        "arvot aluetasolla",input$value_regio_level,
                                        "esitetään vuositason aikasarjakuviona, jossa aikasarjan esimmäinen ajankohta on 2010 - 2012 ja viimeinen 2017 - 2019",
                                        "ja alueina näytetään ",
                                        glue_collapse(region_nms, sep = ", ", last = " ja "), "korostettuna", 
                                        input$value_region_selected
            )
            
        } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            
            dat <- create_municipalities_within_region(varname = input$value_variable, 
                                                       regio_level = input$value_regio_level,
                                                       aluenimi = input$value_region_selected,
                                                       timeseries = FALSE)
            
            alt_teksti <-         paste("Muuttujan ", input$value_variable, 
                                        "arvot esitetään vuositason aikasarjakuviona, jossa aikasarjan esimmäinen ajankohta on 2010 - 2012 ja viimeinen 2017 - 2019. 
                                        Alueet esitetään kuntatasolla käsittäen kunnat jotka kuuluvat alueeseen",
                                        input$value_region_selected, "tasolla", input$value_regio_level,
                                        "Kuntina näytetään",
                                        glue_collapse(unique(dat$aluenimi), sep = ", ", last = " ja ")
            )
        }
    }))
    
    
    
    
    output$aputeksti <- renderText({
        
        txt <- react_value_region_profile()
        return(txt)
    })
  })
}
    
