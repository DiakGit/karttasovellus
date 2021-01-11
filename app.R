source("./global.R")


ui <- fluidPage(
    # use_waitress(),
    uiOutput("output_indicator_class"),
    uiOutput("output_indicator"),
    uiOutput("output_regio_level"),
    uiOutput("output_regio_select"),
    uiOutput("output_regio_show_mode"),
    shinycssloaders::withSpinner(plotOutput("timeseries_plot")),
    shinycssloaders::withSpinner(plotOutput("map_rank_plot"))#,
    
    # uiOutput("output_regio_level_profile"),
    # uiOutput("output_region_profile"),
    # uiOutput("output_button_profile"),
    # textOutput("aputeksti"),
    # # uiOutput("output_save_word"),
    # shinycssloaders::withSpinner(uiOutput("region_profile_html")),
    # tags$hr(),
    # plotOutput("profiilikartta01", width = "90%", height = "500px"),

    # uiOutput("output_save_map"),
    # leaflet::leafletOutput("map1", width = "100%", height = "285px"),
    # DT::dataTableOutput("rank_tbl"),
    # verbatimTextOutput("value"),
    # uiOutput("output_regio_level_profile"),
    # uiOutput("output_region_profile"),
    # uiOutput("region_profile_html"),
    # DT::dataTableOutput("variable_desctiption")
    # gt::gt_output("variable_desctiption_gt")
)


# Serverin logiikka ----
server <- function(input, output) {
    
    ## datat ----
    
    get_dat <- reactive({
        dat <- readRDS("./data/df_v20201102.RDS")
        return(dat)
    })
    
    get_dat_timeseries <- reactive({
        dat_aika <- readRDS("./data/df_v20201121_aikasarja.RDS")
        return(dat_aika)
    })
    
    get_region_data <- reactive({
        
        region_data <- readRDS("./data/region_data.RDS")
        # dat <- dplyr::filter(region_data, level %in% input$value_region_level2)
        return(region_data)
        
    })
    
    
    varlist_diak <- reactive({
        dat <- get_dat()
        dat %>% 
            count(regio_level,var_class,variable) %>% 
            select(-n) %>% 
            arrange(desc(var_class),variable) -> indicator_df
        return(indicator_df)
    })
    
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
        
        # if (input$sidebar_menu == "info"){
        #     tagList()
        # } else {
        tagList(
            selectInput(
                inputId = "value_variable_class", 
                label = "Valitse muuttujaluokka", 
                choices = opt_indicator, 
                selected = opt_indicator[1])
        )
        # }
        
    })
    
    
    output$output_indicator <- renderUI({
        
        req(input$value_variable_class)
        
        indicator_df <- varlist_diak()
        # järjestetään niin että ne indikaattorit joita useammalta tasolta tulee ekana
        opt_indicator <- indicator_df[indicator_df$var_class %in% input$value_variable_class,]$variable
        opt_indicator2 <- names(sort(table(opt_indicator), decreasing = TRUE))
        
        # if (input$sidebar_menu == "info"){
        #     tagList()
        # } else {
        tagList(
            selectInput(inputId = "value_variable", 
                        label = "Valitse muuttuja", 
                        choices = opt_indicator2
                        ,selected = opt_indicator2[1],
                        # option= pickerOptions(
                        #     actionsBox = TRUE,
                        #     liveSearch = TRUE,
                        #     deselectAllText = "Ei mitään",
                        #     selectAllText = "Kaikki",
                        #     noneSelectedText = "Ei yhtään valittuna",
                        #     noneResultsText = "Ei yhtään osumaa"#,
                        #     # maxOptions = 4,
                        #     # maxOptionsText = "Maksimimäärä muuttujia valittu"
                        # ),
                        multiple = FALSE)
        )
        # }
        
    })
    
    output$output_regio_level <- renderUI({
        
        req(input$value_variable_class)
        req(input$value_variable)
        varname <- input$value_variable[1]
        indicator_df <- varlist_diak()
        opt_indicator <- unique(indicator_df[indicator_df$var_class %in% input$value_variable_class & indicator_df$variable %in% varname,]$regio_level)
        opt_indicator <- indicator_df[indicator_df$variable %in% varname,]$regio_level
        opt_indicator <- factor(opt_indicator, levels = c("Maakunnat","Seutukunnat","Kunnat"))
        opt_indicator <- sort(opt_indicator)
        
        # if (input$sidebar_menu == "info"){
        #     tagList()
        # } else {
        tagList(
            radioButtons(inputId = "value_regio_level", 
                         label = "Valitse aluetaso", inline = FALSE,
                         choices = opt_indicator, selected = "Maakunnat")
        )
        # }
        
    })
    
    
    output$output_regio_select <- renderUI({
        
        req(input$value_regio_level)
        
        region_data <- get_region_data()
        tmpdat <- region_data[region_data$level %in% input$value_regio_level,]
        choices <- tmpdat$region_name
        
        tagList(
            selectInput(inputId = "value_region_selected", 
                        label = "Valitse alue",
                        choices = choices, 
                        selected = choices[1])
        )
    })
    
    
    output$output_regio_show_mode <- renderUI({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        # req(input$value_region_selected)
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
            radioButtons(inputId = "value_regio_show_mode", 
                         choices = opt_indicator, 
                         selected = opt_indicator[1],
                         label = "Kuvioissa näytettävät alueet", 
                         inline = FALSE)
        )

    })
    
    
    
    output$output_regio_level_profile <- renderUI({
        
        req(input$value_variable_class)
        req(input$value_variable)
        varname <- input$value_variable[1]
        indicator_df <- varlist_diak()
        opt_indicator <- unique(indicator_df[indicator_df$var_class %in% input$value_variable_class & indicator_df$variable %in% varname,]$regio_level)
        opt_indicator <- indicator_df[indicator_df$variable %in% varname,]$regio_level
        opt_indicator <- factor(opt_indicator, levels = c("Maakunnat","Seutukunnat","Kunnat"))
        opt_indicator <- sort(opt_indicator)
        
        tagList(
            radioButtons(inputId = "value_regio_level_profile", 
                         label = "Valitse aluetaso", inline = FALSE,
                         choices = opt_indicator, selected = "Maakunnat")
        )
    })
    
    output$output_region_profile <- renderUI({
        
        req(input$value_regio_level_profile)
        
        region_data <- get_region_data()
        tmpdat <- region_data[region_data$level %in% input$value_regio_level_profile,]
        choices <- tmpdat$region_name
        
        tagList(
            selectInput(inputId = "value_region_profile", 
                        label = "Valitse alue",
                        choices = choices, 
                        selected = choices[1])
        )
    })
    
    
    output$output_button_profile <- renderUI({
        tagList(
            actionButton("button", "Luo alueprofiili", class="btn btn-primary")
        )
    })
    
    
    ## reaktiivisuus ----
    
    # Define reactiveValue
    rv <- reactiveValues(selected = NULL)
    
    # 1. Pass value of input$timeline_selected to Reactive Value
    observe( {
        rv$map_click_id <- input$map1_shape_click
    })
    # 2. clear selection if different filter is chosen
    observeEvent(input$value_regio_level, {
        rv$map_click_id <- NULL
    })
    
    get_klik <- reactive({
        # req(input$value_variable_class)
        # req(input$value_variable)
        req(input$value_regio_level)
        reg <- readRDS("./data/regiokey.RDS")
        idnimi <- reg[reg$aluetaso == input$value_regio_level,]$aluenimi[1]
        
        # klik <- rv$map_click_id
        # if (is.null(klik)){
            klik <- list("id" = input$value_region_selected)
        # }
        return(klik)
    })
    
    # EVENTREACTIVE
    react_value_regio_level_profile <- eventReactive(input$button, { 
        input$value_regio_level_profile
    })
    
    react_value_region_profile <- eventReactive(input$button, { 
        input$value_region_profile
    })
    
    process_data <- reactive({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        
        dat <- get_dat()
        
        dat_1 <- dat[dat$regio_level %in% input$value_regio_level & dat$variable %in% input$value_variable,]
        
        reg <- readRDS(glue("./data/regio_{input$value_regio_level}.RDS"))
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
        
        if (regio_level == "Maakunnat"){
            muni_key_subset <- geofi::municipality_key_2019 %>% 
                filter(maakunta_name_fi == aluenimi) %>% 
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
    
    process_data_profile_doc <- reactive({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        
        dat <- get_dat()
        
        dat_1 <- dat[dat$regio_level %in% react_value_regio_level_profile() & dat$variable %in% input$value_variable,]
        
        reg <- readRDS(glue("./data/regio_{react_value_regio_level_profile()}.RDS"))
        res <- left_join(reg, dat_1)    
        
        res <- res %>%
            select(-regio_level) %>%
            filter(!is.na(value)) %>%
            arrange(desc(value)) %>%
            mutate(value = round(value, 1)) %>%
            mutate(rank = 1:n())
        
        res <- sf::st_transform(x = res, crs = "+proj=longlat +datum=WGS84")
        
        return(res)
    })
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
    
 
    
    output$map_rank_plot <- renderPlot({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        req(input$value_region_selected)
        req(input$value_regio_show_mode)
        
        dat <- process_data()
        
        region_data <- get_region_data()
        naapurikoodit <- get_naapurikoodit(region_data = region_data,
                                           regio_lvl = input$value_regio_level,
                                           regio_selected = input$value_region_selected)
        dat <- dat %>% #sf::st_transform(crs = 3067) %>% 
            mutate(color = ifelse(aluenimi %in% input$value_region_selected, TRUE, FALSE))

        ## Tolpat ----
        
        dat <- process_data() %>% 
            st_set_geometry(NULL) %>% 
            select(rank,aluekoodi,aluenimi,value) %>% 
            mutate(aluenimi = factor(aluenimi), 
                   aluenimi = fct_reorder(aluenimi, -rank),
                   fill = value,
                   color = ifelse(aluenimi %in% input$value_region_selected, TRUE, FALSE))
        # kuntatasolla kaikki alueet -näkymässä näytetään vaan valitun kunnan arvo
        # dat$color <- ifelse(input$value_regio_level == "Kunnat" & dat$aluenimi , )
        
        
        if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            dat$fill <- ifelse(!dat$aluekoodi %in% naapurikoodit, NA, dat$fill)
            if (input$value_regio_level == "Kunnat"){
                dat$color <- ifelse(!dat$aluekoodi %in% naapurikoodit, FALSE, TRUE)
            }
        } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            
            dat <- create_municipalities_within_region(varname = input$value_variable, 
                                                       regio_level = input$value_regio_level,
                                                       aluenimi = input$value_region_selected,
                                                       timeseries = FALSE)
        }

        ggplot(dat, aes(x = value, y = reorder(aluenimi, -rank))
        ) + 
            geom_col(aes(fill = fill)) +
            theme_ipsum(base_family = "PT Sans",
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans",
                        grid_col = "white") +
            xlim(c(0,max(dat$value, na.rm = TRUE)*1.2)) +
            theme(legend.position = "none",
                  plot.title.position = "plot") +
            # scale_fill_ipsum() +
            scale_fill_viridis_c(option = "viridis", direction = -1, alpha = .4, na.value="grey90") +
            scale_color_manual(values = c("white","black")) + 
            geom_col(aes(color = color), fill = NA) -> plot
        
        # Tolppakuvion eri aluetasoilla erityyppiset tolppakuviot
        if (input$value_regio_level == "Seutukunnat"){
            plot <- plot +
                theme(axis.text.y = element_text(size = 9)) +
                geom_text(dat = dat[!is.na(dat$fill),], 
                          aes(label = paste0(value, " ", rank, "/", max(rank, na.rm = TRUE))), 
                          color = "black", 
                          nudge_x = max(dat$value, na.rm = TRUE)*0.1, 
                          family = "PT Sans", size = 2.5)
        } else if (input$value_regio_level == "Kunnat"){
            
            plot <- plot +
                theme(axis.text.y = element_blank()) +
                geom_text(dat = dat[dat$color,],
                          aes(label = paste0(aluenimi, " ",value, " ", rank, "/", max(rank, na.rm = TRUE))),
                          color = "black",
                          nudge_x = max(dat$value, na.rm = TRUE)*0.2,
                          family = "PT Sans")
        } else if (input$value_regio_level == "Maakunnat"){
            plot <- plot + geom_text(aes(label = value), 
                                     color = "black", 
                                     nudge_x = max(dat$value, na.rm = TRUE)*0.1, 
                                     family = "PT Sans")
        }
        plot + scale_y_discrete(expand = expansion(add = 2)) +
            labs(x = NULL, y = NULL) -> p1
        
        
        ## kartta ----
        
        dat <- process_data()
        dat <- dat %>% #sf::st_transform(crs = 3067) %>% 
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
            
            reg <- readRDS(glue("./data/regio_Kunnat.RDS"))
            res <- left_join(reg, dat) %>% 
                filter(!is.na(aluenimi))
            
            dat <- res %>%
                filter(!is.na(value)) %>%
                arrange(desc(value)) %>%
                mutate(value = round(value, 1)) %>%
                mutate(rank = 1:n(), 
                      color = TRUE) 
        }
            
            
            
        
        ggplot(data = dat, aes(fill = value, color = color)) +
            geom_sf()  +
            scale_fill_viridis(option = "viridis", direction = -1, alpha = .5) +
            scale_color_manual(values = c(alpha("white", 1/3), "black")) +
            theme_minimal(base_family = "PT Sans", base_size = 12) -> p
        
        if (input$value_regio_show_mode == "kaikki tason alueet"){
            p + geom_sf_label(data = dat %>% filter(aluenimi == input$value_region_selected),
                              aes(label = paste(aluenimi, value)),
                              fill = alpha("white",2/3), color = "black", family = "PT Sans") -> p
        } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            p + geom_sf_label(data = dat,
                              aes(label = paste(aluenimi, value)),
                              fill = alpha("white",2/3), color = "black", family = "PT Sans") -> p
        } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
                p + ggrepel::geom_label_repel(data = dat %>%
                                                 sf::st_set_geometry(NULL) %>%
                                                 bind_cols(dat %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                                             aes(label = paste0(aluenimi,"\n", value), x = X, y = Y),
                                             color = "black", fill = alpha("white", 1/3),
                                             family = "PT Sans", size = 3, lineheight = .8) -> p
            
        }
        p + theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  panel.grid = element_blank(),
                  legend.position = "none",
                  plot.title.position = "plot") +
            labs(fill = NULL) -> p2

        # luodaan alaotsikko
        if (input$value_regio_show_mode == "valitun alueen kunnat"){
            kuvan_subtitle <- glue("Kuvassa näytetään alueeseen {input$value_region_selected} tasolla {tolower(input$value_regio_level)} kuuluvat kunnat")
        } else {
            kuvan_subtitle <- glue("Aluetaso: {input$value_regio_level}")
        }
        
        
        p1 + p2 +
            plot_layout(
                ncol = 2,
                widths = c(1, 1.2)
            ) -> plotlist
        wrap_plots(plotlist, ncol = 1) +
            plot_annotation(
                     title = glue("{input$value_variable}"),
                     subtitle = kuvan_subtitle,
                     caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: THL (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"),
                     theme = theme(plot.title = element_text(family = "PT Sans", size = 20),
                                   plot.subtitle = element_text(family = "PT Sans", size = 16),
                                   plot.caption = element_text(family = "PT Sans", size = 11))
                     )

    }, alt = reactive({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        req(input$value_region_selected)
        req(input$value_regio_show_mode)
        
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
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        req(input$value_region_selected)
        req(input$value_regio_show_mode)

        
        # klik <- get_klik()    
        dat <- get_dat_timeseries()
        region_data <- get_region_data()
        naapurikoodit_lst <- region_data[region_data$level %in% input$value_regio_level & 
                                             region_data$region_name %in% input$value_region_selected,"neigbours"]
        
        naapurikoodit <- naapurikoodit_lst %>% unnest(cols = c(neigbours)) %>% pull(neigbours)
        
        
        df <- dat[dat$variable == input$value_variable &
                      dat$regio_level == input$value_regio_level,] #%>% 
            # mutate(color = ifelse(aluenimi %in% input$value_region_selected, TRUE, FALSE))
        
        df2 <- df[df$aluekoodi %in% naapurikoodit,]
        
        
        aika1 <- sort(unique(df$aika)) - 1
        aika2 <- sort(unique(df$aika)) + 1
        labels <- paste0(aika1,"-",aika2)
        
        ggplot() -> plot0
        
        
        if (input$value_regio_show_mode == "kaikki tason alueet"){
            
        plot0 <- plot0 + geom_line(data = df,aes(x = aika, y = value, color= aluenimi, fill= aluenimi), show.legend = FALSE) +
            geom_point(shape = 21, color = "white", stroke = 1, size = 2.5) +
            # ggrepel::geom_text_repel(data = df %>% filter(color, aika == max(aika, na.rm = TRUE)),
            #                          aes(x = aika, y = value, color= aluenimi, label = round(value,1)), family = "PT Sans") +
            geom_text(data = df %>% filter(aika == max(aika, na.rm = TRUE),
                                                          aluenimi == input$value_region_selected),
                                     aes(x = aika, y = value, color= aluenimi, label = paste(aluenimi, round(value,1))),color = "black", family = "PT Sans", nudge_x = .2)
            
        } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            
            # alfa = ifelse(input$value_regio_level == "Kunnat", .1, .2)
            
            plot0 <- plot0 + 
                geom_line(data = df2,aes(x = aika, y = value, color= aluenimi, fill= aluenimi), show.legend = FALSE) +
                geom_line(data = df,
                          aes(x = aika, y = value, group = aluenimi),
                          color = "dim grey", alpha = .1) +
                geom_point(shape = 21, color = "white", stroke = 1, size = 2.5) +
                ggrepel::geom_text_repel(data = df2 %>% filter(aika == max(aika, na.rm = TRUE)),
                                         aes(x = aika, y = value, color= aluenimi, label = round(value,1)), family = "PT Sans") +
                ggrepel::geom_text_repel(data = df2 %>% filter(aika == max(aika, na.rm = TRUE)-1),
                                         aes(x = aika, y = value, color= aluenimi, label = aluenimi), family = "PT Sans")
            
            
        } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            
            dat2 <- create_municipalities_within_region(varname = input$value_variable, 
                                                       regio_level = input$value_regio_level,
                                                       aluenimi = input$value_region_selected,
                                                       timeseries = TRUE)
            
            plot0 <- plot0 + 
                geom_line(data = dat2,aes(x = aika, y = value, color= aluenimi, fill= aluenimi), show.legend = FALSE) +
                geom_point(shape = 21, color = "white", stroke = 1, size = 2.5) +
                ggrepel::geom_text_repel(data = dat2 %>% filter(aika == max(aika, na.rm = TRUE)),
                                         aes(x = aika, y = value, color= aluenimi, label = paste(aluenimi, round(value,1))), nudge_x = .2, family = "PT Sans")
        }
        
        
        # luodaan alaotsikko
        if (input$value_regio_show_mode == "valitun alueen kunnat"){
            kuvan_subtitle <- glue("Kuvassa näytetään alueeseen {input$value_region_selected} tasolla {tolower(input$value_regio_level)} kuuluvat kunnat")
        } else {
            kuvan_subtitle <- glue("Aluetaso: {input$value_regio_level}")
        }
        
        plot0 +
            # 
            # # valittu
            # ggrepel::geom_label_repel(data = df %>% filter(aika == max(aika, na.rm = TRUE)-1,
            #                                                aluenimi == input$value_region_selected),
            #                           aes(label = aluenimi), color = "white", family = "PT Sans") +
            
            scale_x_continuous(breaks = sort(unique(df$aika)), labels = labels) +
            theme_ipsum(base_family = "PT Sans",
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans",
                        grid_col = "white", 
                        plot_title_face = "plain") +
            scale_fill_viridis_d(option = "plasma", direction = -1, begin = .1, end = .9) +
            scale_color_viridis_d(option = "plasma", direction = -1, begin = .1, end = .9) +
            labs(fill = NULL,
                 x = NULL,
                 y = NULL,
                 title = glue("{input$value_variable}"),
                 subtitle = kuvan_subtitle,
                 caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: THL (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}")
                 )  +
            theme(legend.position = "none",
                  plot.title.position = "plot",
                  plot.title = element_text(family = "PT Sans", size = 20),
                  plot.subtitle = element_text(family = "PT Sans", size = 16),
                  plot.caption = element_text(family = "PT Sans", size = 11)) -> plot1
        plot1
        
    }, alt = reactive({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        req(input$value_region_selected)
        req(input$value_regio_show_mode)
        
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
    
    
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #       _    _                             __ _ _ _ _ 
    #      / \  | |_   _  ___ _ __  _ __ ___  / _(_|_) (_)
    #     / _ \ | | | | |/ _ \ '_ \| '__/ _ \| |_| | | | |
    #    / ___ \| | |_| |  __/ |_) | | | (_) |  _| | | | |
    #   /_/   \_\_|\__,_|\___| .__/|_|  \___/|_| |_|_|_|_|
    #                        |_|                          
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    ## alueprofiilit ----
    
    
    create_alueprofiili_content <- function(aluename2 = aluename, 
                                            naapurikoodit = naapurikoodit, 
                                            aluetaso1 = aluetaso1, 
                                            type = "html", 
                                            aikasarja = FALSE){
        
        if (aikasarja){
            dat <- get_dat_timeseries()
        } else {
            dat <- get_dat()
            dat$aika <- NA
        }
        
        # aluetaso1
        
        dat[dat$regio_level %in% aluetaso1 & dat$aluenimi %in% aluename2 ,] %>% 
            select(aika,aluenimi,var_class,variable,value) %>% 
            mutate(rooli = "valinta") -> tmpdat1
        dat[dat$regio_level %in% aluetaso1 & dat$aluekoodi %in% naapurikoodit ,] %>% 
            filter(!aluenimi %in% aluename2) %>% 
            select(aika,aluenimi,var_class,variable,value) %>% 
            mutate(rooli = "naapuri") -> tmpdat2
        tmpdat <- bind_rows(tmpdat1,tmpdat2) 
        
        # 
        dat[dat$regio_level %in% aluetaso1,] %>% 
            group_by(var_class,variable) %>% 
            arrange(desc(value)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            # mutate(maksimi = glue("{round(value, 1)} ({ifelse(nchar(aluenimi) > 10, paste0(substr(aluenimi, 1, 10),'...'), aluenimi)})")) %>% 
            mutate(rooli = "korkein arvo") %>% 
            select(aika,aluenimi,var_class,variable,value,rooli) -> max_dat
        
        dat[dat$regio_level %in% aluetaso1,] %>% 
            group_by(var_class,variable) %>% 
            arrange(value) %>% 
            slice(1) %>% 
            ungroup() %>% 
            # mutate(minimi = glue("{round(value, 1)} ({ifelse(nchar(aluenimi) > 10, paste0(substr(aluenimi, 1, 10),'...'), aluenimi)})")) %>% 
            mutate(rooli = "matalin arvo") %>% 
            select(aika,aluenimi,var_class,variable,value,rooli) -> min_dat
        
        dat[dat$regio_level %in% aluetaso1,] %>% 
            group_by(variable) %>% 
            arrange(desc(value)) %>%
            mutate(sija = 1:n(),
                   n = n()) %>% 
            ungroup() %>% 
            # filter(aluenimi %in% aluename2) %>% 
            select(aika,aluenimi,variable,sija) -> rank_dat
        
        bind_rows(tmpdat,max_dat,min_dat) %>% 
            left_join(rank_dat) %>% 
            mutate(value = round(value, 1)) %>% 
            rename(muuttuja = variable,
                   arvo = value) %>% 
            filter(!is.na(arvo)) %>% 
            select(aika,muuttuja,arvo,sija,everything()) %>% 
            distinct() -> tabdat
        # tmpdat -> tabdat
        return(tabdat)
    }
    
    
    alueprofiilikartta_html <- function(val_aluetaso1 = aluetaso1, 
                                        val_aluename = aluename, 
                                        val_region_data = region_data, 
                                        val_muuttujaluokka = muuttujaluokka,
                                        val_muuttuja = muuttuja){
        
        region_data <- dplyr::filter(val_region_data, level %in% val_aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% val_aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = val_aluename, 
                                              aluetaso1 = val_aluetaso1, 
                                              naapurikoodit = naapurikoodit)
        
        lista1_tbl <- tabdat %>%
            filter(var_class == val_muuttujaluokka,
                   muuttuja == val_muuttuja) %>%
            filter(rooli %in% c("naapuri","valinta")) %>% 
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija)
        mapdata_tmp <- left_join(region_data, lista1_tbl, by = c("region_name" = "aluenimi")) %>% 
            filter(!is.na(muuttuja))
            plot <- ggplot(data = mapdata_tmp, aes(fill = arvo)) +                    
                geom_sf(color = alpha("white", 1/3))  +
                hrbrthemes::theme_ipsum(base_family = "PT Sans", 
                                        base_size = 11, 
                                        plot_title_size = 12) +
                scale_fill_viridis_c(option = "plasma") +
                theme(axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      panel.grid.major = element_blank(),
                      plot.title.position = "plot",
                      plot.title = element_text(face = "plain"),
                      legend.position = "top", 
                      legend.key.width = unit(3,"line")) +
                labs(x = NULL, 
                     y = NULL, 
                     fill = NULL,
                     title = val_muuttuja
                ) +
                geom_sf_label(aes(label = paste0(region_name, "\n", round(arvo,1))), 
                              color = "white", fill = alpha("black", 1/3), family = "PT Sans", size = 3)
        plot
    }
    
    
    alueprofiiliaikasarja_html <- function(val_aluetaso1 = aluetaso1, 
                                           val_aluename = aluename, 
                                           val_region_data = region_data, 
                                           val_muuttujaluokka = muuttujaluokka,
                                           val_muuttuja1 = muuttuja){
        
        region_data <- dplyr::filter(val_region_data, level %in% val_aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% val_aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = val_aluename, 
                                              aluetaso1 = val_aluetaso1, 
                                              naapurikoodit = naapurikoodit, 
                                              aikasarja = TRUE)
        
        tabdat1 <- tabdat[tabdat$var_class == val_muuttujaluokka & tabdat$muuttuja == val_muuttuja1, ]
        
        plotdata_tmp <- tabdat1 %>%
            filter(rooli %in% c("naapuri","valinta")) %>% 
            select(aika,muuttuja,aluenimi,arvo,sija) %>%
            mutate(value = arvo) %>% 
            arrange(muuttuja,sija)
        # plotdata_tmp <- lista1_tbl[lista1_tbl$muuttuja == vars[vii],]
        aika1 <- sort(unique(plotdata_tmp$aika)) - 1
        aika2 <- sort(unique(plotdata_tmp$aika)) + 1
        labels <- paste0(aika1,"-\n",aika2)
            
        plot <- ggplot(data = plotdata_tmp,
                                       aes(x = aika, y = value, color= aluenimi, fill= aluenimi)) +
                geom_line(show.legend = FALSE) +
                geom_point(shape = 21, color = "white", stroke = 1, size = 2.5) +
                ggrepel::geom_text_repel(data = plotdata_tmp %>% filter(aika == max(aika, na.rm = TRUE)),
                                         aes(label = round(value,1)), family = "PT Sans") +
                ggrepel::geom_text_repel(data = plotdata_tmp %>% filter(aika == max(aika, na.rm = TRUE)-1),
                                         aes(label = aluenimi), family = "PT Sans", nudge_x = -1) +
                
                scale_x_continuous(breaks = sort(unique(plotdata_tmp$aika)), labels = labels) +
                theme_ipsum(base_family = "PT Sans", 
                            base_size = 11, 
                            plot_title_size = 12,
                            plot_title_family = "PT Sans",
                            subtitle_family = "PT Sans",
                            grid_col = "white") +
                theme(legend.position = "none",
                      plot.title.position = "plot",
                      plot.title = element_text(face = "plain"),
                      axis.text.x = element_text(size = 9)) +
                scale_fill_viridis_d(option = "viridis", direction = -1, begin = .1, end = .9) +
                scale_color_viridis_d(option = "viridis", direction = -1, begin = .1, end = .9) +
                labs(y = NULL, x = NULL,
                     title = val_muuttuja1
                     )
        return(plot)

    }

    
    ## html-profiilin apufunktioita ----
    
    create_raw_tbl <- function(dd1 = tabdat_tmp, muuttujanimi= muuttujanimi, varnro = 1){
        dd2 <- dd1 %>% 
            mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija) %>% 
            mutate(sija = paste0(sija,"."))
        dd3 <- dd2[dd2$muuttuja == muuttujanimi[varnro], ] %>% 
            select(-muuttuja)
        return(dd3)
    }
    
    create_gt_tbl <- function(lst_df = lista1_tbl02){
        # gt_tbl <- gt(data = lst_df,
        #              rowname_col = "aluenimi"
        # ) %>% 
        #     tab_options(table.width	= "100%", 
        #                 table.align = "left",
        #                 # table.font.size = "80%",
        #                 row_group.background.color = alpha("grey", 1/6)) %>% 
        #     cols_align(
        #         align = "right",
        #         columns = vars(sija)
        #     )
        kableExtra::kable_styling(knitr::kable(lst_df)) -> gt_tbl
        htmltools::HTML(gt_tbl) -> gt_tbl
        return(gt_tbl)
    }
    
    create_alt_text_aikasarja <- function(varname = NULL){
        # alt-tekstiin
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        
        region_data <- get_region_data()
        region_data <- dplyr::filter(region_data, level %in% aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
        
        region_nms <- region_data[region_data$level %in% aluetaso1 & region_data$region_code %in% naapurikoodit, ]$region_name
        
        
        paste("Aikasarjakuvio jossa pystyakselilla esitetään muuttujan",
              varname, "arvot ja vaaka-akselilla vuodet. Aluetaso on",
              aluetaso1, 
              "ja alueina mukana ", 
              glue_collapse(region_nms, sep = ", ", last = " ja ")
        )
    }
    
    create_alt_text_kartta <- function(varname = NULL){
        # alt-tekstiin
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        
        region_data <- get_region_data()
        region_data <- dplyr::filter(region_data, level %in% aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
        
        region_nms <- region_data[region_data$level %in% aluetaso1 & region_data$region_code %in% naapurikoodit, ]$region_name
        
        
        paste("Karttakuvio jossa alueiden väri esittää muuttujan",
              varname, "arvoja vuodelta 2017-2019. Kartan luetaso on",
              aluetaso1, 
              "ja alueina mukana ", 
              glue_collapse(region_nms, sep = ", ", last = " ja ")
        )
    }
    
    
    output$summamuuttuja_01 <- renderUI({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()

        region_data <- get_region_data()
        region_data <- dplyr::filter(region_data, level %in% aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = aluename, 
                                              naapurikoodit = naapurikoodit,
                                              aluetaso1 = aluetaso1)
        
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
        
        output$aikasarja1_01 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[1], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[1]))
        output$aikasarja1_02 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[2], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[2]))
        output$aikasarja1_03 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[3], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[3]))
        output$aikasarja1_04 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[4], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[4]))
        
        output$kartta1_01 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[1], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[1]))
        output$kartta1_02 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[2], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[2]))
        output$kartta1_03 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[3], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[3]))
        output$kartta1_04 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[4], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[4]))
        
        tagList(
            fluidRow(tags$h5(muuttujanimi[1])),
            fluidRow(column(3,lista1_tbl01),column(5,withSpinner(plotOutput("kartta1_01",width = "100%"))),column(4,withSpinner(plotOutput("aikasarja1_01",width = "100%")))),
            fluidRow(tags$h5(muuttujanimi[2])),
            fluidRow(column(3,lista1_tbl02),column(5,plotOutput("kartta1_02",width = "100%")),column(4,plotOutput("aikasarja1_02",width = "100%"))),
            fluidRow(tags$h5(muuttujanimi[3])),
            fluidRow(column(3,lista1_tbl03),column(5,plotOutput("kartta1_03",width = "100%")),column(4,plotOutput("aikasarja1_03",width = "100%"))),
            fluidRow(tags$h5(muuttujanimi[4])),
            fluidRow(column(3,lista1_tbl04),column(5,plotOutput("kartta1_04",width = "100%")),column(4,plotOutput("aikasarja1_04",width = "100%")))
            )
    })
    
    output$inhimillinen_01 <- renderUI({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        
        region_data <- get_region_data()
        region_data <- dplyr::filter(region_data, level %in% aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = aluename, 
                                              naapurikoodit = naapurikoodit,
                                              aluetaso1 = aluetaso1)
        
        muuttujaluokka <- "Inhimillinen huono-osaisuus"
        tabdat_tmp <- tabdat %>% 
            filter(var_class == muuttujaluokka)
        muuttujanimi <- unique(tabdat_tmp$muuttuja)
        
        
        for (ix in seq_along(muuttujanimi)){
            lista_tbl <- create_raw_tbl(dd1 = tabdat_tmp, muuttujanimi= muuttujanimi, varnro = ix)
            tbl_temp1 <- create_gt_tbl(lst_df = lista_tbl)
            assign(x = paste0("lista2_tbl", stringr::str_pad(ix, width = 2, pad = 0)), value = tbl_temp1)
        }
        
        output$aikasarja2_01 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[1], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[1]))
        output$aikasarja2_02 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[2], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[2]))
        output$aikasarja2_03 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[3], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[3]))
        output$aikasarja2_04 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[4], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[4]))
        output$aikasarja2_05 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[5], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[5]))
        output$aikasarja2_06 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[6], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[6]))
        if (aluetaso1 == "Maakunnat"){
            output$aikasarja2_07 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[7], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_aikasarja(varname = muuttujanimi[7]))
            output$aikasarja2_08 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[8], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_aikasarja(varname = muuttujanimi[8]))
            output$aikasarja2_09 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[9], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_aikasarja(varname = muuttujanimi[9]))
            output$aikasarja2_10 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[10], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_aikasarja(varname = muuttujanimi[10]))
            output$aikasarja2_11 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[11], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_aikasarja(varname = muuttujanimi[11]))
        }
        
        output$kartta2_01 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[1], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[1]))
        output$kartta2_02 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[2], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[2]))
        output$kartta2_03 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[3], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[3]))
        output$kartta2_04 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[4], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[4]))
        output$kartta2_05 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[5], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[5]))
        output$kartta2_06 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[6], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[6]))
        if (aluetaso1 == "Maakunnat"){
            output$kartta2_07 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[7], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_kartta(varname = muuttujanimi[7]))
            output$kartta2_08 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[8], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_kartta(varname = muuttujanimi[8]))
            output$kartta2_09 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[9], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_kartta(varname = muuttujanimi[9]))
            output$kartta2_10 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[10], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_kartta(varname = muuttujanimi[10]))
            output$kartta2_11 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[11], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_kartta(varname = muuttujanimi[11]))
        }
        
        if (aluetaso1 == "Maakunnat"){
            tagList(
                fluidRow(tags$h5(muuttujanimi[1])),
                fluidRow(column(3,lista2_tbl01),column(5,plotOutput("kartta2_01",width = "100%")),column(4,plotOutput("aikasarja2_01",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[2])),
                fluidRow(column(3,lista2_tbl02),column(5,plotOutput("kartta2_02",width = "100%")),column(4,plotOutput("aikasarja2_02",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[3])),
                fluidRow(column(3,lista2_tbl03),column(5,plotOutput("kartta2_03",width = "100%")),column(4,plotOutput("aikasarja2_03",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[4])),
                fluidRow(column(3,lista2_tbl04),column(5,plotOutput("kartta2_04",width = "100%")),column(4,plotOutput("aikasarja2_04",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[5])),
                fluidRow(column(3,lista2_tbl05),column(5,plotOutput("kartta2_05",width = "100%")),column(4,plotOutput("aikasarja2_05",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[6])),
                fluidRow(column(3,lista2_tbl06),column(5,plotOutput("kartta2_06",width = "100%")),column(4,plotOutput("aikasarja2_06",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[7])),
                fluidRow(column(3,lista2_tbl07),column(5,plotOutput("kartta2_07",width = "100%")),column(4,plotOutput("aikasarja2_07",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[8])),
                fluidRow(column(3,lista2_tbl08),column(5,plotOutput("kartta2_08",width = "100%")),column(4,plotOutput("aikasarja2_08",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[9])),
                fluidRow(column(3,lista2_tbl09),column(5,plotOutput("kartta2_09",width = "100%")),column(4,plotOutput("aikasarja2_09",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[10])),
                fluidRow(column(3,lista2_tbl10),column(5,plotOutput("kartta2_10",width = "100%")),column(4,plotOutput("aikasarja2_10",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[11])),
                fluidRow(column(3,lista2_tbl11),column(5,plotOutput("kartta2_11",width = "100%")),column(4,plotOutput("aikasarja2_11",width = "100%")))
            )
        } else {
            tagList(
                fluidRow(tags$h5(muuttujanimi[1])),
                fluidRow(column(3,lista2_tbl01),column(5,plotOutput("kartta2_01",width = "100%")),column(4,plotOutput("aikasarja2_01",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[2])),
                fluidRow(column(3,lista2_tbl02),column(5,plotOutput("kartta2_02",width = "100%")),column(4,plotOutput("aikasarja2_02",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[3])),
                fluidRow(column(3,lista2_tbl03),column(5,plotOutput("kartta2_03",width = "100%")),column(4,plotOutput("aikasarja2_03",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[4])),
                fluidRow(column(3,lista2_tbl04),column(5,plotOutput("kartta2_04",width = "100%")),column(4,plotOutput("aikasarja2_04",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[5])),
                fluidRow(column(3,lista2_tbl05),column(5,plotOutput("kartta2_05",width = "100%")),column(4,plotOutput("aikasarja2_05",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[6])),
                fluidRow(column(3,lista2_tbl06),column(5,plotOutput("kartta2_06",width = "100%")),column(4,plotOutput("aikasarja2_06",width = "100%")))
            )
        }
    })
    
    output$sosiaalinen_01 <- renderUI({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        
        region_data <- get_region_data()
        region_data <- dplyr::filter(region_data, level %in% aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = aluename, 
                                              naapurikoodit = naapurikoodit,
                                              aluetaso1 = aluetaso1)
        
        muuttujaluokka <- "Huono-osaisuuden sosiaaliset seuraukset"
        tabdat_tmp <- tabdat %>% 
            filter(var_class == muuttujaluokka)
        muuttujanimi <- unique(tabdat_tmp$muuttuja)

        for (ix in seq_along(muuttujanimi)){
            lista_tbl <- create_raw_tbl(dd1 = tabdat_tmp, muuttujanimi= muuttujanimi, varnro = ix)
            tbl_temp1 <- create_gt_tbl(lst_df = lista_tbl)
            assign(x = paste0("lista3_tbl", stringr::str_pad(ix, width = 2, pad = 0)), value = tbl_temp1)
        }
        
        output$aikasarja3_01 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[1], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[1]))
        output$aikasarja3_02 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[2], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[2]))
        output$aikasarja3_03 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[3], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[3]))
        output$aikasarja3_04 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[4], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[4]))
        if (aluetaso1 == "Maakunnat"){
            output$aikasarja3_05 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[5], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_aikasarja(varname = muuttujanimi[5]))
            output$aikasarja3_06 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[6], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_aikasarja(varname = muuttujanimi[6]))
        }
        
        output$kartta3_01 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[1], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[1]))
        output$kartta3_02 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[2], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[2]))
        output$kartta3_03 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[3], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[3]))
        output$kartta3_04 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[4], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[4]))
        if (aluetaso1 == "Maakunnat"){
            output$kartta3_05 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[5], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_kartta(varname = muuttujanimi[5]))
            output$kartta3_06 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[6], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_kartta(varname = muuttujanimi[6]))
        }
        
        if (aluetaso1 == "Maakunnat"){
            tagList(
                fluidRow(tags$h5(muuttujanimi[1])),
                fluidRow(column(3,lista3_tbl01),column(5,plotOutput("kartta3_01",width = "100%")),column(4,plotOutput("aikasarja3_01",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[2])),
                fluidRow(column(3,lista3_tbl02),column(5,plotOutput("kartta3_02",width = "100%")),column(4,plotOutput("aikasarja3_02",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[3])),
                fluidRow(column(3,lista3_tbl03),column(5,plotOutput("kartta3_03",width = "100%")),column(4,plotOutput("aikasarja3_03",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[4])),
                fluidRow(column(3,lista3_tbl04),column(5,plotOutput("kartta3_04",width = "100%")),column(4,plotOutput("aikasarja3_04",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[5])),
                fluidRow(column(3,lista3_tbl05),column(5,plotOutput("kartta3_05",width = "100%")),column(4,plotOutput("aikasarja3_05",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[6])),
                fluidRow(column(3,lista3_tbl06),column(5,plotOutput("kartta3_06",width = "100%")),column(4,plotOutput("aikasarja3_06",width = "100%")))
            )
        } else {
            tagList(
                fluidRow(tags$h5(muuttujanimi[1])),
                fluidRow(column(3,lista3_tbl01),column(5,plotOutput("kartta3_01",width = "100%")),column(4,plotOutput("aikasarja3_01",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[2])),
                fluidRow(column(3,lista3_tbl02),column(5,plotOutput("kartta3_02",width = "100%")),column(4,plotOutput("aikasarja3_02",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[3])),
                fluidRow(column(3,lista3_tbl03),column(5,plotOutput("kartta3_03",width = "100%")),column(4,plotOutput("aikasarja3_03",width = "100%"))),
                fluidRow(tags$h5(muuttujanimi[4])),
                fluidRow(column(3,lista3_tbl04),column(5,plotOutput("kartta3_04",width = "100%")),column(4,plotOutput("aikasarja3_04",width = "100%")))
            )
        }
        
        
    })
    
    output$taloudellinen_01 <- renderUI({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        
        region_data <- get_region_data()
        region_data <- dplyr::filter(region_data, level %in% aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = aluename, 
                                              naapurikoodit = naapurikoodit,
                                              aluetaso1 = aluetaso1)
        
        muuttujaluokka <- "Huono-osaisuuden taloudelliset yhteydet"
        tabdat_tmp <- tabdat %>% 
            filter(var_class == muuttujaluokka)
        muuttujanimi <- unique(tabdat_tmp$muuttuja)
        
        for (ix in seq_along(muuttujanimi)){
            lista_tbl <- create_raw_tbl(dd1 = tabdat_tmp, muuttujanimi= muuttujanimi, varnro = ix)
            tbl_temp1 <- create_gt_tbl(lst_df = lista_tbl)
            assign(x = paste0("lista4_tbl", stringr::str_pad(ix, width = 2, pad = 0)), value = tbl_temp1)
        }
        
        output$aikasarja4_01 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[1], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[1]))
        output$aikasarja4_02 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[2], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[2]))
        output$aikasarja4_03 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[3], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[3]))
        output$aikasarja4_04 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[4], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[4]))
        output$aikasarja4_05 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[5], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[5]))
        output$aikasarja4_06 <- renderPlot({alueprofiiliaikasarja_html(val_muuttuja = muuttujanimi[6], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_aikasarja(varname = muuttujanimi[6]))
        
        output$kartta4_01 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[1], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[1]))
        output$kartta4_02 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[2], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[2]))
        output$kartta4_03 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[3], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[3]))
        output$kartta4_04 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[4], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[4]))
        output$kartta4_05 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[5], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[5]))
        output$kartta4_06 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[6], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
        }, alt = create_alt_text_kartta(varname = muuttujanimi[6]))
        
        tagList(
            fluidRow(tags$h5(muuttujanimi[1])),
            fluidRow(column(3,lista4_tbl01),column(5,plotOutput("kartta4_01",width = "100%")),column(4,plotOutput("aikasarja4_01",width = "100%"))),
            fluidRow(tags$h5(muuttujanimi[2])),
            fluidRow(column(3,lista4_tbl02),column(5,plotOutput("kartta4_02",width = "100%")),column(4,plotOutput("aikasarja4_02",width = "100%"))),
            fluidRow(tags$h5(muuttujanimi[3])),
            fluidRow(column(3,lista4_tbl03),column(5,plotOutput("kartta4_03",width = "100%")),column(4,plotOutput("aikasarja4_03",width = "100%"))),
            fluidRow(tags$h5(muuttujanimi[4])),
            fluidRow(column(3,lista4_tbl04),column(5,plotOutput("kartta4_04",width = "100%")),column(4,plotOutput("aikasarja4_04",width = "100%"))),
            fluidRow(tags$h5(muuttujanimi[5])),
            fluidRow(column(3,lista4_tbl05),column(5,plotOutput("kartta4_05",width = "100%")),column(4,plotOutput("aikasarja4_05",width = "100%"))),
            fluidRow(tags$h5(muuttujanimi[6])),
            fluidRow(column(3,lista4_tbl06),column(5,plotOutput("kartta4_06",width = "100%")),column(4,plotOutput("aikasarja4_06",width = "100%")))
        )
    })
    
    ### profiili_html ----
    output$region_profile_html <- renderUI({
        
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        
        region_data <- get_region_data()
        region_data <- dplyr::filter(region_data, level %in% aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = aluename, 
                                              naapurikoodit = naapurikoodit,
                                              aluetaso1 = aluetaso1)
        
        tagList(
            fluidRow(column(width = 6,
                            tags$h3(glue("{aluename} ({aluetaso1})")),
                            tags$p("Analyysissä mukana naapurit: ", glue_collapse(unique(tabdat[tabdat$rooli == "naapuri",]$aluenimi), sep = ", ", last = " ja "))
            ),
            column(width = 6,
                   withSpinner(uiOutput("output_save_word"), proxy.height = "100px")
            )
            ),
            tags$hr(),
            ## ## ##
            tags$h4("Summamuuttujat"), 
            ## ## ##
            uiOutput("summamuuttuja_01"),
            ## ## ##
            tags$h4("Inhimillinen huono-osaisuus"),
            ## ## ##
            uiOutput("inhimillinen_01"),
            ## ## ##
            tags$h4("Huono-osaisuuden sosiaaliset seuraukset"),
            ## ## ##
            uiOutput("sosiaalinen_01"),
            ## ## ##
            tags$h4("Huono-osaisuuden taloudelliset yhteydet"),
            ## ## ##
            uiOutput("taloudellinen_01")
        )
        
    })
    
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
                    
                    region_data <- get_region_data()
                    region_data <- dplyr::filter(region_data, level %in% react_value_regio_level_profile())
                    naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
                    
                    # klik <- list("id" = "Veteli")
                    params <- list(region = aluename,
                                   region_level = react_value_regio_level_profile(),
                                   datetime = Sys.time(),
                                   data = get_dat(),
                                   data_aikasarja = get_dat_timeseries(),
                                   spatdat = process_data_profile_doc(),
                                   region_data = region_data,
                                   value_variable = input$value_variable,
                                   naapurikoodit = naapurikoodit#,
                                   # odt_kerroin = 1
                    )
                    
                    tempReport <- file.path(tempdir(), "report.Rmd")
                    # tempReport <- file.path("~/Downloads", "report.Rmd")
                    # dea("./docs/report_template.Rmd", tempReport, overwrite = TRUE)
                    lns <- readLines("./docs/report_template.Rmd") 
                    
                    if (input$value_report_format == ".docx"){
                        # lns2 <- sub("korvaa_asiakirjamalli", "reference_docx: diak_karttasovellus.dotx", lns)
                        # lns3 <- sub("korvaa_dokumenttimuoto", "word_document", lns2)
                        lns3 <- lns
                        file.copy("./docs/diak_karttasovellus.dotx",
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
                        file.copy("./docs/diak_karttasovellus.ott",
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
    ## tallennus ----
    
    output$output_save_word <- renderUI({
        
        req(input$value_variable)
        tagList(
            downloadButton("report", "Tallenna alueprofiili laitteellesi!", class="btn btn-dark"),
            radioButtons("value_report_format",
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
    
    
    
    output$output_save_map <- renderUI({
        
        req(input$value_variable)
        
        tagList(
            # radioButtons("value_map_format", 
            #              "Valitse tiedostomuoto",
            #              choiceNames = list(#"vektorikuva (.pdf)",
            #                                 #"vektorikuva (.svg)",
            #                                 "bittimappikuva (.png)"),
            #              choiceValues = list(#".pdf", 
            #                                  #".svg", 
            #                                  ".png"),
            #              inline = TRUE),
            downloadButton("save_map", "Tallenna kartta bittimappikuvana (.png)")
        )
    })
    
    output$save_map <- downloadHandler(
        
        filename = function() {
            file_name <- glue("map_{janitor::make_clean_names(input$value_variable)}_{tolower(input$value_regio_level)}.png")
            return(file_name)
        },
        content = function(file) {
            
            # shiny::withProgress(
            # message = paste0("Piirretään karttaa"),
            # value = 0,
            # {
            # shiny::incProgress(1/10)
            # Sys.sleep(1)
            dat <- process_data()
            dat <- sf::st_transform(dat, crs = 3067)
            
            ggplot(data = dat, aes(fill = value)) +
                geom_sf(color = alpha("white", 1/3))  +
                scale_fill_viridis(option = "viridis", direction = -1, alpha = .5) +
                theme_minimal(base_family = "PT Sans", base_size = 12) +
                labs(fill = NULL,
                     title = glue("{input$value_variable}"),
                     subtitle = glue("Aluetaso: {input$value_regio_level}"),
                     caption = glue("Data: THL & Diak\n{Sys.Date()}")) -> p
            if (input$value_regio_level != "Kunnat"){
                p + ggrepel::geom_text_repel(data = dat %>%
                                                 sf::st_set_geometry(NULL) %>%
                                                 bind_cols(dat %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                                             aes(label = paste0(aluenimi,"\n", value), x = X, y = Y), 
                                             color = "black", #fill = alpha("black", 2/3), 
                                             family = "PT Sans", size = 2) -> p
            } else {
                p + geom_text(data = dat %>%
                                  sf::st_set_geometry(NULL) %>%
                                  bind_cols(dat %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                              aes(label = value, x = X, y = Y), 
                              color = "black", 
                              family = "PT Sans", 
                              size = 2.2) -> p
            }
            p + theme(axis.text = element_blank(),
                      axis.title = element_blank(),
                      panel.grid = element_blank()) -> p
            # if (input$value_map_format == ".pdf"){
            #     ggsave(filename = file, plot = p, device = cairo_pdf, width = 8.3, height = 11.7)
            # } else if (input$value_map_format == ".svg"){
            #     ggsave(filename = file, plot = p, width = 8.3, height = 11.7)
            # }  else {
            ggsave(filename = file, plot = p, width = 8.3, height = 11.7)
            # }
            # shiny::incProgress(5/10)
            # }
            # )
        }
    )
    
    
    get_variable_description <- reactive({
        dat <- readxl::read_excel("./data/Muuttujakuvaukset_20201102.xlsx") %>% 
            setNames(c("Muuttujaluokka","Muuttuja","Aluetasot","Kuvaus")) %>% 
            mutate(Muuttujaluokka = factor(Muuttujaluokka, levels = c("Summamuuttujat",
                                                                      "Inhimillinen huono-osaisuus",
                                                                      "Huono-osaisuuden taloudelliset seuraukset", 
                                                                      "Huono-osaisuuden sosiaaliset seuraukset"))) %>% 
            arrange(Muuttujaluokka)
        return(dat)
    })
    
    output$variable_desctiption_gt1 <- gt::render_gt({
        iv <- 1    
        dat <- get_variable_description()
        mulu <- unique(dat$Muuttujaluokka)
        dat_tmp <- dat[dat$Muuttujaluokka == mulu[iv],]
        dat_tmp %>% 
            select(-Muuttujaluokka) %>% 
            gt() %>% 
            # gt::tab_header(title = toupper(mulu[iv])) %>%
            tab_options(table.width	= "90%", 
                        table.align = "left",
                        row_group.background.color = alpha("grey", 1/6)) -> tbl1
        return(tbl1)
    })
    
    output$variable_desctiption_gt2 <- gt::render_gt({
        iv <- 2    
        dat <- get_variable_description()
        mulu <- unique(dat$Muuttujaluokka)
        dat_tmp <- dat[dat$Muuttujaluokka == mulu[iv],]
        dat_tmp %>% 
            select(-Muuttujaluokka) %>% 
            gt() %>% 
            # gt::tab_header(title = toupper(mulu[iv])) %>%
            tab_options(table.width	= "90%", 
                        table.align = "left",
                        row_group.background.color = alpha("grey", 1/6)) -> tbl1
        return(tbl1)
    })
    
    output$variable_desctiption_gt3 <- gt::render_gt({
        iv <- 3    
        dat <- get_variable_description()
        mulu <- unique(dat$Muuttujaluokka)
        dat_tmp <- dat[dat$Muuttujaluokka == mulu[iv],]
        dat_tmp %>% 
            select(-Muuttujaluokka) %>% 
            gt() %>% 
            # gt::tab_header(title = toupper(mulu[iv])) %>%
            tab_options(table.width	= "90%", 
                        table.align = "left",
                        row_group.background.color = alpha("grey", 1/6)) -> tbl1
        return(tbl1)
    })
    
    output$variable_desctiption_gt4 <- gt::render_gt({
        iv <- 4    
        dat <- get_variable_description()
        mulu <- unique(dat$Muuttujaluokka)
        dat_tmp <- dat[dat$Muuttujaluokka == mulu[iv],]
        dat_tmp %>% 
            select(-Muuttujaluokka) %>% 
            gt() %>% 
            # gt::tab_header(title = toupper(mulu[iv])) %>%
            tab_options(table.width	= "90%", 
                        table.align = "left",
                        row_group.background.color = alpha("grey", 1/6)) -> tbl1
        return(tbl1)
    })
    
    
}

# shinyApp(ui = ui, server = server)
shinyApp(ui = htmlTemplate("www/index.html"), server = server)
# 
