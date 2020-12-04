source("./global.R")


ui <- fluidPage(
    # use_waitress(),
    uiOutput("output_regio_level_profile"),
    uiOutput("output_region_profile"),
    uiOutput("output_button_profile"),
    textOutput("aputeksti"),
    # uiOutput("output_save_word"),
    shinycssloaders::withSpinner(uiOutput("region_profile_html")),
    tags$hr(),
    # plotOutput("profiilikartta01", width = "90%", height = "500px"),
    uiOutput("output_indicator_class"),
    uiOutput("output_indicator"),
    uiOutput("output_regio_level"),
    uiOutput("output_save_map"),
    leaflet::leafletOutput("map1", width = "100%", height = "285px"),
    # DT::dataTableOutput("rank_tbl"),
    plotOutput("timeseries_plot"),
    plotOutput("rank_plot"),
    # verbatimTextOutput("value"),
    # uiOutput("output_regio_level_profile"),
    # uiOutput("output_region_profile"),
    # uiOutput("region_profile_html"),
    # DT::dataTableOutput("variable_desctiption")
    gt::gt_output("variable_desctiption_gt")
)


# Serverin logiikka ----
server <- function(input, output) {
    
    
    
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
                         label = "Valitse aluetaso", inline = TRUE,
                         choices = opt_indicator, selected = "Maakunnat")
        )
        # }
        
    })
    
    output$output_regio_choice_mode <- renderUI({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        opt_indicator <- c("karttaa klikkaamalla", "kaikki alueet")
        
        tagList(
            checkboxInput(inputId = "value_regio_choice_mode", 
                          label = "Näytä kaikki alueet aikasarjassa", value = FALSE)
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
        
        # if (input$sidebar_menu == "info"){
        #     tagList()
        # } else {
        tagList(
            radioButtons(inputId = "value_regio_level_profile", 
                         label = "Valitse aluetaso", inline = TRUE,
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
        
        klik <- rv$map_click_id
        if (is.null(klik)){
            klik <- list("id" = idnimi)
        }
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
        
        res <- sf::st_transform(x = res, crs = "+proj=longlat +datum=WGS84")
        
        return(res)
    })
    
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
    
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #    ___           _ _ _               _   _             _ _                _       _   
    #   |_ _|_ __   __| (_) | ____ _  __ _| |_| |_ ___  _ __(_) | ___   ___   _(_) ___ | |_ 
    #    | || '_ \ / _` | | |/ / _` |/ _` | __| __/ _ \| '__| | |/ / | | \ \ / / |/ _ \| __|
    #    | || | | | (_| | |   < (_| | (_| | |_| || (_) | |  | |   <| |_| |\ V /| | (_) | |_ 
    #   |___|_| |_|\__,_|_|_|\_\__,_|\__,_|\__|\__\___/|_|  |_|_|\_\\__,_| \_/ |_|\___/ \__|
    #                                                                                       
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    ## indikaattorikuviot ----
    
    output$map1 <- leaflet::renderLeaflet({
        
        fin <- readRDS("./data/regio_Suomi.RDS")
        fin <- sf::st_transform(x = fin, crs = "+proj=longlat +datum=WGS84")
        
        leaflet(fin, options = leafletOptions(attributionControl=FALSE)) %>%
            # addProviderTiles(provider = providers$CartoDB.Positron) %>% 
            addPolygons(color = NA, fill = NA) %>% 
            setView(lng = 26.24578, lat = 65.28952, zoom = 5)
        
    })
    
    observe({
        
        req(input$value_regio_level)
        req(input$value_variable)
        
        dat <- process_data()
        
        pal <- leaflet::colorNumeric(palette = "viridis", domain = dat$value, reverse = TRUE)
        
        labels <- sprintf(
            "%s <br/><i>%s</i>: <strong>%s</strong><br/>Sija: %s/%s",
            dat$aluenimi,dat$variable, dat$value, dat$rank, nrow(dat)
        ) %>% lapply(htmltools::HTML)
        
        proxy <- leafletProxy("map1")
        
        proxy %>% 
            clearShapes() %>% 
            addPolygons(data = dat, 
                        fillColor = ~pal(value),
                        color = "white",
                        weight = 1,
                        opacity = 1,
                        dashArray = "3",
                        fillOpacity = 0.4,
                        highlight = highlightOptions(
                            weight = 2,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.4,
                            bringToFront = TRUE),
                        group = "polygons",
                        label = labels,
                        layerId = dat$aluenimi,
                        labelOptions = labelOptions(opacity = .7,
                                                    style = list("font-weight" = "normal",
                                                                 padding = "2px 4px"),
                                                    textsize = "12px",
                                                    direction = "auto")
            ) -> proxy
        
        # proxy %>% clearControls()   %>%
        #     addLegend(data = dat, pal = pal, values = ~value, opacity = 0.7, title = add_line_break(input$value_variable, n = 20),
        #               position = "bottomright")
        proxy
    })
    
    
    output$rank_plot <- renderPlot({
        
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        
        klik <- get_klik()    
        dat <- process_data() %>% 
            st_set_geometry(NULL) %>% 
            select(rank,aluenimi,value) %>% 
            mutate(aluenimi = factor(aluenimi), 
                   aluenimi = fct_reorder(aluenimi, -rank)) %>% 
            mutate(focus = ifelse(aluenimi == klik$id, TRUE, FALSE))
        
        ggplot(dat, aes(x = value, y = aluenimi, 
                        color = focus,
                        fill = value)) + 
            geom_col() +
            theme_ipsum(base_family = "PT Sans",
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans",
                        grid_col = "white") +
            xlim(c(0,max(dat$value, na.rm = TRUE)*1.2)) +
            labs(y = NULL, x = NULL, 
                 title = input$value_variable, 
                 subtitle = input$value_variable_class) +
            theme(legend.position = "none") +
            # scale_fill_ipsum() +
            scale_fill_viridis_c(option = "viridis", direction = -1, alpha = .4) +
            scale_color_manual(values = c("white","#7e3f9d")) -> plot
        
        if (input$value_regio_level == "Seutukunnat"){
            plot <- plot +
                theme(axis.text.y = element_text(size = 9)) +
                geom_text(dat = dat[dat$focus,], 
                          aes(label = paste0(value, " ", rank, "/", max(dat$rank, na.rm = TRUE))), 
                          color = "black", 
                          nudge_x = max(dat$value, na.rm = TRUE)*0.1, 
                          family = "PT Sans")
        } else if (input$value_regio_level == "Kunnat"){
            plot <- plot +
                theme(axis.text.y = element_blank()) +
                geom_text(dat = dat[dat$focus,],
                          aes(label = paste0(aluenimi, " ",value, " ", rank, "/", max(dat$rank, na.rm = TRUE))), 
                          color = "black", 
                          nudge_x = max(dat$value, na.rm = TRUE)*0.1, 
                          family = "PT Sans")
        } else if (input$value_regio_level == "Maakunnat"){
            plot <- plot + geom_text(aes(label = value), 
                                     color = "black", 
                                     nudge_x = max(dat$value, na.rm = TRUE)*0.1, 
                                     family = "PT Sans")
        }
        plot + scale_y_discrete(expand = expansion(add = 2))
        
        # return(dt)
    }, alt = reactive({
        paste("Palkkikuvio tasolla", 
              input$value_regio_level,
              "jossa pystyakselilla aluenimet ja vaaka-akselilla muuttujan",
              input$value_variable, "arvot. Alue", get_klik()$id, "korostettuna."
        )
    }))
    
    
    output$timeseries_plot <- renderPlot({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        # req(input$value_regio_choice_mode)
        
        
        klik <- get_klik()    
        dat <- get_dat_timeseries()
        region_data <- get_region_data()
        naapurikoodit <- region_data[region_data$level %in% input$value_regio_level & 
                                         region_data$region_name %in% klik$id,]$neigbours[[1]]
        
        df <- dat[dat$variable == input$value_variable &
                      dat$regio_level == input$value_regio_level &
                      dat$aluekoodi %in% naapurikoodit,]
        
        
        aika1 <- sort(unique(df$aika)) - 1
        aika2 <- sort(unique(df$aika)) + 1
        labels <- paste0(aika1,"-",aika2)
        
        ggplot(data = df,
               aes(x = aika, y = value, color= aluenimi, fill= aluenimi)) -> plot0
        if (input$value_regio_choice_mode){
            
            
            alfa = ifelse(input$value_regio_level == "Kunnat", .1, .2)
            
            plot0 <- plot0 + geom_line(data = dat[dat$variable == input$value_variable &
                                                      dat$regio_level == input$value_regio_level,], color = "dim grey", alpha = alfa)
            
            
            
        }
        
        plot0 +
            geom_line(show.legend = FALSE) +
            geom_point(shape = 21, color = "white", stroke = 1, size = 2.5) +
            ggrepel::geom_text_repel(data = df %>% filter(aika == max(aika, na.rm = TRUE)),
                                     aes(label = round(value,1)), family = "PT Sans") +
            ggrepel::geom_text_repel(data = df %>% filter(aika == max(aika, na.rm = TRUE)-1,
                                                          aluenimi != klik$id),
                                     aes(label = aluenimi), family = "PT Sans") +
            # valittu
            ggrepel::geom_label_repel(data = df %>% filter(aika == max(aika, na.rm = TRUE)-1,
                                                           aluenimi == klik$id),
                                      aes(label = aluenimi), color = "white", family = "PT Sans") +
            
            scale_x_continuous(breaks = sort(unique(df$aika)), labels = labels) +
            theme_ipsum(base_family = "PT Sans",
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans",
                        grid_col = "white") +
            theme(legend.position = "none") +
            scale_fill_viridis_d(option = "plasma", direction = -1, begin = .1, end = .9) +
            scale_color_viridis_d(option = "plasma", direction = -1, begin = .1, end = .9) +
            labs(y = NULL, x = NULL,
                 title = input$value_variable,
                 subtitle = input$value_variable_class) -> plot1
        plot1
        
    }, alt = reactive({
        
        # alt-tekstiin
        klik <- get_klik()  
        region_data <- get_region_data()
        naapurikoodit <- region_data[region_data$level %in% input$value_regio_level & 
                                         region_data$region_name %in% klik$id,]$neigbours[[1]]
        region_nms <- region_data[region_data$level %in% input$value_regio_level & region_data$region_code %in% naapurikoodit, ]$region_name
        
        
        paste("Aikasarjakuvio tasolla", 
              input$value_regio_level, 
              ", jossa alueina näytetään ", 
              glue_collapse(region_nms, sep = ", ", last = " ja "),
              ". Vaaka-akselilla esitetään vuodet ja pystyakselilla muuttujan",
              input$value_variable, "arvot."
        )
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
            # mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>%
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija)
        mapdata <- left_join(region_data, lista1_tbl, by = c("region_name" = "aluenimi")) %>% 
            filter(!is.na(muuttuja))
        plotlista <- list()
        vars <- unique(mapdata$muuttuja)
        for (vii in seq_along(vars)){
            mapdata_tmp <- mapdata[mapdata$muuttuja == vars[vii],]
            plotlista[[vii]] <- ggplot(data = mapdata_tmp, aes(fill = arvo)) +                    
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
                      legend.position = "top", 
                      legend.key.width = unit(3,"line")) +
                labs(x = NULL, 
                     y = NULL, 
                     fill = NULL,
                     title = vars[vii]
                ) +
                geom_sf_label(aes(label = paste0(region_name, "\n", round(arvo,1))), 
                              color = "white", fill = alpha("black", 1/3), family = "PT Sans", size = 3)
        }
        wrap_plots(plotlista, ncol = 1)
    }
    
    
    alueprofiiliaikasarja_html <- function(val_aluetaso1 = aluetaso1, 
                                           val_aluename = aluename, 
                                           val_region_data = region_data, 
                                           val_muuttujaluokka = muuttujaluokka,
                                           val_muuttuja = muuttuja){
        
        region_data <- dplyr::filter(val_region_data, level %in% val_aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% val_aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = val_aluename, 
                                              aluetaso1 = val_aluetaso1, 
                                              naapurikoodit = naapurikoodit, 
                                              aikasarja = TRUE)
        
        lista1_tbl <- tabdat %>%
            filter(var_class == val_muuttujaluokka,
                   muuttuja == val_muuttuja) %>%
            filter(rooli %in% c("naapuri","valinta")) %>% 
            select(aika,muuttuja,aluenimi,arvo,sija) %>%
            mutate(value = arvo) %>% 
            arrange(muuttuja,sija)
        
        plotlista <- list()
        vars <- unique(lista1_tbl$muuttuja)
        for (vii in seq_along(vars)){
            plotdata_tmp <- lista1_tbl[lista1_tbl$muuttuja == vars[vii],]
            
            aika1 <- sort(unique(plotdata_tmp$aika)) - 1
            aika2 <- sort(unique(plotdata_tmp$aika)) + 1
            labels <- paste0(aika1,"-\n",aika2)
            
            plotlista[[vii]] <- ggplot(data = plotdata_tmp,
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
                      axis.text.x = element_text(size = 9)) +
                scale_fill_viridis_d(option = "viridis", direction = -1, begin = .1, end = .9) +
                scale_color_viridis_d(option = "viridis", direction = -1, begin = .1, end = .9) +
                labs(y = NULL, x = NULL,
                     title = vars[vii])
        }
        wrap_plots(plotlista, ncol = 1)
    }
    
    ### alueprofiilin_kartat ----
    output$profiilikartta01_01 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Summamuuttujat"
        muuttuja <- "Huono-osaisuus yhteensä"
        alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka,
                                val_muuttuja = muuttuja)
        
    }, alt = "Karttakuva jossa huono-osaisuuden summamuuttujat")
    
    
    output$profiilikartta02_01 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Inhimillinen huono-osaisuus"
        muuttuja <- "Pitkäaikaistyöttömyys"
        alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka, 
                                val_muuttuja = muuttuja)
        
    }, alt = "Karttakuva jossa ihnimillisen huono-osaisuuden osoittimet")
    
    output$profiilikartta03_01 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Huono-osaisuuden sosiaaliset seuraukset"
        muuttuja <- "Kodin ulkopuolelle sijoitetut 0 – 17-vuotiaat"
        alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka, 
                                val_muuttuja = muuttuja)
        
    }, alt = "Karttakuva jossa huono-osaisuuden sosiaalistenseurausten osoittimet")
    
    output$profiilikartta04_01 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Huono-osaisuuden taloudelliset yhteydet"
        muuttuja <- "Päihdehuollon avopalveluissa asiakkaita"
        alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka, 
                                val_muuttuja = muuttuja)
        
    }, alt = "Karttakuva jossa huono-osaisuuden taloudellisten yhtieyksien osoittimet")
    
    
    # 
    ### alueprofiilin_aikasarjat ----
    output$profiiliaikasarja01_01 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Summamuuttujat"
        muuttuja <- "Huono-osaisuus yhteensä"
        alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                   val_aluename = aluename, 
                                   val_region_data = region_data, 
                                   val_muuttujaluokka = muuttujaluokka, 
                                   val_muuttuja = muuttuja)
        
    }, alt = "Aikasarjakuva jossa huono-osaisuuden summamuuttujat")
    
    
    output$profiiliaikasarja02_01 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Inhimillinen huono-osaisuus"
        muuttuja <- "Pitkäaikaistyöttömyys"
        alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                   val_aluename = aluename, 
                                   val_region_data = region_data, 
                                   val_muuttujaluokka = muuttujaluokka, 
                                   val_muuttuja = muuttuja)
        
    }, alt = "Aikasarjakuva jossa ihnimillisen huono-osaisuuden osoittimet")
    
    output$profiiliaikasarja03_01 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Huono-osaisuuden sosiaaliset seuraukset"
        muuttuja <- "Kodin ulkopuolelle sijoitetut 0 – 17-vuotiaat"
        alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                   val_aluename = aluename, 
                                   val_region_data = region_data, 
                                   val_muuttujaluokka = muuttujaluokka, 
                                   val_muuttuja = muuttuja)
        
    }, alt = "Aikasarjakuva jossa huono-osaisuuden sosiaalistenseurausten osoittimet")
    
    output$profiiliaikasarja04_01<- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Huono-osaisuuden taloudelliset yhteydet"
        muuttuja <- "Päihdehuollon avopalveluissa asiakkaita"
        alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                   val_aluename = aluename, 
                                   val_region_data = region_data, 
                                   val_muuttujaluokka = muuttujaluokka, 
                                   val_muuttuja = muuttuja)
        
    }, alt = "Aikasarjakuva jossa huono-osaisuuden taloudellisten yhtieyksien osoittimet")
    
    
    
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
        
        # 1
        varnro <- 1
        lista1_df <- tabdat_tmp %>% 
            mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija) %>% 
            mutate(sija = paste0(sija,".")) %>%
            filter(muuttuja == muuttujanimi[varnro]) %>% 
            select(-muuttuja)
        
        lista1_tbl01 <- gt(data = lista1_df,
                          rowname_col = "aluenimi"
        ) %>% 
            tab_options(table.width	= "90%", 
                        table.align = "left",
                        table.font.size = "80%",
                        row_group.background.color = alpha("grey", 1/6)) %>% 
            cols_align(
                align = "right",
                columns = vars(sija)
            )
        
        output$aikasarja1_01 <- renderPlot({
            
            alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                       val_aluename = aluename, 
                                       val_region_data = region_data, 
                                       val_muuttujaluokka = muuttujaluokka, 
                                       val_muuttuja = muuttujanimi[1])
            
        }, alt = "Aikasarjakuva jossa huono-osaisuuden summamuuttujat")
        
        
        output$kartta1_01 <- renderPlot({
            
            alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                    val_aluename = aluename, 
                                    val_region_data = region_data, 
                                    val_muuttujaluokka = muuttujaluokka,
                                    val_muuttuja = muuttujanimi[1])
            
        }, alt = "Karttakuva jossa huono-osaisuuden summamuuttujat")
        
        # 3
        varnro <- 3
        lista1_df <- tabdat_tmp %>% 
            mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija) %>% 
            mutate(sija = paste0(sija,".")) %>%
            filter(muuttuja == muuttujanimi[varnro]) %>% 
            select(-muuttuja)
        
        lista1_tbl02 <- gt(data = lista1_df,
                          rowname_col = "aluenimi"
        ) %>% 
            tab_options(table.width	= "90%", 
                        table.align = "left",
                        table.font.size = "80%",
                        row_group.background.color = alpha("grey", 1/6)) %>% 
            cols_align(
                align = "right",
                columns = vars(sija)
            )
        
        output$aikasarja1_02 <- renderPlot({
            
            alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                       val_aluename = aluename, 
                                       val_region_data = region_data, 
                                       val_muuttujaluokka = muuttujaluokka, 
                                       val_muuttuja = muuttujanimi[varnro])
            
        }, alt = "Aikasarjakuva jossa huono-osaisuuden summamuuttujat")
        
        
        output$kartta1_02 <- renderPlot({
            
            alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                    val_aluename = aluename, 
                                    val_region_data = region_data, 
                                    val_muuttujaluokka = muuttujaluokka,
                                    val_muuttuja = muuttujanimi[varnro])
            
        }, alt = "Karttakuva jossa huono-osaisuuden summamuuttujat")

        
        tagList(
            fluidRow(
                column(3,
                       tags$h5(muuttujanimi[1]),
                       lista1_tbl01
                ),
                column(5, 
                       plotOutput("kartta1_01", 
                                  width = "100%")
                ),
                column(4, 
                       plotOutput("aikasarja1_01", 
                                  width = "100%")
                ),
            ),
            # 2
            fluidRow(
                column(3,
                       tags$h5(muuttujanimi[3]),
                       lista1_tbl02
                ),
                column(5, 
                       plotOutput("kartta1_02", 
                                  width = "100%")
                ),
                column(4, 
                       plotOutput("aikasarja1_02", 
                                  width = "100%")
                ),
            )
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
        
        # Summamuuttujat
        muuttujaluokka <- "Inhimillinen huono-osaisuus"
        muuttujanimi <- "Pitkäaikaistyöttömyys"
        lista1_df <- tabdat %>% 
            filter(var_class == muuttujaluokka) %>%
            mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija) %>% 
            mutate(sija = paste0(sija,".")) %>%
            filter(muuttuja == muuttujanimi) %>% 
            select(-muuttuja)
        
        lista2_tbl01 <- gt(data = lista1_df,
                          rowname_col = "aluenimi"
        ) %>% 
            tab_options(table.width	= "90%", 
                        table.align = "left",
                        table.font.size = "80%",
                        row_group.background.color = alpha("grey", 1/6)) %>% 
            cols_align(
                align = "right",
                columns = vars(sija)
            )
        
        output$aikasarja2_01 <- renderPlot({
            
            alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                       val_aluename = aluename, 
                                       val_region_data = region_data, 
                                       val_muuttujaluokka = muuttujaluokka, 
                                       val_muuttuja = muuttujanimi)
            
        }, alt = "Aikasarjakuva jossa Inhimillinen huono-osaisuus")
        
        
        output$kartta2_01 <- renderPlot({
            
            alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                    val_aluename = aluename, 
                                    val_region_data = region_data, 
                                    val_muuttujaluokka = muuttujaluokka,
                                    val_muuttuja = muuttujanimi)
            
        }, alt = "Karttakuva jossa Inhimillinen huono-osaisuus")
        
        
        tagList(
            fluidRow(
                column(3,
                       tags$h5(muuttujanimi),
                       lista2_tbl01
                ),
                column(5, 
                       plotOutput("kartta2_01", 
                                  width = "100%")
                ),
                column(4, 
                       plotOutput("aikasarja2_01", 
                                  width = "100%")
                ),
            ),
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
        
        # Summamuuttujat
        muuttujaluokka <- "Summamuuttujat"
        lista1_df <- tabdat %>% 
            filter(var_class == muuttujaluokka) %>%
            mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija) %>% 
            mutate(sija = paste0(sija,".")) %>%
            filter(muuttuja == "Huono-osaisuus yhteensä") %>% 
            select(-muuttuja)
            
        lista1_tbl1 <- gt(data = lista1_df,
                         rowname_col = "aluenimi"#,
                         # groupname_col = "muuttuja"
        ) %>% 
            # gt::tab_header(title = toupper(muuttujaluokka)) %>%
            tab_options(table.width	= "90%", 
                        table.align = "left",
                        table.font.size = "80%",
                        row_group.background.color = alpha("grey", 1/6)) %>% 
            cols_align(
                align = "right",
                columns = vars(sija)
            )
        
        # "Inhimillinen huono-osaisuus"
        muuttujaluokka <- "Inhimillinen huono-osaisuus"
        lista2_df <- tabdat %>%
            filter(var_class == muuttujaluokka) %>%
            mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>%
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija) %>%
            mutate(sija = paste0(sija,".")) %>%
            filter(muuttuja == "Pitkäaikaistyöttömyys") %>% 
            select(-muuttuja)
        
        lista2_tbl1 <- gt(data = lista2_df,
                         rowname_col = "aluenimi"#,
                         # groupname_col = "muuttuja"
        ) %>%
            # gt::tab_header(title = toupper(muuttujaluokka)) %>%
            tab_options(table.width	= "90%",
                        table.align = "left",
                        table.font.size = "80%",
                        row_group.background.color = alpha("grey", 1/6)) %>%
            cols_align(
                align = "right",
                columns = vars(sija)
            )

        # "Huono-osaisuuden sosiaaliset seuraukset"
        muuttujaluokka <- "Huono-osaisuuden sosiaaliset seuraukset"
        lista3_df <- tabdat %>%
            filter(var_class == muuttujaluokka) %>%
            mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>%
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija) %>%
            mutate(sija = paste0(sija,".")) %>%
            filter(muuttuja == "Kodin ulkopuolelle sijoitetut 0 – 17-vuotiaat") %>% 
            select(-muuttuja)

        lista3_tbl1 <- gt(data = lista3_df,
                         rowname_col = "aluenimi"#,
                         # groupname_col = "muuttuja"
        ) %>%
            # gt::tab_header(title = toupper(muuttujaluokka)) %>%
            tab_options(table.width	= "90%",
                        table.align = "left",
                        table.font.size = "80%",
                        row_group.background.color = alpha("grey", 1/6)) %>%
            cols_align(
                align = "right",
                columns = vars(sija)
            )

        # "Huono-osaisuuden taloudelliset yhteydet"
        muuttujaluokka <- "Huono-osaisuuden taloudelliset yhteydet"
        lista4_df <- tabdat %>%
            filter(var_class == muuttujaluokka) %>%
            mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>%
            select(muuttuja,aluenimi,arvo,sija) %>%
            arrange(muuttuja,sija) %>%
            mutate(sija = paste0(sija,".")) %>%
            filter(muuttuja == "Päihdehuollon avopalveluissa asiakkaita") %>% 
            select(-muuttuja)
        
        lista4_tbl1 <- gt(data = lista4_df,
                         rowname_col = "aluenimi"#,
                         # groupname_col = "muuttuja"
        ) %>%
            # gt::tab_header(title = toupper(muuttujaluokka)) %>%
            tab_options(table.width	= "90%",
                        table.align = "left",
                        table.font.size = "80%",
                        row_group.background.color = alpha("grey", 1/6)) %>%
            cols_align(
                align = "right",
                columns = vars(sija)
            )
        
        rivikorkeus <- 46
        kartan_oletuskorkeus <- 350
        aikasarjan_oletuskorkeus <- 350
        
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
            # esimerkki uudesta, yli yhtä monta riviä kuin muuttujaa!
            
            # -----------------------------------------
            tags$h4("Summamuuttujat"), 
            # -----------------------------------------
            # fluidRow(
            #     column(4,
            #            tags$h5("Osoittimen_nimi"),
            #            lista1_tbl1
            #            ),
            #     column(4, 
            #            plotOutput("profiilikartta01_01", 
            #                       width = "100%")
            #     ),
            #     column(4, 
            #            plotOutput("profiiliaikasarja01_01", 
            #                       width = "100%")
            #     ),
            # ),
            uiOutput("summamuuttuja_01"),
            # -----------------------------------------
            tags$h4("Inhimillinen huono-osaisuus"),
            # -----------------------------------------
            uiOutput("inhimillinen_01")
            
            # # -----------------------------------------
            # tags$h4("Inhimillinen huono-osaisuus"), 
            # # -----------------------------------------
            # fluidRow(
            #     column(4,
            #            tags$h5("Osoittimen_nimi"),
            #            lista2_tbl1
            #     ),
            #     column(4, 
            #            plotOutput("profiilikartta02_01", 
            #                       width = "100%")
            #     ),
            #     column(4, 
            #            plotOutput("profiiliaikasarja02_01", 
            #                       width = "100%")
            #     ),
            # ),
            # 
            # # -----------------------------------------
            # tags$h4("Huono-osaisuuden sosiaaliset seuraukset"), 
            # # -----------------------------------------
            # fluidRow(
            #     column(4,
            #            tags$h5("Osoittimen_nimi"),
            #            lista3_tbl1
            #     ),
            #     column(4, 
            #            plotOutput("profiilikartta03_01", 
            #                       width = "100%")
            #     ),
            #     column(4, 
            #            plotOutput("profiiliaikasarja03_01", 
            #                       width = "100%")
            #     ),
            # ),
            # 
            # # -----------------------------------------
            # tags$h4("Huono-osaisuuden taloudelliset yhteydet"), 
            # -----------------------------------------
            # fluidRow(
            #     column(4,
            #            tags$h5("Osoittimen_nimi"),
            #            lista4_tbl1
            #     ),
            #     column(4, 
            #            plotOutput("profiilikartta04_01", 
            #                       width = "100%")
            #     ),
            #     column(4, 
            #            plotOutput("profiiliaikasarja04_01", 
            #                       width = "100%")
            #     ),
            # ),

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
