source("./global.R")


ui <- fluidPage(
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
    leaflet::leafletOutput("map1", width = "100%", height = "685px"),
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


# Define server logic for random distribution app ----
server <- function(input, output) {

    get_dat <- reactive({
        dat <- readRDS("./data/df_v20201102.RDS")
        return(dat)
    })
    
    get_dat_timeseries <- reactive({
        dat <- readRDS("./data/df_v20201121_aikasarja.RDS")
        return(dat)
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
    
    
    # ----------------------------------------------------------------------
    # MAP MAAKUNNAT
    # ----------------------------------------------------------------------
    
    
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
    
    
#     output$rank_tbl <- DT::renderDataTable({
#         
#         req(input$value_variable_class)
#         req(input$value_variable)
#         req(input$value_regio_level)
#         
#         dat <- process_data() %>% 
#             st_set_geometry(NULL) %>% 
#             select(rank,aluenimi,value) 
#         names(dat) <- c("Sijoitus",input$value_regio_level,input$value_variable)
#         
#         # klik <- rv$map_click_id
#         # if (is.null(klik)){
#         #     rownro <- NA
#         # } else {
#         #     rownro <- match(klik$id,dat[[input$value_regio_level]])
#         # }
#         
#         klik <- get_klik()
#         rownro <- match(klik$id,dat[[input$value_regio_level]])
#         
#         dt <- DT::datatable(dat, 
#                         rownames = FALSE, 
#                         # style = "bootstrap4",
#                         # extensions = 'Scroller',
#                         selection = list(mode = 'single', 
#                                          selected = rownro),
#                         # filter = "top", extensions = 'Buttons',
#                         options = list(scrollX = TRUE,
#                                        scrollY = 720,
#                                        paging = FALSE,
#                                        dom = "dt",
#                                        # pageLength = 500,
#                                        language = list(url = 'datatable_translate.json')#,
#                                   #      initComplete  = JS('function() {
#                                   #  $(this.api().table().row(rownro).node()).addClass("selected");
#                                   #  this.api().table().row(rownro).node().scrollIntoView();
#                                   # }')
#                                   )) %>%
#             formatStyle(
#                 input$value_variable,
#                 background = styleColorBar(dat[[input$value_variable]],
#                                            color = '#c799ff',
#                                            angle = -90),
#                 backgroundSize = '100% 90%',
#                 backgroundRepeat = 'no-repeat',
#                 backgroundPosition = 'center'
#             )
#         # return(dt)
#     },
# server = TRUE)
    
    
    output$rank_plot <- renderPlot({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level)
        
        # theme_set(
        #     theme_minimal(base_family = "PT Sans", base_size = 7) +
        #         theme(axis.text.y = element_blank(), 
        #               panel.grid = element_blank(),
        #               # strip.background = element_rect(color = "white"),
        #               # plot.background = element_rect(fill = alpha(colour = "#fffff0", 1/3), color = NA),
        #               plot.title = element_text(size = 9, family = "bold"),
        #               plot.subtitle = element_text(size = 12, family = "bold"))
        # )
        
        klik <- get_klik()    
        dat <- process_data() %>% 
            st_set_geometry(NULL) %>% 
            select(rank,aluenimi,value) %>% 
            mutate(aluenimi = factor(aluenimi), 
                   aluenimi = fct_reorder(aluenimi, -rank)) %>% 
            mutate(focus = ifelse(aluenimi == klik$id, TRUE, FALSE))
        # names(dat) <- c("Sijoitus",input$value_regio_level,input$value_variable)
    
        # rownro <- match(klik$id,dat[[input$value_regio_level]])
        
        
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
        plot
        

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
            
            
            alfa = ifelse(input$value_regio_level == "Kunnat", .1, .4)
            
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
            scale_fill_viridis_d(option = "viridis", direction = -1, begin = .1, end = .9) +
            scale_color_viridis_d(option = "viridis", direction = -1, begin = .1, end = .9) +
            labs(y = NULL, x = NULL,
                 title = input$value_variable,
                 subtitle = input$value_variable_class) -> plot1
        plot1
        
    }, alt = reactive({
        paste("Palkkikuvio tasolla", 
              input$value_regio_level,
              "jossa pystyakselilla aluenimet ja vaaka-akselilla muuttujan",
              input$value_variable, "arvot. Alue", get_klik()$id, "korostettuna."
        )
    }))
    
    
    output$aputeksti <- renderText({
        
        txt <- react_value_region_profile()
        return(txt)
    })

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
                                        val_muuttujaluokka = muuttujaluokka){
        
        region_data <- dplyr::filter(val_region_data, level %in% val_aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% val_aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = val_aluename, 
                                              aluetaso1 = val_aluetaso1, 
                                              naapurikoodit = naapurikoodit)
    
        lista1_tbl <- tabdat %>%
            filter(var_class == val_muuttujaluokka) %>%
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
                                        base_size = 12, 
                                        plot_title_size = 14) +
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
                     title = vars[vii] #glue("Huono-osaisuuden summamuuttujat alueella {val_aluename} sekä \n{glue_collapse(unique(mapdata$region_name)[unique(mapdata$region_name) != val_aluename], sep = ', ', last  = ' ja ')}")
                     # title = add_line_break2(vars[i], n = 35)
                ) +
                geom_sf_label(aes(label = paste0(region_name, "\n", round(arvo,1))), 
                              color = "white", fill = alpha("black", 1/3), family = "PT Sans", size = 3)
                # ggrepel::geom_label_repel(data = mapdata_tmp %>%
                #                               sf::st_set_geometry(NULL) %>%
                #                               bind_cols(mapdata_tmp %>% 
                #                                             sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                #                           aes(label = paste0(region_name, "\n", round(arvo,1)), x = X, y = Y), 
                #                           color = "white", fill = alpha("black", 1/3), family = "PT Sans", size = 3)
        }
        wrap_plots(plotlista, ncol = 1)
    }
    
    
    alueprofiiliaikasarja_html <- function(val_aluetaso1 = aluetaso1, 
                                        val_aluename = aluename, 
                                        val_region_data = region_data, 
                                        val_muuttujaluokka = muuttujaluokka){
        
        region_data <- dplyr::filter(val_region_data, level %in% val_aluetaso1)
        naapurikoodit <- region_data[region_data$region_name %in% val_aluename,]$neigbours[[1]]
        
        tabdat <- create_alueprofiili_content(aluename2 = val_aluename, 
                                              aluetaso1 = val_aluetaso1, 
                                              naapurikoodit = naapurikoodit, 
                                              aikasarja = TRUE)
        
        lista1_tbl <- tabdat %>%
            filter(var_class == val_muuttujaluokka) %>%
            filter(rooli %in% c("naapuri","valinta")) %>% 
            # mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>%
            select(aika,muuttuja,aluenimi,arvo,sija) %>%
            mutate(value = arvo) %>% 
            arrange(muuttuja,sija)
        # mapdata <- left_join(region_data, lista1_tbl, by = c("region_name" = "aluenimi")) %>% 
        #     filter(!is.na(muuttuja))
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
            # valittu
            # ggrepel::geom_label_repel(data = plotdata_tmp %>% filter(aika == max(aika, na.rm = TRUE)-1,
            #                                               aluenimi == klik$id),
            #                          aes(label = aluenimi), color = "white", family = "PT Sans") +
            
            scale_x_continuous(breaks = sort(unique(plotdata_tmp$aika)), labels = labels) +
            theme_ipsum(base_family = "PT Sans", 
                        base_size = 12, 
                        plot_title_size = 14,
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
    
    # alueprofiilin kartat
    
    output$profiilikartta01 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Summamuuttujat"
        alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka)

    }, alt = "Karttakuva jossa huono-osaisuuden summamuuttujat")
    
    
    output$profiilikartta02 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Inhimillinen huono-osaisuus"
        alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka)
        
    }, alt = "Karttakuva jossa ihnimillisen huono-osaisuuden osoittimet")

    output$profiilikartta03 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Huono-osaisuuden sosiaaliset seuraukset"
        alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka)
        
    }, alt = "Karttakuva jossa huono-osaisuuden sosiaalistenseurausten osoittimet")
    
    output$profiilikartta04 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Huono-osaisuuden taloudelliset yhteydet"
        alueprofiilikartta_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka)
        
    }, alt = "Karttakuva jossa huono-osaisuuden taloudellisten yhtieyksien osoittimet")
    

    # alueprofiilin aikasarjat
    
        
    output$profiiliaikasarja01 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Summamuuttujat"
        alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka)

    }, alt = "Aikasarjakuva jossa huono-osaisuuden summamuuttujat")
    
    
    output$profiiliaikasarja02 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Inhimillinen huono-osaisuus"
        alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka)
        
    }, alt = "Aikasarjakuva jossa ihnimillisen huono-osaisuuden osoittimet")

    output$profiiliaikasarja03 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Huono-osaisuuden sosiaaliset seuraukset"
        alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka)
        
    }, alt = "Aikasarjakuva jossa huono-osaisuuden sosiaalistenseurausten osoittimet")
    
    output$profiiliaikasarja04 <- renderPlot({
        
        aluename <- react_value_region_profile()
        aluetaso1 <- react_value_regio_level_profile()
        region_data <- get_region_data()
        muuttujaluokka <- "Huono-osaisuuden taloudelliset yhteydet"
        alueprofiiliaikasarja_html(val_aluetaso1 = aluetaso1, 
                                val_aluename = aluename, 
                                val_region_data = region_data, 
                                val_muuttujaluokka = muuttujaluokka)
        
    }, alt = "Aikasarjakuva jossa huono-osaisuuden taloudellisten yhtieyksien osoittimet")
        
        
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
        mutate(sija = paste0(sija,"."))
    lista1_tbl <- gt(data = lista1_df,
            rowname_col = "aluenimi",
            groupname_col = "muuttuja"
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
        mutate(sija = paste0(sija,"."))
    lista2_tbl <- gt(data = lista2_df,
            rowname_col = "aluenimi",
            groupname_col = "muuttuja"
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
        mutate(sija = paste0(sija,".")) 
    
    lista3_tbl <- gt(data = lista3_df,
            rowname_col = "aluenimi",
            groupname_col = "muuttuja"
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
        mutate(sija = paste0(sija,".")) 
    lista4_tbl <- gt(data = lista4_df,
            rowname_col = "aluenimi",
            groupname_col = "muuttuja"
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
    


    tagList(
        fluidRow(column(width = 6, 
                        tags$h3(glue("{aluename} ({aluetaso1})")),
                        tags$p("Analyysissä mukana naapurit: ", glue_collapse(unique(tabdat[tabdat$rooli == "naapuri",]$aluenimi), sep = ", ", last = " ja "))
                        ),
                 column(width = 6,
                        uiOutput("output_save_word")
                 )
        ),
        tags$hr(),
        fluidRow(
            column(3,
                   tags$h4("Summamuuttujat"),  
                   lista1_tbl
                   ),
            column(5,
                   tags$div(style = "padding-top:60px;"),
                   # withSpinner(
                       plotOutput("profiilikartta01", width = "100%", 
                          height = glue("{(nrow(lista1_df)+1)*rivikorkeus}px")
                          ), proxy.height = "100px"),
            column(4,
                   tags$div(style = "padding-top:60px;"),
                   # withSpinner(
                       plotOutput("profiiliaikasarja01", width = "100%", 
                          height = glue("{(nrow(lista1_df)+1)*rivikorkeus}px")
                          ), proxy.height = "100px")
            ),
        tags$hr(),
        fluidRow(
            column(3,
                   tags$h4("Inhimillinen huono-osaisuus"),
                   lista2_tbl
                  ),
            column(5, 
                   tags$div(style = "padding-top:60px;"),
                   plotOutput("profiilikartta02", width = "100%", 
                              # height = glue("{korkeus2}px")
                              height = glue("{(nrow(lista2_df)+1)*rivikorkeus}px")
                              )
                  ),
            column(4,
                   tags$div(style = "padding-top:60px;"),
                   # withSpinner(
                   plotOutput("profiiliaikasarja02", width = "100%", 
                              height = glue("{(nrow(lista2_df)+1)*rivikorkeus}px")
                   ), proxy.height = "100px")
        ),
        tags$hr(),
        fluidRow(
            column(3,
                   tags$h4("Huono-osaisuuden sosiaaliset seuraukset"),
                   lista3_tbl
            ),
            column(5,
                   tags$div(style = "padding-top:60px;"),
                   plotOutput("profiilikartta03", width = "100%", 
                              # height = glue("{korkeus3}px")
                              height = glue("{(nrow(lista3_df)+1)*rivikorkeus}px")
                   )
            ),
            column(4,
                   tags$div(style = "padding-top:60px;"),
                   # withSpinner(
                   plotOutput("profiiliaikasarja03", width = "100%", 
                              height = glue("{(nrow(lista3_df)+1)*rivikorkeus}px")
                   ), proxy.height = "100px")
        ),
        tags$hr(),
        fluidRow(
            column(3,
                   tags$h4("Huono-osaisuuden taloudelliset yhteydet"),
                   lista4_tbl
            ),
            column(5, 
                   tags$div(style = "padding-top:60px;"),
                   plotOutput("profiilikartta04", width = "100%", 
                              # height = glue("{korkeus4}px")
                              height = glue("{(nrow(lista4_df)+1)*rivikorkeus}px")
            )
        ),
        column(4,
               tags$div(style = "padding-top:60px;"),
               # withSpinner(
               plotOutput("profiiliaikasarja04", width = "100%", 
                          height = glue("{(nrow(lista4_df)+1)*rivikorkeus}px")
               ), proxy.height = "100px")
    )
    )
    })

    # create_report_name <- reactive({
    #     report_name <- glue("alueprofiili_{aluetaso1}_{get_klik()$id}.docx")
    #     return(report_name)
    # })
    

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
                                   spatdat = process_data(),
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
        
        # if (input$sidebar_menu == "info"){
        #     tagList()
        # } else {
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
        # }
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
    
    # output$variable_desctiption <- DT::renderDataTable({
    #     
    #     dat <- readxl::read_excel("./data/Muuttujakuvaukset_20201102.xlsx") %>% 
    #         setNames(c("Muuttujaluokka","Muuttuja","Aluetasot","Kuvaus"))
    #     
    #     
    #     
    #     
    #     
    #     dt <- datatable(dat, rownames = FALSE, 
    #                     style = "bootstrap4",
    #                     # selection = list(mode = 'single', selected = rownro),
    #                     # filter = "top", extensions = 'Buttons',
    #                     options = list(pageLength = 20,
    #                                    paging = FALSE,
    #                                    scrollY = "340px",
    #                                    # dom = "dt",
    #                                    language = list(url = 'datatable_translate.json')))
    #     return(dt)
    # })
    
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
