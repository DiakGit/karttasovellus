source("./global.R")


ui <- fluidPage(
    uiOutput("output_save_word"),
    textOutput("aputeksti"),
    
    uiOutput("output_indicator_class"),
    uiOutput("output_indicator"),
    uiOutput("output_regio_level"),
    uiOutput("output_save_map"),
    leaflet::leafletOutput("map1", width = "100%", height = "685px"),
    DT::dataTableOutput("rank_tbl"),
    # verbatimTextOutput("value"),
    uiOutput("region_profile_html"),
    DT::dataTableOutput("variable_desctiption")
)


# Define server logic for random distribution app ----
server <- function(input, output) {

    get_dat <- reactive({
        dat <- readRDS("./data/df_v20201102.RDS")
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
                pickerInput(inputId = "value_variable_class", 
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
                pickerInput(inputId = "value_variable", 
                            label = "Valitse muuttuja", 
                            choices = opt_indicator2
                            ,selected = opt_indicator2[1],
                            option= pickerOptions(
                                actionsBox = TRUE,
                                liveSearch = TRUE,
                                deselectAllText = "Ei mitään",
                                selectAllText = "Kaikki",
                                noneSelectedText = "Ei yhtään valittuna",
                                noneResultsText = "Ei yhtään osumaa"#,
                                # maxOptions = 4,
                                # maxOptionsText = "Maksimimäärä muuttujia valittu"
                            ),multiple = FALSE)
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
                radioButtons(inputId = "value_regio_level2", 
                             label = "Valitse aluetaso", inline = TRUE,
                             choices = opt_indicator, selected = "Maakunnat")
            )
        # }
        
    })
    

    # Define reactiveValue
    rv <- reactiveValues(selected = NULL)
    
    # 1. Pass value of input$timeline_selected to Reactive Value
    observe( {
        rv$map_click_id <- input$map1_shape_click
    })
    # 2. clear selection if different filter is chosen
    observeEvent(input$value_regio_level2, {
        rv$map_click_id <- NULL
    })
    
    get_klik <- reactive({
        # req(input$value_variable_class)
        # req(input$value_variable)
        req(input$value_regio_level2)
        reg <- readRDS("./data/regiokey.RDS")
        idnimi <- reg[reg$aluetaso == input$value_regio_level2,]$aluenimi[1]
        
        klik <- rv$map_click_id
        if (is.null(klik)){
            klik <- list("id" = idnimi)
        }
        return(klik)
    })
    
    
    process_data <- reactive({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level2)
        
        dat <- get_dat()
        
        dat_1 <- dat[dat$regio_level %in% input$value_regio_level2 & dat$variable %in% input$value_variable,]
        
        reg <- readRDS(glue("./data/regio_{input$value_regio_level2}.RDS"))
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
        
        leaflet(fin, options = leafletOptions()) %>%
            addProviderTiles(provider = providers$CartoDB.Positron) %>% 
            addPolygons(color = NA, fill = NA) %>% 
            setView(lng = 26.24578, lat = 65.28952, zoom = 5)
        
    })
    
    observe({
        
        req(input$value_regio_level2)
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
        
        proxy %>% clearControls()   %>%
            addLegend(data = dat, pal = pal, values = ~value, opacity = 0.7, title = add_line_break(input$value_variable, n = 20),
                      position = "bottomright")
    })
    
    
    output$rank_tbl <- DT::renderDataTable({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level2)
        
        dat <- process_data() %>% 
            st_set_geometry(NULL) %>% 
            select(rank,aluenimi,value) 
        names(dat) <- c("Sijoitus",input$value_regio_level2,input$value_variable)
        
        # klik <- rv$map_click_id
        # if (is.null(klik)){
        #     rownro <- NA
        # } else {
        #     rownro <- match(klik$id,dat[[input$value_regio_level2]])
        # }
        
        klik <- get_klik()
        rownro <- match(klik$id,dat[[input$value_regio_level2]])
        
        dt <- datatable(dat, rownames = FALSE, 
                        selection = list(mode = 'single', selected = rownro),
                        # filter = "top", extensions = 'Buttons',
                        options = list(pageLength = 20,
                                       paging = FALSE,
                                       scrollY = "680px",
                                       dom = "dt",
                                       language = list(url = 'datatable_translate.json'))) %>%
            formatStyle(
                input$value_variable,
                background = styleColorBar(dat[[input$value_variable]],
                                           color = '#c799ff',
                                           angle = -90),
                backgroundSize = '100% 90%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center'
            )
        return(dt)
    })
    
    
    output$aputeksti <- renderText({
        
        req(input$value_variable_class)
        req(input$value_variable)
        req(input$value_regio_level2)
        
        klik <- get_klik()

        aluenimi_kartta <- klik$id

        dat <- process_data() %>%
            st_set_geometry(NULL) %>%
            select(rank,aluenimi,value)
        aluenimi_taulukko <- unique(dat[input$rank_tbl_rows_selected,]$aluenimi)

        if (aluenimi_kartta != aluenimi_taulukko){
            aluename <- aluenimi_taulukko
        } else {
            aluename <- aluenimi_kartta
        }
        
        region_data <- get_region_data()
        region_data <- dplyr::filter(region_data, level %in% input$value_regio_level2)
        naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
        return(naapurikoodit)
    })

    create_alueprofiili_content <- function(aluename2 = aluename, naapurikoodit = naapurikoodit, type = "html"){
        
        dat <- get_dat()

        dat[dat$regio_level %in% input$value_regio_level2 & dat$aluenimi %in% aluename2 ,] %>% 
            select(aluenimi,var_class,variable,value) %>% 
            mutate(rooli = "valinta") -> tmpdat1
        dat[dat$regio_level %in% input$value_regio_level2 & dat$aluekoodi %in% naapurikoodit ,] %>% 
            filter(!aluenimi %in% aluename2) %>% 
            select(aluenimi,var_class,variable,value) %>% 
            mutate(rooli = "naapuri") -> tmpdat2
        tmpdat <- bind_rows(tmpdat1,tmpdat2) 
        
        # 
        dat[dat$regio_level %in% input$value_regio_level2,] %>% 
            group_by(var_class,variable) %>% 
            arrange(desc(value)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            # mutate(maksimi = glue("{round(value, 1)} ({ifelse(nchar(aluenimi) > 10, paste0(substr(aluenimi, 1, 10),'...'), aluenimi)})")) %>% 
            mutate(rooli = "korkein arvo") %>% 
            select(aluenimi,var_class,variable,value,rooli) -> max_dat
        
        dat[dat$regio_level %in% input$value_regio_level2,] %>% 
            group_by(var_class,variable) %>% 
            arrange(value) %>% 
            slice(1) %>% 
            ungroup() %>% 
            # mutate(minimi = glue("{round(value, 1)} ({ifelse(nchar(aluenimi) > 10, paste0(substr(aluenimi, 1, 10),'...'), aluenimi)})")) %>% 
            mutate(rooli = "matalin arvo") %>% 
            select(aluenimi,var_class,variable,value,rooli) -> min_dat
        
        dat[dat$regio_level %in% input$value_regio_level2,] %>% 
            group_by(variable) %>% 
            arrange(desc(value)) %>%
            mutate(sija = 1:n(),
                   n = n()) %>% 
            ungroup() %>% 
            # filter(aluenimi %in% aluename2) %>% 
            select(aluenimi,variable,sija) -> rank_dat
        
        bind_rows(tmpdat,max_dat,min_dat) %>% 
            left_join(rank_dat) %>% 
            mutate(value = round(value, 1)) %>% 
            rename(muuttuja = variable,
                   arvo = value) %>% 
            filter(!is.na(arvo)) %>% 
            select(muuttuja,arvo,sija,everything()) %>% 
            distinct() -> tabdat
        # tmpdat -> tabdat
        return(tabdat)
    }
    
    
    
    output$region_profile_html <- renderUI({
        # klik <- rv$map_click_id
        klik <- get_klik()
        
        aluenimi_kartta <- klik$id
        
        dat <- process_data() %>% 
            st_set_geometry(NULL) %>% 
            select(rank,aluenimi,value)
        aluenimi_taulukko <- unique(dat[input$rank_tbl_rows_selected,]$aluenimi)
                                    
        if (aluenimi_kartta != aluenimi_taulukko){
            aluename <- aluenimi_taulukko
        } else {
            aluename <- aluenimi_kartta
        }

        region_data <- get_region_data()
        region_data <- dplyr::filter(region_data, level %in% input$value_regio_level2)
        naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]

    tabdat <- create_alueprofiili_content(aluename2 = aluename, naapurikoodit = naapurikoodit)
    
    # Summamuuttujat
    muuttujaluokka <- "Summamuuttujat"
    lista1_tbl <- tabdat %>% 
        filter(var_class == muuttujaluokka) %>%
        mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
        select(muuttuja,aluenimi,arvo,sija) %>%
        arrange(muuttuja,sija) %>% 
        mutate(sija = paste0(sija,".")) %>% 
        gt(
            rowname_col = "aluenimi",
            groupname_col = "muuttuja"
        ) %>% 
        gt::tab_header(title = toupper(muuttujaluokka)) %>%
        tab_options(table.width	= "90%", 
                    table.align = "left",
                    row_group.background.color = alpha("grey", 1/6)) %>% 
        cols_align(
            align = "right",
            columns = vars(sija)
        )
    
    # "Inhimillinen huono-osaisuus"
    muuttujaluokka <- "Inhimillinen huono-osaisuus"
    lista2_tbl <- tabdat %>% 
        filter(var_class == muuttujaluokka) %>%
        mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
        select(muuttuja,aluenimi,arvo,sija) %>%
        arrange(muuttuja,sija) %>% 
        mutate(sija = paste0(sija,".")) %>% 
        gt(
            rowname_col = "aluenimi",
            groupname_col = "muuttuja"
        ) %>% 
        gt::tab_header(title = toupper(muuttujaluokka)) %>%
        tab_options(table.width	= "90%", 
                    table.align = "left",
                    row_group.background.color = alpha("grey", 1/6)) %>% 
        cols_align(
            align = "right",
            columns = vars(sija)
        )
    
    # "Huono-osaisuuden sosiaaliset seuraukset"
    muuttujaluokka <- "Huono-osaisuuden sosiaaliset seuraukset"
    lista3_tbl <- tabdat %>% 
        filter(var_class == muuttujaluokka) %>%
        mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
        select(muuttuja,aluenimi,arvo,sija) %>%
        arrange(muuttuja,sija) %>% 
        mutate(sija = paste0(sija,".")) %>% 
        gt(
            rowname_col = "aluenimi",
            groupname_col = "muuttuja"
        ) %>% 
        gt::tab_header(title = toupper(muuttujaluokka)) %>%
        tab_options(table.width	= "90%", 
                    table.align = "left",
                    row_group.background.color = alpha("grey", 1/6)) %>% 
        cols_align(
            align = "right",
            columns = vars(sija)
        )
    
    # "Huono-osaisuuden taloudelliset yhteydet"
    muuttujaluokka <- "Huono-osaisuuden taloudelliset yhteydet"
    lista4_tbl <- tabdat %>% 
        filter(var_class == muuttujaluokka) %>%
        mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
        select(muuttuja,aluenimi,arvo,sija) %>%
        arrange(muuttuja,sija) %>% 
        mutate(sija = paste0(sija,".")) %>% 
        gt(
            rowname_col = "aluenimi",
            groupname_col = "muuttuja"
        ) %>% 
        gt::tab_header(title = toupper(muuttujaluokka)) %>%
        tab_options(table.width	= "90%", 
                    table.align = "left",
                    row_group.background.color = alpha("grey", 1/6)) %>% 
        cols_align(
            align = "right",
            columns = vars(sija)
        )

    tagList(
        fluidRow(column(width = 12, 
                        tags$h3(glue("{aluename} ({input$value_regio_level2})"))
        )),
        fluidRow(column(12,
                        
                        tags$div(style = "padding-top: 10px;"),
                        # tags$h4("Summamuuttujat"),
                        tags$p("Analyysissä mukana", glue_collapse(unique(tabdat$aluenimi), sep = ", ", last = " ja ")),
                        lista1_tbl,

                        tags$div(style = "padding-top: 50px;"),
                        # tags$h4("Inhimillinen huono-osaisuus"),
                        lista2_tbl,
                        
                        tags$div(style = "padding-top: 50px;"),
                        # tags$h4("Huono-osaisuuden sosiaaliset seuraukset"),
                        lista3_tbl,
                        
                        tags$div(style = "padding-top: 50px;"),
                        # tags$h4("Huono-osaisuuden taloudelliset yhteydet"),
                        lista4_tbl
        ))
    )
    
    })
    
    # create_report_name <- reactive({
    #     report_name <- glue("alueprofiili_{input$value_regio_level2}_{get_klik()$id}.docx")
    #     return(report_name)
    # })
    
    output$report <- downloadHandler(

        filename = function() {
            file_name <- glue("alueprofiili_{get_klik()$id}_{tolower(input$value_regio_level2)}{input$value_report_format}")
            return(file_name)
        },
        content = function(file) {
            
            shiny::withProgress(
            message = paste0("Luodaan dokumenttia"),
            value = 0,
            {
            shiny::incProgress(1/10)
            Sys.sleep(1)
            

                    # Set up parameters to pass to Rmd document
                    klik <- get_klik()
                    # aluename <- klik$id
                    aluenimi_kartta <- klik$id

                    dat <- process_data() %>%
                        st_set_geometry(NULL) %>%
                        select(rank,aluenimi,value)
                    aluenimi_taulukko <- unique(dat[input$rank_tbl_rows_selected,]$aluenimi)

                    if (aluenimi_kartta != aluenimi_taulukko){
                        aluename <- aluenimi_taulukko
                    } else {
                        aluename <- aluenimi_kartta
                    }
                    shiny::incProgress(3/10)
                    
                    region_data <- get_region_data()
                    region_data <- dplyr::filter(region_data, level %in% input$value_regio_level2)
                    naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
                    
                    # klik <- list("id" = "Veteli")
                    params <- list(region = aluename,
                                   region_level = input$value_regio_level2,
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
            downloadButton("report", "Tallenna alueprofiili laitteellesi!"),
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
            file_name <- glue("map_{janitor::make_clean_names(input$value_variable)}_{tolower(input$value_regio_level2)}.png")
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
                        scale_fill_viridis(option = "viridis", direction = -1) +
                        theme_minimal(base_family = "PT Sans", base_size = 12) +
                        labs(fill = NULL,
                             title = glue("{input$value_variable}"),
                             subtitle = glue("Aluetaso: {input$value_regio_level2}"),
                             caption = glue("Data: THL & Diak\n{Sys.Date()}")) -> p
                    if (input$value_regio_level2 != "Kunnat"){
                        p + ggrepel::geom_text_repel(data = dat %>%
                                                         sf::st_set_geometry(NULL) %>%
                                                         bind_cols(dat %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                                                     aes(label = value, x = X, y = Y), color = "white", family = "PT Sans", alpha = .6, size = 2.5) -> p
                    } else {
                        p + geom_text(data = dat %>%
                                          sf::st_set_geometry(NULL) %>%
                                          bind_cols(dat %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                                      aes(label = value, x = X, y = Y), 
                                      color = "white", 
                                      family = "PT Sans", 
                                      alpha = .9, 
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
    
    output$variable_desctiption <- DT::renderDataTable({
        
        dat <- readxl::read_excel("./data/Muuttujakuvaukset_20201102.xlsx") %>% 
            setNames(c("Muuttujaluokka","Muuttuja","Aluetasot","Kuvaus"))
        
        
        dt <- datatable(dat, rownames = FALSE, 
                        # selection = list(mode = 'single', selected = rownro),
                        # filter = "top", extensions = 'Buttons',
                        options = list(pageLength = 20,
                                       paging = FALSE,
                                       scrollY = "340px",
                                       # dom = "dt",
                                       language = list(url = 'datatable_translate.json')))
        return(dt)
    })

}

# shinyApp(ui = ui, server = server)
shinyApp(ui = htmlTemplate("www/index.html"), server = server)

