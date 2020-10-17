source("./global.R")


ui <- fluidPage(
    
    uiOutput("output_indicator_class"),
    uiOutput("output_indicator"),
    uiOutput("output_regio_level"),
    leaflet::leafletOutput("map1", width = "100%", height = "685px"),
    DT::dataTableOutput("rank_tbl"),
    uiOutput("region_profile_html"),
    DT::dataTableOutput("variable_desctiption"),
    uiOutput("output_save_map")
    

)


# Define server logic for random distribution app ----
server <- function(input, output) {

    get_dat <- reactive({
        dat <- readRDS("./data/df_v20200423.RDS")
        return(dat)
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
                            choices = opt_indicator2)
                            # ,selected = opt_indicator2[1],
                            # option= pickerOptions(
                            #     actionsBox = TRUE,
                            #     liveSearch = TRUE,
                            #     deselectAllText = "Ei mitään",
                            #     selectAllText = "Kaikki",
                            #     noneSelectedText = "Ei yhtään valittuna",
                            #     noneResultsText = "Ei yhtään osumaa",
                            #     maxOptions = 4, 
                            #     maxOptionsText = "Maksimimäärä muuttujia valittu"
                            # ),multiple = TRUE)
            )
        # }
        
    })
    
    output$output_regio_level <- renderUI({
        
        req(input$value_variable_class)
        req(input$value_variable)
        
        indicator_df <- varlist_diak()
        opt_indicator <- unique(indicator_df[indicator_df$var_class %in% input$value_variable_class & indicator_df$variable %in% input$value_variable,]$regio_level)
        opt_indicator <- indicator_df[indicator_df$variable %in% input$value_variable,]$regio_level
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
    
    output$kartta <- renderLeaflet({
        leaflet() %>% 
            addTiles()
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
    
    
    
    output$region_profile_html <- renderUI({
        # klik <- rv$map_click_id
        klik <- get_klik()
        
        
        if (is.null(klik$id)){
            tagList(
                tags$p("Valitse ensin alue klikkaamalla karttaa!", class = "lead")
            ) 
        } else {
            
            dat <- get_dat()
            
            dat[dat$regio_level %in% input$value_regio_level2 & dat$aluenimi %in% klik$id,] %>% 
                select(var_class,variable,value) -> tmpdat
            # 
            dat[dat$regio_level %in% input$value_regio_level2,] %>% 
                group_by(var_class,variable) %>% 
                arrange(desc(value)) %>% 
                slice(1) %>% 
                ungroup() %>% 
                mutate(maksimi = glue("{round(value, 1)} ({ifelse(nchar(aluenimi) > 10, paste0(substr(aluenimi, 1, 10),'...'), aluenimi)})")) %>% 
                select(variable,maksimi) -> max_dat
            
            dat[dat$regio_level %in% input$value_regio_level2,] %>% 
                group_by(variable) %>% 
                arrange(value) %>% 
                slice(1) %>% 
                ungroup() %>% 
                mutate(minimi = glue("{round(value, 1)} ({ifelse(nchar(aluenimi) > 10, paste0(substr(aluenimi, 1, 10),'...'), aluenimi)})")) %>% 
                select(variable,minimi) -> min_dat
            
            dat[dat$regio_level %in% input$value_regio_level2,] %>% 
                group_by(variable) %>% 
                arrange(desc(value)) %>%
                mutate(sija = 1:n(),
                       n = n()) %>% 
                ungroup() %>% 
                filter(aluenimi %in% klik$id) %>% 
                select(variable,sija) -> rank_dat
            
            left_join(tmpdat,max_dat) %>% 
                left_join(min_dat) %>% 
                left_join(rank_dat) %>% 
                mutate(value = round(value, 1)) %>% 
                rename(muuttuja = variable,
                       arvo = value) %>% 
                filter(!is.na(arvo)) %>% 
                select(muuttuja,arvo,sija,everything()) -> tabdat
            
            tagList(
                fluidRow(column(width = 12, 
                                tags$h4(glue("{klik$id} ({input$value_regio_level2})")),
                # ),
                # column(6, downloadButton("report", 
                #                          glue("Tallenna {tolower(input$value_regio_level2)}_{klik$id}.pdf")),
                )),
                fluidRow(column(12,
                                
                                tags$h5("Summamuuttujat"),
                                tabdat %>% 
                                    filter(var_class == "Summamuuttujat") %>% 
                                    select(-var_class) %>% 
                                    knitr::kable(format = "html") %>% 
                                    kableExtra::kable_styling() %>% 
                                    HTML(),
                                
                                tags$h5("Inhimillinen huono-osaisuus"),
                                
                                tabdat %>% 
                                    filter(var_class == "Inhimillinen huono-osaisuus") %>% 
                                    select(-var_class) %>% 
                                    knitr::kable(format = "html") %>% 
                                    kableExtra::kable_styling() %>% 
                                    HTML(),
                                
                                tags$h5("Huono-osaisuuden sosiaaliset seuraukset"),
                                
                                tabdat %>% 
                                    filter(var_class == "Huono-osaisuuden sosiaaliset seuraukset") %>% 
                                    select(-var_class) %>% 
                                    knitr::kable(format = "html") %>% 
                                    kableExtra::kable_styling() %>% 
                                    HTML(),
                                
                                tags$h5("Huono-osaisuuden taloudelliset yhteydet"),
                                
                                tabdat %>% 
                                    filter(var_class == "Huono-osaisuuden taloudelliset yhteydet") %>% 
                                    select(-var_class) %>% 
                                    knitr::kable(format = "html") %>% 
                                    kableExtra::kable_styling() %>% 
                                    HTML()
                ))
            )
        }
    })
    
    output$report <- downloadHandler(
        
        filename = glue("{tolower(input$value_regio_level2)}_{rv$map_click_id$id}.pdf"),
        content = function(file) {
            
            shiny::withProgress(
                message = paste0("Luodaan raporttia"),
                value = 0,
                {
                    shiny::incProgress(1/10)
                    Sys.sleep(1)
                    
                    tempReport <- file.path(tempdir(), "report.Rmd")
                    file.copy("./docs/report.Rmd", tempReport, overwrite = TRUE)
                    shiny::incProgress(2/10)
                    temp_header <- file.path(tempdir(), "header.tex")
                    file.copy("./docs/header.tex", temp_header, overwrite = TRUE)
                    shiny::incProgress(3/10)
                    logo <- file.path(tempdir(), "diak-logo-1.pdf")
                    file.copy("./docs/diak-logo-1.pdf", logo, overwrite = TRUE)
                    shiny::incProgress(4/10)
                    # Set up parameters to pass to Rmd document
                    klik <- rv$map_click_id
                    params <- list(region = klik$id,
                                   region_level = input$value_regio_level2,
                                   data = get_dat())
                    shiny::incProgress(5/10)
                    
                    rmarkdown::render(tempReport, output_file = file,
                                      params = params,
                                      envir = new.env(parent = globalenv())
                    )
                    shiny::incProgress(8/10)
                }
            )
        }
    )
    
    
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
        
        dat <- readxl::read_excel("./data/muuttujakuvaukset.xlsx") %>% 
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

