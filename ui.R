source("./global.R")

shinyUI(bs4DashPage(
    old_school = FALSE,
    sidebar_mini = TRUE,
    sidebar_collapsed = TRUE,
    controlbar_collapsed = FALSE,
    controlbar_overlay = FALSE,
    title = "DIAK: huono-osaisuus Suomessa",
    # navbar = bs4DashNavbar(
    #   skin = "light",
    #   status = "white",
    #   border = TRUE,
    #   sidebarIcon = "bars",
    #   controlbarIcon = "th",
    #   fixed = FALSE,
    #   leftUi = tags$h3("Huono-osaisuus Suomessa"), 
    #   rightUi = tags$p(tags$img(src = "app_diak.png", style = "height: 62px; padding-left:20px;"), 
    #                    tags$img(src = "app_vipu.png", style = "height: 62px; padding-left:20px;"),
    #                    tags$img(src = "app_eu.png", style = "height: 62px; padding-left:20px;"),
    #                    tags$img(src = "app_sokra.png", style = "height: 62px;  padding-left:20px; padding-right:20px;"))
    # ),
    sidebar = bs4DashSidebar(
        skin = "light",
        status = "primary",
        title = "Huono-osaisuus Suomessa", #tags$img(src = "https://www.diak.fi/wp-content/themes/diak/dist/images/diak-logo-1.svg", style = "height: 32px;"),
        brandColor = "white",
        url = "https://www.diak.fi/tutkimus-ja-kehitys/",
        # src = "https://www.diak.fi/wp-content/themes/diak/dist/images/diak-logo-1.svg",
        elevation = 3,
        opacity = 0.8,
        # bs4SidebarUserPanel(
        #     img = "https://www.diak.fi/wp-content/themes/diak/dist/images/diak-logo-1.svg", 
        #     text = "Welcome Onboard!"
        # ),
        bs4SidebarMenu(id = "sidebar_menu", flat = TRUE,
                       bs4SidebarHeader("Valitse sivut"),
                       bs4SidebarMenuItem(
                           "Kartta",
                           tabName = "map",
                           icon = "map"
                       ),
                       # bs4SidebarMenuItem(
                       #   "Kuviot",
                       #   tabName = "charts",
                       #   icon = "bar-chart"
                       # ),
                       bs4SidebarMenuItem(
                           "Lisätietoja",
                           tabName = "info",
                           icon = "info-circle"
                       )
        )
    ),
    body = bs4DashBody(
        bs4TabItems(
            bs4TabItem(
                tabName = "map",
                box(width = 12, 
                    headerBorder = FALSE,
                    collapsible = FALSE,
                    # closable = FALSE,
                    # title = "",
                    fluidRow(column(width = 3,
                                    uiOutput("output_indicator_class"),
                                    uiOutput("output_indicator")),
                             column(
                                 width = 9, 
                                 uiOutput("output_info_controlbar"))
                    )),
                fluidRow(
                    tabBox(id = "value_regio_level", 
                           type = "tabs",
                           title = "",
                           headerBorder = FALSE,
                           # solidHeader = FALSE
                           # elevation = 4,
                           closable = FALSE,
                           width = 5,
                           # solidHeader = TRUE, 
                           # status = "primary",
                           collapsible = FALSE,
                           # bs4TabPanel(tabName = "Maakunnat", active = TRUE, 
                           
                           tabPanel(tabName = "Kartta", active = FALSE,
                                    uiOutput("output_regio_level"),
                                    withSpinner(leaflet::leafletOutput("map1", width = "100%", height = "685px"))
                           ),
                           tabPanel(tabName = "Tallenna kartta", active = FALSE,
                                    uiOutput("output_save_map")
                           )
                    ),
                    tabBox(id = "value_regio_level3",
                           type = "tabs",
                           title = "",
                           # elevation = 4,
                           closable = FALSE,
                           width = 7,
                           # solidHeader = FALSE,
                           # # status = "primary",
                           collapsible = FALSE,
                           # headerBorder = TRUE,
                           
                           tabPanel(tabName = "Taulukko", active = TRUE,
                                    withSpinner(DT::dataTableOutput("rank_tbl"))
                           ),
                           tabPanel(tabName = "Alueprofiili", active = FALSE,
                                    withSpinner(uiOutput("region_profile_html"))
                           )
                    )
                )
            ),
            bs4TabItem(
                tabName = "info",
                fluidRow(
                    box(
                        title = "Aineisto",
                        width = 9,
                        tags$p("Mauris molestie sagittis risus, cursus posuere magna dapibus sed. Sed sed mauris enim. Proin consequat bibendum facilisis. 
            Morbi eget rhoncus odio. Proin pretium, tellus id ornare pulvinar, magna eros sollicitudin magna, nec fringilla enim dui at erat. 
            Sed vulputate neque odio, vel aliquet enim accumsan nec. Pellentesque dictum, justo ut ullamcorper dictum, massa nisl facilisis lacus, 
            quis malesuada odio metus at quam. Integer vel imperdiet turpis."),
                        tags$h4("Muuttujakuvaus"),
                        DT::dataTableOutput("variable_desctiption")
                    ),
                    box(
                        title = "Verkkosovellus",
                        width = 3,
                        
                        tags$p("Tämä verkkosovellus on tehty",tags$a(href = "https://www.r-project.org/", "R"),"-kielellä ja erityisesti ",
                               tags$a(href = "https://shiny.rstudio.com", "Shiny"),"-paketin avulla. 
                 Sovelluksen lähdekoodi on avoimesti lisensöity (MIT-lisenssi) ja se löytyy ",
                               tags$a(href = "https://gitlab.com/muuankarski/diak_app", "Gitlab"),"-palvelusta."),
                        tags$p("Löysitkö ohjelmasta bugin? Eikö jokin ominaisuus toimi toivotulla tavalla? Tuliko sinulle mieleen jokin parannusehdotus? 
          Ei hätää, tapoja osallistua sovelluksen kehittämiseen on monia:"),
                        tags$ul(
                            tags$li("Toteuta korjaus tai uusi omainaisuus itse ja jätä ",tags$a(href = "https://gitlab.com/muuankarski/diak_app/-/merge_requests", "Gitlab"),"-palvelussa pull request."),
                            tags$li("Avaa uusi ",tags$a(href = "https://gitlab.com/muuankarski/diak_app/-/issues", "issue"),
                                    "GitLab-palvelussa ja kuvaa havaitsemasi bugi tai toive uudesta ominaisuudesta siellä."),
                            tags$li("Lähetä meiliä osoitteeseen ",tags$a(href = "mailto:sakari.kainulainen@diak.fi", tags$code("sakari.kainulainen@diak.fi")))
                        )
                    )
                )
            )
        ),
        box(width = 12, headerBorder = FALSE, collapsible = FALSE,
            fluidRow(column(4, 
                            tags$p(icon("copyright"), "DIAK 2020")),
                     column(8, 
                            tags$p(style="text-align: right;",
                                   tags$img(src = "app_diak.png", style = "height: 62px; padding-left:20px;"), 
                                   tags$img(src = "app_vipu.png", style = "height: 62px; padding-left:20px;"),
                                   tags$img(src = "app_eu.png", style = "height: 62px; padding-left:20px;"),
                                   tags$img(src = "app_sokra.png", style = "height: 62px;  padding-left:20px; padding-right:20px;")))))
    )
))
