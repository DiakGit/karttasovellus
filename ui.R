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
                    bs4SocialCard(src = "database.png",
                        title = "Aineisto",
                        width = 8,
                        tags$h4("Muuttujakuvaus"),
                        DT::dataTableOutput("variable_desctiption")
                    ),
                    bs4SocialCard(src = "chart-bar.png",
                        title = "Sovellus",
                        width = 4,
                        
                        tags$strong("Huono-osaisuus Suomessa -verkkosovellus"),
                        tags$p("Sovellusversio", tags$code("0.1.0"),tags$br(),"Aineistoversio", tags$code("0.1.0")),

                        tags$p("Tämä verkkosovellus on tehty",tags$a(href = "https://www.r-project.org/", "R"),"-kielellä",
                               tags$a(href = "https://shiny.rstudio.com", "Shiny"),"-kirjaston avulla. 
                 Sovelluksen lähdekoodi on avoimesti lisensöity ja saatavilla ",
                               tags$a(href = "https://gitlab.com/muuankarski/diak_app", "Gitlab"),"-palvelusta."),
                        
                        
                        tags$p("Mikäli löysit sovelluksesta bugin tai sinulla on idea tai toteutus uudesta ominaisuudesta voit:"),
                        tags$ul(
                            tags$li("toteuttaa ominaisuuden/korjauksen ja jättää",tags$a(href = "https://gitlab.com/muuankarski/diak_app/-/merge_requests", "merge requestin"),"Gitlab-palvelussa,"),
                            tags$li("avata uuden ",tags$a(href = "https://gitlab.com/muuankarski/diak_app/-/issues", "issuen"),
                                    "GitLab-palvelussa ja kuvata bugin/ominaisuuden siinä tai"),
                            tags$li("laittaa meiliä osoitteeseen ",tags$a(href = "mailto:sakari.kainulainen@diak.fi", tags$code("sakari.kainulainen@diak.fi")))
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
