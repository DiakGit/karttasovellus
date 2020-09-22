source("./global.R")

shinyUI(bs4DashPage(
    old_school = FALSE,
    sidebar_mini = TRUE,
    sidebar_collapsed = TRUE,
    controlbar_collapsed = FALSE,
    controlbar_overlay = FALSE,
    title = "DIAK: huono-osaisuus Suomessa",
    navbar = bs4DashNavbar(
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = "bars",
      controlbarIcon = "th",
      fixed = FALSE,
      leftUi = tags$h3("Huono-osaisuus Suomessa")#,
      # rightUi = tags$p(tags$img(src = "app_diak.png", style = "height: 62px; padding-left:20px;"),
      #                  tags$img(src = "app_vipu.png", style = "height: 62px; padding-left:20px;"),
      #                  tags$img(src = "app_eu.png", style = "height: 62px; padding-left:20px;"),
      #                  tags$img(src = "app_sokra.png", style = "height: 62px;  padding-left:20px; padding-right:20px;"))
    ),
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
                       ),
                       bs4SidebarMenuItem(
                           "Saavutettavuusseloste",
                           tabName = "accessibility",
                           icon = "blind"
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
                        tags$p("Sovellusversio", tags$code("0.1.1"),tags$br(),
                               "Aineistoversio", tags$code("0.1.2")),

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
            ),
            bs4TabItem(
                tabName = "accessibility",
                bs4SocialCard(src = "blind.png",
                              title = "Saavutettavuusseloste",
                              width = 12,
                              tags$p(
                                tags$i("Saavutettavuudella"), "tarkoitetaan, että verkkosivut ja mobiilisovellukset sekä niiden sisällöt ovat sellaisia, 
                                  että kuka tahansa voisi niitä käyttää ja ymmärtää mitä niissä sanotaan."),
                              
                              tags$p("Diakin verkkosivuja koskee laki digitaalisten palvelujen tarjoamisesta, joka pohjautuu  EU:n saavutettavuusdirektiiviin. 
                                         Lain mukaan kaikilla tulee olla tasavertaiset mahdollisuudet käyttää digitaalisia palveluita."),
                              
                              tags$p("Tämä saavutettavuusseloste koskee",tags$a(href = 'https://www.thl.fi/sokra',"Sokra-hankkeessa"), "toteutettu ", 
                                     tags$a(href = 'https://diak.shinyapps.io/karttasovellus/', 'karttasivusto')," on julkaistu 23.4.2020,
                                  joka tarkoittaa sitä, että sivuston tulee noudattaa WCAG 2.1 –kriteeristön AA-tasoa 23.9.2020 mennessä."),
                              
                              tags$h3("Digipalvelun saavutettavuuden tila"),
                              tags$p("Diakin itsearviona toteutetun saavutettavuusarvioinnin mukaan mahdollisia saavutettavuuspuutteita sivustolla", tags$a(href = 'https://diak.shinyapps.io/karttasovellus/', 'diak.shinyapps.io/karttasovellus/'), 
                                     "saattaa olla:"),
                              # 
                              tags$li(" On todennäköistä, että ruudunlukuohjelma pystyy lukemaan sivustoa heikosti"),
                              tags$li("Sivustolla navigointi ilman hiirtä on kankeaa"),
                              # tags$li(tags$del("Info-sivu ei näy mobiililaitteille")),
                              tags$li("Kartalla näkyvien värien kontrastit eivät välttämättä ole riittäviä, 
                                          väriskaalan soveltuvuus värisokeille epävarma.Karttakuvioiden informaatio on kuitenkin tarjolla samaan aikaan kirjoitetussa muodossa oikean reunan alueprofiileissa, 
                                          mikä auttaa sivuston käyttämistä ilman karttakuvia."),
                              
                              tags$li("PDF:ien värikontrasteissa osa maakuntien väreistä on liian vaaleita, 
                                          jotta päällä oleva teksti näkyisi riittävän hyvin saavutettavuusstandardin mukaan"),
                              
                              tags$p("Karttasivustoa koskeva seloste on laadittu 19.9.2020. Seloste perustuu itsearvioon ja käyttäjiltä saatuun palautteeseen. 
                                         Syyskuun aikana valmistuu raportti ulkopuolisen tekemästä auditoinnista, jossa arvioidaan palvelun tekninen toimivuus ja ladattavien karttojen auditointi."),
                              
                              tags$p("Näiden arvioiden ja raportin perustella karttasivustoon tullaan tekemään saavutettavuutta parantavia korjauksia."),
                              
                              tags$h3("Palautetta saavutettavuudesta?"),
                              tags$p("Huomasitko saavutettavuuspuutteen digipalvelussamme? Kerro se meille ja
                                  teemme parhaamme puutteen korjaamiseksi"),
                              tags$p("Ilmoita puute tällä",tags$a(href = 'https://www.diak.fi/diak/anna-palautetta/', 'verkkolomakkeella'),
                                     ". Valitse kohta verkkosivuston saavutettavuus. Kerro palautteessa, mitä Diakin hallinnoimaa sivustoa palaute koskee."),
                              tags$p("Jos huomaat sivustolla saavutettavuusongelmia, anna ensin palautetta sivuston ylläpitäjälle. Vastauksessa voi mennä 14 päivää.
                                  Jos et ole tyytyväinen saamaasi vastaukseen tai et saa vastausta lainkaan kahden viikon aikana, voit ilmoittaa asiasta valvontaviranomaiselle."),
                              
                              tags$strong("Valvontaviranomaisen yhteystiedot:"),
                              tags$p("Etelä-Suomen aluehallintovirasto",tags$br(),
                                     "Saavutettavuuden valvonnan yksikkö",tags$br(),
                                     "www.saavutettavuusvaatimukset.fi",tags$br(),
                                     "saavutettavuus(at)avi.fi",tags$br(),
                                     "puhelinnumero vaihde 0295 016 000")
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
