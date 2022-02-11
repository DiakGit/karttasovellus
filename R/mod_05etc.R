#' 05etc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_05etc_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(class="container_1280",
             tags$div(class="row",
                      tags$div(class = "col-lg-12",
                               tags$h2(id = "info", "Lisätietoja"),
                               tags$h3("Aineisto"),
                               tags$h4("Summamuttujat"),
                               gt_output(ns("variable_desctiption_gt1")),
                               tags$h4("Inhimillinen huono-osaisuus"),
                               gt_output(ns("variable_desctiption_gt2")),
                               tags$h4("Huono-osaisuuden taloudelliset seuraukset"),
                               gt_output(ns("variable_desctiption_gt3")),
                               tags$h4("Huono-osaisuuden sosiaaliset seuraukset"),
                               gt_output(ns("variable_desctiption_gt4")),
                               tags$h4("Postinumerotarkastelu"),
                               gt_output(ns("variable_desctiption_gt5"))
                               
                      )          
             )),
    
    tags$div(class = "container_1280",
             tags$div(class = "row",
                      tags$div(class = "col-lg-8",
                               tags$h3("Sovellus"),
                               tags$strong("Huono-osaisuus Suomessa -verkkosovellus"),
                               tags$ul(
                                 tags$li("Sovellusversio",
                                         tags$code("0.9.0001")),
                                 tags$li("Aineistoversio",
                                         tags$code("0.9.0001"))
                               ),
                               tags$p("Tämä verkkosovellus on tehty", 
                                      tags$a(href = "https://www.r-project.org", "R"), "-kielellä", 
                                      tags$a(href = "https://shiny.rstudio.com", "Shiny"), "-kirjaston avulla. Sovelluksen lähdekoodi on avoimesti lisensöity ja saatavilla", 
                                      tags$a(href = "https://github.com/DiakGit/karttasovellus", "Github"), "-palvelusta."),
                               tags$p("Mikäli löysit sovelluksesta bugin tai sinulla on idea tai toteutus uudesta ominaisuudesta voit:"),
                               tags$ul(
                                 tags$li("toteuttaa ominaisuuden/korjauksen ja jättää", 
                                         tags$a(href = "https://github.com/DiakGit/karttasovellus/pulls", "pull requestin"), "Github-palvelussa"),
                                 tags$li("avata uuden", 
                                         tags$a(href = "https://github.com/DiakGit/karttasovellus/issues", "issuen"), "Github-palvelussa ja kuvata bugin/ominaisuuden siinä tai"),
                                 tags$li("laittaa meiliä osoitteeseen", 
                                         tags$a(href = "mailto:sakari.kainulainen@diak.fi", tags$code("sakari.kainulainen@diak.fi")), "tai"),
                                 tags$li("laittaa palautetta lomakkeella", 
                                         tags$a(href = "https://www.diak.fi/kumppanille-ja-kehittajalle/kehittamistyokalut/huono-osaisuus-suomessa-karttasivusto/#73053a2b", "Diakin sivuilta"))
                               )))),
    
    
    tags$div(class = "container_1280",
             tags$div(class = "row",
                      tags$div(class = "col-lg-8",
                               tags$h2(id = "saavutettavuus", "Saavutettavuusseloste"),
                               tags$p("Saavutettavuudella tarkoitetaan, että verkkosivut ja mobiilisovellukset sekä niiden sisällöt ovat sellaisia,
    että kuka tahansa voisi niitä käyttää ja ymmärtää mitä niissä sanotaan."),
    tags$p("Diakin verkkosivuja koskee laki digitaalisten palvelujen tarjoamisesta, joka pohjautuu  EU:n saavutettavuusdirektiiviin. 
    Lain mukaan kaikilla tulee olla tasavertaiset mahdollisuudet käyttää digitaalisia palveluita."),
    tags$p("Tämä saavutettavuusseloste koskee", tags$a(href="https://www.thl.fi/sokra", "Sokra-hankkeessa"),
           "toteutettu", tags$a(href="https://diak.shinyapps.io/karttasovellus/", "karttasivusto"), "on julkaistu 23.4.2020,
    joka tarkoittaa sitä, että sivuston tulee noudattaa WCAG 2.1 –kriteeristön AA-tasoa 23.9.2020 mennessä."),
    tags$h3("Digipalvelun saavutettavuuden tila"),
    tags$p(tags$a(href ="https://diak.shinyapps.io/karttasovellus/", "Karttasivusto"),"täyttää pääosin WCAG 2.1 -kriteeristön A & AA -saavutettavuusvaatimukset."),
    tags$h3("Saavutettavuusselosteen laatiminen"),
    tags$p("Karttasivustoa koskeva seloste on laadittu 19.9.2020 ja sitä on päivitetty 11.2.2022. 
    Seloste perustuu itsearvioon, Poutapilvi Oy:n tekemään auditointiraporttiin sekä 
    seuraavilla testausohjelmistoilla suoritettuun tarkastukseen."),
    tags$strong("Ruudunlukijat"),
    tags$ul(
      tags$li(tags$a(href = "https://www.nvaccess.org/", "NVDA")),
      tags$li(tags$a(href = "https://wiki.gnome.org/Projects/Orca", "Orca")),
      tags$li(tags$a(href = "https://www.chromevox.com/", "ChromeVox"))
    ),
    tags$strong("Saavutettavuuden tarkastuohjelmat"),
    tags$ul(
      tags$li(tags$a(href = "https://ainspector.github.io/", "AInspector")),
      tags$li(tags$a(href = "https://thepaciellogroup.github.io/WAI-ARIA-Usage/WAI-ARIA_usage.html", "WAI-ARIA Usage")),
      tags$li(tags$a(href = "https://validator.w3.org/nu/", "Nu Html Checker")),
      tags$li(tags$a(href = "https://developer.paciellogroup.com/resources/contrastanalyser/", "Colour Contrast Analyser")),
      tags$li(tags$a(href = "https://wave.webaim.org/", "WAVE Web Accessibility Evaluation Tool"))
    ),
    tags$p("Näiden arvioiden ja ulkopuolisen raportin perusteella karttasivuston ensimmäiseen versioon tehtiin lukuisia saavutettavuutta parantavia korjauksia."),
    tags$h3("Palautetta saavutettavuudesta?"),
    tags$p("Huomasitko saavutettavuuspuutteen digipalvelussamme? Kerro se meille ja eemme parhaamme puutteen korjaamiseksi"),
    tags$p("Jos huomaat sivustolla saavutettavuusongelmia, anna ensin palautetta sivuston ylläpitäjälle. Vastauksessa voi mennä 14 päivää. Jos et ole tyytyväinen saamaasi vastaukseen tai et saa vastausta lainkaan kahden viikon aikana, voit ilmoittaa asiasta valvontaviranomaiselle."),
    tags$strong("Valvontaviranomaisen yhteystiedot:"),
    tags$p("Etelä-Suomen aluehallintovirasto",tags$br(),
           "Saavutettavuuden valvonnan yksikkö",tags$br(),
           "www.saavutettavuusvaatimukset.fi",tags$br(),
           "saavutettavuus(at)avi.fi",tags$br(),
           "puhelinnumero vaihde 0295 016 000")
                      )
             )
    ),
    
    tags$footer(class = "bd-footer py-5 mt-5 bg-dark-diak container_1280",
                tags$div(class = "tp-bg-2 padding-top--medium text--small content--hyphenate tp-links-1 cf",
                         tags$div(class = "container-fluid",
                                  tags$div(class = "row",
                                           tags$div(class = "col-sm-6 col-md-3 footer-content margin-bottom--medium",
                                                    tags$img(src="https://www.diak.fi/wp-content/themes/diak/dist/images/diak-logo-2.svg",
                                                             width="250",
                                                             height="77",
                                                             alt="Diakonia-ammattikorkeakoulun log")),
                                           tags$div(class = "col-sm-6 col-md-3 footer-content margin-bottom--medium",
                                                    tags$p(class = "footer-content",
                                                           tags$strong("Diakonia-ammattikorkeakoulu"), 
                                                           tags$br(),
                                                           "PL 12, 00511 Helsinki")),
                                           tags$div(class = "col-sm-6 col-md-3 footer-content margin-bottom--medium")
                                  )
                         )
                )
    ),
    
    tags$html(HTML('</main>'))
  )
}
    
#' 05etc Server Functions
#' @import gt
#'
#' @noRd 
mod_05etc_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ## muuttujaluettelot ----
    
    
    get_variable_description <- reactive({
      load(system.file("data", "muuttujakuvaukset.rda", package="karttasovellus"))
      return(muuttujakuvaukset)
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
    
    output$variable_desctiption_gt5 <- gt::render_gt({
      iv <- 5
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
    
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
