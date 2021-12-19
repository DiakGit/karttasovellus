#' 02navi UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02navi_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML('<a class="sr-only sr-only-focusable" href="#maincontent">Skip to main</a>'),
        tags$nav(class = "navbar navbar-expand-lg navbar-light navbar-xyz sticky-top grey-background container_1280", 
             tags$a(class="navbar-brand", role="brand", href = "#",
                  tags$img(src = "https://www.diak.fi/wp-content/themes/diak/dist/images/diak-logo-1.svg", 
                           style = "height: 40px; padding-right: 0px;", 
                           alt = "logo")
                           ),
                 tags$div(class = "lead", "Huono-osaisuus Suomessa"),
                         # HTML('<button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarNav" aria-controls="navbarNav" aria-expanded="false" aria-label="Avaa valikko" aria-label="Toggle navigation">
                         #          <span class="navbar-toggler-icon"></span>
                         #      </button>'),
                 HTML('<button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false" aria-label="Avaa valikko">
                          <span class="navbar-toggler-icon"></span>
                      </button>'),
                         tags$div(role = "navigation", class = "collapse navbar-collapse justify-content-between", id="navbarResponsive",
                                  tags$ul(class = "navbar-nav ml-auto",
                                          tags$li(class = "nav-item",
                                                  tags$a(class="nav-link", href="#ohje", "Ohje")
                                                  ),
                                          tags$li(class = "nav-item",
                                                  tags$a(class="nav-link", href="#indikaattorivertailu", "Indikaattorivertailu")
                                          ),
                                          tags$li(class = "nav-item",
                                                  tags$a(class="nav-link", href="#alueprofiili", "Luo alueprofiili")
                                          ),
                                          tags$li(class = "nav-item",
                                                  tags$a(class="nav-link", href="#info", "Lisätietoja")
                                          ),
                                          tags$li(class = "nav-item",
                                                  tags$a(class="nav-link", href="#saavutettavuus", "Saavutettavuusseloste")
                                          )
                                          ))
                  
                ),
                                tags$html(HTML('<main id="maincontent">')),
                tags$h2("", id = "alku"),
                tags$div(class = "container_1280", 
                         tags$div(class = "row",
                                  tags$div(class = "col-lg-8",
                                           tags$h1(id = "content", "Huono-osaisuus Suomessa"),
                                           tags$p(class = "lead",
                                                  "Sovelluksessa voit tarkastella erilaisia huono-osaisuuden osoittimia sekä luoda  profiileja alueista. Aluetasoja on kolme: maakunnat, seutukunnat ja kunnat."),
                                           tags$h2(id = "ohje", "Näin käytät sovellusta"),
                                           tags$p(
                                             tags$a(href = "#indikaattorivertailu", "Muuttujien tarkasteluun"), "ja", 
                                             tags$a(href = "#alueprofiili", "alueprofiilien laatimiseen"), 
                                             "ovat omat valikkonsa"),
                                           tags$p(
                                           tags$strong("Indikaattorivertailun"),
                                           "valitse muuttujaluokka-valikosta voit säädellä haluatko katsoa huono-osaisuuden summatietoja vai tarkemmin kunkin huono-osaisuuden ulottuvuuden osoittimia. Valitse muuttuja-valikosta voit valita tarkemman osoittimen ja valitse aluetaso-valikosta aluetason. ", tags$br(),"Voit myös valita, näkyykö pylväs-, kartta- ja aikasarjakuviossa:"
                                           ),
                                           tags$ul(
                                             tags$li("kaikki valitun tason alueet,"),
                                             tags$li("valittu alue ja sen naapurit, vai"),
                                             tags$li("valitun maakunnan/seutukunnan kunnat")
                                           ),
                                           tags$p("Pylväs- ja karttakuvio sekä aikasarjakuvio muuttuvat samanaikaisesti valinnan muuttuessa.",tags$br(),
                                                  tags$strong("Alueprofiilin"), "täytyy valita vain aluetaso sekä alue. Alueprofiili sisältää aina kaikkien osoittimien tiedot sekä valitulta alueelta että alueen rajanaapureista, sekä niiden aikasarjakuviot. Lisäksi alueprofiilissa esitetään kunkin osoittimen korkein ja matalin arvo.", tags$br(),
                                                  tags$a(href = "https://www.youtube.com/playlist?list=PLX8L6VZCYbFIzAqc4InxFEbP0SmfOrDzL", "Katso opastusvideot sivuston käytöstä!")),
                                           tags$p("Jos haluat viitata karttasivustoon tekstissäsi, käytä lähdeviitteenä:",
                                                  tags$em("Diakonia-ammattikorkeakoulu (i.a.)"),"ja lähdeluettelossa:", 
                                                  tags$em("Diakonia-ammattikorkeakoulu (i.a.) Huono-osaisuus Suomessa –karttasivusto. Saatavilla pp.kk.vuosi www.diak.fi/eriarvoisuus")),
                                           tags$p("Indikaattorit ja muuttujat on muodostettu vuosien 2017-2019 tilastojen keskiarvosta vuosivaihtelun minimoimiseksi, ja suhteuttamalla arvo keskimmäiseen eli mediaanimaakuntaan/-seutukuntaan/-kuntaan vertailun helpottamiseksi. Aikasarjoissa lukemat vuodesta 2010 alkaen kolmen vuoden liukuvina keskiarvoina.",
                                                  tags$a(href = "https://www.diak.fi/kumppanille-ja-kehittajalle/kehittamistyokalut/huono-osaisuus-suomessa-karttasivusto/#73053a2b", 
                                                         "Lue lisää Diakin sivuilta!"))
                                           ),
                                  tags$div(class = "col-lg-4",
                                           tags$div(style = "padding-top: 20px;"),
                                                    tags$div(class = "col-sm",
                                                             tags$a(href = "https://ec.europa.eu/regional_policy/fi/funding/erdf/", 
                                                                    tags$img(src = "www/app_eu.png",
                                                                             class="rounded mx-auto d-block",
                                                                             style="height: 160px;",
                                                                             alt = "Euroopan unionin lippu, jonka alla teksti: Euroopan unioni, Euroopan aluekehitysrahasto"))),
                                                             tags$a(href = "https://www.rakennerahastot.fi", 
                                                                    tags$img(src = "www/app_vipu.png", 
                                                                             class="rounded mx-auto d-block",
                                                                             style="height: 90px; padding-top: 20px;",
                                                                             alt = "Rakennerahastot.fi -verkkopalvelun logo")),
                                                             tags$a(href = "https://thl.fi/fi/web/hyvinvoinnin-ja-terveyden-edistamisen-johtaminen/osallisuuden-edistaminen/heikoimmassa-asemassa-olevien-osallisuus", 
                                                                    tags$img(src = "www/app_sokra.png", 
                                                                             class="rounded mx-auto d-block",
                                                                             style="height: 100px;padding-top: 20px;", 
                                                                             alt = "Sosiaalisen osallisuuden edistämisen koordinaatiohankeen Sokran logo")),
                                                                    tags$a(href = "https://tutkittutieto.fi", 
                                                                    tags$img(src = "www/app_diak.png", 
                                                                             class="rounded mx-auto d-block",
                                                                             style="height: 80px;padding-top: 20px;", 
                                                                             alt = "Diakonia-ammattikorkeakoulun logo")),
                                                             tags$a(href = "https://tutkittutieto.fi", 
                                                                    tags$img(src = "www/app_teema.jpg", 
                                                                             class="rounded mx-auto d-block",
                                                                             style="height: 110px; padding-top: 20px;", 
                                                                             alt = "Tutkitun tiedon teemavuoden logo"))
                                  )))
  )
}
    
#' 02navi Server Functions
#'
#' @noRd 
mod_02navi_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
    
## To be copied in the server

