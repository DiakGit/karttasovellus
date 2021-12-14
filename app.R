source("./global.R")


ui <- fluidPage(lang = "fi",
                title = "Tilastot kartalle",
                tags$head(
                  tags$link(rel="shortcut icon", href="favicon.ico"),
                  tags$link(rel="stylesheet", href="custom.css")
                  ),
                meta() %>% 
                    meta_description(description = "DIAK: Huono-osaisuus Suomessa -verkkosovellus") %>% 
                    meta_social(
                        title = "DIAK: Huono-osaisuus Suomessa -verkkosovellus",
                        description = "Sovelluksessa voit tarkastella erilaisia huono-osaisuuden osoittimia sekä luoda profiileja alueista",
                        url = "",
                        image = "logo_650x650.jpg",
                        image_alt = "An image for social media cards",
                        twitter_creator = "@muuankarski",
                        twitter_card_type = "summary_large_image",
                        twitter_site = "@muuankarski"
                    ),
                theme = bslib::bs_theme(bootswatch = "cosmo",
                                        # bg = "#0b3d91", fg = "white", primary = "#FCC780",
                                        base_font = font_google("PT Sans"),
                                        code_font = font_google("Space Mono")),
                tags$html(HTML('<a class="sr-only sr-only-focusable" href="#maincontent">Skip to main</a>')),
                tags$html(HTML('
    <nav class="navbar navbar-light navbar-xyz sticky-top grey-background container_1280">
      <a class="navbar-brand" role="brand" href = "#"><img src = "https://www.diak.fi/wp-content/themes/diak/dist/images/diak-logo-1.svg" style = "height: 40px; padding-right: 0px;" alt = "logo"></a>
      <div class = "lead">Huono-osaisuus Suomessa</div>
      <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false" aria-label="Avaa valikko">
        <span class="navbar-toggler-icon"></span>
      </button>
      <div role = "navigation" class="collapse navbar-collapse justify-content-between" id="navbarResponsive">
        <ul class="navbar-nav ml-auto">
          <li class="nav-item">
            <a class="nav-link" href="#ohje">Ohje</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#indikaattorivertailu">Indikaattorivertailu</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#alueprofiili">Luo alueprofiili</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#info">Lisätietoja</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="#saavutettavuus">Saavutettavuusseloste</a>
          </li>
        </ul>
      </div>
  </nav>')),
                tags$html(HTML('<main id="maincontent">')),
                tags$h2("", id = "alku"),
                fluidRow(column(width = 12,
                tags$html(HTML('
  <div class="container_1280">
    <div class="row">
      <div class="col-lg-8">

  <h1 id = "content">Huono-osaisuus Suomessa</h1>                
                
  <p class="lead">Sovelluksessa voit tarkastella erilaisia huono-osaisuuden osoittimia sekä luoda  profiileja alueista. Aluetasoja on kolme: maakunnat, seutukunnat ja kunnat.</p>
  <h2 id = "ohje">Näin käytät sovellusta</h2>
  
   <p><a href="#indikaattorivertailu">Muuttujien tarkasteluun</a> ja 
   <a href="#alueprofiili">alueprofiilien laatimiseen</a> ovat omat valikkonsa.</p>
   
  <strong>Indikaattorivertailun</strong> valitse muuttujaluokka-valikosta voit säädellä haluatko katsoa huono-osaisuuden summatietoja vai tarkemmin kunkin huono-osaisuuden ulottuvuuden osoittimia. 
  Valitse muuttuja-valikosta voit valita tarkemman osoittimen ja 
  valitse aluetaso-valikosta aluetason.<br/>
  Voit myös valita, näkyykö pylväs-, kartta- ja aikasarjakuviossa: 
  <ul>
    <li>kaikki valitun tason alueet,</li> 
    <li>valittu alue ja sen naapurit, vai</li> 
    <li>valitun maakunnan/seutukunnan kunnat</li>
  </ul> 
  <br/> 
  Pylväs- ja karttakuvio sekä aikasarjakuvio muuttuvat samanaikaisesti valinnan muuttuessa.
  <br/>
  <strong>Alueprofiilin</strong> täytyy valita vain aluetaso sekä alue. Alueprofiili sisältää aina kaikkien osoittimien tiedot sekä valitulta alueelta että alueen 
  rajanaapureista, sekä niiden aikasarjakuviot. Lisäksi alueprofiilissa esitetään kunkin osoittimen korkein ja matalin arvo.
    <br/>
    <p><a href= "https://www.youtube.com/playlist?list=PLX8L6VZCYbFIzAqc4InxFEbP0SmfOrDzL">Katso opastusvideot sivuston käytöstä!</a></p>
  Jos haluat viitata karttasivustoon tekstissäsi, käytä lähdeviitteenä:
    <em>Diakonia-ammattikorkeakoulu (i.a.)</em> ja lähdeluettelossa: <em>Diakonia-ammattikorkeakoulu (i.a.) Huono-osaisuus Suomessa –karttasivusto. Saatavilla pp.kk.vuosi www.diak.fi/eriarvoisuus</em>
  <br/>

  <p>Indikaattorit ja muuttujat on muodostettu vuosien 2017-2019 tilastojen keskiarvosta vuosivaihtelun minimoimiseksi, ja suhteuttamalla arvo keskimmäiseen eli mediaanimaakuntaan/-seutukuntaan/-kuntaan vertailun helpottamiseksi. Aikasarjoissa lukemat vuodesta 2010 alkaen kolmen vuoden liukuvina keskiarvoina.
    <a href="https://www.diak.fi/kumppanille-ja-kehittajalle/kehittamistyokalut/huono-osaisuus-suomessa-karttasivusto/#73053a2b">Lue lisää Diakin sivuilta!</a>
    </p>
  </div>                            
                               
<div class="col-lg-4">
  <div style = "padding-top: 20px;"></div>
    <div class="row">
    <div class="col-sm">
      <a href = "https://ec.europa.eu/regional_policy/fi/funding/erdf/"><img src="app_eu.png" style="height: 160px;" alt = "Euroopan unionin lippu, jonka alla teksti: Euroopan unioni, Euroopan aluekehitysrahasto"/></a>
      </div>
      <div class="col-sm">
      </div>
        </div>
        <div style = "padding-top: 20px;"></div>
  <div class="row">
      <div class="col-sm">
              <a href = "https://www.rakennerahastot.fi"><img src="app_vipu.png" style="height: 62px; padding-right: 5px;" alt = "Rakennerahastot.fi -verkkopalvelun logo"/></a>  
              <a href = "https://thl.fi/fi/web/hyvinvoinnin-ja-terveyden-edistamisen-johtaminen/osallisuuden-edistaminen/heikoimmassa-asemassa-olevien-osallisuus"><img src="app_sokra.png" style="height: 62px;" alt = "Sosiaalisen osallisuuden edistämisen koordinaatiohankeen Sokran logo"/></a>
      <!--/div>
            <div class="col-sm"-->
        <a href = "https://www.diak.fi"><img src="app_diak.png" style="height: 62px;" alt = "Diakonia-ammattikorkeakoulun logo"/></a>
      </div>
      </div>
      <div style = "padding-bottom: 20px;"></div>
       <a href = "https://tutkittutieto.fi"><img src="app_teema.jpg" style="height: 100px;" alt = "Tutkitun tiedon teemavuoden logo"/></a>
      </div>
</div>
</div>')))),
tags$div(class = "container_1280 grey-background",
         tags$div(class = "row",
                  tags$div(class = "col-lg-3",
                                    tags$h2(id = "indikaattorivertailu", "Indikaattorivertailu"),
                                    uiOutput("output_indicator_class"),
                                    uiOutput("output_indicator"),
                                    uiOutput("output_regio_level"),
                                    uiOutput("output_regio_select"),
                                    uiOutput("output_regio_show_mode"),
                                    uiOutput("output_save_data_indicator")
                                    ),
                  tags$div(class = "col-lg-9",
                                    plotOutput("map_rank_plot", width = "100%", height = "800px")
                           )
                  ),
tags$div(class = "row",
         tags$div(class = "col-lg-12",
                  plotOutput("timeseries_plot", width = "100%", height = "550px")
         )
)),
tags$div(style="padding-top: 40px"),

tags$div(class = "container_1280 grey-background",
         tags$div(class = "row",
                  tags$div(class = "col-lg-12",
                           tags$div(style="padding-top: 40px"),
                           tags$h2(id = "alueprofiili", "Alueprofiili"),
                           tags$p("Alueprofiilissa näet kaikki aineiston osoittimet luokan mukaan ryhmiteltynä. Kustakin osoittimesta näytetään valitun alueen lisäksi sen rajanaapurit sekä  osoittimen korkeimman ja matalimman arvon alueet. Alueet on järjestetty kunkin osoittimen kohdalla sijan mukaan."),
                           tags$p("Valitse ensin aluetaso, sitten alue ja paina lopuksi <i>Luo alueprofiili</i> -painiketta. Alueprofiilin luominen kestää noin 30 sekuntia. Voit tallentaa profiilin laitteellesi")
                  )),
         tags$div(class = "row",
                  tags$div(class = "col-lg-6",
                           uiOutput("output_regio_level_profile")
                           ),
                  tags$div(class = "col-lg-6",
                           uiOutput("output_region_profile")
                  )
                  ),
tags$div(class = "row",
         tags$div(class = "col-lg-12",
                  uiOutput("output_button_profile")
                  )
         ),
tags$div(class = "row",
         tags$div(class = "col-lg-12",
                  uiOutput("region_profile_html")
         )
)),
tags$div(class="container_1280",
         tags$div(class="row",
                  tags$div(class = "col-lg-12",
                           tags$h2(id = "info", "Lisätietoja"),
                           tags$h3("Aineisto"),
                           tags$h4("Summamuttujat"),
                           gt_output("variable_desctiption_gt1"),
                           tags$h4("Inhimillinen huono-osaisuus"),
                           gt_output("variable_desctiption_gt2"),
                           tags$h4("Huono-osaisuuden taloudelliset seuraukset"),
                           gt_output("variable_desctiption_gt3"),
                           tags$h4("Huono-osaisuuden sosiaaliset seuraukset"),
                           gt_output("variable_desctiption_gt4")
                           )          
          )),

                tags$html(HTML('
  <div class="container_1280">
    <div class="row">
      <div class="col-lg-8">
    
    <h3>Sovellus</h3>
    
    <strong>Huono-osaisuus Suomessa -verkkosovellus</strong>
    <p>Sovellusversio
    <code>0.3.0</code><br/>
    Aineistoversio
    <code>0.2.0</code>
    </p>
    <p>Tämä verkkosovellus on tehty
    <a href="https://www.r-project.org/">R</a>-kielellä
    <a href="https://shiny.rstudio.com">Shiny</a>-kirjaston avulla. 
    Sovelluksen lähdekoodi on avoimesti lisensöity ja saatavilla 
    <a href="https://github.com/DiakGit/karttasovellus">Github</a>-palvelusta.</p>
    
    <p>Mikäli löysit sovelluksesta bugin tai sinulla on idea tai toteutus uudesta ominaisuudesta voit:</p>
    <ul>
    <li>
    toteuttaa ominaisuuden/korjauksen ja jättää
    <a href="https://github.com/DiakGit/karttasovellus/pulls">pull requestin</a>Github-palvelussa,
    </li>
    <li>
    avata uuden <a href="https://github.com/DiakGit/karttasovellus/issues">issuen</a> Github-palvelussa 
    ja kuvata bugin/ominaisuuden siinä tai
    </li>
    <li>
    laittaa meiliä osoitteeseen 
    <a href="mailto:sakari.kainulainen@diak.fi"><code>sakari.kainulainen@diak.fi</code></a> tai
    </li>
    <li>
      laittaa palautetta lomakkeella <a href = "https://www.diak.fi/kumppanille-ja-kehittajalle/kehittamistyokalut/huono-osaisuus-suomessa-karttasivusto/#73053a2b">Diakin sivuilta</a>
    </li>
    </ul>
    
    </div>  
</div>      
    </div>      


  <div class="container_1280">
    <div class="row">
      <div class="col-lg-8">

    <h2 id = "saavutettavuus">Saavutettavuusseloste</h2>
    
    <p>
    <i>Saavutettavuudella</i> tarkoitetaan, että verkkosivut ja mobiilisovellukset sekä niiden sisällöt ovat sellaisia,
    että kuka tahansa voisi niitä käyttää ja ymmärtää mitä niissä sanotaan.
    </p>
    <p>Diakin verkkosivuja koskee laki digitaalisten palvelujen tarjoamisesta, joka pohjautuu  EU:n saavutettavuusdirektiiviin. 
    Lain mukaan kaikilla tulee olla tasavertaiset mahdollisuudet käyttää digitaalisia palveluita.</p>
    <p>Tämä saavutettavuusseloste koskee <a href="https://www.thl.fi/sokra">Sokra-hankkeessa</a>
    toteutettu <a href="https://diak.shinyapps.io/karttasovellus/">karttasivusto</a> on julkaistu 23.4.2020,
    joka tarkoittaa sitä, että sivuston tulee noudattaa WCAG 2.1 –kriteeristön AA-tasoa 23.9.2020 mennessä.
    </p>
    <h3>Digipalvelun saavutettavuuden tila</h3>

    <p> <a href ="https://diak.shinyapps.io/karttasovellus/">Karttasivusto</a> 
    täyttää pääosin WCAG 2.1 -kriteeristön A & AA -saavutettavuusvaatimukset.</p>
    <p>
      
    <h3>Saavutettavuusselosteen laatiminen</h3>
    <p>Karttasivustoa koskeva seloste on laadittu 19.9.2020 ja sitä on päivitetty 3.11.2020. 
    Seloste perustuu itsearvioon, Poutapilvi Oy:n tekemään auditointiraporttiin sekä 
    seuraavilla testausohjelmistoilla suoritettuun tarkastukseen.</p>
    
    <strong>Ruudunlukijat</strong>
    <ul>
      <li><a href="https://www.nvaccess.org/">NVDA</a></li>
      <li><a href="https://wiki.gnome.org/Projects/Orca">Orca</a></li>
      <li><a href="https://www.chromevox.com/">ChromeVox</a></li>

    </ul>
        <strong>Saavutettavuuden tarkastuohjelmat</strong>
    <ul>
      <li><a href="https://ainspector.github.io/">AInspector</a></li>
      <li> <a href="https://thepaciellogroup.github.io/WAI-ARIA-Usage/WAI-ARIA_usage.html">WAI-ARIA Usage</a></li>
      <li><a href="https://validator.w3.org/nu/">Nu Html Checker</a></li>
      <li><a href="https://developer.paciellogroup.com/resources/contrastanalyser/">Colour Contrast Analyser</a></li>
      <li><a href="https://wave.webaim.org/">WAVE Web Accessibility Evaluation Tool</a></li>
      
      

    </ul>
    
    <p>Näiden arvioiden ja ulkopuolisen raportin perusteella karttasivuston ensimmäiseen versioon 
    tehtiin lukuisia saavutettavuutta parantavia korjauksia.</p>
    
    <h3>Palautetta saavutettavuudesta?</h3>
    <p>Huomasitko saavutettavuuspuutteen digipalvelussamme? Kerro se meille ja eemme parhaamme puutteen korjaamiseksi
    </p>
    <p>
    Ilmoita puute tällä <a href="https://www.diak.fi/diak/anna-palautetta/">verkkolomakkeella</a>. 
    Valitse kohta verkkosivuston saavutettavuus. Kerro palautteessa, mitä Diakin hallinnoimaa sivustoa palaute koskee.
    </p>
    <p>
      Jos huomaat sivustolla saavutettavuusongelmia, anna ensin palautetta sivuston ylläpitäjälle. 
      Vastauksessa voi mennä 14 päivää. Jos et ole tyytyväinen saamaasi vastaukseen tai 
      et saa vastausta lainkaan kahden viikon aikana, voit ilmoittaa asiasta valvontaviranomaiselle.
    </p>
    <strong>Valvontaviranomaisen yhteystiedot:</strong>
    <p>
      Etelä-Suomen aluehallintovirasto<br/>
      Saavutettavuuden valvonnan yksikkö<br/>
      www.saavutettavuusvaatimukset.fi<br/>
      saavutettavuus(at)avi.fi<br/>
      puhelinnumero vaihde 0295 016 000
    </p>

        </div>
    </div>
  </div>

  </main>              
                
                
')),


tags$html(HTML('
               <footer class="bd-footer py-5 mt-5 bg-dark-diak container_1280">
	<div class="tp-bg-2 padding-top--medium text--small content--hyphenate tp-links-1 cf">
		<div class="container-fluid">
			<div class="row">
				<div class="col-sm-6 col-md-3 footer-content margin-bottom--medium" aria-hidden="true">
					<img src="https://www.diak.fi/wp-content/themes/diak/dist/images/diak-logo-2.svg" width="250" height="77" loading="lazy" alt="" class="image--block" />
				</div>
															<div class="col-sm-6 col-md-3 footer-content margin-bottom--medium">
							<p><strong>Diakonia-ammattikorkeakoulu</strong><br />
PL 12, 00511 Helsinki<br />
<a class = "footer-content" href="https://www.diak.fi/diak/kampukset/">Kampusten postiosoitteet</a></p>
<p>Vaihde: 029 469 6000<br />
<a class = "footer-content" href="mailto:kirjaamo@diak.fi">kirjaamo@diak.fi</a><br />
<a class = "footer-content" href="mailto:etunimi.sukunimi@diak.fi">etunimi.sukunimi@diak.fi</a><br />
<a class = "footer-content" href="https://www.diak.fi/diak/yhteystiedot/">Yhteystiedot</a><br />
<a class = "footer-content" href="https://www.diak.fi/diak/yhteystiedot/henkilohaku/" target="_blank" rel="noopener">Henkilöhaku</a></p>
<p><a class = "footer-content" href="https://www.diak.fi/diak/organisaatio/saavutettavuusseloste/" rel="noopener">Saavutettavuus</a><br />
<a class = "footer-content" href="https://www.diak.fi/diak/organisaatio/tietosuoja/">Tietosuoja</a><br />
<a class = "footer-content" href="https://www.diak.fi/diak/anna-palautetta/">Palaute</a></p>
						</div>
											<div class="col-sm-6 col-md-3 footer-content margin-bottom--medium">
							<p><strong>Diakilaiselle:<br />
</strong><a class = "footer-content" href="https://my.diak.fi/" target="_blank" rel="noopener">MyDiak</a><br />
<a class = "footer-content" href="http://lukujarjestykset.diak.fi/" target="_blank" rel="noopener">Lukujärjestykset</a><br />
<a class = "footer-content" href="https://portal.office.com/" target="_blank" rel="noopener">Office365</a> (sähköposti)<br />
<a class = "footer-content" href="https://diakle.diak.fi/login/index.php" target="_blank" rel="noopener">Diakle</a><br />
<a class = "footer-content" href="https://www.diak.fi/opiskelu/tuki-ja-palvelut/it-palvelut/">IT-palvelut</a><br />
<a class = "footer-content" href="https://www.diak.fi/diak/kirjasto/">Kirjasto- ja tietopalvelut</a><br />
<a class = "footer-content" href="https://app.incy.io/login/saml" target="_blank" rel="noopener">Incy-poikkeamaraportointi</a><br />
<a class = "footer-content" href="https://www.jobiili.fi/" target="_blank" rel="noopener">Jobiili</a><br />
<a class = "footer-content" href="http://www.odiako.fi" target="_blank" rel="noopener">Opiskelijakunta O&#8217;Diako</a><br />
<a class = "footer-content" href="https://dialogi.diak.fi/">Dialogi-media</a></p>
<p><a class = "footer-content" href="https://vuosikatsaus2020.diak.fi/" target="_blank" rel="noopener">Vuosikatsaus</a></p>
						</div>
													<div class="col-sm-6 col-md-3 footer-content margin-bottom--medium">
					<p><strong>Seuraa meitä:</strong></p>
											<a class = "footer-content" href="https://www.diak.fi/tilaa-uutiskirjeita/" class="link--block footer__link-block margin-bottom">
							<svg xmlns="http://www.w3.org/2000/svg" width="26" height="21" viewBox="0 0 26 21" aria-hidden="true" focusable="false"><path fill="#FFF" fill-rule="evenodd" d="M26 6.587v11.52a2.328 2.328 0 01-2.321 2.322H2.32A2.328 2.328 0 010 18.107V6.587c.435.479.929.9 1.465 1.262 2.409 1.64 4.846 3.28 7.211 5.006 1.22.9 2.728 2.002 4.31 2.002h.029c1.581 0 3.09-1.103 4.309-2.002 2.365-1.712 4.802-3.366 7.225-5.006A8.546 8.546 0 0026 6.587zm0-4.266c0 1.625-1.204 3.09-2.481 3.976-2.263 1.567-4.541 3.134-6.79 4.715-.943.653-2.54 1.988-3.714 1.988h-.03c-1.175 0-2.77-1.335-3.714-1.988-2.249-1.581-4.527-3.148-6.775-4.715C1.466 5.6 0 3.96 0 2.64 0 1.219.769 0 2.321 0H23.68A2.338 2.338 0 0126 2.321z"/></svg>							Tilaa uutiskirje						</a>
										<p>
						Diak somessa:<br />
						<a class = "footer-content" href="https://twitter.com/diakamk" rel="noopener nofollow" class="link-effect-1 link--some">
							<svg xmlns="http://www.w3.org/2000/svg" width="25" height="20" viewBox="0 0 25 20" aria-hidden="true" focusable="false"><path fill="#FFF" fill-rule="evenodd" d="M22.11 4.907c.015.216.015.43.015.645 0 6.57-5.081 14.136-14.369 14.136-2.861 0-5.52-.812-7.756-2.23.406.047.798.063 1.22.063 2.36 0 4.534-.786 6.27-2.124-2.22-.046-4.082-1.477-4.722-3.445a6.5 6.5 0 00.955.078 5.4 5.4 0 001.328-.17c-2.315-.461-4.05-2.461-4.05-4.877v-.062a5.102 5.102 0 002.283.632 4.94 4.94 0 01-2.252-4.136c0-.926.252-1.77.688-2.51 2.486 3.015 6.223 4.985 10.413 5.199a5.579 5.579 0 01-.124-1.137C12.01 2.23 14.26 0 17.058 0c1.454 0 2.768.602 3.69 1.568A10.014 10.014 0 0023.955.37a4.976 4.976 0 01-2.222 2.737 10.224 10.224 0 002.909-.77 10.715 10.715 0 01-2.533 2.57"/></svg>							<span class="screen-reader-text">Twitter</span>
						</a>
						<a class = "footer-content" href="https://www.facebook.com/diakamk" rel="noopener nofollow" class="link-effect-1 link--some">
							<svg xmlns="http://www.w3.org/2000/svg" width="13" height="24" viewBox="0 0 13 24" aria-hidden="true" focusable="false"><path fill="#FFF" fill-rule="evenodd" d="M12.54 3.886h-2.279c-1.783 0-2.116.832-2.116 2.028v2.66h4.25l-.565 4.166H8.145v10.688H3.703V12.74H0V8.575h3.703V5.506C3.703 1.944 5.952 0 9.233 0c1.566 0 2.915.113 3.308.17v3.716z"/></svg>							<span class="screen-reader-text">Facebook</span>
						</a>
						<a class = "footer-content" href="https://www.instagram.com/diakamk" rel="noopener nofollow" class="link-effect-1 link--some">
							<svg xmlns="http://www.w3.org/2000/svg" width="23" height="23" viewBox="0 0 23 23" aria-hidden="true" focusable="false"><path fill="#FFF" fill-rule="evenodd" d="M19.759 3.482c0-.552-.45-1-1.003-1h-2.523a1 1 0 00-1 1v2.393c0 .55.448 1 1 1h2.523c.552 0 1.003-.45 1.003-1V3.482zm-8.604 3.279c-2.465 0-4.468 1.942-4.468 4.338 0 2.393 2.003 4.337 4.468 4.337 2.482 0 4.483-1.944 4.483-4.337 0-2.396-2.001-4.338-4.483-4.338zm8.604 2.668h-1.96c.188.595.29 1.247.29 1.901 0 3.712-3.105 6.716-6.934 6.716-3.814 0-6.92-3.004-6.92-6.716 0-.654.103-1.306.291-1.901H2.48v9.4c0 .492.392.886.885.886h15.508a.881.881 0 00.886-.885V9.429zm2.523 9.994a2.872 2.872 0 01-2.857 2.859H2.86A2.873 2.873 0 010 19.423V2.86A2.873 2.873 0 012.859 0h16.566a2.872 2.872 0 012.857 2.859v16.564z"/></svg>							<span class="screen-reader-text">Instagram</span>
						</a>
						<a class = "footer-content" href="https://www.youtube.com/channel/UCFRCstrYYjAzHrmHi1fuOIg" rel="noopener nofollow" class="link-effect-1 link--some">
							<svg xmlns="http://www.w3.org/2000/svg" width="23" height="17" viewBox="0 0 23 17" aria-hidden="true" focusable="false"><path fill="#FFF" fill-rule="evenodd" d="M9.126 11.064l6.212-3.21-6.212-3.246v6.456zM11.5 0c4.839 0 8.047.231 8.047.231.45.051 1.438.051 2.31.975 0 0 .707.694.912 2.285.244 1.861.23 3.722.23 3.722V8.96s.014 1.86-.23 3.722c-.205 1.578-.911 2.284-.911 2.284-.873.912-1.861.912-2.31.963 0 0-3.21.244-8.048.244-5.981-.052-7.816-.231-7.816-.231-.514-.09-1.669-.064-2.542-.976 0 0-.706-.706-.91-2.284C-.014 10.82 0 8.959 0 8.959V7.213s-.013-1.86.231-3.722c.205-1.591.911-2.285.911-2.285.873-.924 1.861-.924 2.31-.975 0 0 3.21-.231 8.048-.231z"/></svg>							<span class="screen-reader-text">YouTube</span>
						</a>
						<a class = "footer-content" href="https://www.linkedin.com/school/4284627" rel="noopener nofollow" class="link-effect-1 link--some">
							<svg xmlns="http://www.w3.org/2000/svg" width="23" height="23" viewBox="0 0 23 23" aria-hidden="true" focusable="false"><path fill="#FFF" fill-rule="evenodd" d="M3.439 18.659H6.79V8.589H3.44v10.07zM7.008 5.484c-.015-.986-.726-1.74-1.872-1.74-1.146 0-1.9.754-1.9 1.74 0 .958.725 1.741 1.857 1.741h.014c1.175 0 1.9-.783 1.9-1.74zm4.976 4.525v.046h-.029l.03-.046zm0 0v-1.42H8.633s.043.943 0 10.07h3.351v-5.63c0-.29.015-.595.102-.812.247-.595.798-1.22 1.727-1.22 1.204 0 1.683.915 1.683 2.279v5.383h3.351v-5.775c0-3.09-1.654-4.527-3.86-4.527-1.76 0-2.561.958-3.003 1.652zm10.302-5.83v13.928a4.18 4.18 0 01-4.179 4.179H4.18A4.18 4.18 0 010 18.107V4.18A4.18 4.18 0 014.179 0h13.928a4.18 4.18 0 014.179 4.179z"/></svg>							<span class="screen-reader-text">LinkedIn</span>
						</a>
					</p>
											<p>
							<a class = "footer-content" href="https://www.3inalliance.eu" class="link-effect-1 no-arrow" lang="en">
								<span class="screen-reader-text">
									Member of 3IN Alliance								</span>
								<img src="https://www.diak.fi/wp-content/themes/diak/dist/images/logo-3inalliance-01.svg" width="254" height="70" loading="lazy" alt="" class="image--block" />
							</a>
						</p>
									</div>
			</div>
		</div>
	</div></footer>
               ')),
tags$html(HTML('</main>'))
)

# Serverin logiikka ----
server <- function(input, output) {
    
    ## datat ----
    
    get_dat <- reactive({
        # dat <- readRDS("./data/df_v20201102.RDS")  
        dat <- readRDS("./data/df_v20211104.RDS")
        return(dat)
    })
    
    get_dat_timeseries <- reactive({
        dat_aika <- readRDS("./data/df_v20211104_aikasarja.RDS")
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
        
        if (input$value_variable_class == "Summamuuttujat"){
            opt_indicator2 <- factor(opt_indicator2, levels = c("Huono-osaisuus yhteensä",
                                                                "Huono-osaisuuden sosiaaliset seuraukset", 
                                                                "Huono-osaisuuden taloudelliset yhteydet", 
                                                                "Inhimillinen huono-osaisuus"))
        } else {
            opt_indicator2 <- factor(opt_indicator2)
        }
        
        # if (input$sidebar_menu == "info"){
        #     tagList()
        # } else {
        tagList(
            selectInput(inputId = "value_variable", 
                        label = "Valitse muuttuja", 
                        choices = opt_indicator2
                        ,selected = levels(opt_indicator2)[1],
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
        opt_indicator <- factor(opt_indicator, levels = c("Hyvinvointialueet","Seutukunnat","Kunnat"))
        opt_indicator <- sort(opt_indicator)
        
        # if (input$sidebar_menu == "info"){
        #     tagList()
        # } else {
        tagList(
            radioButtons(inputId = "value_regio_level", 
                         label = "Valitse aluetaso", inline = FALSE,
                         choices = opt_indicator, selected = "Hyvinvointialueet")
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
        opt_indicator <- factor(opt_indicator, levels = c("Hyvinvointialueet","Seutukunnat","Kunnat"))
        opt_indicator <- sort(opt_indicator)
        
        tagList(
            radioButtons(inputId = "value_regio_level_profile", 
                         label = "Valitse aluetaso", inline = FALSE,
                         choices = opt_indicator, selected = "Hyvinvointialueet")
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
        
        if (regio_level == "Hyvinvointialueet"){
            muni_key_subset <- geofi::municipality_key_2019 %>% 
                filter(hyvinvointialue_name_fi == aluenimi) %>% 
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
    ## indikaattorikuvioiden_datan_lataus ----
    ## 
    ## 
    output$save_data_indicator <- downloadHandler(
        
        filename = function() {
            file_name <- glue("data_{janitor::make_clean_names(input$value_variable)}_{tolower(input$value_regio_level)}.csv")
            return(file_name)
        },
        content = function(file) {
            
            dat <- get_dat_timeseries()
            region_data <- get_region_data()
            naapurikoodit_lst <- region_data[region_data$level %in% input$value_regio_level & 
                                                 region_data$region_name %in% input$value_region_selected,"neigbours"]
            
            naapurikoodit <- naapurikoodit_lst %>% 
                unnest(cols = c(neigbours)) %>% 
                pull(neigbours)
            
            
            df <- dat[dat$variable == input$value_variable &
                          dat$regio_level == input$value_regio_level,]
            
            readr::write_excel_csv2(x = df, file = file)
        }
    )
    
    output$output_save_data_indicator <- renderUI({
        
        req(input$value_variable)
        tagList(
            downloadButton("save_data_indicator", "Tallenna data csv-muodossa!", class="btn btn-dark"),
        )
    })
    
    
    
    
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
            geom_col(fill = "#253494") +
            theme_ipsum(base_family = "PT Sans",
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans",
                        grid_col = "white") +
            xlim(c(0,max(dat$value, na.rm = TRUE)*1.2)) +
            theme(legend.position = "right",
                  plot.title.position = "plot") +
            # scale_fill_ipsum() +
            # scale_fill_viridis_c(option = "viridis", direction = -1, alpha = .4, na.value="grey90") +
            # scale_fill_fermenter(palette = "YlGnBu") +
            scale_color_manual(values = c("grey80","black")) + 
            geom_col(aes(color = color), fill = NA, show.legend = FALSE) -> plot
        
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
        } else if (input$value_regio_level == "Hyvinvointialueet"){
            plot <- plot + geom_text(aes(label = value), 
                                     color = "black", 
                                     nudge_x = max(dat$value, na.rm = TRUE)*0.1, 
                                     family = "PT Sans")
        }
        plot + scale_y_discrete(expand = expansion(add = 2)) +
            labs(x = NULL, y = NULL, fill = NULL) -> p1
        
        
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
        
        
        
        
        # ggplot(data = dat, aes(fill = value, color = color)) +
        ggplot(data = dat, aes(fill = value)) +
            geom_sf(color = alpha("white", 1/3))  +
            geom_sf(aes(color = color), fill = NA, show.legend = FALSE)  +    
            # scale_fill_viridis(option = "viridis", direction = -1, alpha = .5) +
            scale_fill_fermenter(palette = "YlGnBu", type = "seq") +
            scale_color_manual(values = c(alpha("white", 1/3), "black")) +
            theme_minimal(base_family = "PT Sans", base_size = 12) -> p
        
        if (input$value_regio_show_mode == "kaikki tason alueet"){
            if (input$value_regio_level != "Kunnat"){
                p + geom_sf_label(data = dat %>% filter(!aluenimi %in% input$value_region_selected), 
                                  aes(label = value), 
                                  family = "PT Sans", 
                                  color = "black", 
                                  fill = "white", 
                                  size = 2.5) -> p
            }
            p <- p + geom_sf_label(data = dat %>% filter(aluenimi == input$value_region_selected),
                                   aes(label = paste(aluenimi, value)),
                                   fill = "white", color = "black", family = "PT Sans")
            
        } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            p + geom_sf_label(data = dat,
                              aes(label = paste(aluenimi, value)),
                              fill = "white", color = "black", family = "PT Sans") -> p
        } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            p + ggrepel::geom_label_repel(data = dat %>%
                                              sf::st_set_geometry(NULL) %>%
                                              bind_cols(dat %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                                          aes(label = paste0(aluenimi,"\n", value), x = X, y = Y),
                                          color = "black", fill = alpha("white", 2/3),
                                          family = "PT Sans", size = 3, lineheight = .8) -> p
            
        }
        p + theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  panel.grid = element_blank(),
                  # legend.position = "left",
                  legend.position = c(0.1, 0.5),
                  plot.title.position = "plot") +
            labs(fill = paste0(add_line_break2(input$value_variable, 20), "\n(suhdeluku)")) -> p2
        
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
        
        naapurikoodit <- naapurikoodit_lst %>% 
            unnest(cols = c(neigbours)) %>% 
            pull(neigbours)
        
        
        df <- dat[dat$variable == input$value_variable &
                      dat$regio_level == input$value_regio_level,] #%>% 
        # mutate(color = ifelse(aluenimi %in% input$value_region_selected, TRUE, FALSE))
        df_gini <- df %>% group_by(aika) %>% 
            mutate(gini = round(ineq::Gini(value),2)) %>% 
            ungroup()
        
        df2 <- df[df$aluekoodi %in% naapurikoodit,] %>% 
            filter(!aluenimi %in% input$value_region_selected)
        
        
        aika1 <- sort(unique(df$aika)) - 1
        aika2 <- sort(unique(df$aika)) + 1
        labels <- paste0(aika1,"-",aika2)
        
        ggplot() -> plot0
        
        df_selected_cs <- df %>% 
            filter(aika == max(aika, na.rm = TRUE),
                   aluenimi == input$value_region_selected)
        
        df_selected_ts <- df %>% 
            filter(aluenimi == input$value_region_selected)
        
        if (input$value_regio_show_mode == "kaikki tason alueet"){
            
            plot0 <- plot0 + 
                geom_line(data = df, aes(x = aika, y = value, color= aluenimi, fill= aluenimi), show.legend = FALSE) +
                geom_line(data = df_selected_ts, aes(x = aika, y = value), color = "black") +
                geom_point(data = df_selected_ts, aes(x = aika, y =  value),
                           fill = "black", 
                           color = "white", shape = 21, size = 2.5, stroke = 1) +
                # geom_point(shape = 21, color = "white", stroke = 1, size = 2.5) +
                # ggrepel::geom_text_repel(data = df %>% filter(color, aika == max(aika, na.rm = TRUE)),
                #                          aes(x = aika, y = value, color= aluenimi, label = round(value,1)), family = "PT Sans") +
                geom_text(data = df_selected_cs,
                          aes(x = aika, 
                              y = value, 
                              color= aluenimi, 
                              label = paste(aluenimi, round(value,1))),
                          color = "black", 
                          family = "PT Sans", 
                          nudge_x = .3)
            
            # gini
            plot_gini <- ggplot() + 
                geom_line(data = df_gini, aes(x = aika, y = gini), color = "#CD5C5C") +
                geom_text(data = df_gini, aes(x = aika, y = gini, label = gini), color = "#CD5C5C")
            # gini
            
        } else if (input$value_regio_show_mode == "valittu alue ja sen naapurit"){
            
            # alfa = ifelse(input$value_regio_level == "Kunnat", .1, .2)
            
            plot0 <- plot0 + 
                geom_line(data = df2,aes(x = aika, y = value, color= aluenimi, fill= aluenimi), show.legend = FALSE) +
                geom_line(data = df,
                          aes(x = aika, y = value, group = aluenimi),
                          color = "dim grey", alpha = .1) +
                geom_point(data = df2, 
                           aes(x = aika, y = value,fill= aluenimi), shape = 21, color = "white", stroke = 1, size = 2.5) +
                ggrepel::geom_text_repel(data = df2 %>% filter(aika == max(aika, na.rm = TRUE)),
                                         aes(x = aika, y = value, color= aluenimi, 
                                             label = paste(aluenimi, round(value,1))), 
                                         family = "PT Sans", nudge_x = .3) +
                geom_line(data = df_selected_ts, aes(x = aika, y = value), color = "black") +
                geom_point(data = df_selected_ts, aes(x = aika, y = value),
                           fill = "black", 
                           color = "white", shape = 21, size = 2.5, stroke = 1) +
                
                geom_text(data = df_selected_cs,
                          aes(x = aika, 
                              y = value, 
                              color= aluenimi, 
                              label = paste(aluenimi, round(value,1))),
                          color = "black", 
                          family = "PT Sans", 
                          nudge_x = .3)
            # ggrepel::geom_text_repel(data = df2 %>% filter(aika == max(aika, na.rm = TRUE)-1),
            #                          aes(x = aika, y = value, color= aluenimi, label = aluenimi), 
            #                          family = "PT Sans")
            # gini
            plot_gini <- ggplot() + 
                geom_line(data = df_gini, aes(x = aika, y = gini), color = "#CD5C5C") +
                geom_text(data = df_gini, aes(x = aika, y = gini, label = gini), color = "#CD5C5C")
            # gini
            
            
        } else if (input$value_regio_show_mode == "valitun alueen kunnat"){
            
            dat2 <- create_municipalities_within_region(varname = input$value_variable, 
                                                        regio_level = input$value_regio_level,
                                                        aluenimi = input$value_region_selected,
                                                        timeseries = TRUE)
            
            dat2_gini <- dat2 %>% group_by(aika) %>% 
                mutate(gini = round(ineq::Gini(value),2)) %>% 
                ungroup()
            
            plot0 <- plot0 + 
                geom_line(data = dat2,aes(x = aika, y = value, color= aluenimi, fill= aluenimi), show.legend = FALSE) +
                geom_point(shape = 21, color = "white", stroke = 1, size = 2.5) +
                ggrepel::geom_text_repel(data = dat2 %>% filter(aika == max(aika, na.rm = TRUE)),
                                         aes(x = aika, y = value, color= aluenimi, label = paste(aluenimi, round(value,1))), nudge_x = .2, family = "PT Sans")
            
            # gini
            plot_gini <- ggplot() + 
                geom_line(data = dat2_gini, aes(x = aika, y = gini), color = "#CD5C5C") +
                geom_text(data = dat2_gini, aes(x = aika, y = gini, label = gini), color = "#CD5C5C")
            # gini
            
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
                        axis_title_size = 12,
                        plot_title_face = "plain") +
            # scale_fill_brewer(palette = "YlGnBu") +
            # scale_color_brewer(palette = "YlGnBu") +
            # scale_fill_viridis_d(option = "plasma", direction = -1, begin = .1, end = .9) +
            # scale_color_viridis_d(option = "plasma", direction = -1, begin = .1, end = .9) +
            labs(fill = NULL,
                 x = NULL,
                 y = paste0(add_line_break2(input$value_variable, 50), "\n(suhdeluku)"),
                 title = glue("{input$value_variable}"),
                 subtitle = kuvan_subtitle,
                 caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: THL (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}")
            )  +
            theme(legend.position = "none",
                  panel.grid.major.x = element_line(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.title.position = "plot",
                  plot.margin = unit(c(0,0,0,0), "mm"),
                  plot.title = element_text(family = "PT Sans", size = 20),
                  plot.subtitle = element_text(family = "PT Sans", size = 16),
                  plot.caption = element_text(family = "PT Sans", size = 11)) -> plot1
        
        plot_gini <- plot_gini + scale_x_continuous(breaks = sort(unique(df$aika)), labels = labels) +
            theme_ipsum(base_family = "PT Sans",
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans",
                        axis_title_size = 12,
                        plot_title_face = "plain") +
            # scale_fill_brewer(palette = "YlGnBu") +
            # scale_color_brewer(palette = "YlGnBu") +
            # scale_fill_viridis_d(option = "plasma", direction = -1, begin = .1, end = .9) +
            # scale_color_viridis_d(option = "plasma", direction = -1, begin = .1, end = .9) +
            labs(fill = NULL,
                 x = NULL,
                 subtitle = "Eriarvoisuuden gini-kerroin")  +
            theme(legend.position = "none",
                  panel.grid.major.x = element_line(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.title.position = "plot",
                  axis.title.x = element_blank(),
                  plot.margin = unit(c(0,0,0,0), "mm"),
                  axis.text.x = element_blank(),
                  plot.title = element_text(family = "PT Sans", size = 20),
                  plot.subtitle = element_text(family = "PT Sans", size = 16),
                  plot.caption = element_text(family = "PT Sans", size = 11))
        
        patchwork::wrap_plots(plot1,plot_gini, ncol = 1, heights = c(1,0.3))
        
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
    
    ### alueprofiilin kartta ----
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
            # scale_fill_viridis_c(option = "plasma") +
            scale_fill_fermenter(palette = "YlGnBu") +
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
                          color = "black", fill = "white", family = "PT Sans", size = 3)
        plot
    }
    
    ### alueprofiilin aikasarja ----
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
                                     aes(label = paste(aluenimi, round(value,1))), family = "PT Sans") +
            # ggrepel::geom_text_repel(data = plotdata_tmp %>% filter(aika == max(aika, na.rm = TRUE)-1),
            #                          aes(label = aluenimi), family = "PT Sans", nudge_x = -1) +
            
            scale_x_continuous(breaks = sort(unique(plotdata_tmp$aika)), labels = labels) +
            theme_ipsum(base_family = "PT Sans", 
                        base_size = 11, 
                        plot_title_size = 12,
                        plot_title_family = "PT Sans",
                        subtitle_family = "PT Sans"#,
                        # grid_col = "white"
            ) +
            theme(legend.position = "none",
                  plot.title.position = "plot",
                  panel.grid.major.x = element_line(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.title = element_text(face = "plain"),
                  axis.text.x = element_text(size = 9)) +
            # scale_fill_viridis_d(option = "viridis", direction = -1, begin = .1, end = .9) +
            # scale_color_viridis_d(option = "viridis", direction = -1, begin = .1, end = .9) +
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
        if (aluetaso1 == "Hyvinvointialueet"){
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
        if (aluetaso1 == "Hyvinvointialueet"){
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
        
        if (aluetaso1 == "Hyvinvointialueet"){
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
        if (aluetaso1 == "Hyvinvointialueet"){
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
        if (aluetaso1 == "Hyvinvointialueet"){
            output$kartta3_05 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[5], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_kartta(varname = muuttujanimi[5]))
            output$kartta3_06 <- renderPlot({alueprofiilikartta_html(val_muuttuja = muuttujanimi[6], val_aluetaso1 = aluetaso1, val_aluename = aluename, val_region_data = region_data, val_muuttujaluokka = muuttujaluokka)
            }, alt = create_alt_text_kartta(varname = muuttujanimi[6]))
        }
        
        if (aluetaso1 == "Hyvinvointialueet"){
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
            column(width = 4,
                   withSpinner(uiOutput("output_save_word"), proxy.height = "100px")
            ),
            column(width = 2,
                   uiOutput("output_save_data_profile")
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
    ## wordin tallennus ----
    
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
    
    ## datan tallennus ----
    ## 
    output$save_data_profile <- downloadHandler(
        
        filename = function() {
            
            aluename <- react_value_region_profile()
            aluetaso1 <- react_value_regio_level_profile()
            
            file_name <- glue("alueprofiili_data_{janitor::make_clean_names(aluename)}_{tolower(aluetaso1)}.csv")
            return(file_name)
        },
        content = function(file) {
            
            dat <- get_dat_timeseries()
            region_data <- get_region_data()
            
            aluename <- react_value_region_profile()
            aluetaso1 <- react_value_regio_level_profile()
            
            naapurikoodit_lst <- region_data[region_data$level %in% aluetaso1 & 
                                                 region_data$region_name %in% aluename,"neigbours"]
            
            naapurikoodit <- naapurikoodit_lst %>% 
                unnest(cols = c(neigbours)) %>% 
                pull(neigbours)
            
            dat[dat$regio_level %in% aluetaso1 & dat$aluenimi %in% aluename ,] %>% 
                select(aika,aluenimi,var_class,variable,value) %>% 
                mutate(rooli = "valinta") -> tmpdat1
            dat[dat$regio_level %in% aluetaso1 & dat$aluekoodi %in% naapurikoodit ,] %>% 
                filter(!aluenimi %in% aluename) %>% 
                select(aika,aluenimi,var_class,variable,value) %>% 
                mutate(rooli = "naapuri") -> tmpdat2
            tmpdat <- bind_rows(tmpdat1,tmpdat2) 
            
            readr::write_excel_csv2(x = tmpdat, file = file)
        }
    )
    
    output$output_save_data_profile <- renderUI({
        
        req(input$value_variable)
        tagList(
            downloadButton("save_data_profile", "Tallenna data csv-muodossa!", class="btn btn-dark")
        )
    })
    
    
    ## muuttujaluettelot ----
    
    
    get_variable_description <- reactive({
        dat <- readRDS("./data/muuttujakuvaukset.RDS")
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

shinyApp(ui = ui, server = server)
# shinyApp(ui = htmlTemplate("www/index.html"), server = server)
# 
