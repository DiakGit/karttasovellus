#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
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
                theme = bslib::bs_theme(#bootswatch = "cosmo",
                                        base_font = font_google("Source Sans Pro"),
                                        code_font = font_google("Space Mono")
                  ),
      # mod_01meta_ui("meta_ui_1"),
      mod_02navi_ui("02navi_ui_1"),
      mod_03indi_ui("03indi_ui_1"),
      mod_04alueprof_ui("04alueprof_ui_1"),
      mod_05etc_ui("05etc_ui_1")

    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'karttasovellus'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
                      # tags$link(rel="shortcut icon", href="favicon.ico"),
    tags$link(rel="stylesheet", href="custom.css"),
    # # **<!-- CSS only -->**
      # tags$link(rel="stylesheet",
      #           href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.2/dist/css/bootstrap.min.css",
      #           integrity="sha384-uWxY/CJNBR+1zjPWmfnSnVxwRheevXITnMqoEIeG1LJrdI0GlVs/9cVSyPYXdcSF",
      #           crossorigin="anonymous"),
    #   # **<!-- JavaScript Bundle with Popper -->**
      tags$script(src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.2/dist/js/bootstrap.bundle.min.js",
                  integrity="sha384-kQtW33rZJAHjgefvhyyzcGF3C5TFyBQBA13V1RKPf4uH+bwyzQxZ6CmMZHmNBEfJ",
                  crossorigin="anonymous"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom.min.js",
                integrity="sha256-AsUX4SJE1+yuDu5+mAVzJbuYNPHj/WroHuZ8Ir/CkE0=",
                crossorigin="anonymous"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.headroom.min.js",
                integrity="sha256-ZX/yNShbjqsohH1k95liqY9Gd8uOiE1S4vZc+9KQ1K4=",
                crossorigin="anonymous"),
    tags$script(src="https://dplyr.tidyverse.org/pkgdown.js")#,
    # tags$link(rel="stylesheet",
    #           href="https://dplyr.tidyverse.org/tidyverse-2.css")
  )
}

