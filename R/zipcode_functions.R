#' Process cross-sectional zipcode data
#' 
#' @param varname A string.
#'
#' @export
process_zipdata <- function(varname = "Kokonaislukema"){
  dtmp <- karttasovellus::dfzip_v20220105[karttasovellus::dfzip_v20220105$variable == varname, ]
  return(dtmp)
}

# dd2 <- process_zipdata()

#' Process time-series zipcode data
#' 
#' @param varname A string.
#'
#' @export
process_zipdata_timeseries <- function(varname = "Kokonaislukema"){
  # dtmp <- karttasovellus::dfzip_v20220105_aikasarja[karttasovellus::dfzip_v20220105_aikasarja$variable == varname & karttasovellus::dfzip_v20220105_aikasarja$aika == year, ]
  dtmp <- karttasovellus::dfzip_v20220105_aikasarja[karttasovellus::dfzip_v20220105_aikasarja$variable == varname, ]
  return(dtmp)
}

# dd3 <- process_zipdata_timeseries()

#' Get zipcode region data
#' 
#' @export
get_region_zipdata <- function(){
  karttasovellus::region_data_zip
}

# dd4 <- get_region_zipdata()

#' Process time-series zipcode data
#' 
#' @param regio_selected A numeric
#' @param value_regio_level A string
#'
#' @export
get_koodit_zip <- function(regio_selected = 161, 
                           value_regio_level = "Seutukunnat"){
  # region_data_kunta <- karttasovellus::region_data
  aluedata <- geofi::municipality_key_2021
  # haetaan valitun alueen kuntanumerot
  if (value_regio_level == "Kunnat"){
    kuntanrot <- regio_selected
  } else if (value_regio_level == "Seutukunnat"){
    # if (!regio_selected %in% aluedata$seutukunta_code) return()
    kuntanrot <- aluedata[aluedata$seutukunta_code %in% regio_selected,]$municipality_code
  } else if (value_regio_level == "Hyvinvointialueet"){
    kuntanrot <- aluedata[aluedata$hyvinvointialue_code %in% regio_selected,]$municipality_code
    }
  neigh <- karttasovellus::region_data_zip[karttasovellus::region_data_zip$kuntanro %in% kuntanrot,]$region_code

  return(neigh)
}


#' Create zipcode map
#' 
#' @param input_value_region_selected A numeric
#' @param input_value_regio_show_mode A string
#' @param input_value_variable A string
#' @param leaflet A logical
#'
#' @import leaflet
#' @import leaflet.extras
#'
#' @export
map_zipcodes <- function(input_value_region_selected = 91,
                         input_value_regio_level = "Kunnat",
                         input_value_variable = "Kokonaislukema",
                         leaflet = FALSE){
  
  
  # input_value_regio_level <- "Postinumeroalueet"
  region_data <- get_region_zipdata()
  dat <- process_zipdata(varname = input_value_variable)
  dat <- left_join(region_data %>% select(-kuntanro),
                   dat,by = c("region_name" = "aluenimi"), keep = TRUE) 

  dat <- dat %>%
    mutate(color = ifelse(aluenimi %in% input_value_region_selected, TRUE, FALSE))
  
  # Laitetaan sen sijaan aluetaso tähän
  # if (input_value_regio_show_mode == "kaikki tason alueet"){
  #   dat <- dat
  # } else if (input_value_regio_show_mode == "valittu alue ja sen naapurit"){
  #   naapurikoodit <- get_naapurikoodit_zip(regio_selected = input_value_region_selected)
  #   dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
  # }  else if (input_value_regio_show_mode == "valittu alue"){
  #   naapurikoodit <- karttasovellus::region_data_zip[karttasovellus::region_data_zip$kuntanro == input_value_region_selected,]$region_code
  #   dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
  # }
    zipcodes <- get_koodit_zip(regio_selected = input_value_region_selected, 
                                    value_regio_level = input_value_regio_level)
    dat <- dat %>% filter(aluekoodi %in% zipcodes)

  
  # luodaan alaotsikko
  kuvan_subtitle <- glue("Aluetaso: {input_value_regio_level}")
  
  if (!leaflet){
    
    ggplot(data = dat, aes(fill = value)) +
      geom_sf(color = alpha("white", 1/3))  +
      geom_sf(aes(color = color), fill = NA, show.legend = FALSE)  +    
      scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1) +
      scale_color_manual(values = c(alpha("white", 1/3), "black")) +
      theme_ipsum(base_family = "PT Sans",
                  plot_title_family = "PT Sans",
                  subtitle_family = "PT Sans",
                  grid_col = "white",
                  plot_title_face = "plain") -> p
    
    # if (input_value_regio_show_mode == "kaikki tason alueet"){
    #   p <- p + geom_sf_label(data = dat %>% filter(aluenimi == input_value_region_selected),
    #                          aes(label = paste(aluenimi, value)),
    #                          fill = "white", color = "black", family = "PT Sans")
    #   
    # } else if (input_value_regio_show_mode %in% c("valittu alue ja sen naapurit","valittu alue")){
    #   p + geom_sf_label(data = dat,
    #                     aes(label = paste(aluenimi, round(value, 1))),
    #                     fill = "white", color = "black", family = "PT Sans") -> p
    # }
    p + theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = c(0.1, 0.5),
              plot.title.position = "plot") +
      labs(title = glue("{input_value_variable}"),
           subtitle = kuvan_subtitle,
           caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: Tilastokeskus Paavo (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"),
           fill = paste0(add_line_break2(input_value_variable, 20), "\n(suhdeluku)"))
  } else {
    dat_wgs84 <- sf::st_transform(x = dat, crs = "+proj=longlat +datum=WGS84")
    
    pal <- leaflet::colorNumeric(palette = "YlGnBu", domain = dat_wgs84$value)
    
    labels <- sprintf(
      "<italic>%s</italic> (%s)<br/>%s<br/><strong>%s</strong>",
      dat_wgs84$region_name, dat_wgs84$region_code, dat_wgs84$kuntanimi, round(dat_wgs84$value,1)
    ) %>% lapply(htmltools::HTML)
    
    
    base <- leaflet(data = dat_wgs84) %>% 
      addTiles(urlTemplate = "http://tiles.kartat.kapsi.fi/taustakartta/{z}/{x}/{y}.jpg",
               options = tileOptions(opacity = .4))

    base %>%   
      addPolygons(fillColor = ~pal(value),
                  color = "white",
                  weight = 2,
                  opacity = .6,
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.4,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(opacity = .7,
                                              style = list("font-weight" = "normal",
                                                           padding = "2px 4px"),
                                              textsize = "12px",
                                              direction = "auto")
      ) %>% 
      addLegend(pal = pal, 
                values = ~value, 
                opacity = 0.7,
                # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                title = input_value_variable,
                position = "bottomright") %>% 
      leaflet.extras::addFullscreenControl()
  }
}

#' Create zipcode bar chart
#' 
#' @param input_value_region_selected A numeric
#' @param input_value_regio_level A string
#' @param input_value_variable A string
#'
#' @export
plot_zipcodes_bar <- function(input_value_region_selected = 91,
                              input_value_regio_level = "Kunnat",
                              input_value_variable = "Kokonaislukema"){
  
  region_data <- get_region_zipdata()
  dat <- process_zipdata(varname = input_value_variable)

  dat <- dat %>%
    mutate(color = ifelse(aluenimi %in% input_value_region_selected, TRUE, FALSE))

  naapurikoodit <- get_koodit_zip(regio_selected = input_value_region_selected,
                                  value_regio_level = input_value_regio_level)
  dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
  
  # luodaan alaotsikko
  kuvan_subtitle <- glue("Aluetaso: {input_value_regio_level}")
  
  dat$aluenimi <- paste0(dat$aluenimi, " (", dat$aluekoodi, ") ", dat$kuntanimi)
  
  ggplot(data = dat, aes(y = reorder(aluenimi, value), 
                         x = value, 
                         fill = value)) +
    geom_col() +
    geom_text(aes(label = round(value,1)), 
              color = "black", 
              nudge_x = max(dat$value, na.rm = TRUE)*0.2, 
              family = "PT Sans") +
    scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1) +
    scale_color_fermenter(palette = "YlGnBu", type = "seq", direction = 1) +
    theme_ipsum(base_family = "PT Sans",
                plot_title_family = "PT Sans",
                subtitle_family = "PT Sans",
                grid_col = "white",
                plot_title_face = "plain") + 
  theme(plot.title.position = "plot",
        legend.position = "none") +
    labs(title = glue("{input_value_variable}"),
         subtitle = kuvan_subtitle,
         caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: Tilastokeskus Paavo (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"),
         fill = paste0(add_line_break2(input_value_variable, 20), "\n(suhdeluku)"))
}

#' Create zipcode line chart
#' 
#' @param input_value_region_selected A numeric
#' @param input_value_regio_show_mode A string
#' @param input_value_variable A string
#'
#' @export
plot_zipcodes_line <- function(input_value_region_selected = 91,
                               # input_value_regio_show_mode = "kaikki tason alueet"
                               input_value_regio_level = "Kunnat",
                               input_value_regio_show_mode = "kaikki tason alueet",
                               input_value_variable = "Kokonaislukema"){
  
  region_data <- get_region_zipdata()
  dat <- process_zipdata_timeseries(varname = input_value_variable)
  
  dat <- dat %>% 
    mutate(color = ifelse(aluenimi %in% input_value_region_selected, TRUE, FALSE))
  
  # if (input_value_regio_show_mode == "kaikki tason alueet"){
  #   dat <- dat
  # } else if (input_value_regio_show_mode == "valittu alue ja sen naapurit"){
  #   naapurikoodit <- get_naapurikoodit_zip(regio_selected = input_value_region_selected)
  #   dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
  # }  else if (input_value_regio_show_mode == "valittu alue"){
  #   naapurikoodit <- karttasovellus::region_data_zip[karttasovellus::region_data_zip$kuntanro == input_value_region_selected,]$region_code
  #   dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
  # }
  naapurikoodit <- get_koodit_zip(regio_selected = input_value_region_selected, 
                                  value_regio_level = input_value_regio_level)
  dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
  
  # luodaan alaotsikko
  kuvan_subtitle <- glue("Aluetaso: {input_value_regio_level}")
  
  dat$aluenimi <- paste0(dat$aluenimi, " (", dat$aluekoodi, ")\n", dat$kuntanimi)
  
  ggplot(data = dat, aes(y = value, 
                         x = aika, 
                         color = aluenimi,
                         group = aluenimi)) +
    geom_line() +
    # scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1) +
    # scale_color_fermenter(palette = "YlGnBu", type = "seq", direction = 1) +
    theme_ipsum(base_family = "PT Sans",
                plot_title_family = "PT Sans",
                subtitle_family = "PT Sans",
                grid_col = "white",
                plot_title_face = "plain") -> p
  
  # if (input_value_regio_show_mode == "kaikki tason alueet"){
  #    
  #   
  # } else if (input_value_regio_show_mode %in% c("valittu alue ja sen naapurit","valittu alue")){
    p <- p + ggrepel::geom_text_repel(data = dat %>% filter(aika == max(aika, na.rm = TRUE)),
                                      aes(x = aika, y = value, color= aluenimi,
                                          label = paste(aluenimi, round(value,1))),
                                      family = "PT Sans", nudge_x = .3)
  # }
  p + theme(plot.title.position = "plot",
            legend.position = "none") +
    labs(title = glue("{input_value_variable}"),
         subtitle = kuvan_subtitle,
         caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: Tilastokeskus Paavo (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"),
         fill = paste0(add_line_break2(input_value_variable, 20), "\n(suhdeluku)"))
}
