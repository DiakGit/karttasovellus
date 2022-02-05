if (F){
  library(karttasovellus)
  library(dplyr)
  library(glue)
  library(ggplot2)
  library(sf)
  library(hrbrthemes)
  library(leaflet)
  library(tidyr)
  library(forcats)
}


#' Process cross-sectional zipcode data
#' 
#' @param varname A string.
#'
#' @export
process_zipdata <- function(varname = "Kokonaislukema"){
  load(system.file("data", "dfzip_v20220202.rda", package="karttasovellus"))
  dtmp <- dfzip_v20220202[dfzip_v20220202$variable %in% varname, ]
  return(dtmp)
}

# dd2 <- process_zipdata()

#' Process time-series zipcode data
#' 
#' @param varname A string.
#'
#' @export
process_zipdata_timeseries <- function(varname = "Kokonaislukema"){
  load(system.file("data", "dfzip_v20220202_aikasarja.rda", package="karttasovellus"))
  dtmp <- dfzip_v20220202_aikasarja[dfzip_v20220202_aikasarja$variable %in% varname, ]
  return(dtmp)
}

# dd3 <- process_zipdata_timeseries()

#' Get zipcode region data
#' 
#' @export
get_region_zipdata <- function(){
  load(system.file("data", "region_data_zip.rda", package="karttasovellus"))
  return(region_data_zip)
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
  
  load(system.file("data", "region_data_zip.rda", package="karttasovellus"))
  neigh <- region_data_zip[region_data_zip$kuntanro %in% kuntanrot,]$region_code

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
                         leaflet = FALSE, 
                         alueprofiili = FALSE){
  
  
  # input_value_regio_level <- "Postinumeroalueet"
  region_data <- get_region_zipdata()
  dat <- process_zipdata(varname = input_value_variable)
  dat <- left_join(region_data %>% select(-kuntanro),
                   dat,by = c("region_name" = "aluenimi"), keep = TRUE) 

  dat <- dat %>%
    mutate(color = ifelse(aluenimi %in% input_value_region_selected, TRUE, FALSE))
  
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
    
    p <- p + theme(axis.text.x = element_blank(),
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
    if (!alueprofiili){
    p <- p + ggrepel::geom_label_repel(data = dat %>%  
                                  sf::st_set_geometry(NULL) %>%
                                  bind_cols(dat %>%
                                              sf::st_centroid() %>%
                                              sf::st_coordinates() %>% as_tibble()),
                                aes(label = paste0(aluenimi, "\n",
                                                   round(value,1)), x = X, y = Y),
                                color = "black", fill = "white", family = "PT Sans", lineheight = .8)      
    }
    p
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


#' Create leaflet only zipcode map for alueprofiili
#' 
#' @param input_value_region_selected A numeric
#' @param input_value_regio_show_mode A string
#'
#' @import leaflet
#' @import leaflet.extras
#'
#' @export
map_zipcodes_alueprofiili <- function(input_value_region_selected = 91,
                         input_value_regio_level = "Kunnat", 
                         zipvars = c('Kokonaislukema',
                                     'Alimpaan tuloluokkaan kuuluvat taloudet',
                                     'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
                                     'Työttömät',
                                     'Peruskoulutuksen omaavat')){

  region_data <- get_region_zipdata()
  load(system.file("data", "dfzip_v20220202.rda", package="karttasovellus"))
  # dfzip <- dfzip_v20220202 %>% 
  #   pivot_wider(names_from = variable, values_from = value)  
  dat <- left_join(region_data %>% select(-kuntanro),
                   dfzip_v20220202,by = c("region_name" = "aluenimi"), keep = TRUE)
  
  zipcodes <- get_koodit_zip(regio_selected = input_value_region_selected, 
                             value_regio_level = input_value_regio_level)
  dat <- dat %>% filter(aluekoodi %in% zipcodes)
  
  dat_wgs84 <- sf::st_transform(x = dat, crs = "+proj=longlat +datum=WGS84")
    
  base <- leaflet(data = dat_wgs84) %>% 
    addTiles(urlTemplate = "http://tiles.kartat.kapsi.fi/taustakartta/{z}/{x}/{y}.jpg",
             options = tileOptions(opacity = .4))
  
  for (ii in seq_along(zipvars)){
  
  dat_wgs84_tmp <- dat_wgs84[dat_wgs84$variable %in% zipvars[ii],]
  
  pal <- leaflet::colorNumeric(palette = "YlGnBu", domain = dat_wgs84_tmp[["value"]])

  lab <- sprintf(
      "<strong>%s</strong><br/><italic>%s</italic> (%s)<br/>%s<br/><strong>%s</strong>",
      zipvars[ii],
      dat_wgs84_tmp$region_name, 
      dat_wgs84_tmp$region_code, 
      dat_wgs84_tmp$kuntanimi, 
      round(dat_wgs84_tmp$value,1)
    ) %>% lapply(htmltools::HTML)

  
  highlight_opts <- highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.4,
      bringToFront = TRUE)
    
  label_opts <- labelOptions(opacity = .7,
                 style = list("font-weight" = "normal",
                              padding = "2px 4px"),
                 textsize = "12px",
                 direction = "auto")

  base <- base %>%   
      addPolygons(data = dat_wgs84_tmp,
                  fillColor = ~pal(value),
                  group = zipvars[ii],
                  color = "white",
                  weight = 2,
                  opacity = .6,
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlight_opts,
                  label = lab,
                  labelOptions = label_opts) #%>% 
    # addLegend(pal = pal, values = ~value, group = zipvars[ii], position = "bottomright", title = zipvars[ii])
    

  }
  base %>% 
  # addLegend(pal = pal1,
  #           values = ~Kokonaislukema,
  #           opacity = 0.7,
  #           title = "",
  #           position = "bottomright") %>%
    addLayersControl(
      baseGroups = zipvars,
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    leaflet.extras::addFullscreenControl()
  
}

#' Create zipcode bar chart
#' 
#' @param input_value_region_selected A numeric
#' @param input_value_regio_level A string
#' @param input_value_variable A string
#'
#' @export
plot_zipcodes_bar <- function(input_value_region_selected = 5,
                              input_value_regio_level = "Hyvinvointialueet",
                              input_value_variable = "Kokonaislukema"){
  
  region_data <- get_region_zipdata()
  dat <- process_zipdata(varname = input_value_variable)

  dat <- dat %>%
    mutate(color = ifelse(aluenimi %in% input_value_region_selected, TRUE, FALSE))

  naapurikoodit <- get_koodit_zip(regio_selected = input_value_region_selected,
                                  value_regio_level = input_value_regio_level)
  dat <- dat %>% filter(aluekoodi %in% naapurikoodit, 
                        !is.na(value))
  
  # luodaan alaotsikko
  kuvan_subtitle <- glue("Aluetaso: {input_value_regio_level}")
  
  dat$aluenimi <- paste0(dat$aluenimi, " (", dat$aluekoodi, ") ", dat$kuntanimi)
  med <- median(dat$value, na.rm = TRUE)
  
  if (input_value_regio_level == "Kunnat"){
    dat$value_nudge <- ifelse(dat$value >= 100, 
                              dat$value + med*.15, 
                              dat$value - med*.15)
  } else {
    dat$value_nudge <- ifelse(dat$value >= 100, 
                              dat$value + med*.22, 
                              dat$value - med*.22)
  }
  ggplot(data = dat, aes(y = reorder(aluenimi, value), 
                         x = value, 
                         fill = value)) +
    # geom_col() +
    # xlim(c(min(dat$value, na.rm = TRUE)*0.6,max(dat$value, na.rm = TRUE)*1.3)) +
    geom_vline(xintercept = 100, color = alpha("dim grey", 1/3), linetype = "dashed") +
    geom_segment(aes(x = 100, 
                     yend = reorder(aluenimi, value), 
                     xend = value), 
                 color = alpha("dim grey", 1/3), 
                 alpha=1, 
                 show.legend = FALSE) +
    geom_point(aes(fill = value), color = "dim grey", shape = 21, size = 5, show.legend = FALSE) + 
    geom_text(aes(label = round(value,0), 
                  x = value_nudge), 
              color = "black", 
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
         fill = paste0(add_line_break2(input_value_variable, 20), "\n(suhdeluku)")) -> p
  print(p)
}


#' Create zipcode bar chart
#' 
#' @param input_value_region_selected A numeric
#' @param input_value_regio_level A string
#' @param zipvars A string
#'
#' @export
plot_zipcodes_dotplot_alueprofiili <- function(input_value_region_selected = 5,
                              input_value_regio_level = "Hyvinvointialueet",
                              zipvars = c('Kokonaislukema',
                                          'Alimpaan tuloluokkaan kuuluvat taloudet',
                                          'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
                                          'Työttömät',
                                          'Peruskoulutuksen omaavat')){
  region_data <- get_region_zipdata()
  dat <- process_zipdata(varname = zipvars)
  naapurikoodit <- get_koodit_zip(regio_selected = input_value_region_selected,
                                  value_regio_level = input_value_regio_level)
  dat <- dat %>% filter(aluekoodi %in% naapurikoodit, 
                        !is.na(value))
  
  # luodaan alaotsikko
  kuvan_subtitle <- glue("Aluetaso: {input_value_regio_level}")
  
  dat$aluenimi <- paste0(dat$aluenimi, " (", dat$aluekoodi, ") ", dat$kuntanimi)
  med <- median(dat$value, na.rm = TRUE)
  
  if (input_value_regio_level == "Kunnat"){
    dat$value_nudge <- ifelse(dat$value >= 100, 
                              dat$value + med*.15, 
                              dat$value - med*.15)
  } else {
    dat$value_nudge <- ifelse(dat$value >= 100, 
                              dat$value + med*.22, 
                              dat$value - med*.22)
  }
  dat$variable <- factor(dat$variable, levels = zipvars)
  
  aluenimet <- arrange(dat[dat$variable == "Kokonaislukema",], value) %>% pull(aluenimi)
  dat$aluenimi <- factor(dat$aluenimi, levels = aluenimet)
  
  ggplot(data = dat, aes(y = aluenimi, 
                         x = value, 
                         fill = value)) +
    geom_vline(xintercept = 100, color = alpha("dim grey", 1/3), linetype = "dashed") +
    geom_segment(aes(x = 100, 
                     yend = reorder(aluenimi, value), 
                     xend = value), 
                 color = alpha("dim grey", 1/3), 
                 alpha=1, 
                 show.legend = FALSE) +
    geom_point(aes(fill = value), color = "dim grey", shape = 21, size = 4, show.legend = FALSE) + 
    geom_text(aes(label = round(value,0), 
                  x = value_nudge),
              color = "black", 
              family = "PT Sans") +
    scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1) +
    scale_color_fermenter(palette = "YlGnBu", type = "seq", direction = 1) +
    theme_ipsum(base_family = "PT Sans",
                plot_title_family = "PT Sans",
                subtitle_family = "PT Sans",
                grid_col = "white",
                plot_title_face = "plain") + 
    theme(plot.title.position = "plot",
          legend.position = "top") +
    labs(title = "Kaikki postinumeroaluetason osoittimet",
         subtitle = kuvan_subtitle,
         caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: Tilastokeskus Paavo (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"),
         fill = NULL
         ) +
    facet_wrap(~variable, nrow = 1, scales = "free_x") -> p
  print(p)
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
                               # input_value_regio_show_mode = "kaikki tason alueet",
                               input_value_variable = "Kokonaislukema"){
  
  region_data <- get_region_zipdata()
  dat <- process_zipdata_timeseries(varname = input_value_variable)
  
  dat <- dat %>% 
    mutate(color = ifelse(aluenimi %in% input_value_region_selected, TRUE, FALSE))
  
  naapurikoodit <- get_koodit_zip(regio_selected = input_value_region_selected, 
                                  value_regio_level = input_value_regio_level)
  dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
  
  # luodaan alaotsikko
  kuvan_subtitle <- glue("Aluetaso: {input_value_regio_level}")
  
  dat$aluenimi <- paste0(dat$aluenimi, " (", dat$aluekoodi, ")\n", dat$kuntanimi)
  
  aika1 <- sort(unique(dat$aika)) - 1
  aika2 <- sort(unique(dat$aika)) + 1
  labels <- paste0(aika1,"-",aika2)

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
                plot_title_face = "plain") -> p
  
  # if (input_value_regio_show_mode == "kaikki tason alueet"){
  #    
  #   
  # } else if (input_value_regio_show_mode %in% c("valittu alue ja sen naapurit","valittu alue")){
  # p <- p + ggrepel::geom_text_repel(  
  p <- p + geom_text(
      data = dat %>% filter(aika == max(aika, na.rm = TRUE)),
      aes(x = aika, y = value, color= aluenimi,
          label = paste(aluenimi, round(value,1))),
      family = "PT Sans", nudge_x = .3)
  # }
  p + theme(plot.title.position = "plot",
            legend.position = "none",
            panel.grid.major.x = element_line(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank()) +
    scale_x_continuous(breaks = sort(unique(dat$aika)), labels = labels) +
    labs(title = glue("{input_value_variable}"),
         subtitle = kuvan_subtitle,
         caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: Tilastokeskus Paavo (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"),
         fill = paste0(add_line_break2(input_value_variable, 20), "\n(suhdeluku)"))
}


#' Create zipcode table
#' 
#' @param input_value_region_selected A numeric
#' @param input_value_regio_level A string
#'
#' @export
table_zipcodes <- function(input_value_region_selected = 91,
                           input_value_regio_level = "Kunnat", 
                           zipvars = c('Kokonaislukema',
                                       'Alimpaan tuloluokkaan kuuluvat taloudet',
                                       'Alimpaan tuloluokkaan kuuluvat täysi-ikäiset',
                                       'Työttömät',
                                       'Peruskoulutuksen omaavat'),
                           print = FALSE){
  
  naapurikoodit <- get_koodit_zip(regio_selected = input_value_region_selected,
                                  value_regio_level = input_value_regio_level)
  load(system.file("data", "dfzip_v20220202.rda", package="karttasovellus"))
  dfzip_v20220202 %>% 
    filter(aluekoodi %in% naapurikoodit) %>% 
    select(aluekoodi, aluenimi, variable, value) %>% 
    mutate(value = round(value, 1),
           variable = factor(variable, levels = zipvars)) %>% 
    arrange(variable) %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    arrange(desc(Kokonaislukema)) -> tmpt
  if (print){
    print(kable(tmpt,
                format = "pipe", 
                row.names = FALSE))
  } else {
    gt::gt(tmpt)
  }
    
  
}
