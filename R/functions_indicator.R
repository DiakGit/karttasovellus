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

#' Get cross-section data
#' 
#' @export
get_dat <- function(){
  load(system.file("data", "df_v20211104.rda", package="karttasovellus"))
  return(df_v20211104)
}

#' Get time-series data
#' 
#' @export
get_dat_timeseries <- function(){
  load(system.file("data", "df_v20211104_aikasarja.rda", package="karttasovellus"))
  return(df_v20211104_aikasarja)
}

#' Get data of regions
#' 
#' @export
get_region_data <- function(){
  load(system.file("data", "region_data.rda", package="karttasovellus"))
  return(region_data)
}

#' Get list of unique indicators
#'
#' @export
varlist_diak <- function(){
  dat <- get_dat()
  dat %>%
    count(regio_level,var_class,variable) %>%
    select(-n) %>%
    arrange(desc(var_class),variable) -> indicator_df
  return(indicator_df)
}

#' Process municipality data
#' 
#' @param input_value_regio_level A string.
#' @param input_value_variable A string.
#' @param timeseries A logical.
#' @param dat_cs A data frame. An optional data to speed up things
#' @param dat_ts A data frame. An optional data to speed up things
#'
#' @export
process_data <- function(input_value_regio_level = "Kunnat",
                         input_value_variable = "Nuorisotyöttömyys", 
                         timeseries = FALSE,
                         spatial = TRUE,
                         dat_ts = NULL,
                         dat_cs = NULL){
  
  if (timeseries){
    if (is.null(dat_ts)) dat <- get_dat_timeseries() else dat <- dat_ts
  } else {
    if (is.null(dat_cs)) dat <- get_dat() else dat <- dat_cs
  } 

  dat_1 <- dat[dat$regio_level %in% input_value_regio_level & dat$variable %in% input_value_variable,]
  
  if (spatial){ # eli jos karttaa varten
    if (input_value_regio_level == "Hyvinvointialueet"){
      load(system.file("data", "regio_Hyvinvointialueet.rda", package="karttasovellus"))
      reg <- regio_Hyvinvointialueet
    } else if (input_value_regio_level == "Seutukunnat"){
      load(system.file("data", "regio_Seutukunnat.rda", package="karttasovellus"))
      reg <- regio_Seutukunnat
    } else if (input_value_regio_level == "Kunnat"){
      load(system.file("data", "regio_Kunnat.rda", package="karttasovellus"))
      reg <- regio_Kunnat
    }
    res <- left_join(reg, dat_1) %>% 
      select(-regio_level) %>%
      filter(!is.na(value)) %>%
      arrange(desc(value)) %>%
      mutate(value = round(value, 1)) %>%
      mutate(rank = 1:n())
  } else {
    res <- dat_1 %>% 
      select(-regio_level) %>%
      filter(!is.na(value)) %>%
      arrange(desc(value)) %>%
      mutate(value = round(value, 1)) %>%
      mutate(rank = 1:n())
  }
  return(res)
}

# library(profvis)
# profvis({
#   plot_map()
# })



#' Create list of region neighbors
#' 
#' @param input_value_regio_level A string.
#' @param input_value_variable A string.
#' @param input_value_region_selected A string.
#' @param timeseries A logical.
#'
#' @export
create_municipalities_within_region <- function(input_value_variable = "Nuorisotyöttömyys", 
                                                input_value_regio_level = "Kunnat",
                                                input_value_region_selected = "Helsinki",
                                                timeseries = TRUE,
                                                dat_ts = NULL,
                                                dat_cs = NULL){
  varname <- input_value_variable
  regio_level <- input_value_regio_level
  aluenimi <- input_value_region_selected

  if (timeseries){
    if (is.null(dat_ts)) dat <- get_dat_timeseries() else dat <- dat_ts
  } else {
    if (is.null(dat_cs)) dat <- get_dat() else dat <- dat_cs
  } 
  
  dat_1 <- dat[dat$regio_level %in% "Kunnat" & dat$variable %in% varname,]
  
  if (regio_level == "Hyvinvointialueet"){
    muni_key_subset <- geofi::municipality_key_2021 %>% 
      mutate(hyvinvointialue_name_fi = sub("hyvinvointialue", "HVA", hyvinvointialue_name_fi)) %>% 
      filter(hyvinvointialue_name_fi == aluenimi) %>% 
      select(name_fi)
  } else {
    muni_key_subset <- geofi::municipality_key_2021 %>% 
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


# get_naapurikoodit ----
get_naapurikoodit <- function(input_value_regio_level = "Kunnat",
                              input_value_region_selected = "Helsinki",
                              region_data = NULL){
  if (is.null(region_data)) region_data <- get_region_data()
  naapurikoodit_lst <- region_data[region_data$level %in% input_value_regio_level & 
                                     region_data$region_name %in% input_value_region_selected,"neigbours"]
  naapurikoodit <- naapurikoodit_lst %>% tidyr::unnest(cols = c(neigbours)) %>% pull(neigbours)
  return(naapurikoodit)
}


#' Create bar plot
#' 
#' @param input_value_regio_level A string.
#' @param input_value_variable A string.
#' @param input_value_region_selected A string.
#' @param input_value_regio_show_mode A string.
#'
#' @export
plot_rank_bar <- function(input_value_regio_level = "Seutukunnat",
                      input_value_variable = "Nuorisotyöttömyys",
                      input_value_region_selected = "Helsinki",
                      input_value_regio_show_mode = "valitun alueen kunnat"){
  
  dat_cs <- get_dat()
  
  dat <- process_data(input_value_regio_level = input_value_regio_level,
                      input_value_variable = input_value_variable, 
                      dat_cs = dat_cs, 
                      spatial = FALSE)
  
  region_data <- get_region_data()
  naapurikoodit <- get_naapurikoodit(input_value_regio_level = input_value_regio_level,
                                     input_value_region_selected = input_value_region_selected, 
                                     region_data = region_data)
  dat <- dat %>% mutate(color = ifelse(aluenimi %in% input_value_region_selected, TRUE, FALSE))
  
  dat <- dat %>% 
    select(rank,aluekoodi,aluenimi,value,color) %>% 
    mutate(aluenimi = factor(aluenimi), 
           aluenimi = fct_reorder(aluenimi, -rank),
           fill = value)
  
  if (input_value_regio_show_mode == "valittu alue ja sen naapurit"){
    dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
    if (input_value_regio_level == "Kunnat"){
      # dat$color <- ifelse(!dat$aluekoodi %in% naapurikoodit, FALSE, TRUE)
    }
  } else if (input_value_regio_show_mode == "valitun alueen kunnat"){
    
    dat <- create_municipalities_within_region(input_value_variable = input_value_variable, 
                                               input_value_regio_level = input_value_regio_level,
                                               input_value_region_selected = input_value_region_selected,
                                               timeseries = FALSE, 
                                               dat_cs = dat_cs)
    dat <- dat %>% mutate(color = ifelse(aluenimi %in% input_value_region_selected, TRUE, FALSE))
  }
  
  # luodaan alaotsikko
  if (input_value_regio_show_mode == "valitun alueen kunnat"){
    kuvan_subtitle <- glue("Kuvassa näytetään alueeseen {input_value_region_selected} tasolla {tolower(input_value_regio_level)} kuuluvat kunnat")
  } else {
    kuvan_subtitle <- glue("Aluetaso: {input_value_regio_level}")
  }
  med <- median(dat$value, na.rm = TRUE)
  dat$value_nudge <- ifelse(dat$value >= 100, 
                            dat$value + med*.15, 
                            dat$value - med*.15)
  ggplot(dat, aes(x = value, y = reorder(aluenimi, -rank))) + 
    theme_ipsum(base_family = "Lato",
                plot_title_family = "Lato",
                subtitle_family = "Lato",
                grid_col = "white",
                plot_title_face = "plain") +
    xlim(c(min(dat$value, na.rm = TRUE)*0.6,max(dat$value, na.rm = TRUE)*1.3)) +
    theme(legend.position = "right",
          plot.title.position = "plot") +
    scale_color_manual(values = c("white","black")) + 
    # geom_col(aes(color = color, fill = value), show.legend = FALSE, size = 1.1) +
    # geom_segment(aes(y = 0, xend = value, 
    #                           yend = aluenimi, color="red"), alpha=.5, show.legend = FALSE) +
    geom_vline(xintercept = 100, color = alpha("dim grey", 1/3), linetype = "dashed") +
    geom_segment(aes(x = 100, 
                     yend = reorder(aluenimi, -rank), 
                     xend = value), 
                 color = alpha("dim grey", 1/3), 
                 alpha=1, 
                 show.legend = FALSE) +
    geom_point(aes(fill = value), color = "dim grey", shape = 21, size = 5, show.legend = FALSE) + 
    geom_point(data = dat[dat$color,], 
               fill = NA, color = "#FF00FF", shape = 21, size = 5, stroke = 1.5, show.legend = FALSE) + 
    
    
    scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1) -> plot
  
  plot <- plot + geom_text(aes(label = round(value), 
                               x = value_nudge), 
                           color = "black", 
                           family = "Lato")
  
  plot + scale_y_discrete(expand = expansion(add = 2)) +
    labs(x = NULL, y = NULL, fill = NULL) +
    labs(title = add_line_break2(glue("{input_value_variable}"), 50),
         subtitle = kuvan_subtitle,
         caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: THL (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"))

  
    
}


#' Create municipality map
#' 
#' @param input_value_regio_level A string.
#' @param input_value_variable A string.
#' @param input_value_region_selected A string.
#' @param input_value_regio_show_mode A string.
#' @param leaflet A logical.
#'
#' @export
plot_map <- function(input_value_regio_level = "Hyvinvointialueet",
                          input_value_variable = "Huono-osaisuus yhteensä",
                          input_value_region_selected = "Varsinais-Suomen HVA",
                          input_value_regio_show_mode = "valittu alue ja sen naapurit",
                          leaflet = FALSE
                     ){
  
  df_cs <- get_dat()
  
  dat <- process_data(input_value_regio_level = input_value_regio_level,
                      input_value_variable = input_value_variable, 
                      dat_cs = df_cs)
  
  region_data <- get_region_data()
  naapurikoodit <- get_naapurikoodit(input_value_regio_level = input_value_regio_level,
                                     input_value_region_selected = input_value_region_selected,
                                     region_data = region_data)
  dat <- dat %>% mutate(color = ifelse(aluenimi %in% input_value_region_selected, TRUE, FALSE))
  
  if (input_value_regio_show_mode == "kaikki tason alueet"){
    dat <- dat
  } else if (input_value_regio_show_mode == "valittu alue ja sen naapurit"){
    dat <- dat %>% filter(aluekoodi %in% naapurikoodit)
  } else if (input_value_regio_show_mode == "valitun alueen kunnat"){
    
    dat <- create_municipalities_within_region(input_value_variable = input_value_variable, 
                                               input_value_regio_level = input_value_regio_level,
                                               input_value_region_selected = input_value_region_selected,
                                               timeseries = FALSE, 
                                               dat_cs = df_cs)
    
    load(system.file("data", "regio_Kunnat.rda", package="karttasovellus"))
    reg <- regio_Kunnat
    res <- left_join(reg, dat) %>% 
      filter(!is.na(aluenimi))
    
    dat <- res %>%
      filter(!is.na(value)) %>%
      arrange(desc(value)) %>%
      mutate(value = round(value, 1)) %>%
      mutate(rank = 1:n(), 
             color = TRUE) 
  }
  
  if (!leaflet){
  
  # luodaan alaotsikko
  if (input_value_regio_show_mode == "valitun alueen kunnat"){
    kuvan_subtitle <- glue("Kuvassa näytetään alueeseen {input_value_region_selected} tasolla {tolower(input_value_regio_level)} kuuluvat kunnat")
  } else {
    kuvan_subtitle <- glue("Aluetaso: {input_value_regio_level}")
  }
  
  ggplot(data = dat, aes(fill = value)) +
    geom_sf(color = alpha("white", 1/3))  +
    geom_sf(aes(color = color), fill = NA, show.legend = FALSE)  +    
    scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1) +
    scale_color_manual(values = c(alpha("white", 1/3), "#FF00FF")) +
    theme_ipsum(base_family = "Lato",
                plot_title_family = "Lato",
                subtitle_family = "Lato",
                grid_col = "white",
                plot_title_face = "plain") -> p
  
  if (input_value_regio_show_mode == "kaikki tason alueet"){
    if (input_value_regio_level != "Kunnat"){
      p + geom_sf_label(data = dat %>% filter(!aluenimi %in% input_value_region_selected), 
                        aes(label = value), 
                        family = "Lato", 
                        color = "black", 
                        fill = "white", 
                        size = 2.5) -> p
    }
    p <- p + geom_sf_label(data = dat %>% filter(aluenimi == input_value_region_selected),
                           aes(label = paste(aluenimi, value)),
                           fill = "white", color = "black", family = "Lato")
    
  } else if (input_value_regio_show_mode == "valittu alue ja sen naapurit"){
    p + geom_sf_label(data = dat,
                      aes(label = paste(aluenimi, value)),
                      fill = "white", color = "black", family = "Lato") -> p
  } else if (input_value_regio_show_mode == "valitun alueen kunnat"){
    # p + ggrepel::geom_label_repel(
      p + geom_label(
                        data = dat %>%
                                    sf::st_set_geometry(NULL) %>%
                                    bind_cols(dat %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                                  aes(label = paste0(aluenimi,"\n", value), x = X, y = Y),
                                  color = "black", fill = alpha("white", 2/3),
                                  family = "Lato", size = 3, lineheight = .8) -> p
    
  }
  p + theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = c(0.1, 0.5),
            plot.title.position = "plot") +
    labs(title = add_line_break2(glue("{input_value_variable}"), 50),
         subtitle = kuvan_subtitle,
         caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: THL (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"),
         fill = paste0(add_line_break2(input_value_variable, 20), "\n(suhdeluku)"))
  
  } else {
    dat_wgs84 <- sf::st_transform(x = dat, crs = "+proj=longlat +datum=WGS84")
    
    pal <- leaflet::colorNumeric(palette = "YlGnBu", domain = dat_wgs84$value)
    
    labels <- sprintf(
      "<italic>%s</italic> (%s)<br/><strong>%s</strong>",
      dat_wgs84$aluenimi, dat_wgs84$aluekoodi,  round(dat_wgs84$value,1)
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




#' Create timeseries plot
#' 
#' @param input_value_regio_level A string.
#' @param input_value_variable A string.
#' @param input_value_region_selected A string.
#' @param input_value_regio_show_mode A string.
#'
#' @export
plot_timeseries <- function(input_value_regio_level = "Kunnat",
                     input_value_variable = "Nuorisotyöttömyys",
                     input_value_region_selected = "Helsinki",
                     input_value_regio_show_mode = "valittu alue ja sen naapurit"){
  
  dat_ts <- get_dat_timeseries()
  
  df <- process_data(input_value_regio_level = input_value_regio_level,
                      input_value_variable = input_value_variable,
                      timeseries = TRUE, 
                     dat_ts = dat_ts)
  region_data <- get_region_data()
  naapurikoodit_lst <- region_data[region_data$level %in% input_value_regio_level & 
                                     region_data$region_name %in% input_value_region_selected,"neigbours"]
  
  naapurikoodit <- naapurikoodit_lst %>% 
    unnest(cols = c(neigbours)) %>% 
    pull(neigbours)
  
  if (input_value_regio_level == "Hyvinvointialueet"){
    load(system.file("data", "ineq_data.rda", package="karttasovellus"))
    df_gini <- ineq_data
    df_gini_kaikki <- df_gini[df_gini$variable == input_value_variable &
                                df_gini$regio_level == "Kunnat",]
    df_gini <- df_gini[df_gini$variable == input_value_variable &
                         df_gini$regio_level == input_value_regio_level,]
    df_gini <- bind_rows(df_gini,df_gini_kaikki)
    
    df_gini2 <- df_gini[df_gini$aluekoodi %in% naapurikoodit,]
    df_gini2 <- bind_rows(df_gini2,df_gini_kaikki)
  }
  df2 <- df[df$aluekoodi %in% naapurikoodit,] %>% 
    filter(!aluenimi %in% input_value_region_selected)

  aika1 <- sort(unique(df$aika)) - 1
  aika2 <- sort(unique(df$aika)) + 1
  labels <- paste0(aika1,"-",aika2)
  
  ggplot() -> plot0
  
  df_selected_cs <- df %>% 
    filter(aika == max(aika, na.rm = TRUE),
           aluenimi == input_value_region_selected)
  
  df_selected_ts <- df %>% 
    filter(aluenimi == input_value_region_selected)
  
  if (input_value_regio_show_mode == "kaikki tason alueet"){
    
    plot0 <- plot0 + 
      geom_line(data = df, aes(x = aika, y = value, color= aluenimi), show.legend = FALSE) +
      geom_line(data = df_selected_ts, aes(x = aika, y = value), color = "#FF00FF") +
      geom_point(data = df_selected_ts, aes(x = aika, y =  value),
                 fill = "#FF00FF", 
                 color = "white", shape = 21, size = 3, stroke = 1.2) +
      geom_text(data = df_selected_cs,
                aes(x = aika, 
                    y = value, 
                    color= aluenimi, 
                    label = paste(aluenimi, round(value,1))),
                color = "#FF00FF", 
                family = "Lato", 
                nudge_x = .3)
    
    if (input_value_regio_level == "Hyvinvointialueet"){
      plot_gini <- ggplot() + 
        geom_line(data = df_gini, aes(x = aika, y = gini, color = aluenimi)) +
        geom_text(
        # ggrepel::geom_text_repel(
          data = df_gini %>% filter(aika == max(aika, na.rm = TRUE)),
          aes(x = aika, y = gini, color= aluenimi, label = paste(aluenimi, round(gini,2))), nudge_x = .2, family = "Lato")
      
    }
    
    
  } else if (input_value_regio_show_mode == "valittu alue ja sen naapurit"){
    
    plot0 <- plot0 + 
      geom_line(data = df2,aes(x = aika, y = value, color= aluenimi), show.legend = FALSE) +
      geom_line(data = df,
                aes(x = aika, y = value, group = aluenimi),
                color = "dim grey", alpha = .1) +
      geom_point(data = df2, 
                 aes(x = aika, y = value,fill= aluenimi), shape = 21, color = "white", stroke = 1, size = 2.5) +
      # ggrepel::geom_text_repel(
      geom_text(
        data = df2 %>% filter(aika == max(aika, na.rm = TRUE)),
        aes(x = aika, y = value, color= aluenimi, 
            label = paste(aluenimi, round(value,1))), 
                               family = "Lato", nudge_x = .3) +
      geom_line(data = df_selected_ts, aes(x = aika, y = value), color = "#FF00FF") +
      geom_point(data = df_selected_ts, aes(x = aika, y = value),
                 fill = "#FF00FF", 
                 color = "white", shape = 21, size = 3, stroke = 1.2) +
      
      geom_text(data = df_selected_cs,
                aes(x = aika, 
                    y = value, 
                    color= aluenimi, 
                    label = paste(aluenimi, round(value,1))),
                color = "black", 
                family = "Lato", 
                nudge_x = .3)
    
    # gini
    if (input_value_regio_level == "Hyvinvointialueet"){
    plot_gini <- ggplot() + 
      geom_line(data = df_gini2, aes(x = aika, y = gini, color = aluenimi)) +
      geom_line(data = df_gini, aes(x = aika, y = gini, color = aluenimi), alpha = .1) +
      # ggrepel::geom_text_repel(
      geom_text(
        data = df_gini2 %>% filter(aika == max(aika, na.rm = TRUE)),
        aes(x = aika, y = gini, color= aluenimi, 
            label = paste(aluenimi, round(gini,1))), 
                               family = "Lato", nudge_x = .3)
    }
    
  } else if (input_value_regio_show_mode == "valitun alueen kunnat"){
    
    dat2 <- create_municipalities_within_region(input_value_variable = input_value_variable, 
                                                input_value_regio_level = input_value_regio_level,
                                                input_value_region_selected = input_value_region_selected,
                                                timeseries = TRUE, 
                                                dat_ts = dat_ts)
    
    if (input_value_regio_level == "Hyvinvointialueet"){
    dat2_gini <- dat2 %>% group_by(aika) %>% 
      mutate(gini = round(ineq::Gini(value),2)) %>% 
      ungroup()
    df_gini2 <- df_gini[df_gini$aluenimi %in% input_value_region_selected,]
    
    plot_gini <- ggplot() + 
      geom_line(data = df_gini2, aes(x = aika, y = gini, color = aluenimi)) +
      geom_line(data = df_gini, aes(x = aika, y = gini, color = aluenimi), alpha = .1) +
      # ggrepel::geom_text_repel(
      geom_text(
        data = df_gini2 %>% filter(aika == max(aika, na.rm = TRUE)),
        aes(x = aika, y = gini, color= aluenimi, 
            label = paste(aluenimi, round(gini,1))), family = "Lato", nudge_x = .3)
    }
    
    plot0 <- plot0 + 
      geom_line(data = dat2,aes(x = aika, y = value, color= aluenimi), show.legend = FALSE) +
      geom_point(shape = 21, color = "white", shape = 21, size = 3, stroke = 1.2) +
      # ggrepel::geom_text_repel(
      geom_text(
        data = dat2 %>% filter(aika == max(aika, na.rm = TRUE)),
        aes(x = aika, y = value, color= aluenimi, label = paste(aluenimi, round(value,1))), 
        nudge_x = .2, family = "Lato")
    
    
  }
  # luodaan alaotsikko
  if (input_value_regio_show_mode == "valitun alueen kunnat"){
    kuvan_subtitle <- glue("Kuvassa näytetään alueeseen {input_value_region_selected} tasolla {tolower(input_value_regio_level)} kuuluvat kunnat")
  } else {
    kuvan_subtitle <- glue("Aluetaso: {input_value_regio_level}")
  }
  
  plot0 +
    scale_x_continuous(breaks = sort(unique(df$aika)), labels = labels) +
    theme_ipsum(base_family = "Lato",
                plot_title_family = "Lato",
                subtitle_family = "Lato",
                base_size = 14,
                plot_title_face = "plain") +
    labs(fill = NULL,
         x = NULL,
         y = paste0(add_line_break2(input_value_variable, 50), "\n(suhdeluku)"),
         title = glue("{input_value_variable}"),
         subtitle = kuvan_subtitle
    )  +
    theme(legend.position = "none",
          panel.grid.major.x = element_line(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title.position = "plot",
          plot.margin = unit(c(0,0,0,0), "mm")#,
    ) -> plot1
  
  labsit <- labs(fill = NULL,
       x = NULL,
       caption = glue("Huono-osaisuus Suomessa -karttasovellus (Diak)\nData: THL (perusdata) & Diak (mediaanisuhteutus)\nTiedot haettu:{Sys.Date()}"))
  
  
  if (input_value_regio_level == "Hyvinvointialueet"){
  plot_gini <- plot_gini + scale_x_continuous(breaks = sort(unique(df$aika)), labels = labels) +
    theme_ipsum(base_family = "Lato",
                plot_title_family = "Lato",
                subtitle_family = "Lato",
                base_size = 14,
                plot_title_face = "plain") + labsit + 
    labs(subtitle = paste0("Indikaattorin ", add_line_break2(input_value_variable, 50), "\neriarvoisuuden kuntatason gini-kerroin aluetasolla: ", input_value_regio_level)) +
    theme(legend.position = "none",
          panel.grid.major.x = element_line(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title.position = "plot",
          axis.title.x = element_blank(),
          plot.margin = unit(c(10,0,0,0), "mm"),
          axis.text.x = element_blank()
  )
  plotti <- patchwork::wrap_plots(plot1,plot_gini, ncol = 1, heights = c(1,1))
  } else {
    plotti <- plot1 + labsit
  }
  return(plotti)

}


#' Alt text function
#' 
#' @param which_plot A string.
#' @param input_value_regio_level A string.
#' @param input_value_variable A string.
#' @param input_value_region_selected A string.
#' @param input_value_regio_show_mode A string.
#'
#' @export
alt_txt_indicator <- function(
                    which_plot = "dotplot", # map # timeseries
                    input_value_regio_show_mode = "kaikki tason alueet",
                    input_value_variable = "Huono-osaisuus yhteensä",
                    input_value_regio_level = "Hyvinvointialueet",
                    input_value_region_selected = "Etelä-Karjalan HVA"
                    ){
  
  kuvatyyppi <- ifelse(which_plot == "dotplot", "Pistekuviossa", 
                       ifelse(which_plot == "map", "Kartassa", "Aikasarjakuviossa"))
  taytto <- ifelse(which_plot == "dotplot", "pisteiden väri", 
                       ifelse(which_plot == "map", "alueiden väri", "pisteiden ja viivan väri"))
  spessu <- ifelse(which_plot == "dotplot", 
"\nPistekuviossa vaaka-akselilla on osoittimen arvo ja pystyakselilla alueiden nimet laskevassa järjestyksessä osoittimen arvon mukaan. Kunkin alueen pisteestä on vaakaviiva osoittimen mediaaniarvoon 100, jonka kohdalla on pystyviiva.", 
                   ifelse(which_plot == "map", 
                          "", 
                          ""))
  giniteksti <- ifelse(input_value_regio_level == "Hyvinvointialueet" & which_plot == "timeseries", 
                       "Aikasarjakuvion alla näytetään kunkin hyvinvointialueen kuntien välinen eriarvoisuus gini-kertoimena. Kerroin näytetään vain alueilta joihin kuuluu vähintään viisi kuntaa.", 
                       "")
  
  kertoo_mista <- ifelse(which_plot == "timeseries",
                         "erottaa alueiden aikasarjat toisistaan",
                         "kertoo osoittimen arvon niin että tummempi väri kertoo korkeammasta osoittimen arvosta")
  
  
  if (input_value_regio_show_mode == "kaikki tason alueet"){
    
    regio_show_mode <- glue::glue("{kuvatyyppi} näytetään kaikki aluetason alueet ja korostettuna on {input_value_region_selected}")
    
    
  } else if (input_value_regio_show_mode == "valittu alue ja sen naapurit"){
    
    region_data <- get_region_data()
    naapurikoodit <- get_naapurikoodit(input_value_regio_level = input_value_regio_level,
                                       input_value_region_selected = input_value_region_selected)
    region_nms <- region_data[region_data$level %in% input_value_regio_level & region_data$region_code %in% naapurikoodit, ]$region_name
    regio_show_mode <- glue::glue('{kuvatyyppi} näytetään alueet {glue::glue_collapse(region_nms, sep = ", ", last = " ja ")}, korostettuna {input_value_region_selected}')
    
  } else if (input_value_regio_show_mode == "valitun alueen kunnat"){
    
    dat <- create_municipalities_within_region(input_value_variable =  input_value_variable,
                                               input_value_regio_level = input_value_regio_level,
                                               input_value_region_selected = input_value_region_selected,
                                               timeseries = FALSE)
    regio_show_mode <- glue::glue('{kuvatyyppi} näytetään alueen {input_value_region_selected} kunnat {glue::glue_collapse(unique(dat$aluenimi), sep = ", ", last = " ja ")}')    
  }
  
  alt_teksti <- glue::glue("
{kuvatyyppi} {input_value_variable} osoittimen arvot aluetasolla {input_value_regio_level}. 
{regio_show_mode}.
{kuvatyyppi} {taytto} {kertoo_mista}.{spessu}
{giniteksti}")
  
  return(alt_teksti)
}


