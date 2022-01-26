if (FALSE) {
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





#' Process data for profile
#' 
#' @param input_value_regio_level_profile A string.
#'
#' @export
process_data_profile_doc <- function(input_value_regio_level_profile = "Hyvinvointialueet"){
  
  dat <- karttasovellus::get_dat()
  
  dat_1 <- dat[dat$regio_level %in% input_value_regio_level_profile,]
  
  if (input_value_regio_level_profile == "Hyvinvointialueet"){
    reg <- karttasovellus::regio_Hyvinvointialueet
  } else if (input_value_regio_level_profile == "Seutukunnat"){
    reg <- karttasovellus::regio_Seutukunnat
  } else if (input_value_regio_level_profile == "Kunnat"){
    reg <- karttasovellus::regio_Kunnat
  }
  res <- left_join(reg, dat_1)
  
  res <- res %>%
    select(-regio_level) %>%
    filter(!is.na(value)) %>%
    arrange(desc(value)) %>%
    mutate(value = round(value, 1)) %>%
    mutate(rank = 1:n())
  
  res <- sf::st_transform(x = res, crs = "+proj=longlat +datum=WGS84")
  return(res)
  
}


#' Process data for profile
#' 
#' @param input_value_region_profile A string.
#' @param input_value_regio_level_profile A string.
#' @param type A string.
#' @param aikasarja A logical.
#'
#' @export
create_alueprofiili_content <- function(input_value_region_profile = "Itä-Uudenmaan HVA", 
                                        input_value_regio_level_profile = "Hyvinvointialueet", 
                                        type = "html", 
                                        aikasarja = FALSE){
  
  aluename2 <- input_value_region_profile
  aluetaso1 <- input_value_regio_level_profile
  region_data <- get_region_data()
  naapurikoodit <- region_data[region_data$level %in% aluetaso1 & region_data$region_name %in% aluename2,]$neigbours[[1]]
  
  
  if (aikasarja){
    dat <- get_dat_timeseries()
  } else {
    dat <- get_dat()
    dat$aika <- NA
  }
  
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


#' Process data for profile
#' 
#' @param input_value_region_profile A string.
#' @param input_value_regio_level_profile A string.
#' @param val_muuttujaluokka A string.
#' @param val_muuttuja A string.
#'
#' @export
alueprofiilikartta_html <- function(input_value_regio_level_profile = "Hyvinvointialueet", 
                                    input_value_region_profile = "Itä-Uudenmaan HVA", 
                                    val_muuttujaluokka = "Summamuuttujat",
                                    val_muuttuja = "Huono-osaisuus yhteensä"){
  
  
  val_aluename <- input_value_region_profile
  val_aluetaso1 <- input_value_regio_level_profile
  
  region_data <- get_region_data()
  region_data <- dplyr::filter(region_data, level %in% val_aluetaso1)
  naapurikoodit <- region_data[region_data$region_name %in% val_aluename,]$neigbours[[1]]
  
  tabdat <- create_alueprofiili_content(input_value_region_profile = val_aluename, 
                                        input_value_regio_level_profile = val_aluetaso1)

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

#' Process data for profile
#' 
#' @param input_value_region_profile A string.
#' @param input_value_regio_level_profile A string.
#' @param val_muuttujaluokka A string.
#' @param val_muuttuja A string.
#'
#' @export
alueprofiiliaikasarja_html <- function(input_value_regio_level_profile = "Hyvinvointialueet", 
                                       input_value_region_profile = "Itä-Uudenmaan HVA", 
                                       val_muuttujaluokka = "Summamuuttujat",
                                       val_muuttuja1 = "Huono-osaisuus yhteensä"){
  
  val_aluename <- input_value_region_profile
  val_aluetaso1 <- input_value_regio_level_profile
  
  region_data <- get_region_data()
  region_data <- dplyr::filter(region_data, level %in% val_aluetaso1)
  naapurikoodit <- region_data[region_data$region_name %in% val_aluename,]$neigbours[[1]]
  
  tabdat <- create_alueprofiili_content(input_value_region_profile = val_aluename, 
                                        input_value_regio_level_profile = val_aluetaso1, 
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


#' Process data for profile
#' 
#' @param dd1 A data frame
#' @param muuttujanimi A string.
#' @param varnro A integer
#'
#' @export
create_raw_tbl <- function(dd1 = tabdat_tmp, 
                           muuttujanimi= muuttujanimi, 
                           varnro = 1){
  dd2 <- dd1 %>% 
    mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>% 
    select(muuttuja,aluenimi,arvo,sija) %>%
    arrange(muuttuja,sija) %>% 
    mutate(sija = paste0(sija,"."))
  dd3 <- dd2[dd2$muuttuja == muuttujanimi[varnro], ] %>% 
    select(-muuttuja)
  return(dd3)
}


#' Process data for profile
#' 
#' @param lst_df A data frame
#'
#' @export
create_gt_tbl <- function(lst_df = lista1_tbl02){
  kableExtra::kable_styling(knitr::kable(lst_df)) -> gt_tbl
  htmltools::HTML(gt_tbl) -> gt_tbl
  return(gt_tbl)
}

#' Process data for profile
#' 
#' @param varname A string.
#'
#' @export
create_alt_text_aikasarja <- function(varname = NULL, 
                                      input_value_regio_level_profile = "Hyvinvointialueet",
                                      input_value_region_profile = "Itä-Uudenmaan HVA"){
  # alt-tekstiin
  
  aluename <- input_value_region_profile
  aluetaso1 <- input_value_regio_level_profile
  
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

#' Process data for profile
#' 
#' @param varname A string.
#'
#' @export
create_alt_text_kartta <- function(varname = NULL, 
                                   input_value_regio_level_profile = "Hyvinvointialueet",
                                   input_value_region_profile = "Itä-Uudenmaan HVA"){
  # alt-tekstiin
  
   aluename <- input_value_region_profile
   aluetaso1 <- input_value_regio_level_profile
  
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
