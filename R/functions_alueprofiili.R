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
    load(system.file("data", "regio_Hyvinvointialueet.rda", package="karttasovellus"))
    reg <- regio_Hyvinvointialueet
  } else if (input_value_regio_level_profile == "Seutukunnat"){
    load(system.file("data", "regio_Seutukunnat.rda", package="karttasovellus"))
    reg <- regio_Seutukunnat
  } else if (input_value_regio_level_profile == "Kunnat"){
    load(system.file("data", "regio_Kunnat.rda", package="karttasovellus"))
    reg <- regio_Kunnat
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
    mutate(asema = 1:n(),
           n = n()) %>% 
    ungroup() %>% 
    # filter(aluenimi %in% aluename2) %>% 
    select(aika,aluenimi,variable,asema) -> rank_dat
  
  bind_rows(tmpdat,max_dat,min_dat) %>% 
    left_join(rank_dat) %>% 
    mutate(value = round(value, 1)) %>% 
    rename(muuttuja = variable,
           arvo = value) %>% 
    filter(!is.na(arvo)) %>% 
    select(aika,muuttuja,arvo,asema,everything()) %>% 
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
                                    val_muuttuja = "Huono-osaisuus yhteensä",
                                    region_data = NULL,
                                    tabdat = NULL,
                                    naapurikoodit = NULL){
  
  
  val_aluename <- input_value_region_profile
  val_aluetaso1 <- input_value_regio_level_profile
  
  if (is.null(region_data)){
    region_data <- get_region_data()
    region_data <- dplyr::filter(region_data, level %in% val_aluetaso1)
    naapurikoodit <- region_data[region_data$region_name %in% val_aluename,]$neigbours[[1]]    
  }
  if (is.null(tabdat)){
    tabdat <- create_alueprofiili_content(input_value_region_profile = val_aluename, 
                                          input_value_regio_level_profile = val_aluetaso1)    
  }

  lista1_tbl <- tabdat %>%
    filter(var_class == val_muuttujaluokka,
           muuttuja == val_muuttuja) %>%
    filter(rooli %in% c("naapuri","valinta")) %>% 
    select(muuttuja,aluenimi,arvo,asema) %>%
    arrange(muuttuja,asema)
  mapdata_tmp <- left_join(region_data, lista1_tbl, by = c("region_name" = "aluenimi")) %>% 
    filter(!is.na(muuttuja))
  
  nregs <- length(unique(mapdata_tmp$arvo))
  
  plot <- ggplot(data = mapdata_tmp, aes(fill = arvo)) +                    
    geom_sf(color = alpha("white", 1/3))  +
    hrbrthemes::theme_ipsum(base_family = "Lato", 
                            base_size = 11, 
                            plot_title_size = 12) +
    # scale_fill_viridis_c(option = "plasma") +
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
                  color = "black", fill = "white", family = "Lato", size = 3, label.size = 0)
  
  if (nregs > 1) plot <- plot +  scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1)
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
                                       input_value_region_profile = "Etelä-Karjalan HVA", 
                                       val_muuttujaluokka = "Summamuuttujat",
                                       val_muuttuja1 = "Huono-osaisuus yhteensä",
                                       region_data = NULL,
                                       tabdat = NULL,
                                       naapurikoodit = NULL){
  
  
  val_aluename <- input_value_region_profile
  val_aluetaso1 <- input_value_regio_level_profile
  
  if (is.null(region_data)){
    region_data <- get_region_data()
    region_data <- dplyr::filter(region_data, level %in% val_aluetaso1)
    naapurikoodit <- region_data[region_data$region_name %in% val_aluename,]$neigbours[[1]]    
  }
  if (is.null(tabdat)){
    tabdat <- create_alueprofiili_content(input_value_region_profile = val_aluename, 
                                          input_value_regio_level_profile = val_aluetaso1,
                                          aikasarja = TRUE)    
  }
  tabdat1 <- tabdat[tabdat$var_class == val_muuttujaluokka & tabdat$muuttuja == val_muuttuja1, ]
  
  plotdata_tmp <- tabdat1 %>%
    filter(rooli %in% c("naapuri","valinta")) %>% 
    select(aika,muuttuja,aluenimi,arvo,asema) %>%
    mutate(value = arvo) %>% 
    arrange(muuttuja,asema)
  # plotdata_tmp <- lista1_tbl[lista1_tbl$muuttuja == vars[vii],]
  aika1 <- sort(unique(plotdata_tmp$aika)) - 1
  aika2 <- sort(unique(plotdata_tmp$aika)) + 1
  labels <- paste0(aika1,"-\n",aika2)
  
  plot <- ggplot(data = plotdata_tmp,
                 aes(x = aika, y = value, color= aluenimi, fill= aluenimi)) +
    geom_line(show.legend = FALSE) +
    geom_point(shape = 21, color = "white", stroke = 1, size = 2.5) +
    ggrepel::geom_text_repel(data = plotdata_tmp %>% filter(aika == max(aika, na.rm = TRUE)),
                             aes(label = paste(aluenimi, round(value,1))), family = "Lato") +
    # ggrepel::geom_text_repel(data = plotdata_tmp %>% filter(aika == max(aika, na.rm = TRUE)-1),
    #                          aes(label = aluenimi), family = "Lato", nudge_x = -1) +
    
    scale_x_continuous(breaks = sort(unique(plotdata_tmp$aika)), labels = labels) +
    theme_ipsum(base_family = "Lato", 
                base_size = 11, 
                plot_title_size = 12,
                plot_title_family = "Lato",
                subtitle_family = "Lato"#,
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
    select(muuttuja,aluenimi,arvo,asema) %>%
    arrange(muuttuja,asema) #%>% 
    # mutate(asema = paste0(asema,".")) # piste pois koska ongelma gt:llä tehdyissä taulukoissa (tulee aina nro\.)
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


# printtifunktiot ----


#' Add line breaks
#' 
#' @param x A string.
#' @param n A integer
#'
#' @export
print_add_line_break2 <- function(x = "very many many characters and words and sentences",
                            n = 20){
  y <- gsub(paste0("(.{1,",n,"})(\\s|$)"), "\\1\n", x)
  y <- sub("\n$", "", y)
  return(y)
} 

#' Create table for print
#' 
#' @param df A data frame.
#' @param varclass A string.
#'
#' @export
print_create_table <- function(varclass = "Summamuuttujat",
                               valid_vars = "Huono-osaisuus yhteensä",
                               tabdat2 = tabdat
                               ){
  
  tabdat2 %>% 
    filter(var_class == varclass,
           muuttuja %in% valid_vars) %>% 
    select(-var_class) -> tmptbl
  
  vars <- unique(tmptbl$muuttuja)
  for (vi in seq_along(vars)){
    cat("\n\n")
    cat(paste0("## ", vars[vi]))
    cat("\n\n")
    print(kable(tmptbl[tmptbl$muuttuja == vars[vi],] %>% 
                  mutate(aluenimi = paste0(aluenimi, " (", rooli, ") ")) %>%
                  arrange(asema) %>% 
                  select(aluenimi,arvo,asema),
                
                format = "pipe", 
                row.names = FALSE))
  }
}

#' Create table for print
#' 
#' @param varclass a string.
#' @param reglevel a string.
#' @param spatdat a sf
#' @param naapurikoodit a integer.
#' @param ncol a integer.
#'
#' @export
print_create_map_plot <- function(varclass = "Inhimillinen huono-osaisuus", 
                            reglevel = "Kunnat", 
                            regname = "Alajärvi", 
                            naapurit = c(91,92,924),
                            valid_vars = c("Huono-osaisuus yhteensä", "Inhimillinen huono-osaisuus", 
                                           "Koulutuksen ulkopuolelle jääneet 17–24-vuotiaat", "Nuorisotyöttömyys", 
                                           "Pitkäaikaistyöttömyys", "Toimeentulotukea pitkäaikaisesti saaneet 18–24-vuotiaat", 
                                           "Toimeentulotukea pitkäaikaisesti saaneet 25–64-vuotiaat", 
                                           "Kunnan yleinen pienituloisuusaste", "Alkoholikuolleisuus 35-64 vuotiailla", 
                                           "Itsemurhakuolleisuus", "Itsensä yksinäiseksi tuntevien osuus", 
                                           "Psyykkisesti merkittävästi kuormittuneiden osuus", "Terveytensä keskitasoiseksi tai sitä huonommaksi kokevien osuus", 
                                           "Huono-osaisuuden sosiaaliset seuraukset", "Kodin ulkopuolelle sijoitetut 0 – 17-vuotiaat", 
                                           "Poliisin tietoon tulleet henkeen ja terveyteen kohdistuneet rikokset", 
                                           "Päihteiden vaikutuksen alaisena tehdyistä rikoksista syyllisiksi epäillyt", 
                                           "0 - 17-vuotiaat lapset, joista on tehty lastensuojeluilmoitus", 
                                           "Rahanpuutteen vuoksi ruoasta, lääkkeistä tai lääkärikäynneistä tinkimään joutuneiden osuus", 
                                           "Korkeat asiakasmaksut haitanneet hoidon saantia", "Huono-osaisuuden taloudelliset yhteydet", 
                                           "Kunnan osarahoittama työmarkkinatuki", "Lastensuojelun laitos- ja perhehoidon nettokäyttökustannukset", 
                                           "Aikuisten mielenterveyden avohoitokäynnit", "Päihteiden vuoksi sairaaloiden ja terveyskeskusten vuodeosastoilla hoidetut potilaat", 
                                           "Päihdehuollon avopalveluissa asiakkaita", "Täydentävä toimeentulotuki", 
                                           "Päihteiden vaikutuksen alaisena tehdyistä rikoksista syyllisiksi epäillyt", 
                                           "Poliisin tietoon tulleet henkeen ja terveyteen kohdistuneet rikokset", 
                                           "Lastensuojelun laitos- ja perhehoidon nettokäyttökustannukset", 
                                           "Huono-osaisuuden sosiaaliset seuraukset"),
                            ncol = 2){
  
  datmpt <- get_dat() %>% filter(regio_level == reglevel)
  spatdat <- process_data_profile_doc(input_value_regio_level_profile = reglevel)
  
  dattemp <- datmpt %>% 
    filter(var_class %in% varclass, 
           variable %in% valid_vars)
  
  vars_original <- dattemp %>% 
    distinct(variable) %>% 
    pull(variable)
  
if (length(vars_original) > 6) zz = 2 else zz = 1
for (i in 1:zz){
  if (zz == 1){
    vars <- vars_original
  } else if (i == 1){
    vars <- vars_original[1:6]
  } else {
    vars <- vars_original[7:length(vars_original)]
  }
    gg_x <- list()
    for (i in 1:length(vars)){
      plot_dat <- dattemp %>% filter(variable %in% vars[i]) 
      dat_naapurit <- spatdat[spatdat$aluekoodi %in% naapurit,] %>% select(-value)
      dat_naapurit2 <- left_join(dat_naapurit, plot_dat %>% select(aluekoodi,value)
      )
      nregs <- length(unique(dat_naapurit2[dat_naapurit2$variable == vars[i],]$value))

      ggplot(data = dat_naapurit2, aes(fill = value, label = value)) +                    
        geom_sf(color = alpha("white", 1/3))  +
        theme_ipsum(base_family = "Lato", 
                    base_size = 9, 
                    plot_title_size = 10,
                    plot_title_family = "Lato",
                    subtitle_family = "Lato"
        ) -> p
      if (nregs > 1){
        p <- p + scale_fill_fermenter(palette = "YlGnBu", type = "seq", direction = 1)  
      }
      gg_x[[i]] <- p + theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x =  element_blank(),
              axis.title.y =  element_blank(),
              plot.title.position = "plot",
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        labs(x = NULL, y = NULL, fill = NULL,
             title = add_line_break2(vars[i], n = 35)) +
        geom_label(data = dat_naapurit2 %>%  
                     sf::st_set_geometry(NULL) %>%
                     bind_cols(dat_naapurit2 %>%
                                 sf::st_centroid() %>%
                                 sf::st_coordinates() %>% as_tibble()),
                   aes(label = paste0(aluenimi, "\n",
                                      round(value,1)), x = X, y = Y),
                   color = "black", fill = alpha("white", 2/3), size = 2.5, label.size = 0, family = "Lato", lineheight = .8)
    }
    gg_x2 <- purrr::compact(gg_x)
    blank <- ggplot() + theme_minimal()
    if (length(gg_x2) == 3){
      gg_x2[[4]] <- blank
      gg_x2[[5]] <- blank
      gg_x2[[6]] <- blank      
    } else if (length(gg_x2) == 4){
      gg_x2[[5]] <- blank
      gg_x2[[6]] <- blank      
    } else if (length(gg_x2) == 5){
      gg_x2[[6]] <- blank      
    }
    p1 <- wrap_plots(gg_x2, ncol = ncol) +
      plot_annotation(
        subtitle = glue("Kuhunkin kuvioon on merkitty alueen '{regname}' lisäksi alueen rajanaapurit"),
        theme = theme_minimal(base_family = "Lato", base_size = 10) + 
          theme(plot.background = element_rect(fill = NA, color = alpha(colour = "dim grey", 1/6)),
                plot.subtitle = element_text(size = 11))
      )
    print(p1)
  }
}

#' Create table for print
#' 
#' @param varclass a string.
#' @param reglevel a string.
#' @param spatdat a sf
#' @param naapurikoodit a integer.
#' @param ncol a integer.
#'
#' @export
print_create_timeseries_plot <- function(varclass = "Summamuuttujat", 
                                         reglevel = "Kunnat", 
                                         regname = "Alajärvi", 
                                         naapurit = c(91,92),
                                         valid_vars = c("Huono-osaisuus yhteensä", "Inhimillinen huono-osaisuus", 
                                                        "Koulutuksen ulkopuolelle jääneet 17–24-vuotiaat", "Nuorisotyöttömyys", 
                                                        "Pitkäaikaistyöttömyys", "Toimeentulotukea pitkäaikaisesti saaneet 18–24-vuotiaat", 
                                                        "Toimeentulotukea pitkäaikaisesti saaneet 25–64-vuotiaat", 
                                                        "Kunnan yleinen pienituloisuusaste", "Alkoholikuolleisuus 35-64 vuotiailla", 
                                                        "Itsemurhakuolleisuus", "Itsensä yksinäiseksi tuntevien osuus", 
                                                        "Psyykkisesti merkittävästi kuormittuneiden osuus", "Terveytensä keskitasoiseksi tai sitä huonommaksi kokevien osuus", 
                                                        "Huono-osaisuuden sosiaaliset seuraukset", "Kodin ulkopuolelle sijoitetut 0 – 17-vuotiaat", 
                                                        "Poliisin tietoon tulleet henkeen ja terveyteen kohdistuneet rikokset", 
                                                        "Päihteiden vaikutuksen alaisena tehdyistä rikoksista syyllisiksi epäillyt", 
                                                        "0 - 17-vuotiaat lapset, joista on tehty lastensuojeluilmoitus", 
                                                        "Rahanpuutteen vuoksi ruoasta, lääkkeistä tai lääkärikäynneistä tinkimään joutuneiden osuus", 
                                                        "Korkeat asiakasmaksut haitanneet hoidon saantia", "Huono-osaisuuden taloudelliset yhteydet", 
                                                        "Kunnan osarahoittama työmarkkinatuki", "Lastensuojelun laitos- ja perhehoidon nettokäyttökustannukset", 
                                                        "Aikuisten mielenterveyden avohoitokäynnit", "Päihteiden vuoksi sairaaloiden ja terveyskeskusten vuodeosastoilla hoidetut potilaat", 
                                                        "Päihdehuollon avopalveluissa asiakkaita", "Täydentävä toimeentulotuki", 
                                                        "Päihteiden vaikutuksen alaisena tehdyistä rikoksista syyllisiksi epäillyt", 
                                                        "Poliisin tietoon tulleet henkeen ja terveyteen kohdistuneet rikokset", 
                                                        "Lastensuojelun laitos- ja perhehoidon nettokäyttökustannukset", 
                                                        "Huono-osaisuuden sosiaaliset seuraukset"),
                                         ncol = 2){
  
  dattemp <- get_dat_timeseries() %>% filter(var_class %in% varclass, 
                                             regio_level %in% reglevel,
                                             variable %in% valid_vars)
  vars_original <- dattemp %>% 
    distinct(variable) %>% 
    pull(variable)
  
  if (length(vars_original) > 6) zz = 2 else zz = 1
  for (i in 1:zz){
    if (zz == 1){
      vars <- vars_original
    } else if (i == 1){
      vars <- vars_original[1:6]
    } else {
      vars <- vars_original[7:length(vars_original)]
    }
    gg_x <- list()
    for (i in 1:length(vars)){
    
    plot_dat <- dattemp %>% filter(variable %in% vars[i])
    
    plotdata_tmp <- plot_dat[plot_dat$aluekoodi %in% naapurit,]
    
    aika1 <- sort(unique(plotdata_tmp$aika)) - 1
    aika2 <- sort(unique(plotdata_tmp$aika)) + 1
    labels <- paste0(aika1,"-\n",aika2)
    
    gg_x[[i]] <- ggplot(data = plotdata_tmp,
                aes(x = aika, y = value, color= aluenimi, fill= aluenimi)) +
      geom_line(show.legend = FALSE) +
      geom_point(shape = 21, color = "white", stroke = 1, size = 2) +
      geom_text(data = plotdata_tmp %>% filter(aika == max(aika, na.rm = TRUE)),
                aes(label = paste0(aluenimi, " ",round(value,1))), family = "Lato", size = 2.8) +
      scale_x_continuous(breaks = sort(unique(plotdata_tmp$aika)), labels = labels) +
      theme_ipsum(base_family = "Lato", 
                  base_size = 9, 
                  plot_title_size = 10,
                  plot_title_family = "Lato",
                  subtitle_family = "Lato"
      ) +
      theme(legend.position = "none",
            plot.title.position = "plot",
            panel.grid.major.x = element_line(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.x = element_text(size = 7)) +
      labs(y = NULL, x = NULL,
           title = add_line_break2(vars[i], n = 35))
  }
  
  gg_x2 <- purrr::compact(gg_x)
  blank <- ggplot() + theme_minimal()
  if (length(gg_x2) == 3){
    gg_x2[[4]] <- blank
    gg_x2[[5]] <- blank
    gg_x2[[6]] <- blank      
  } else if (length(gg_x2) == 4){
    gg_x2[[5]] <- blank
    gg_x2[[6]] <- blank      
  } else if (length(gg_x2) == 5){
    gg_x2[[6]] <- blank      
  }
  p1 <- wrap_plots(gg_x2, ncol = ncol) +
    plot_annotation(
      subtitle = glue("Kuhunkin kuvioon on merkitty alueen '{regname}' lisäksi alueen rajanaapurit"),
      theme = theme_minimal(base_family = "Lato", base_size = 10) + 
        theme(plot.background = element_rect(fill = NA, color = alpha(colour = "dim grey", 1/6)), 
              plot.subtitle = element_text(size = 11))
    )
  print(p1)
  }
}

#' Create table for print
#' 
#' @param params_region_level a string.
#' @param params_region a string.
#' @param params_naapurikoodit a integer.
#'
#' @export
print_create_tabdat <- function(params_region_level = "Kunnat",
                                params_region = "Alajärvi",
                                params_naapurikoodit = c(91,924,92)){
  
  dat <- get_dat()
  dat[dat$regio_level %in% params_region_level & dat$aluenimi %in% params_region,] %>% 
    select(aluenimi,var_class,variable,value) %>% 
    mutate(rooli = "valittu") -> tmpdat1
  dat[dat$regio_level %in% params_region_level & dat$aluekoodi %in% params_naapurikoodit,] %>% 
    filter(!aluenimi %in% params_region) %>% 
    select(aluenimi,var_class,variable,value) %>% 
    mutate(rooli = "naapuri")-> tmpdat2
  tmpdat <- bind_rows(tmpdat1,tmpdat2)
  
  ## 
  dat[dat$regio_level %in% params_region_level,] %>% 
    group_by(var_class,variable) %>% 
    arrange(desc(value)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(rooli = "korkein arvo") %>%
    select(aluenimi,var_class,variable,value,rooli) -> max_dat
  
  dat[dat$regio_level %in% params_region_level,] %>% 
    group_by(var_class,variable) %>% 
    arrange(value) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(rooli = "matalin arvo") %>% 
    select(aluenimi,var_class,variable,value,rooli) -> min_dat
  
  dat[dat$regio_level %in% params_region_level,] %>% 
    group_by(variable) %>% 
    arrange(desc(value)) %>%
    mutate(asema = 1:n(),
           n = n()) %>% 
    ungroup() %>% 
    select(aluenimi,variable,asema) -> rank_dat
  
  bind_rows(tmpdat,max_dat,min_dat) %>% 
    left_join(rank_dat) %>% 
    mutate(value = round(value, 1)) %>% 
    rename(muuttuja = variable,
           arvo = value) %>% 
    filter(!is.na(arvo)) %>% 
    select(muuttuja,arvo,asema,everything()) %>% 
    distinct() -> tabdat
  return(tabdat)
}
