
library(readxl)
library(dplyr)
library(tidyr)
library(glue)
library(fs)
library(ggplot2)
library(here)
#options(tibble.print_max = 200)
setwd(here("./inst/extras/"))


## POIKKILEIKKAUSDATA ----

# dir_ls(here("./data_raw/"), glob = "*.xlsx") -> flies
# dd <- read_excel("./data_raw/Kopio_Huono-osaisuuden mittarit - KAIKKI.xlsx")
dd <- read_excel("./data_raw/Huono-osaisuusindikaattorit - uusimmat.xlsx")
fs::file_copy("../data_storage/v20211104/huono_osaisuusindikaattorit_20211104.xlsx", "./data_raw/")
dd <- read_excel("./data_raw/huono_osaisuusindikaattorit_20211104.xlsx") %>% 
  select(1:31) %>% 
  filter(!is.na(Aika)) %>% 
  select(-Aika)

df_tmp <- dd %>%
  rename(aluekoodi = Aluekoodi,
         aluenimi = Aluenimi,
         regio_level = Aluetaso) %>% 
  mutate(aluekoodi = as.integer(ifelse(regio_level == "Seutukunnat", sub("^SK", "", aluekoodi), aluekoodi))) %>% 
  select(regio_level,everything()) %>% 
  # maakunnat veks
  filter(regio_level != "Maakunnat") %>% 
  # hyvinvointialue -> hyvinvointialueet
  mutate(regio_level = ifelse(regio_level == "Hyvinvointialue", "Hyvinvointialueet", regio_level)) %>% 
  mutate(aluenimi = ifelse(aluenimi == "Helsinki (sosiaali- ja terveydenhuolto, sekä pelastustoimi)", 
                           "Helsingin kaupunki", aluenimi)) %>% 
  mutate(aluenimi = ifelse(grepl("hyvinvointialue", aluenimi), 
                           sub("hyvinvointialue", "HVA", aluenimi),
                           aluenimi))

# Uusi data, jossa sarakenimet ei korjattu
tibble(name1 = names(df_tmp)) %>% 
  mutate(osoitin = ifelse(grepl("^[A-z] - ", name1), TRUE, FALSE),
         summa = ifelse(grepl("^[A-Z][a-z]", name1), TRUE, FALSE)) %>%
  mutate(name2 = ifelse(summa, toupper(name1), sub("^[A-z] - ", "", name1)
  )) %>% 
  pull(name2) -> new_names
names(df_tmp) <- new_names


ekaiso <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Tsekataan mitkä muuttujat
df_var_class <- tibble(
  nms = names(df_tmp)
) %>% 
  mutate(nms_sani = gsub(" |-", "", nms)) %>% 
  mutate(all_upper = grepl("^[[:upper:]]+$", nms_sani)) %>% 
  mutate(var_class = ifelse(all_upper, nms, NA)) %>% 
  mutate(var_class = zoo::na.locf0(var_class)) %>% 
  mutate(var_class = ifelse(all_upper, "Summamuuttujat", ekaiso(tolower(var_class)))) 

df <- df_tmp %>% 
  pivot_longer(names_to = "variable", values_to = "value", cols = 4:ncol(.)) %>% 
  left_join(df_var_class %>% select(nms,var_class,all_upper), 
            by = c("variable" = "nms")) %>% 
  # Piennetään ALL_UPPER nimet
  mutate(variable = ifelse(all_upper, ekaiso(tolower(variable)), variable))

# Otetaan seutukuntanimiksi Tilastokeskuksen lyhyemmät
geofi::municipality_key_2021 %>% 
  count(seutukunta_name_fi,seutukunta_code) %>% 
  rename(aluekoodi = seutukunta_code,
         aluename = seutukunta_name_fi) %>% 
  select(-n) -> sk_names

df_v20211104 <- left_join(df,sk_names) %>% 
  mutate(aluenimi = ifelse(regio_level == "Seutukunnat", aluename, aluenimi)) %>% 
  select(-aluename) %>% 
  filter(!variable %in% c("Inhimillinen","Sosiaalinen","Taloudellinen"),
         !is.na(value)) 

# saveRDS(df2, here("./data/df_v20211104.RDS"), 
#         compress = FALSE)

save(df_v20211104, file = here::here("data/df_v20211104.rda"),
     compress = "bzip2")


# Apudata karttojen tekoon
muni <- geofi::get_municipalities(year = 2021) %>%
  filter(maakunta_name_fi != "Ahvenanmaa")
regio_Seutukunnat <- muni %>% 
  group_by(seutukunta_code) %>% 
  summarise() %>% rename(aluekoodi = seutukunta_code) 
regio_Hyvinvointialueet <- muni %>% 
  group_by(hyvinvointialue_code) %>% 
  summarise() %>% rename(aluekoodi = hyvinvointialue_code)
regio_Kunnat <- muni %>% 
  select(municipality_code) %>% 
  rename(aluekoodi = municipality_code)
regio_Suomi <- muni %>% 
  mutate(maa = "Suomi") %>% 
  group_by(maa) %>% 
  summarise()

# saveRDS(regio_Suomi, 
#         here("data/regio_Suomi.RDS"))
save(regio_Suomi, file = here::here("data/regio_Suomi.rda"),
     compress = "bzip2")
# saveRDS(regio_Hyvinvointialueet, 
#         here("data/regio_Hyvinvointialueet.RDS"))
save(regio_Hyvinvointialueet, file = here::here("data/regio_Hyvinvointialueet.rda"),
     compress = "bzip2")
# saveRDS(regio_Seutukunnat, 
#         here("data/regio_Seutukunnat.RDS"))
save(regio_Seutukunnat, file = here::here("data/regio_Seutukunnat.rda"),
     compress = "bzip2")
# saveRDS(regio_Kunnat, 
#         here("data/regio_Kunnat.RDS"))
save(regio_Kunnat, file = here::here("data/regio_Kunnat.rda"),
     compress = "bzip2")


## NAAPURIDATA ----

# tehdään aluekoodi/aluenimi -data vielä
muni <- get_municipalities(year = 2021) %>%
  filter(maakunta_name_fi != "Ahvenanmaa")
bind_rows(
  muni %>% 
    group_by(seutukunta_code,seutukunta_name_fi) %>% 
    summarise() %>% rename(region_code = seutukunta_code,
                           region_name = seutukunta_name_fi) %>% 
    mutate(level = "Seutukunnat"),
  muni %>% 
    group_by(hyvinvointialue_code,hyvinvointialue_name_fi) %>% 
    summarise() %>% rename(region_code = hyvinvointialue_code,
                           region_name = hyvinvointialue_name_fi) %>% 
    mutate(region_name = ifelse(grepl("hyvinvointialue", region_name), 
                             sub("hyvinvointialue", "HVA", region_name),
                             region_name)) %>% 
    mutate(level = "Hyvinvointialueet"),
  muni %>% 
    group_by(municipality_code,municipality_name_fi) %>% 
    summarise() %>% rename(region_code = municipality_code,
                           region_name = municipality_name_fi) %>% 
    mutate(level = "Kunnat")
) -> region_data


# lisätään naapurialueet
lvs <- unique(region_data$level)
datalist <- list()
for (ii in seq_along(lvs)){
  region_data2 <- region_data[region_data$level == lvs[ii],]
  datalist2 <- list()
  for (iii in 1:nrow(region_data2)){
    this_region <- region_data2$region_code[[iii]]
    sf::st_intersection(x = region_data2, 
                        y = region_data2[region_data2$region_code == this_region,]) %>%
      pull(region_code) -> neigbours
    datalist2[[iii]] <- tibble(region_code = this_region, 
                               level = lvs[ii],
                               neigbours = list(neigbours))
  }
  datalist[[ii]] <- do.call(bind_rows, datalist2)
}
neigbour_data <- do.call(bind_rows, datalist)

region_data2 <- left_join(region_data,neigbour_data)
region_data <- region_data2
# saveRDS(region_data2, "./data/region_data.RDS")
save(region_data, file = here::here("data/region_data.rda"),
     compress = "bzip2")

## AIKASARJADATA ----
dd <- read_excel("./data_raw/Aikasarjadata KORJATTU.xlsx")
fs::file_copy("../data_storage/v20211104/aikasarjadata_20211104.xlsx", 
              "./data_raw/")
dd <- read_excel("./data_raw/aikasarjadata_20211104.xlsx")

df_tmp <- dd %>%
  rename(aluekoodi = Aluekoodi,
         aluenimi = Aluenimi,
         regio_level = Aluetaso) %>% 
  mutate(aluekoodi = as.integer(ifelse(regio_level == "Seutukunnat", sub("^SK", "", aluekoodi), aluekoodi))) %>% 
  select(regio_level,everything()) %>% 
  rename(aika = Aika) %>% 
  # aika muotoa 2017-2019 - otetaan eka vuosi ja lisätään siihen 1
  ## 2017-2019 ->> 2018
  mutate(aika = as.integer(substr(aika, start = 1, stop = 4))+1) %>% 
  # # maakunnat veks
  # filter(regio_level != "Maakunnat") %>% 
  # # hyvinvointialue -> hyvinvointialueet
  mutate(regio_level = ifelse(regio_level == "Hyvinvointialue", "Hyvinvointialueet", regio_level)) %>% 
mutate(aluenimi = ifelse(aluenimi == "Helsinki (sosiaali- ja terveydenhuolto, sekä pelastustoimi)", 
                         "Helsingin kaupunki", aluenimi))  %>% 
  mutate(aluenimi = ifelse(grepl("hyvinvointialue", aluenimi), 
                              sub("hyvinvointialue", "HVA", aluenimi),
                           aluenimi))

# Uusi data, jossa sarakenimet ei korjattu
tibble(name1 = names(df_tmp)) %>% 
  mutate(osoitin = ifelse(grepl("^[A-z] - ", name1), TRUE, FALSE),
         summa = ifelse(grepl("^[A-Z][a-z]", name1), TRUE, FALSE)) %>%
  mutate(name2 = ifelse(summa, toupper(name1), sub("^[A-z] - ", "", name1)
  )) %>% 
  pull(name2) -> new_names
names(df_tmp) <- new_names


ekaiso <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Tsekataan mitkä muuttujat
df_var_class <- tibble(
  nms = names(df_tmp)
) %>% 
  mutate(nms_sani = gsub(" |-", "", nms)) %>% 
  mutate(all_upper = grepl("^[[:upper:]]+$", nms_sani)) %>% 
  mutate(var_class = ifelse(all_upper, nms, NA)) %>% 
  mutate(var_class = zoo::na.locf0(var_class)) %>% 
  mutate(var_class = ifelse(all_upper, "Summamuuttujat", ekaiso(tolower(var_class)))) 

df <- df_tmp %>% 
  pivot_longer(names_to = "variable", values_to = "value", cols = 5:ncol(.)) %>% 
  left_join(df_var_class %>% select(nms,var_class,all_upper), 
            by = c("variable" = "nms")) %>% 
  # Piennetään ALL_UPPER nimet
  mutate(variable = ifelse(all_upper, ekaiso(tolower(variable)), variable))

# Otetaan seutukuntanimiksi Tilastokeskuksen lyhyemmät
geofi::municipality_key_2021 %>% 
  count(seutukunta_name_fi,seutukunta_code) %>% 
  rename(aluekoodi = seutukunta_code,
         aluename = seutukunta_name_fi) %>% 
  select(-n) -> sk_names

df_v20211104_aikasarja <- left_join(df,sk_names) %>% 
  mutate(aluenimi = ifelse(regio_level == "Seutukunnat", aluename, aluenimi)) %>% 
  select(-aluename) %>% 
  filter(!variable %in% c("Inhimillinen","Sosiaalinen","Taloudellinen"),
         !is.na(value)) 

# saveRDS(df2, here("./data/df_v20211104_aikasarja.RDS"), 
#         compress = FALSE)
save(df_v20211104_aikasarja, file = here::here("data/df_v20211104_aikasarja.rda"),
     compress = "bzip2")


# Muuttujakuvaukset ----
muuttujakuvaukset <- readxl::read_excel("./data_raw/Muuttujakuvaukset_20201102.xlsx") %>% 
  setNames(c("Muuttujaluokka","Muuttuja","Aluetasot","Kuvaus")) %>% 
  mutate(Muuttujaluokka = factor(Muuttujaluokka, levels = c("Summamuuttujat",
                                                            "Inhimillinen huono-osaisuus",
                                                            "Huono-osaisuuden taloudelliset seuraukset", 
                                                            "Huono-osaisuuden sosiaaliset seuraukset"))) %>% 
  arrange(Muuttujaluokka) %>% 
  mutate(Aluetasot = sub("Maakunnat", "Hyvinvointialueet", Aluetasot))
# saveRDS(object = dat, file = "./data/muuttujakuvaukset.RDS")
save(muuttujakuvaukset, file = here::here("data/muuttujakuvaukset.rda"),
     compress = "bzip2")


if (F){
  # dokumentoidaan datat
  setwd(here::here())
  
  document_data <- function(dat, neim, description = "Data is data"){
    
    rivit <- vector()
    rivit <- c(rivit,
               neim,"",
               description,"",
               paste0("@format A data frame with ",nrow(dat)," rows and ",ncol(dat)," variables:"),
               "\\describe{")
    nms <- names(dat)
    nms_rivit <- vector()
    for (i in seq_along(nms)) nms_rivit <- c(nms_rivit,paste0("\\item{",nms[i],"}{",nms[i],"}"))
    rivit <- c(rivit,nms_rivit,"}")
    rivit <- paste0("#' ", rivit)
    rivit <- c(rivit,paste0('"',neim,'"'))
    cat(rivit, sep = "\n")
  }
  
  sub("./data/", "", dir_ls(path = "./data"))
  # df_v20211104.rda
  document_data(dat = df_v20211104, neim = "df_v20211104", description = "This is internal data for Karttasovellus shiny app")
  # df_v20211104_aikasarja.rda
  document_data(dat = df_v20211104_aikasarja, neim = "df_v20211104_aikasarja", description = "This is internal data for Karttasovellus shiny app")
  # muuttujakuvaukset.rda
  document_data(dat = muuttujakuvaukset, neim = "muuttujakuvaukset", description = "This is internal data for Karttasovellus shiny app")
  # regio_Hyvinvointialueet.rda
  document_data(dat = regio_Hyvinvointialueet, neim = "regio_Hyvinvointialueet", description = "This is internal data for Karttasovellus shiny app")
  # regio_Kunnat.rda
  document_data(dat = regio_Kunnat, neim = "regio_Kunnat", description = "This is internal data for Karttasovellus shiny app")
  # regio_Seutukunnat.rda
  document_data(dat =  regio_Seutukunnat, neim = " regio_Seutukunnat", description = "This is internal data for Karttasovellus shiny app")
  # regio_Suomi.rda
  document_data(dat = regio_Suomi, neim = "regio_Suomi", description = "This is internal data for Karttasovellus shiny app")
  # region_data.rda
  document_data(dat = region_data, neim = "region_data", description = "This is internal data for Karttasovellus shiny app")

  
  }


if (F){
  
  # Luodaan raaka muuttujakuvaus
  readRDS("./data/df_v20201102.RDS") %>% 
    distinct(var_class,variable,regio_level) %>% 
    group_by(var_class,variable) %>% 
    summarise(aluetasot = paste(regio_level, collapse = ", ")) %>% 
    ungroup() %>% 
    mutate(kuvaus = "Mauris molestie sagittis risus, cursus posuere magna dapibus sed. Sed sed mauris enim. Proin consequat bibendum facilisis. Morbi eget rhoncus odio. Proin pretium, tellus id ornare pulvinar, magna eros sollicitudin magna, nec fringilla enim dui at erat. Sed vulputate neque odio, vel aliquet enim accumsan nec. Pellentesque dictum, justo ut ullamcorper dictum, massa nisl facilisis lacus, quis malesuada odio metus at quam. Integer vel imperdiet turpis.") -> dada
  
  writexl::write_xlsx(x = dada, path = "./data/muuttujakuvaukset.xlsx")
  
}


