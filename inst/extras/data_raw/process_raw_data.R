
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
dd <- read_excel("../../../data_storage/v20230224/Huono-osaisuusindikaattorit - uusimmat.xlsx") %>% 
  # select(1:31) %>% 
  filter(!is.na(Aika)) %>% 
  select(-Aika)

dd <- dd[,colSums(is.na(dd))<nrow(dd)]

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
                           "Helsingin kaupunki", aluenimi),
         aluenimi = ifelse(aluenimi == "Vantaa-Keravan hyvinvointialue", 
                           "Vantaan ja Keravan hyvinvointialue", 
                           aluenimi)
         ) %>% 
  mutate(aluenimi = ifelse(grepl("hyvinvointialue", aluenimi), 
                           sub("hyvinvointialue", "HVA", aluenimi),
                           aluenimi),
         aluenimi = ifelse(grepl("Uusimaan", aluenimi), 
                           sub("Uusimaan", "Uudenmaan", aluenimi),
                           aluenimi))

# korjataan hyvinvointialueiden aluekoodit
hva_codes <- geofi::municipality_key_2022 %>% 
  count(hyvinvointialue_name_fi,hyvinvointialue_code) %>% 
  select(-n) %>%
  rename(aluenimi = hyvinvointialue_name_fi) %>% 
  mutate(aluenimi = ifelse(grepl("hyvinvointialue", aluenimi), 
                           sub("hyvinvointialue", "HVA", aluenimi),
                           aluenimi))

df_tmp <- left_join(df_tmp,hva_codes) %>% 
  mutate(aluekoodi = ifelse(regio_level == "Hyvinvointialueet", hyvinvointialue_code, aluekoodi)) %>% 
  select(-hyvinvointialue_code)


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
geofi::municipality_key_2022 %>% 
  count(seutukunta_name_fi,seutukunta_code) %>% 
  rename(aluekoodi = seutukunta_code,
         aluename = seutukunta_name_fi) %>% 
  select(-n) -> sk_names

df_v20230224 <- left_join(df,sk_names) %>% 
  mutate(aluenimi = ifelse(regio_level == "Seutukunnat", aluename, aluenimi)) %>% 
  select(-aluename) %>% 
  filter(!variable %in% c("Inhimillinen","Sosiaalinen","Taloudellinen"),
         !is.na(value)) 

# saveRDS(df2, here("./data/df_v20211104.RDS"), 
#         compress = FALSE)

save(df_v20230224, file = here::here("data/df_v20230224.rda"),
     compress = "bzip2")


# Apudata karttojen tekoon
muni <- geofi::get_municipalities(year = 2022) %>%
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
muni <- get_municipalities(year = 2022) %>%
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
dd <- read_excel("../../../data_storage/v20230224/Aikasarjadata UUSIN.xlsx", 
           col_types = c(rep("text", 4), rep("numeric", 26)))

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
                         "Helsingin kaupunki", aluenimi),
       aluenimi = ifelse(aluenimi == "Vantaa-Keravan hyvinvointialue", 
                         "Vantaan ja Keravan hyvinvointialue", aluenimi))  %>% 
  mutate(aluenimi = ifelse(grepl("hyvinvointialue", aluenimi), 
                              sub("hyvinvointialue", "HVA", aluenimi),
                           aluenimi),
         aluenimi = ifelse(grepl("Uusimaan", aluenimi), 
                           sub("Uusimaan", "Uudenmaan", aluenimi),
                           aluenimi))

# korjataan hyvinvointialueiden aluekoodit
hva_codes <- geofi::municipality_key_2022 %>% 
  count(hyvinvointialue_name_fi,hyvinvointialue_code) %>% 
  select(-n) %>%
  rename(aluenimi = hyvinvointialue_name_fi) %>% 
  mutate(aluenimi = ifelse(grepl("hyvinvointialue", aluenimi), 
                           sub("hyvinvointialue", "HVA", aluenimi),
                           aluenimi))

df_tmp <- left_join(df_tmp,hva_codes) %>% 
  mutate(aluekoodi = ifelse(regio_level == "Hyvinvointialueet", hyvinvointialue_code, aluekoodi)) %>% 
  select(-hyvinvointialue_code)

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
geofi::municipality_key_2022 %>% 
  count(seutukunta_name_fi,seutukunta_code) %>% 
  rename(aluekoodi = seutukunta_code,
         aluename = seutukunta_name_fi) %>% 
  select(-n) -> sk_names

df_v20230224_aikasarja <- left_join(df,sk_names) %>% 
  mutate(aluenimi = ifelse(regio_level == "Seutukunnat", aluename, aluenimi)) %>% 
  select(-aluename) %>% 
  filter(!variable %in% c("Inhimillinen","Sosiaalinen","Taloudellinen"),
         !is.na(value)) 

save(df_v20230224_aikasarja, file = here::here("data/df_v20230224_aikasarja.rda"),
     compress = "bzip2")


# Väestömäärät eri tasoilla!
library(pxweb)
library(dplyr)
library(janitor)
pxweb_query_list <- list("Alue 2021"=c("*"), "Tiedot"=c("M411"),"Vuosi"=c("*"))
# Download data 
px_data <- pxweb_get(url = "https://pxdata.stat.fi/PXWeb/api/v1/fi/Kuntien_avainluvut/2021/kuntien_avainluvut_2021_aikasarja.px",
                     query = pxweb_query_list)
px_tibble <- as.data.frame(px_data, 
                           column.name.type = "text", 
                           variable.value.type = "text") %>% 
  as_tibble() %>% 
  mutate(Vuosi = as.integer(Vuosi)) %>% 
  filter(Vuosi >= 2011)
pop_data_orig <- clean_names(px_tibble)
names(pop_data_orig) <- c("aluenimi","aika","pop")
# valitaan vaan kunnat
pop_data_raw <- pop_data_orig %>% 
  right_join(geofi::municipality_key_2022 %>% 
               select(municipality_name_fi,
                      seutukunta_name_fi,
                      hyvinvointialue_name_fi), 
             by = c("aluenimi" = "municipality_name_fi"))

pop_data <- bind_rows(
  # kuntataso
  pop_data_raw %>% 
    mutate(regio_level = "Kunnat") %>% 
    select(regio_level,aluenimi,aika,pop),
  # seutukuntataso  
  pop_data_raw %>% 
    group_by(seutukunta_name_fi,aika) %>% 
    summarise(pop = sum(pop, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(aluenimi = seutukunta_name_fi) %>% 
    mutate(regio_level = "Seutukunnat") %>% 
    select(regio_level,aluenimi,aika,pop),
  # hyvinvointialuetaso
  pop_data_raw %>% 
    group_by(hyvinvointialue_name_fi,aika) %>% 
    summarise(pop = sum(pop, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(aluenimi = hyvinvointialue_name_fi) %>% 
    mutate(regio_level = "Hyvinvointialueet") %>% 
    select(regio_level,aluenimi,aika,pop)
) %>% mutate(aluenimi = ifelse(grepl("hyvinvointialue", aluenimi), 
                               sub("hyvinvointialue", "HVA", aluenimi),
                               aluenimi)) %>% 
  # aluekoodit vielä
  left_join(
    df_v20230224 %>% 
      distinct(regio_level,aluekoodi,aluenimi)    
  )

save(pop_data, file = here::here("data/pop_data.rda"),
     compress = "bzip2")

karttasovellus::document_data(dat = pop_data, neim = "pop_data", description = "Population data for computing population weighted gini coefficients")




# lasketaan kuntatason eriarvoisuus
dat_gini_raw <- df_v20230224_aikasarja %>% 
  filter(regio_level == "Kunnat") %>% 
  left_join(pop_data) %>% 
  left_join(geofi::municipality_key_2022 %>% 
               select(municipality_name_fi,
                      #seutukunta_name_fi,
                      hyvinvointialue_name_fi), 
             by = c("aluenimi" = "municipality_name_fi")) %>% 
  mutate(hyvinvointialue_name_fi = sub("hyvinvointialue", "HVA", hyvinvointialue_name_fi))

ineq_data <- bind_rows(
  # dat_gini_raw %>% 
  # group_by(seutukunta_name_fi,var_class,variable,aika) %>% 
  # summarise(gini = round(acid::weighted.gini(x = value, w = pop)$Gini[1],2)) %>% 
  # mutate(regio_level = "Seutukunnat") %>% 
  # ungroup() %>% 
  # rename(aluenimi = seutukunta_name_fi),
dat_gini_raw %>% 
  group_by(hyvinvointialue_name_fi,var_class,variable,aika) %>% 
  # lasketaan gini vaan jos alueeseen kuuluu vähintään viisi kuntaa!
  mutate(n = n()) %>% 
  filter(n >= 5) %>% select(-n) %>% 
  #ungroup() %>% distinct(hyvinvointialue_name_fi) %>% print(n = 50)
  summarise(gini = round(acid::weighted.gini(x = value, w = pop)$Gini[1],2)) %>% 
  mutate(regio_level = "Hyvinvointialueet") %>% 
  ungroup() %>% 
  rename(aluenimi = hyvinvointialue_name_fi),
# Kuntatasolla lasketaan kaikkien kuntien välinen eriarvoisuus per indikaattori/vuosi
dat_gini_raw %>% 
  group_by(var_class,variable,aika) %>% 
  summarise(gini = round(acid::weighted.gini(x = value, w = pop)$Gini[1],2)) %>% 
  mutate(regio_level = "Kunnat") %>% 
  ungroup() %>% 
  mutate(aluenimi = "KAIKKI KUNNAT")
) %>% 
  # aluekoodit vielä
  left_join(
    df_v20230224 %>% 
      distinct(regio_level,aluekoodi,aluenimi))

save(ineq_data, file = here::here("data/ineq_data.rda"),
     compress = "bzip2")

karttasovellus::document_data(dat = ineq_data, 
                              neim = "ineq_data", 
                              description = "municipality level weighted ginis for all indicators at various regional breakdowns")

# Muuttujakuvaukset ----
muuttujakuvaukset <- read_excel("../../../data_storage/v20230224/Muuttujakuvaukset - UUSI.xlsx") %>% 
  setNames(c("Muuttujaluokka","Muuttuja","Aluetasot","Kuvaus")) %>% 
  mutate(Muuttujaluokka = factor(Muuttujaluokka, levels = c("Summamuuttujat",
                                                            "Inhimillinen huono-osaisuus",
                                                            "Huono-osaisuuden taloudelliset seuraukset", 
                                                            "Huono-osaisuuden sosiaaliset seuraukset",
                                                            "Postinumerotarkastelu",
                                                            "Gini-kerroin"))) %>% 
  arrange(Muuttujaluokka) #%>% 
# mutate(Aluetasot = sub("Maakunnat", "Hyvinvointialueet", Aluetasot))

save(muuttujakuvaukset, file = here::here("data/muuttujakuvaukset.rda"),
     compress = "bzip2")
karttasovellus::document_data(dat = muuttujakuvaukset,neim = "muuttujakuvaukset")




if (F){
  # dokumentoidaan datat
  setwd(here::here())
  
  
  
  sub("./data/", "", dir_ls(path = "./data"))
  # df_v20211104.rda
  karttasovellus::document_data(dat = df_v20211104, neim = "df_v20211104", description = "This is internal data for Karttasovellus shiny app")
  # df_v20211104_aikasarja.rda
  karttasovellus::document_data(dat = df_v20211104_aikasarja, neim = "df_v20211104_aikasarja", description = "This is internal data for Karttasovellus shiny app")
  # muuttujakuvaukset.rda
  karttasovellus::document_data(dat = muuttujakuvaukset, neim = "muuttujakuvaukset", description = "This is internal data for Karttasovellus shiny app")
  # regio_Hyvinvointialueet.rda
  karttasovellus::document_data(dat = regio_Hyvinvointialueet, neim = "regio_Hyvinvointialueet", description = "This is internal data for Karttasovellus shiny app")
  # regio_Kunnat.rda
  karttasovellus::document_data(dat = regio_Kunnat, neim = "regio_Kunnat", description = "This is internal data for Karttasovellus shiny app")
  # regio_Seutukunnat.rda
  karttasovellus::document_data(dat =  regio_Seutukunnat, neim = " regio_Seutukunnat", description = "This is internal data for Karttasovellus shiny app")
  # regio_Suomi.rda
  karttasovellus::document_data(dat = regio_Suomi, neim = "regio_Suomi", description = "This is internal data for Karttasovellus shiny app")
  # region_data.rda
  karttasovellus::document_data(dat = region_data, neim = "region_data", description = "This is internal data for Karttasovellus shiny app")

  
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

# Postinumeroaluedata ----

paavo_raw <- geofi::get_zipcodes(year = 2022)
regio_Postinumeroalueet <- select(paavo_raw, -pinta_ala,-namn,-vuosi,-objectid,-kunta,-gml_id) %>% 
  rename(region_code = posti_alue,
         region_name = nimi)
save(regio_Postinumeroalueet, file = here::here("data/regio_Postinumeroalueet.rda"),
     compress = "bzip2")



karttasovellus::document_data(dat = regio_Postinumeroalueet, 
                              neim = "regio_Postinumeroalueet", 
                              description = "Zipcode sf data from 2021 including zipcode and municipality number")

datalist2 <- list()
region_data2 <- regio_Postinumeroalueet
nrows <- nrow(region_data2)
for (iii in 1:nrows){
  print(paste0(iii,"/",nrows))
  this_region <- region_data2$region_code[[iii]]
  sf::st_intersection(x = region_data2, 
                      y = region_data2[region_data2$region_code == this_region,]) %>%
    pull(region_code) -> neigbours
  datalist2[[iii]] <- tibble(region_code = this_region, 
                             neigbours = list(neigbours))
}
neigbour_data <- do.call(bind_rows, datalist2)
region_data_zip <- left_join(region_data2,neigbour_data) %>% 
  mutate(level = "Postinumeroalueet")
# saveRDS(region_data2, "./data/region_data.RDS")
save(region_data_zip, file = here::here("data/region_data_zip.rda"),
     compress = "bzip2")
karttasovellus::document_data(dat = region_data_zip, 
                              neim = "region_data_zip", 
                              description = "Zipcode sf with neighbours from 2021")


## Poikkileikkaus ----

dd <- read_excel("../../../data_storage/v20230224/Postinumerodata - UUSIN.xlsx")

dfzip_v20230224 <- dd %>%
  rename(aluekoodi = Postinumeroalue,
         aluenimi = `Postinumeroalueen nimi`,
         kuntanimi = `Kunnan nimi`,
         kuntanro = Kuntakoodi) %>% 
  pivot_longer(names_to = "variable", values_to = "value", cols = 5:ncol(.)) %>% 
  mutate(regio_level = "Postinumeroalueet",
         kuntanro = as.integer(kuntanro)) %>% 
  select(regio_level,everything())

save(dfzip_v20230224, file = here::here("data/dfzip_v20230224.rda"),
     compress = "bzip2")

karttasovellus::document_data(dat = dfzip_v20230224, 
                              neim = "dfzip_v20230224", 
                              description = "Cross-sectional zipcode level attribute data")

## Aikasarja ----
# fs::file_copy("../../../data_storage/v20220126/postinumeroaikasarjat_uusin.xlsx", "./data_raw/")
dd <- read_excel("../../../data_storage/v20230224/Postinumeroaikasarjat - UUSIN.xlsx")

dfzip_v20230224_aikasarja <- dd %>%
  rename(aluekoodi = Postinumeroalue,
         aluenimi = `Postinumeroalueen nimi`,
         aika = Vuosi,
         kuntanimi = `Kunnan nimi`,
         kuntanro = Kuntakoodi) %>% 
  pivot_longer(names_to = "variable", values_to = "value", cols = 6:ncol(.)) %>% 
  mutate(regio_level = "Postinumeroalueet",
         kuntanro = as.integer(kuntanro),
         aika = as.integer(sub("-.+$", "", aika))+1) %>% 
  select(regio_level,everything())

save(dfzip_v20230224_aikasarja, file = here::here("data/dfzip_v20230224_aikasarja.rda"),
     compress = "bzip2")

karttasovellus::document_data(dat = dfzip_v20230224_aikasarja, 
                              neim = "dfzip_v20230224_aikasarja", 
                              description = "Time-series zipcode level attribute data")





