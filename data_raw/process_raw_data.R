library(readxl)
library(dplyr)
library(tidyr)
library(glue)
library(fs)
library(ggplot2)
library(here)
#options(tibble.print_max = 200)
dir_create(here("./data"))

# dir_ls(here("./data_raw/"), glob = "*.xlsx") -> flies
dd <- read_excel("./data_raw/Kopio_Huono-osaisuuden mittarit - KAIKKI.xlsx")

df_tmp <- dd %>%
  rename(aluekoodi = Aluekoodi,
         aluenimi = Aluenimi,
         regio_level = Aluetaso) %>% 
  mutate(aluekoodi = as.integer(ifelse(regio_level == "Maakunnat", sub("^MK", "", aluekoodi),
                                       ifelse(regio_level == "Seutukunnat", sub("^SK", "", aluekoodi), aluekoodi)))) %>% 
  select(regio_level,everything())  


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
geofi::municipality_key_2018 %>% 
  count(seutukunta_name_fi,seutukunta_code) %>% 
  rename(aluekoodi = seutukunta_code,
         aluename = seutukunta_name_fi) %>% 
  select(-n) -> sk_names

df2 <- left_join(df,sk_names) %>% 
  mutate(aluenimi = ifelse(regio_level == "Seutukunnat", aluename, aluenimi)) %>% 
  select(-aluename) %>% 
  filter(!variable %in% c("Inhimillinen","Sosiaalinen","Taloudellinen"),
         !is.na(value)) 

saveRDS(df2, here("./data/df_v20200423.RDS"), 
        compress = FALSE)

# Apudata karttojen tekoon
muni <- geofi::get_municipalities(year = 2017) %>%
  filter(maakunta_name_fi != "Ahvenanmaa")
regio_Seutukunnat <- muni %>% 
  group_by(seutukunta_code) %>% 
  summarise() %>% rename(aluekoodi = seutukunta_code) 
regio_Maakunnat <- muni %>% 
  group_by(maakunta_code) %>% 
  summarise() %>% rename(aluekoodi = maakunta_code)
regio_Kunnat <- muni %>% 
  select(municipality_code) %>% 
  rename(aluekoodi = municipality_code)
regio_Suomi <- muni %>% 
  mutate(maa = "Suomi") %>% 
  group_by(maa) %>% 
  summarise()

saveRDS(regio_Suomi, 
        here("data/regio_Suomi.RDS"))
saveRDS(regio_Maakunnat, 
        here("data/regio_Maakunnat.RDS"))
saveRDS(regio_Seutukunnat, 
        here("data/regio_Seutukunnat.RDS"))
saveRDS(regio_Kunnat, 
        here("data/regio_Kunnat.RDS"))







# tehdään aluekoodi/aluenimi -data vielä
muni <- get_municipalities(year = 2019) %>%
  filter(maakunta_name_fi != "Ahvenanmaa")
bind_rows(
  muni %>% 
    group_by(seutukunta_code,seutukunta_name_fi) %>% 
    summarise() %>% rename(region_code = seutukunta_code,
                           region_name = seutukunta_name_fi) %>% 
    mutate(level = "Seutukunnat"),
  muni %>% 
    group_by(maakunta_code,maakunta_name_fi) %>% 
    summarise() %>% rename(region_code = maakunta_code,
                           region_name = maakunta_name_fi) %>% 
    mutate(level = "Maakunnat"),
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

saveRDS(region_data2, "./data/region_data.RDS")







if (F){

# Luodaan raaka muuttujakuvaus
readRDS("./data/df_v20200320.RDS") %>% 
  distinct(var_class,variable,regio_level) %>% 
  group_by(var_class,variable) %>% 
  summarise(aluetasot = paste(regio_level, collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(kuvaus = "Mauris molestie sagittis risus, cursus posuere magna dapibus sed. Sed sed mauris enim. Proin consequat bibendum facilisis. Morbi eget rhoncus odio. Proin pretium, tellus id ornare pulvinar, magna eros sollicitudin magna, nec fringilla enim dui at erat. Sed vulputate neque odio, vel aliquet enim accumsan nec. Pellentesque dictum, justo ut ullamcorper dictum, massa nisl facilisis lacus, quis malesuada odio metus at quam. Integer vel imperdiet turpis.") -> dada

writexl::write_xlsx(x = dada, path = "./data/muuttujakuvaukset.xlsx")

}
