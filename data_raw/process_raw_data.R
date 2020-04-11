library(readxl)
library(dplyr)
library(tidyr)
library(glue)
library(fs)
library(ggplot2)
library(here)
#options(tibble.print_max = 200)
dir_create(here("./data"))

dir_ls(here("./data_raw"), glob = "*.xlsx") -> flies

# ensin tsekataan nimet
namelist <- list()
aluetaso <- c("Kunnat","Maakunnat","Seutukunnat")
for (i in 1:3){
  namelist[[i]] <- tibble(names = read_excel(flies[i]) %>% 
                            names(.), taso = aluetaso[i])
}
do.call(bind_rows, namelist) %>% 
  count(names)

# Sitten pinotaan datat
datalist <- list()
aluetaso <- c("Kunnat","Maakunnat","Seutukunnat")
for (i in 1:3){
  datalist[[i]] <- read_excel(flies[i]) %>% 
    mutate(regio_level = aluetaso[i])
}
df <- do.call(bind_rows, datalist) %>%
  # setNames(tolower(names(.))) %>%
  rename(aluekoodi = Aluekoodi,
         aluenimi = Aluenimi) %>% 
  mutate(aluekoodi = as.integer(ifelse(regio_level == "Maakunnat", sub("^MK", "", aluekoodi),
                                       ifelse(regio_level == "Seutukunnat", sub("^SK", "", aluekoodi), aluekoodi)))) %>% 
  select(regio_level,everything()) %>% 
  pivot_longer(names_to = "variable", values_to = "value", cols = 4:ncol(.))

# Luokitellaan taustamuuttujat
df$var_class <- ifelse(grepl("^I ", df$variable), "Inhimillinen huono-osaisuus", 
                       ifelse(grepl("^S ", df$variable), "Huono-osaisuuden sosiaaliset seuraukset",
                              ifelse(grepl("^T ", df$variable), "Huono-osaisuuden taloudelliset seuraukset", 
                                     "Summamuuttujat")))
table(df$var_class)
# Siistitään muuttujanimet
ekaiso <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
df$variable <- ekaiso(tolower(sub("^. - ", "", df$variable)))

# Otetaan seutukuntanimiksi Tilastokeskuksen lyhyemmät
geofi::municipality_key_2018 %>% 
  count(sk_name,sk_code) %>% 
  rename(aluekoodi = sk_code) %>% 
  select(-n) -> sk_names

df2 <- left_join(df,sk_names) %>% 
  mutate(aluenimi = ifelse(regio_level == "Seutukunnat", sk_name, aluenimi)) %>% 
  select(-sk_name) %>% 
  filter(!variable %in% c("Inhimillinen","Sosiaalinen","Taloudellinen"),
         !is.na(value)) 

saveRDS(df2, here("./data/df_v20200320.RDS"), 
        compress = FALSE)

# Apudata karttojen tekoon
muni <- geofi::get_municipalities(year = 2017) %>% 
  filter(mk_name != "Ahvenanmaa")
regio_Seutukunnat <- muni %>% 
  group_by(sk_code) %>% 
  summarise() %>% rename(aluekoodi = sk_code) 
regio_Maakunnat <- muni %>% 
  group_by(mk_code) %>% 
  summarise() %>% rename(aluekoodi = mk_code)
regio_Kunnat <- muni %>% 
  select(kunta) %>% rename(aluekoodi = kunta)
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



# Luodaan raaka muuttujakuvaus
readRDS("./data/df_v20200320.RDS") %>% 
  distinct(var_class,variable,regio_level) %>% 
  group_by(var_class,variable) %>% 
  summarise(aluetasot = paste(regio_level, collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(kuvaus = "Mauris molestie sagittis risus, cursus posuere magna dapibus sed. Sed sed mauris enim. Proin consequat bibendum facilisis. Morbi eget rhoncus odio. Proin pretium, tellus id ornare pulvinar, magna eros sollicitudin magna, nec fringilla enim dui at erat. Sed vulputate neque odio, vel aliquet enim accumsan nec. Pellentesque dictum, justo ut ullamcorper dictum, massa nisl facilisis lacus, quis malesuada odio metus at quam. Integer vel imperdiet turpis.") -> dada

writexl::write_xlsx(x = dada, path = "./data/muuttujakuvaukset.xlsx")


