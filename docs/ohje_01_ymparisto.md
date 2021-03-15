Diak karttasovellus: sovelluksen kehittäminen rstudio.cloud -ympäristössä
=========================================================


- ympäristön luomisen prosessi [50s videona](http://software.markuskainu.fi/diak/kartat2020/diak_rstudiocloud_20210315.mp4)

## rstudio.cloud

1. Mene osoitteeseen: [rstudio.cloud](https://rstudio.cloud) (RStudio) ja luo uusi tili joko Googlen tai Githubin tunnuksilla tai luomalla uusi tunnus palveluun. Kirjaudu palveluun tunnuksellasi.
2. Karttasovelluksen lähdekoodi löytyy Githubista osoitteessa `https://github.com/DiakGit/karttasovellus`. RStudiossa luo uusi projekti ja valitse *New Project from Git Repository* ja liitä ruutuun karttasovelluksen Github-osoite.
3. Asenna tarvittavat paketit suorittamalla seuraava komento: `source("./data_raw/install_packages.R")` (kestää muutaman minuutin)

## Lataa ja käsittele data

1. Lataa kolmen ekselin zippi täältä:  [software.markuskainu.fi/diak/kartat2020/diak_ekselit_202103.zip](http://software.markuskainu.fi/diak/kartat2020/diak_ekselit_202103.zip) ja siirrä (upload) se RStudioon kansioon `data_raw`. Vaihtoehtoisesti voi suorittaa seuraavan komennon, joka lataa zipin ja purkaa sen.
2. prosessoi datat ajamalla seuraava komento: `source("./data_raw/process_raw_data.R")`. Kestää parikymmentä sekuntia.

## Käynnistä sovellus

- käynnistä sovellus komennolla `shiny::runApp()`

