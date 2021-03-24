Diak karttasovellus: sovelluksen kehittäminen rstudio.cloud -ympäristössä
=========================================================


## Sovelluksen kehitysympäristön luominen

- ympäristön luomisen prosessi [50s videona](http://software.markuskainu.fi/diak/kartat2020/diak_rstudiocloud_20210315.mp4)

### rstudio.cloud

1. Mene osoitteeseen: [rstudio.cloud](https://rstudio.cloud) (RStudio) ja luo uusi tili joko Googlen tai Githubin tunnuksilla tai luomalla uusi tunnus palveluun. Kirjaudu palveluun tunnuksellasi.
2. Karttasovelluksen lähdekoodi löytyy Githubista osoitteessa `https://github.com/DiakGit/karttasovellus`. RStudiossa luo uusi projekti ja valitse *New Project from Git Repository* ja liitä ruutuun karttasovelluksen Github-osoite.
3. Asenna tarvittavat paketit suorittamalla seuraava komento: `source("./data_raw/install_packages.R")` (kestää muutaman minuutin)

### Lataa ja käsittele data

1. Lataa kolmen ekselin zippi täältä:  [software.markuskainu.fi/diak/kartat2020/diak_ekselit_202103.zip](http://software.markuskainu.fi/diak/kartat2020/diak_ekselit_202103.zip) ja siirrä (upload) se RStudioon kansioon `data_raw`. Vaihtoehtoisesti voi suorittaa seuraavan komennon, joka lataa zipin ja purkaa sen.
2. prosessoi datat ajamalla seuraava komento: `source("./data_raw/process_raw_data.R")`. Kestää parikymmentä sekuntia.

### Käynnistä sovellus

- käynnistä sovellus komennolla `shiny::runApp()`


## git ja Github


- Kun käyt läpi alla oleva stepit, katso tämä video: [How to use Git and GitHub with R
](https://www.youtube.com/watch?v=kL6L2MNqPHg&t=567s)
- Tutustu lisäsi tähän materiaaliin soveltuvin osin: [Happy Git and GitHub for the useR
](https://happygitwithr.com/)

Aluksi konfiguroi oman ympäristösi gittiin omat tietosi. Asenna aluksi `usethis`-paketti komennolla `install.packages("usethis")`. Sitten kopioi alla oleva funktiokutsu ja täytä siihen omat tietosi.

```r
library(usethis)
use_git_config(user.name = "Etunimi Sukunimi", user.email = "etunimi.sukunimi@organisaatio.fi")
```

Seuraavaksi luo tili [Github](https://github.com)-palveluun. Mene palvelussa kohtaan *Settings* > *Developer Setting* > *Personal access tokens* (`https://github.com/settings/tokens`), luo uusi *token* ja kopioi se työpöydälle. (Token on pitkä numerosarja)

RStudiossa suorita seuraava funktiokutsu `gitcreds::gitcreds_set(url = "https://github.com")` ja anna sille edellä luomasi token. Nyt sinulla *pitäisi* olla mahdollisuus a) tehdä paikallisesti muutoksia versiohistoriaan ja b) julkaista niitä Github-palvelussa. Jotta pystyt viemään muutokset karttasovelluksen repositoryyn (`https://github.com/DiakGit/karttasovellus`) minun täytyy antaa luomallesi tilille oikeudet tehdä siihen muutoksia.


