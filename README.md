
<!-- README.md is generated from README.Rmd. Please edit that file -->

# karttasovellus

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

`karttasovellus` on `shiny`-sovellus, joka tuottaa eriteltyä tietoa
huono-osaisuudesta hyvinvointialueittain, seutukunnittain tai
kunnittain.

## Kehittäminen

Kloonaa sovellus itsellesi RStudioon ja avaa se. Avaa skripti
`./dev/run_dev.R` komennolla `file.edit("./dev/run_dev.R")` ja suorita
kaikki skriptin rivit. Sovellus käynnistyy *Viewer*-paneeliin.

Sovelluksen sisältö on ripoteltu eri skripteihin kansioon `./R`.
Shiny-moduulit on nimetty `mod_**_****.R`.

Sovellus näkyy osoitteessa:
<https://diak.shinyapps.io/karttasovellus_test>

## Asennus

Voit asentaa karttasovellus-paketin

``` r
remotes::install_github("diakgit/karttasovellus, ref = "devel") 
```

Asentamisen jälkeen saat laskurin käyttöösi komennolla

``` r
karttasovellus::run_app()
```

## Resursseja kehittämisen tueksi

### Shiny

-   [Wickham (tekeillä) *Mastering Shiny*](https://mastering-shiny.org/)
-   [Youtube-haku:
    “mastering+shiny”](https://www.youtube.com/results?search_query=mastering+shiny)

### Golem

-   [`golem` homepage](https://thinkr-open.github.io/golem/)
-   [Colin Fay & Co (2022) *Engineering Production-Grade Shiny
    Apps*](https://engineering-shiny.org/)
-   [Youtube-haku:
    “golem+shiny”](https://www.youtube.com/results?search_query=golem+shiny)

## Viittaa

``` r
citation("karttasovellus")
#> 
#> To cite package 'karttasovellus' in publications use:
#> 
#>   Diakonia-ammattikorkeakoulu (2022). karttasovellus: Huono-osaisuus
#>   Suomessa verkkosovellus. R package version 1.0.
#>   https://github.com/DiakGit/karttasovellus
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {karttasovellus: Huono-osaisuus Suomessa verkkosovellus},
#>     author = {{Diakonia-ammattikorkeakoulu}},
#>     year = {2022},
#>     note = {R package version 1.0},
#>     url = {https://github.com/DiakGit/karttasovellus},
#>   }
```
