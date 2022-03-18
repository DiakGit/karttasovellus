FROM rocker/geospatial:latest

RUN install2.r shiny remotes magrittr glue processx htmltools ggplot2 stringr dplyr knitr pkgload bslib xtable attempt hunspell shiny leaflet svglite rmarkdown tidyr testthat config sf spelling shinycssloaders readr ragg patchwork metathis leaflet.extras kableExtra janitor ineq hrbrthemes gt golem ggrepel geofi forcats acid

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
ADD . /home/shinyusr
WORKDIR /home/shinyusr
CMD Rscript deploy.R
