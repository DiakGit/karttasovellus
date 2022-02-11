FROM rocker/geospatial:latest

RUN install2.r shiny remotes magrittr glue processx htmltools ggplot2 stringr dplyr knitr pkgload bslib xtable attempt hunspell shiny leaflet svglite rmarkdown tidyr testthat config sf spelling shinycssloaders readr ragg patchwork metathis leaflet.extras kableExtra janitor ineq hrbrthemes gt golem ggrepel geofi forcats acid

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
#EXPOSE 80
#CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');karttasovellus::run_app()"
COPY deploy.R deploy.R
CMD Rscript deploy.R
