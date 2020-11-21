# CRAN
libs_cran <- c("shiny","shinyWidgets","shinycssloaders","dplyr","readxl","geofi","ggplot2","tidyr","sf","ggrepel","leaflet","DT","glue","knitr","patchwork","stringr","xtable","extrafont","forcats","viridis","purrr","janitor","remotes")
inst <- match(libs_cran, .packages(all=TRUE)); install.packages(libs_cran[which(is.na(inst))])
