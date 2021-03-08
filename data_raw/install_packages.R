# CRAN
libs_cran <- c("shiny","shinycssloaders","gt","dplyr","readxl","geofi","ggplot2","tidyr","sf","ggrepel","glue","knitr","patchwork","stringr","xtable","extrafont","forcats","viridis","purrr","janitor","remotes","geofi")
inst <- match(libs_cran, .packages(all=TRUE)); install.packages(libs_cran[which(is.na(inst))])
