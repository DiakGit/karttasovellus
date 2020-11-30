# devtools::install_github("RinteRface/bs4Dash")

library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(readxl)
library(geofi)
library(ggplot2)
library(tidyr)
library(sf)
library(ggrepel)
library(gt)
library(leaflet)
library(DT)
library(glue)
library(knitr)
library(patchwork)
library(stringr)
library(xtable)
library(extrafont)
library(forcats)
library(viridis)
library(purrr)
library(janitor)
library(hrbrthemes)
# library(waiter)

add_line_break <- function(x = "very many many characters and words and sentences",
                           n = 20){
  y <- gsub(paste0("(.{1,",n,"})(\\s|$)"), "\\1<br/>", x)
  y <- sub("<br/>$", "", y)
  return(y)
} 

add_line_break2 <- function(x = "very many many characters and words and sentences",
                            n = 20){
  y <- gsub(paste0("(.{1,",n,"})(\\s|$)"), "\\1\n", x)
  y <- sub("\n$", "", y)
  return(y)
} 

options(scipen = 999)
