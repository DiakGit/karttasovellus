#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
#' 
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList


get_dat <- reactive({
  # dat <- readRDS("./data/df_v20201102.RDS")  
  # dat <- readRDS("www/data/df_v20211104.RDS")
  dat <- karttasovellus::df_v20211104
  return(dat)
})

get_dat_timeseries <- reactive({
  dat_aika <- karttasovellus::df_v20211104_aikasarja
  return(dat_aika)
})

get_region_data <- reactive({
  
  region_data <- karttasovellus::region_data
  # dat <- dplyr::filter(region_data, level %in% input$value_region_level2)
  return(region_data)
  
})


varlist_diak <- reactive({
  dat <- get_dat()
  dat %>% 
    count(regio_level,var_class,variable) %>% 
    select(-n) %>% 
    arrange(desc(var_class),variable) -> indicator_df
  return(indicator_df)
})

