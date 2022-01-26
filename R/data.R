#' df_v20211104
#' 
#' This is internal data for Karttasovellus shiny app
#' 
#' @format A data frame with 7620 rows and 7 variables:
#' \describe{
#' \item{regio_level}{regio_level}
#' \item{aluekoodi}{aluekoodi}
#' \item{aluenimi}{aluenimi}
#' \item{variable}{variable}
#' \item{value}{value}
#' \item{var_class}{var_class}
#' \item{all_upper}{all_upper}
#' }
"df_v20211104"

#' df_v20211104_aikasarja
#' 
#' This is internal data for Karttasovellus shiny app
#' 
#' @format A data frame with 72142 rows and 8 variables:
#' \describe{
#' \item{regio_level}{regio_level}
#' \item{aluekoodi}{aluekoodi}
#' \item{aluenimi}{aluenimi}
#' \item{aika}{aika}
#' \item{variable}{variable}
#' \item{value}{value}
#' \item{var_class}{var_class}
#' \item{all_upper}{all_upper}
#' }
"df_v20211104_aikasarja"

#' muuttujakuvaukset
#' 
#' This is internal data for Karttasovellus shiny app
#' 
#' @format A data frame with 27 rows and 4 variables:
#' \describe{
#' \item{Muuttujaluokka}{Muuttujaluokka}
#' \item{Muuttuja}{Muuttuja}
#' \item{Aluetasot}{Aluetasot}
#' \item{Kuvaus}{Kuvaus}
#' }
"muuttujakuvaukset"

#' regio_Hyvinvointialueet
#' 
#' This is internal data for Karttasovellus shiny app
#' 
#' @format A data frame with 22 rows and 2 variables:
#' \describe{
#' \item{aluekoodi}{aluekoodi}
#' \item{geom}{geom}
#' }
"regio_Hyvinvointialueet"

#' regio_Kunnat
#' 
#' This is internal data for Karttasovellus shiny app
#' 
#' @format A data frame with 293 rows and 2 variables:
#' \describe{
#' \item{aluekoodi}{aluekoodi}
#' \item{geom}{geom}
#' }
"regio_Kunnat"

#'  regio_Seutukunnat
#' 
#' This is internal data for Karttasovellus shiny app
#' 
#' @format A data frame with 66 rows and 2 variables:
#' \describe{
#' \item{aluekoodi}{aluekoodi}
#' \item{geom}{geom}
#' }
"regio_Seutukunnat"

#' regio_Suomi
#' 
#' This is internal data for Karttasovellus shiny app
#' 
#' @format A data frame with 1 rows and 2 variables:
#' \describe{
#' \item{maa}{maa}
#' \item{geom}{geom}
#' }
"regio_Suomi"

#' region_data
#' 
#' This is internal data for Karttasovellus shiny app
#' 
#' @format A data frame with 381 rows and 5 variables:
#' \describe{
#' \item{region_code}{region_code}
#' \item{region_name}{region_name}
#' \item{geom}{geom}
#' \item{level}{level}
#' \item{neigbours}{neigbours}
#' }
"region_data"

#' pop_data
#' 
#' Population data for computing population weighted gini coefficients
#' 
#' @format A data frame with 4010 rows and 5 variables:
#' \describe{
#' \item{regio_level}{regio_level}
#' \item{aluenimi}{aluenimi}
#' \item{aika}{aika}
#' \item{pop}{pop}
#' \item{aluekoodi}{aluekoodi}
#' }
"pop_data"

#' ineq_data
#' 
#' municipality level weighted ginis for all indicators at various regional breakdowns
#' 
#' @format A data frame with 17087 rows and 7 variables:
#' \describe{
#' \item{aluenimi}{aluenimi}
#' \item{var_class}{var_class}
#' \item{variable}{variable}
#' \item{aika}{aika}
#' \item{gini}{gini}
#' \item{regio_level}{regio_level}
#' \item{aluekoodi}{aluekoodi}
#' }
"ineq_data"

#' regio_Postinumeroalueet
#' 
#' Zipcode sf data from 2021 including zipcode and municipality number
#' 
#' @format A data frame with 3027 rows and 4 variables:
#' \describe{
#' \item{region_code}{region_code}
#' \item{region_name}{region_name}
#' \item{kuntanro}{kuntanro}
#' \item{geom}{geom}
#' }
"regio_Postinumeroalueet"

#' dfzip_v20220125
#' 
#' Cross-sectional zipcode level attribute data
#' 
#' @format A data frame with 11976 rows and 7 variables:
#' \describe{
#' \item{regio_level}{regio_level}
#' \item{aluekoodi}{aluekoodi}
#' \item{aluenimi}{aluenimi}
#' \item{kuntanro}{kuntanro}
#' \item{kuntanimi}{kuntanimi}
#' \item{variable}{variable}
#' \item{value}{value}
#' }
"dfzip_v20220125"

#' dfzip_v20220125_aikasarja
#' 
#' Time-series zipcode level attribute data
#' 
#' @format A data frame with 83832 rows and 8 variables:
#' \describe{
#' \item{regio_level}{regio_level}
#' \item{aluekoodi}{aluekoodi}
#' \item{aluenimi}{aluenimi}
#' \item{kuntanro}{kuntanro}
#' \item{kuntanimi}{kuntanimi}
#' \item{aika}{aika}
#' \item{variable}{variable}
#' \item{value}{value}
#' }
"dfzip_v20220125_aikasarja"

#' region_data_zip
#' 
#' Zipcode sf with neighbours from 2021
#' 
#' @format A data frame with 3027 rows and 6 variables:
#' \describe{
#' \item{region_code}{region_code}
#' \item{region_name}{region_name}
#' \item{kuntanro}{kuntanro}
#' \item{neigbours}{neigbours}
#' \item{geom}{geom}
#' \item{level}{level}
#' }
"region_data_zip"

