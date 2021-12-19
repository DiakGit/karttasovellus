#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'

#' add_line_break
#'
#' @description A add a line break
#'
#' @return string
#' 
#' @export
#'
add_line_break <- function(x = "very many many characters and words and sentences",
                           n = 20){
  y <- gsub(paste0("(.{1,",n,"})(\\s|$)"), "\\1<br/>", x)
  y <- sub("<br/>$", "", y)
  return(y)
} 

#' add_line_break2
#'
#' @description A add a line break2
#'
#' @return string
#' 
#' @export
#'
add_line_break2 <- function(x = "very many many characters and words and sentences",
                            n = 20){
  y <- gsub(paste0("(.{1,",n,"})(\\s|$)"), "\\1\n", x)
  y <- sub("\n$", "", y)
  return(y)
}

