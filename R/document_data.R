#' Document an internal data
#'
#' @param dat data.frame
#' @param neim string name of the data
#' @param description Description of the data
#'
#' @export
document_data <- function(dat, neim, description = "Data is data"){
  
  rivit <- vector()
  rivit <- c(rivit,
             neim,"",
             description,"",
             paste0("@format A data frame with ",nrow(dat)," rows and ",ncol(dat)," variables:"),
             "\\describe{")
  nms <- names(dat)
  nms_rivit <- vector()
  for (i in seq_along(nms)) nms_rivit <- c(nms_rivit,paste0("\\item{",nms[i],"}{",nms[i],"}"))
  rivit <- c(rivit,nms_rivit,"}")
  rivit <- paste0("#' ", rivit)
  rivit <- c(rivit,paste0('"',neim,'"'))
  cat(rivit, sep = "\n")
}