process_docs <- function(aluetaso1 = "Kunnat", 
                         aluename = "Alajärvi", 
                         format = ".docx", 
                         process_dir = "~/diak_temp"){
  
  
  region_data <- get_region_data()
  region_data <- dplyr::filter(region_data, level %in% aluetaso1)
  naapurikoodit <- region_data[region_data$region_name %in% aluename,]$neigbours[[1]]
  tabdat <- create_alueprofiili_content(input_value_region_profile = aluename, 
                                        input_value_regio_level_profile = aluetaso1)
  
  
  file_name <- glue("alueprofiili_{tolower(janitor::make_clean_names(aluename))}_{tolower(aluetaso1)}_{sub('\\\\.', '', format)}{format}")
  if (file.exists(file.path(process_dir, file_name))) return(print("exists")) 
  rmd_name <- glue("alueprofiili_{tolower(janitor::make_clean_names(aluename))}_{tolower(aluetaso1)}_{sub('\\\\.', '', format)}.Rmd")
  
  params <- list(region = aluename,
                 region_level = aluetaso1,
                 datetime = Sys.time())
  
  tempReport <- file.path(process_dir, rmd_name)
  lns <- readLines(system.file("templates", "report_template.Rmd", package="karttasovellus"))
  
  if (format == ".docx"){
    lns3 <- lns
    file.copy(system.file("templates", 
                          "diak_karttasovellus.dotx", 
                          package="karttasovellus"),
              "~/diak_temp",
              overwrite = TRUE)
    params[["fig_width_default"]] <- 12
    params[["fig_height_default"]] <- 10
    params[["doc_format"]] <- "docx"
  } else {
    lns2 <- sub("reference_docx: diak_karttasovellus.dotx", "reference_odt: diak_karttasovellus.ott", lns)
    lns3 <- sub("word_document", "odt_document", lns2)
    file.copy(system.file("templates", "diak_karttasovellus.ott", package="karttasovellus"), #"./docs/diak_karttasovellus.ott",
              "~/diak_temp",
              overwrite = TRUE)
    params[["fig_width_default"]] <- 12
    params[["fig_height_default"]] <- 10
    params[["doc_format"]] <- "odt"
  }
  writeLines(lns3, tempReport)
  rmarkdown::render(tempReport, 
                    output_file = file_name, 
                    params = params,
                    envir = new.env(parent = globalenv()
                    ))
  file.remove(tempReport)
}

# Toimii niin että muokkaa which_level sekä sorttauksen suuntaan että saaat useamman proseesin pyörimään.
# oletuksen pyörittää kahta rinnakkain docx ja odt formaatteja


library(dplyr)
library(sf)
library(karttasovellus)

regs <- get_region_data() %>% sf::st_drop_geometry()

library(foreach)
library(doParallel)
cores <- 2
cl <- makeCluster(cores) #not to overload your computer
registerDoParallel(cl)
# levels <- unique(regs$level)
formats <- c(".docx",".odt")
logfile <- "~/diak_temp/logi.txt"
file.create(logfile)
which_level <- "Seutukunnat"                         

if (which_level == "Kunnat"){
  
  regtmp <- regs[regs$level == which_level,]
  foreach(iv=seq_along(formats)) %dopar% {
    library(karttasovellus)
    library(dplyr)
    library(glue)
    library(ggplot2)
    
    nms <- sort(unique(regtmp$region_name), decreasing = FALSE)
    for (ii in seq_along(nms)){
      cat(nms[ii], file = logfile, sep = "\n", append = TRUE)
      process_docs(aluetaso1 = which_level, 
                   aluename = nms[ii], 
                   format = formats[iv])
    }
    stopCluster(cl)
  }
  
} else if (which_level == "Hyvinvointialueet") {
  
  regtmp <- regs[regs$level == which_level,]                       
  foreach(iv=seq_along(formats)) %dopar% {
    library(karttasovellus)
    library(dplyr)
    library(glue)
    library(ggplot2)
    
    nms <- unique(regtmp$region_name)
    for (ii in seq_along(nms)){
      cat(nms[ii], file = logfile, sep = "\n", append = TRUE)
      process_docs(aluetaso1 = which_level, 
                   aluename = nms[ii], 
                   format = formats[iv])
    }
    stopCluster(cl)
  }
  
} else if (which_level == "Seutukunnat") {
  
  regtmp <- regs[regs$level == which_level,]                       
  foreach(iv=seq_along(formats)) %dopar% {
    library(karttasovellus)
    library(dplyr)
    library(glue)
    library(ggplot2)
    
    nms <- unique(regtmp$region_name)
    for (ii in seq_along(nms)){
      cat(nms[ii], file = logfile, sep = "\n", append = TRUE)
      process_docs(aluetaso1 = which_level, 
                   aluename = nms[ii], 
                   format = formats[iv])
    }
    stopCluster(cl)
  }
  
}


system('rsync -avzhe "ssh -i /home/aurelius/avaimet/nucsrv-rsync-key" --progress --include "*.docx" --include "*.odt" --exclude "*" ~/diak_temp/ muuankarski@kapsi.fi:~/sites/software.markuskainu.fi/www/diak/alueprofiilit_docs/')

