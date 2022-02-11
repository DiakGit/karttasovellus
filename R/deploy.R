library(rsconnect)

# Print a list of app dependencies. Libraries need to be loaded
# before publishing so deployApp() knows what is necessary.
error_on_missing_name <- function(name){
  var <- Sys.getenv(name, unset=NA)
  if(is.na(var)){
    stop(paste0("cannot find ",name),call. = FALSE)
  }
  gsub("\"", '',var)
}

Sys.setlocale(locale="en_US.UTF-8")

# Set the account info for deployment.
setAccountInfo(name   = error_on_missing_name("SHINY_ACC_NAME"),
               token  = error_on_missing_name("TOKEN"),
               secret = error_on_missing_name("SECRET"))

# Deploy the application.
# deployApp(
#   appFiles = c("ui.R","server.R", "global.R", "www", "translations", "pkgs.R"#, "renv/activate.R","renv.lock",'.Rprofile'
#   ),
#   appName = "korona_atc_app",
#   forceUpdate = TRUE,
#   appTitle = "korona_atc_app_title",
#   account = "kelaresearchandanalytics")
rsconnect::deployApp(launch.browser = FALSE,
                     lint = FALSE,
                     appName = error_on_missing_name("MASTERNAME"),
                     account = 'diak',
                     forceUpdate = TRUE)
