# Run this script from the dashboard project folder in RStudio.
if (!file.exists('app.R')) {
  stop('Open Student_Early_Warning_Shiny.Rproj first, then source this script.')
}
source('setup.R', local = TRUE)
# Bind only to this computer. Keep R running while demonstrating the app.
shiny::runApp('.', host = '127.0.0.1', launch.browser = TRUE)
