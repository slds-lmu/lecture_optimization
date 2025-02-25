library(shiny)
runApp()
#devtools::install_github('rstudio/rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='juliambr',
  token='C934EB3146CEFC5A8784CBE6A83AD690',
  secret='llTjQjRLJSLWZfRsZ6pi8zhMa5AguxJrftFroFOd')

deployApp(appName = "balls")

