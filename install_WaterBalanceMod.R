library("devtools")
library("roxygen2")

wd <- "C:/Users/username/Desktop/WaterBalanceMod_Git/waterbalancemodel/WaterBalanceMod/" #replace with your local path, if using windows
setwd(wd)
devtools::document()
setwd("..")
try(devtools::uninstall("WaterBalanceMod"))
devtools::install("WaterBalanceMod", upgrade = "never")
library("WaterBalanceMod")
reload("WaterBalanceMod")