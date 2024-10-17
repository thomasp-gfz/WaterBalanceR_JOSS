library("devtools")
library("roxygen2")

if (Sys.info()["sysname"] == "Windows"){
  wd <- "...waterbalancemodel/" #replace with your local path, if using windows
  }else{
  wd <- "...waterbalancemodel/" #replace with your local path, if not using windows
}

setwd(wd)

file.remove("NAMESPACE")

for (file in list.files("man", full.names=T)){
  file.remove(file)
}

files <- c(list.files("./R/", full.names=T), "DESCRIPTION")
file <- files[1]

for (file in files){
  nonasc <- suppressMessages(suppressWarnings(tools::showNonASCIIfile(file)))
  if(length(nonasc) > 0){
    print(file)
    print(nonasc)
    #stop("Found non ascii character")
  }
}

rm(list=ls())

devtools.name <- "Thomas Piernicke"
devtools.desc.author <- "Thomas Piernicke <thomasp@gfz-potsdam.de> [aut, cre]"

devtools::document()

setwd("..")

check <- devtools::check("WaterBalanceMod", env_vars = list("_R_CHECK_LICENSE_"=FALSE))
print(check)

devtools::build("WaterBalanceMod")

try(devtools::uninstall("WaterBalanceMod"))
devtools::install("WaterBalanceMod", upgrade = "never")

library("WaterBalanceMod")
reload("WaterBalanceMod")

print(check)



