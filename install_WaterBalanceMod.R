# SPDX-FileCopyrightText: 2025 GFZ Helmholtz Centre for Geosciences
# SPDX-FileCopyrightText: 2025 Thomas Piernicke <thomasp@gfz.de>
# SPDX-License-Identifier: AGPL-3.0-only 

library("devtools")
library("roxygen2")

wd <- "C:/Users/username/Desktop/WaterBalanceMod_Git/waterbalancemodel/WaterBalanceMod/" #replace with your local path
setwd(wd)
devtools::document()
setwd("..")
try(devtools::uninstall("WaterBalanceMod"))
devtools::install("WaterBalanceMod", upgrade = "never")
library("WaterBalanceMod")
reload("WaterBalanceMod")