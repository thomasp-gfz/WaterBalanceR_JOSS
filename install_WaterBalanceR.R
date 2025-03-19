# SPDX-FileCopyrightText: 2025 GFZ Helmholtz Centre for Geosciences
# SPDX-FileCopyrightText: 2025 Thomas Piernicke <thomasp@gfz.de>
# SPDX-License-Identifier: AGPL-3.0-only 

library("devtools")
library("roxygen2")

wd <- "C:/Users/username/Desktop/WaterBalanceR_Git/WaterBalanceR/WaterBalanceR/" #replace with your local path
setwd(wd)
devtools::document()
setwd("..")
try(devtools::uninstall("WaterBalanceR"))
devtools::install("WaterBalanceR", upgrade = "never")
library("WaterBalanceR")
reload("WaterBalanceR")