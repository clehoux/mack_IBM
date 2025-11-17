##### my packages ################################################################################
## CRAN
cran.packages <- c("raster","terra", "geosphere","stars",
                   "reticulate","future.apply", "NetLogoR", 
                   "scales", "tidyverse", "units" , "ggthemes", "ggpubr", "ggpmisc",
                   "rnaturalearth","rnaturalearthdata", "gdistance","furrr","progressr", "rslurm")
install.this <- cran.packages[!(cran.packages %in% utils::installed.packages()[,"Package"])]
if(length(install.this)>1) install.packages(install.this)
dummy <- lapply(cran.packages, require, character.only = TRUE)


## github
git.packages <- c( 'rnaturalearthhires','catchR')
install.this <- git.packages[!(git.packages %in% utils::installed.packages()[,"Package"])]
if('rnaturalearthhires' %in% install.this)  devtools::install_github("ropensci/rnaturalearthhires")
if('catchR' %in% install.this)  devtools::install_github("iml-assess/catchR@eli_parallel")
dummy <- lapply(git.packages, require, character.only = TRUE)

log10p1_trans = function() scales::trans_new("log10p1", transform=function(x) log10(x+1), inverse=function(x) (10^x)-1)#inverse function is necessary for legend

theme_set(theme_mackerel())

# load modules for model
source("R/utils/getnafo.R")
source("R/modules/adult_feeding_bioenergy.R")
source("R/modules/spawning.R")
source("R/modules/transformation.R")
source("R/modules/larvae_feeding_bioenergy.R")
source("R/modules/avoid_land.R")
