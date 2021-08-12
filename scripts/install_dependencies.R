# Run this script once to install dependencies

# options(pkgType="mac.binary") # may help if running on mac

install.packages("Hmisc")      # weighted variance function wtd.var
install.packages("devtools")   # install_github function
install.packages("raster")     # loading .tif mask
install.packages("rstatix")    # for Games-Howell posthoc tests in ANOVA
install.packages("sf")         # mapping simple features
install.packages("tidyverse")  # data functions and syntax
install.packages("rgdal")      # loading shapefiles
install.packages("ggpubr")     # publication ready theme theme_pubr for ggplots

library(devtools)

# taxon checking
install_github("rubysaltbush/apcnames")

# TERN data
install_github("ternaustralia/ausplotsR", build_vignettes = TRUE, dependencies = TRUE)