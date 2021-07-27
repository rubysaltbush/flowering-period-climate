# Run this script once to install dependencies

# options(pkgType="mac.binary") # may help if running on mac

install.packages("Hmisc")      # weighted variance function wtd.var
install.packages("devtools")   # install_github
install.packages("ggnewscale") # mapping two scales on a map
install.packages("raster")     # loading .tif mask
install.packages("rstatix")    # used in anova
install.packages("sf")         # mapping
install.packages("tidyverse")  # data functions
install.packages("rgdal")      # loading shapefiles

library(devtools)

# taxon checking
install_github("rubysaltbush/apcnames")

# TERN data
install_github("ternaustralia/ausplotsR", build_vignettes = TRUE, dependencies = TRUE)