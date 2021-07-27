# Dependencies
# source("scripts/install_dependencies.R") # only needed once
library(ausplotsR)
library(Hmisc)
library(tidyverse)
library(apcnames)
library(sf)
library(ggnewscale)
library(raster)
library(rstatix)
library(rgdal)

# Options
options(scipen = 300) #set scientific numbers only for huge numbers

# Functions
source("scripts/cache_csv.R")
source("scripts/do_regression.R")