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

# Data
source("scripts/ausplots_sites.R")
source("scripts/ausplots_site_species.R")
source("scripts/species_flowering_time.R")
source("scripts/species_woodiness.R")
source("scripts/site_species_unfiltered.R")
source("scripts/sites.R")
source("scripts/site_species.R")
source("scripts/species_niche_centroids.R")
source("scripts/niche.R")
source("scripts/range.R")
source("scripts/species.R")
source("scripts/species_biome.R")
source("scripts/colour_scales.R")