# Dependencies
# source("scripts/install_dependencies.R") # only needed once
library(ausplotsR)
library(Hmisc)
library(tidyverse)
library(apcnames)
library(sf)
library(raster)
library(rstatix)
library(rgdal)
library(ggpubr)
library(GGally)
library(car)

# Options
options(scipen = 300) # set scientific numbers only for very large numbers

# Functions
source("scripts/functions/cache_csv.R")
source("scripts/functions/do_regression.R")

# Preparatory data
source("scripts/prepdata/ausplots_sites.R") # needs internet. All site data from TERN AusPlots.
source("scripts/prepdata/ausplots_site_species.R") # needs internet, takes a while. All species-in-site occurrence data from AusPlots, with taxonomic matching.
source("scripts/prepdata/species_flowering_time.R") # flowering period data from AusTraits and other sources
source("scripts/prepdata/species_woodiness.R") # woodiness data curated from AusTraits
source("scripts/prepdata/site_species_unfiltered.R") # species-in-sites with trait data, filtered by trait coverage only

# Final data
source("scripts/finaldata/sites.R") # site and climate data filtered by site locations, with community weighted means (CWM)
source("scripts/finaldata/site_species.R") # species-in-sites, traits, site and climate data, filtered by all above filters
source("scripts/finaldata/species_all.R") # species with species-niche-centroids, trait data and range (EOO)
source("scripts/finaldata/colour_scales.R") # colour scales for graphs

# Remove preparatory data and cache_csv function
rm(ausplots_sites, ausplots_site_species, species_flowering_time,
   species_woodiness, site_species_unfiltered, cache_csv)

# Analyses
source("scripts/analyses/ausplots_sampling_by_biome_area.R") # regress biome area vs number of AusPlots, results in figure
source("scripts/analyses/CWM_regressions.R") # main community weighted mean and species niche centroid regression analyses, outputs results and graphs
source("scripts/analyses/species_regressions.R") # species level regression, output results and graph
rm(do_regression)
source("scripts/analyses/woodiness_ttests.R") # tests of woody vs herbaceous species
source("scripts/analyses/biome_anova.R") # comparison of flowering period length among biomes

# Graphs
source("scripts/graphs/maps.R") # map figures
source("scripts/graphs/histograms.R") # histograms by family and biome
