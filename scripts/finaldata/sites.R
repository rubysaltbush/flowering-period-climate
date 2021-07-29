sites <- cache_csv("data_cache/sites.csv", function(){
  #join site info to list of remaining sites to check site details
  sites <- as.data.frame(unique(site_species_unfiltered$site_unique))
  colnames(sites) <- c("site_unique")
  sites <- sites %>%
    dplyr::left_join(ausplots_sites, by = "site_unique")
  
  #what is the species richness for each site/visit?
  speciesrichness <- site_species_unfiltered %>%
    dplyr::group_by(site_unique) %>%
    dplyr::summarise(no_species = length(unique(canonicalName)))
  sites <- sites %>%
    dplyr::left_join(speciesrichness, by = "site_unique")
  rm(speciesrichness)
  
  ##check spatial independence##
  #first check sites with repeat visits - same location but different times
  sitedupes <- sites[1 != ave(sites$site_location_name,sites$site_location_name,FUN=length),]
  paste(length(unique(sitedupes$site_location_name)), "sites have repeated visits")
  table(sitedupes$state)
  paste(sum(duplicated(sites$site_location_name)), "repeat site visits will be removed from the analysis")
  paste("Visits with highest species richness will be retained")
  #most repeat visit sites in SA, then Qld, NT
  #drop data from repeat visits to sites - retain visits with highest species richness
  #as these most representative of functional diversity at a location
  sites <- sites %>%
    dplyr::group_by(site_location_name) %>%
    dplyr::slice_max(order_by = no_species, with_ties = FALSE)
  rm(sitedupes)
  
  #now also check - what is the distance between sites?
  #first duplicate latitude/longitude columns so I don't lose these
  sites <- sites %>%
    dplyr::mutate(long = longitude, lat = latitude)
  #convert sites to an sf spatial object and assign CRS of WGS84
  sites <- sf::st_as_sf(sites, coords = c("long","lat"))
  st_crs(sites) <- 4326
  #calculate distance to nearest site
  distances <- sf::st_distance(sites, sites)
  distances <- apply(distances, 2, sort)
  sites$nearest_site_km <- distances[2,]/1000
  #SOME SITES ARE <100m apart
  paste(nrow(sites[sites$nearest_site_km < 0.5,]), "sites are less than 500 metres from their nearest site")
  #drop one site from each of pair of sites <500 metres apart
  #retaining sites with highest species richness where possible
  todrop <- sites[sites$nearest_site_km < 0.5,]
  todrop <- todrop %>%
    dplyr::group_by(nearest_site_km) %>%
    dplyr::slice_min(order_by = no_species, with_ties = FALSE)
  paste(nrow(todrop), "sites <500m from their nearest site will be removed ")
  #remove sites <500m apart, then recalculate distances between sites to check it worked
  sites <- sites %>%
    dplyr::filter(!(site_unique %in% todrop$site_unique))
  #recalculate distance to nearest site
  distances <- sf::st_distance(sites, sites)
  distances <- apply(distances, 2, sort)
  sites$nearest_site_km <- distances[2,]/1000
  
  if (nrow(sites[sites$nearest_site_km < 0.5,]) != 0){
    stop("sites remain that are too close to each other, fix it")
  } else{
    paste(nrow(sites[sites$nearest_site_km < 0.5,]), "sites within 500m of another site remain")
  }
  rm(todrop, distances)
  
  #NOT removing sites with low species richness as these (by definition) still represent a high
  #proportion of cover at these sites - they are just not very diverse locations!
  paste("species richness cutoff of 5 species would remove",
        nrow(filter(sites, no_species <= 5)), "sites")
  
  ##########COMMUNITY WEIGHTED MEANS###########
  #calculate 'community weighted means' (CWMs) by multiplying species proportional
  #abundances by matched trait values
  ##MONTHS COUNT##
  #calculate weighted mean of monthsCount per unique site and store in new df
  monthsCount_CWM <- site_species_unfiltered %>%
    dplyr::group_by(site_unique) %>%
    dplyr::summarise(monthsCount_CWM = weighted.mean(monthsCount, flowertime_weight))
  #calculate variance of weighted monthsCount per unique site and store as column in new df
  monthsCount_CWV <- site_species_unfiltered %>%
    dplyr::group_by(site_unique) %>%
    dplyr::summarise(monthsCount_CWV = Hmisc::wtd.var(monthsCount, flowertime_weight, normwt = TRUE))
  #wtd.var function returns NaN for sites with only 1 species
  #(because dividing by n-1 = dividing by zero in these cases)
  #replace NaN with zero, as variance for these sites is zero (only 1 observation!)
  monthsCount_CWV$monthsCount_CWV[is.na(monthsCount_CWV$monthsCount_CWV)] = 0
  
  #join CWM and CWV to site info
  sites <- sites %>%
    dplyr::left_join(monthsCount_CWM, by = "site_unique") %>%
    dplyr::left_join(monthsCount_CWV, by = "site_unique")
  rm(monthsCount_CWM, monthsCount_CWV)
  
  ##biomes##
  #read in detailed biome shapefile, don't use for imaging but do use
  #for any spatial manipulations
  biome_sf <- sf::st_read("data_input/ibra7_biomes.shp")
  #make spatially valid as update detects geometry errors in shapefile
  biome_sf <- sf::st_make_valid(biome_sf)
  #add attributes from biome shapefile to sites by location intersect
  sites <- sf::st_join(sites, biome_sf)
  #get rid of non-useful columns
  sites <- dplyr::select(sites, site_unique, monthsCount_CWM,
                         monthsCount_CWV,
                         no_species, state, biome,
                         established_date, visit_start_date,
                         location_description, bioregion_name,
                         site_location_name, latitude:longitude,
                         nearest_site_km, geometry)
  rm(biome_sf)
  #check how many sites in each biome
  table(sites$biome)
  #and how many in each state?
  table(sites$state)
  #removing duplicate and <500m apart sites has reduced montane grasslands from
  #14 to 12 sites, not too bad, and sampling looks a bit more evenly distributed
  #between states at least
  
  #read in environmental data for each site
  #Jiang Mingkai's AWAP 1930-2018 rainfall predictability data - ~5km resolution
  #using precipitation predictability data binned by log3 scale (7 bins)
  prec_predictability <- read_rds("data_input/Australia_predictability/output_rainfall_exp/selected_site_rainfall_preditability_exp_bin3.rds")
  prec_predictability <- prec_predictability %>%
    dplyr::select(site_unique, prec_predictability = Predictability,
                  prec_const = Constancy, prec_cont = Contingency)
  
  #using temperature predictability data binned on 5 degree scale (~7 bins?)
  temp_predictability <- read_rds("data_input/Australia_predictability/selected_site_temperature_fixed_bin_preditability.rds")
  temp_predictability <- temp_predictability %>%
    dplyr::select(site_unique, temp_predictability = Predictability,
                  temp_const = Constancy, temp_cont = Contingency, MAT:MAP,
                  latitude:longitude)
  
  #join temp and precipitation data
  climate_data <- prec_predictability %>%
    dplyr::left_join(temp_predictability, by = "site_unique")
  rm(temp_predictability, prec_predictability)
  #read in mask to remove potentially unreliable climate scores
  mask_AWAP <- raster("data_input/mask_AWAP.tif")
  #join mask scores from raster to climate_data by longitude and latitude of locations
  climate_data <- sf::st_as_sf(climate_data, coords = c("longitude","latitude"))
  st_crs(climate_data) <- 4326
  climate_data$mask_AWAP <- extract(mask_AWAP, climate_data)
  #ggplot() +
    #geom_sf(data = climate_data, aes(colour = mask_AWAP))
  rm(mask_AWAP)
  #coordinates(climate_data) <- c("longitude", "latitude")
  #projection(climate_data) <- CRS("+proj=longlat +ellps=WGS84")
  #spplot(climate_data, "mask_AWAP")
  
  #remove sites with 0 value (~60 from whole list of 810)
  climate_data <- climate_data %>%
    dplyr::filter(mask_AWAP == 1)
  
  #match to site data. first remove sf
  climate_data <- st_drop_geometry(climate_data)
  sites <- sites %>%
    dplyr::left_join(climate_data, by = "site_unique")
  rm(climate_data)
  
  #what is coverage of climatic variables?
  paste(sum(is.na(sites$temp_predictability)), "sites missing temp_predictability data")
  paste(sum(is.na(sites$prec_predictability)), "sites missing prec_predictability data")
  paste(sum(is.na(sites$MAP)), "sites missing MAP data")
  paste(sum(is.na(sites$MAT)), "sites missing MAT data")
  #52 sites now missing data for all measures now mask applied
  #ggplot() +
    #geom_sf(data = sites, aes(colour = prec_predictability))
  #sites missing data look to be in right place
  
  #log transform precipitation variables
  #plot(sites$MAP)
  sites$log10MAP <- log10(sites$MAP)
  #plot(sites$log10MAP)
  
  sites
})