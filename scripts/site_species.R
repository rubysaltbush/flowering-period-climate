site_species <- cache_csv("data_cache/site_species.csv", function(){
  site_species <- site_species_unfiltered %>%
  # subset site_species_unfiltered to match sites data
  dplyr::filter(site_unique %in% sites$site_unique) %>%
  #combine above data sets into big df
  #first remove unnecessary variables in site_species
  dplyr::select(herbarium_determination:proportion, family, family_group,
                monthsBinary:monthsCount, woodiness:woody, Jan:Dec,
                site_prop_flowertime, flowertime_weight, species_weight)

  #set levels for family_group to show in order of species richness
  site_species$family_group <-
    factor(site_species$family_group,
           levels = c("Fabaceae", "Poaceae", "Myrtaceae", "Asteraceae", "Proteaceae",
                      "Chenopodiaceae", "Cyperaceae", "Other families"))
  
  # combine species, traits and site level data
  site_species <- dplyr::left_join(site_species, sites, by = "site_unique")
})