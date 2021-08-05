site_species <- cache_csv("data_cache/site_species.csv", function(){
  site_species <- site_species_unfiltered %>%
  # subset site_species_unfiltered to match sites data
  dplyr::filter(site_unique %in% sites$site_unique)
  
  # define most important plant families
  #first get count of occurrences of taxa in families across all sites
  families <- as.data.frame(table(site_species$family))
  colnames(families) <- c("family", "occurrence")
  #then sum together proportional cover (weight in CWM analyses) for families across all sites
  fam2 <- aggregate(site_species$flowertime_weight, list(site_species$family), sum)
  colnames(fam2) <- c("family", "sum_weight")
  #then count number of species in each family included in data set
  fam3 <- site_species %>%
    dplyr::select(canonicalName, family) %>%
    distinct() %>%
    group_by(family) %>%
    summarise(species_richness = n())
  #join together
  families <- families %>%
    left_join(fam2, by = "family") %>%
    left_join(fam3, by = "family")
  rm(fam2, fam3)
  
  #assign families to a family_group, based on the top 7 families by each measure
  for (i in 1:nrow(families)) {
    if (families$family[i] %in% c("Poaceae", "Fabaceae", "Chenopodiaceae", "Myrtaceae",
                                  "Asteraceae", "Proteaceae", "Cyperaceae")) {
      families$family_group[i] <- families$family[i]
    } else {
      families$family_group[i] <- "Other families"
    }
  }
  rm(i)
  
  write_csv(families, "data_output/ausplots_families.csv")
  
  #join family_group back onto site_species by family
  #first get rid of other columns
  families <- families %>%
    dplyr::select(family, family_group)
  site_species <- site_species %>%
    left_join(families, by = "family")
  rm(families)
  
  # remove unnecessary variables in site_species
  site_species <- site_species %>%
    dplyr::select(herbarium_determination:proportion, family, family_group,
                  monthsBinary:monthsCount, woodiness:woody, Jan:Dec,
                  site_prop_flowertime, flowertime_weight, species_weight)
  
  # combine species, traits and site level data
  site_species <- dplyr::left_join(site_species, sites, by = "site_unique")
})

#set levels for family_group to show in order of species richness
site_species$family_group <- factor(site_species$family_group,
         levels = c("Fabaceae", "Poaceae", "Myrtaceae", "Asteraceae", "Proteaceae",
                    "Chenopodiaceae", "Cyperaceae", "Other families"))
