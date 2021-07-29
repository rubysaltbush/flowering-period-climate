species_all <- cache_csv("data_cache/species_all.csv", function(){
  
  ##species, traits and range size##
  species_all <- site_species %>%
    dplyr::select(canonicalName, family, family_group, monthsBinary, monthsCount, woodiness, woody) %>%
    dplyr::distinct()
  species_all$woodiness <- factor(species_all$woodiness, levels = c("herbaceous", "semi_woody", "woody"))
  
  #join to range size data from RG
  range <- read_csv("data_input/Range_size.csv")
  colnames(range) <- c("canonicalName", "eoo_aus", "aoo_aus")
  range <- dplyr::select(range, canonicalName, eoo_aus)
  species_all <- species_all %>%
    dplyr::left_join(range, by = "canonicalName")
  rm(range)
  
  #species niche centroids (SNC) = "the average environment value across all 
  #plots in which the species is present, again possibly weighted by abundance" ter Braak et al 2018
  #multiply site environmental variables by weight of species occurrence within those sites
  #precipitation predictability
  SNC1 <- site_species %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(precpred_SNC = weighted.mean(prec_predictability, species_weight, na.rm = TRUE))
  #precipitation contingency (seasonal variability, similar year to year)
  SNC2 <- site_species %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(preccont_SNC = weighted.mean(prec_cont, species_weight, na.rm = TRUE))
  #precipitation constancy (rainfall consistency across months of the year)
  SNC3 <- site_species %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(precconst_SNC = weighted.mean(prec_const, species_weight, na.rm = TRUE))
  #mean annual precipitation (mm)
  SNC4 <- site_species %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(MAP_SNC = weighted.mean(MAP, species_weight, na.rm = TRUE))
  #log10 transformation of mean annual precipitation (mm)
  SNC5 <- site_species %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(log10MAP_SNC = weighted.mean(log10MAP, species_weight, na.rm = TRUE))
  #mean annual temperature (*C)
  SNC6 <- site_species %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(MAT_SNC = weighted.mean(MAT, species_weight, na.rm = TRUE))
  #temperature predictability
  SNC7 <- site_species %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(temppred_SNC = weighted.mean(temp_predictability, species_weight, na.rm = TRUE))
  #temperature contingency
  SNC8 <- site_species %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(tempcont_SNC = weighted.mean(temp_cont, species_weight, na.rm = TRUE))
  #temperature constancy
  SNC9 <- site_species %>%
    dplyr::group_by(canonicalName) %>%
    dplyr::summarise(tempconst_SNC = weighted.mean(temp_const, species_weight, na.rm = TRUE))
  #join all together with SNC trait values
  species_all <- species_all %>%
    dplyr::left_join(SNC1, by = "canonicalName") %>%
    dplyr::left_join(SNC2, by = "canonicalName") %>%
    dplyr::left_join(SNC3, by = "canonicalName") %>%
    dplyr::left_join(SNC4, by = "canonicalName") %>%
    dplyr::left_join(SNC5, by = "canonicalName") %>%
    dplyr::left_join(SNC6, by = "canonicalName") %>%
    dplyr::left_join(SNC7, by = "canonicalName") %>%
    dplyr::left_join(SNC8, by = "canonicalName") %>%
    dplyr::left_join(SNC9, by = "canonicalName")
  rm(SNC1, SNC2, SNC3, SNC4, SNC5, SNC6, SNC7, SNC8, SNC9)
  
  species_all
})
