site_species_unfiltered <- cache_csv("data_cache/site_species_unfiltered.csv", function(){
  #join flowering time data to vegcover data
  site_species_unfiltered <- ausplots_site_species %>%
    dplyr::left_join(species_flowering_time, by = "canonicalName")
  
  #remove non-Angiosperm taxa from data given these DON'T HAVE FLOWERS
  #will also remove any unmatched/not accepted species
  site_species_unfiltered <- dplyr::filter(site_species_unfiltered, site_species_unfiltered$subclass == "Magnoliidae")
  #remove taxa with only genus or family level IDs as these cannot have matching
  #flowering time data
  site_species_unfiltered <- dplyr::filter(site_species_unfiltered, site_species_unfiltered$taxonRank == "Species")
  
  ##test trait coverage of flowering time data for vegcover species##
  #re-calculate proportion cover as proportion of species-level Magnoliid per site
  #create new data of percent_cover sums per site
  flower_site_cover <- aggregate(site_species_unfiltered$percent_cover, list(site_species_unfiltered$site_unique), sum)
  colnames(flower_site_cover) <- c("site_unique", "site_angio_spp_cover")
  site_species_unfiltered <- left_join(site_species_unfiltered, flower_site_cover, by = "site_unique")
  rm(flower_site_cover)
  
  #calculate proportional cover for each Magnoliid SPECIES per site, a value between 0-1
  #that represents a species' proportion of Magnoliid SPECIES cover at a site
  site_species_unfiltered$propangiospp <- site_species_unfiltered$percent_cover/site_species_unfiltered$site_angio_spp_cover
  
  #drop species that do not have matching trait data
  site_species_unfiltered <- site_species_unfiltered %>%
    dplyr::filter(!is.na(site_species_unfiltered$monthsCount))
  
  #create temp data of proportion of flowering species cover with matching trait data per site
  flowertime_sites <- aggregate(site_species_unfiltered$propangiospp, list(site_species_unfiltered$site_unique), sum)
  colnames(flowertime_sites) <- c("site_unique", "site_prop_flowertime")
  
  #create histogram of flowering time trait coverage at sites
  ggplot(data.frame(flowertime_sites$site_prop_flowertime), aes(x = flowertime_sites$site_prop_flowertime)) +
    geom_histogram() +
    xlab("Proportion of angiosperm species cover with flowering period trait data") +
    ylab("Number of AusPlots") +
    theme_pubr()
  ggsave("figures/flowering time trait cover.png", width = 7, height = 3.6)
  
  #join back onto vegcover data
  site_species_unfiltered <- site_species_unfiltered %>%
    left_join(flowertime_sites, by = "site_unique")
  rm(flowertime_sites)
  
  #drop all sites that have trait data for <0.8 proportion of site cover (currently 7 sites)
  site_species_unfiltered <- site_species_unfiltered %>%
    dplyr::filter(site_species_unfiltered$site_prop_flowertime >= 0.8)
  
  #re-calculate proportion as proportion of cover at a site for which we have
  #matching flowering time trait data - this will be "flowertime_weight"
  site_flowertime <- aggregate(site_species_unfiltered$percent_cover, list(site_species_unfiltered$site_unique), sum)
  colnames(site_flowertime) <- c("site_unique", "site_flowertime_cover")
  site_species_unfiltered <- left_join(site_species_unfiltered, site_flowertime, by = "site_unique")
  rm(site_flowertime)
  
  #calculate proportional cover for species with matching trait data - this will be
  #the weight in the Community Weighted Mean (will sum to 1 per site)
  site_species_unfiltered$flowertime_weight <- site_species_unfiltered$percent_cover/site_species_unfiltered$site_flowertime_cover
  
  #weight each months' flowering by the flowertime_weight to graph flowering month by month
  #first decompose flowering period into 12 separate variables
  site_species_unfiltered$monthsBinary2 <- site_species_unfiltered$monthsBinary
  site_species_unfiltered <- site_species_unfiltered %>%
    tidyr::extract(monthsBinary2, into = c("Jan", "Feb", "Mar", "Apr", "May",
                                           "Jun", "Jul", "Aug", "Sep", "Oct",
                                           "Nov", "Dec"),
                   '(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)')
  #first transform to numeric variables, then multiply months by species weight in plot
  for(i in c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep",
             "Oct", "Nov", "Dec")){
    site_species_unfiltered[[i]] <- as.numeric(unlist(site_species_unfiltered[[i]]))
    site_species_unfiltered[[i]] <- site_species_unfiltered[[i]] * site_species_unfiltered$flowertime_weight
  }
  rm(i)
  
  ##for calculation of species niche centroids below##
  #sum species raw percent_cover across all sites per species
  species_cover <- aggregate(site_species_unfiltered$percent_cover, list(site_species_unfiltered$canonicalName), sum)
  colnames(species_cover) <- c("canonicalName", "species_cov")
  site_species_unfiltered <- dplyr::left_join(site_species_unfiltered, species_cover, by = "canonicalName")
  rm(species_cover)
  #then divide species percent cover at a site by its total percent cover across all sites
  #giving a species' weight per plot relative to total distribution of that species
  site_species_unfiltered <- site_species_unfiltered %>%
    mutate(species_weight = percent_cover/species_cov)
  
  #for plotting, good to know what the distribution of different features is
  #between the dominant families (both by species richness and by cover). BUT
  #to do this need to define "family_category", with top 7 families and all
  #other families in an "other" category
  #first get count of occurences of taxa in families across all sites
  families <- as.data.frame(table(site_species_unfiltered$family))
  colnames(families) <- c("family", "occurence")
  #then sum together proportional cover (weight in CWM analyses) for families across all sites
  fam2 <- aggregate(site_species_unfiltered$flowertime_weight, list(site_species_unfiltered$family), sum)
  colnames(fam2) <- c("family", "sum_weight")
  #then count number of species in each family included in data set
  fam3 <- site_species_unfiltered %>%
    dplyr::select(canonicalName, family) %>%
    distinct() %>%
    group_by(family) %>%
    summarise(species_richness = n())
  #join together
  families <- families %>%
    left_join(fam2, by = "family") %>%
    left_join(fam3, by = "family")
  rm(fam2, fam3)
  #strong correlation but slight differences between top families for number of
  #occurrences, cover across sites and species richness.
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
  #set levels for family_group to show in order of species richness
  families$family_group <- factor(families$family_group,
                                  levels = c("Fabaceae", "Poaceae", "Myrtaceae",
                                             "Asteraceae", "Proteaceae",
                                             "Chenopodiaceae", "Cyperaceae",
                                             "Other families"))
  write_csv(families, "data_output/ausplots_families.csv")
  
  #join family_group back onto site_species_unfiltered by family
  #first get rid of other columns
  families <- families %>%
    dplyr::select(family, family_group)
  site_species_unfiltered <- site_species_unfiltered %>%
    left_join(families, by = "family")
  rm(families)
  
  #add woodiness data to site_species_unfiltered
  site_species_unfiltered <- site_species_unfiltered %>%
    dplyr::left_join(species_woodiness, by = "canonicalName")
  #check coverage of woodiness data - check again once sites subsetted???
  paste(sum(is.na(site_species_unfiltered$woody)), "observations missing woodiness data")
  
  site_species_unfiltered
})
