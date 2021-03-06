# prepare and cache data of AusPlots species-in-sites with matched flowering
# period trait data, filtered based on trait coverage (>= 80 %) but not yet
# filtered based on site location

site_species_unfiltered <- cache_csv("data_cache/site_species_unfiltered.csv", function(){
  # join flowering period data to site_species data
  site_species_unfiltered <- ausplots_site_species %>%
    dplyr::left_join(species_flowering_time, by = "canonicalName")
  
  # remove non-Angiosperm taxa from data set given these don't have flowers
  # will also remove any unmatched/not accepted species
  # Australian Plant Census label Angiosperms "Magnoliidae"
  site_species_unfiltered <- dplyr::filter(site_species_unfiltered, 
                                           site_species_unfiltered$subclass == "Magnoliidae")
  # remove taxa with only genus or family level IDs as these cannot have matching
  # flowering time data
  site_species_unfiltered <- dplyr::filter(site_species_unfiltered, 
                                           site_species_unfiltered$taxonRank == "Species")
  
  # test trait coverage of flowering time data for AusPlots Angiosperm species
  # re-calculate proportion cover as proportion of Angiosperm species per site
  # create new data of percent_cover sums per site (with only Angiosperm species)
  flower_site_cover <- aggregate(site_species_unfiltered$percent_cover, 
                                 list(site_species_unfiltered$site_unique), sum)
  colnames(flower_site_cover) <- c("site_unique", "site_angio_spp_cover")
  site_species_unfiltered <- left_join(site_species_unfiltered, flower_site_cover, by = "site_unique")
  rm(flower_site_cover)
  
  # calculate proportional cover for each Angiosperm species per site, a value between 0-1
  # that represents a species' proportion of Angiosperm species cover at a site
  site_species_unfiltered$propangiospp <- site_species_unfiltered$percent_cover/site_species_unfiltered$site_angio_spp_cover
  
  # drop species that do not have matching trait data
  site_species_unfiltered <- site_species_unfiltered %>%
    dplyr::filter(!is.na(site_species_unfiltered$monthsCount))
  
  # create temp data of proportion of Angiosperm species cover with matching trait data per site
  flowertime_sites <- aggregate(site_species_unfiltered$propangiospp, list(site_species_unfiltered$site_unique), sum)
  colnames(flowertime_sites) <- c("site_unique", "site_prop_flowertime")
  
  # export histogram of flowering period trait coverage at sites
  ggplot(data.frame(flowertime_sites$site_prop_flowertime), aes(x = flowertime_sites$site_prop_flowertime)) +
    geom_histogram() +
    xlab("Proportion of angiosperm species cover with flowering period trait data") +
    ylab("Number of AusPlots") +
    theme_pubr()
  ggsave("figures/Fig S1 flowering time trait cover.png", width = 7, height = 3.6)
  
  # join back onto site_species data
  site_species_unfiltered <- site_species_unfiltered %>%
    left_join(flowertime_sites, by = "site_unique")
  rm(flowertime_sites)
  
  # drop all sites that have trait data for <0.8 proportion of site cover
  site_species_unfiltered <- site_species_unfiltered %>%
    dplyr::filter(site_species_unfiltered$site_prop_flowertime >= 0.8)
  
  # re-calculate proportion as proportion of cover at a site for which we have
  # matching flowering time trait data - this will be "flowertime_weight"
  # used as weight in calculating Community Weighted Means (will sum to 1 per site)
  site_flowertime <- aggregate(site_species_unfiltered$percent_cover, list(site_species_unfiltered$site_unique), sum)
  colnames(site_flowertime) <- c("site_unique", "site_flowertime_cover")
  site_species_unfiltered <- left_join(site_species_unfiltered, site_flowertime, by = "site_unique")
  rm(site_flowertime)
  site_species_unfiltered$flowertime_weight <- site_species_unfiltered$percent_cover/site_species_unfiltered$site_flowertime_cover
  
  # weight each months flowering by species flowertime_weight to graph flowering 
  # month by month
  # first decompose flowering period into 12 separate variables
  site_species_unfiltered$monthsBinary2 <- site_species_unfiltered$monthsBinary
  site_species_unfiltered <- site_species_unfiltered %>%
    tidyr::extract(monthsBinary2, into = c("Jan", "Feb", "Mar", "Apr", "May",
                                           "Jun", "Jul", "Aug", "Sep", "Oct",
                                           "Nov", "Dec"),
                   '(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)')
  # then transform to numeric variables, then multiply months by species weight in plot
  for(i in c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep",
             "Oct", "Nov", "Dec")){
    site_species_unfiltered[[i]] <- as.numeric(unlist(site_species_unfiltered[[i]]))
    site_species_unfiltered[[i]] <- site_species_unfiltered[[i]] * site_species_unfiltered$flowertime_weight
  }
  rm(i)
  
  # for calculation of species niche centroids
  # sum species raw percent_cover across all sites per species
  species_cover <- aggregate(site_species_unfiltered$percent_cover, list(site_species_unfiltered$canonicalName), sum)
  colnames(species_cover) <- c("canonicalName", "species_cov")
  site_species_unfiltered <- dplyr::left_join(site_species_unfiltered, species_cover, by = "canonicalName")
  rm(species_cover)
  # then divide species percent cover at a site by its total percent cover across all sites
  # giving a species weight per plot relative to total distribution of that species
  site_species_unfiltered <- site_species_unfiltered %>%
    mutate(species_weight = percent_cover/species_cov)
  
  # add woodiness data to site_species_unfiltered
  site_species_unfiltered <- site_species_unfiltered %>%
    dplyr::left_join(species_woodiness, by = "canonicalName")
  
  site_species_unfiltered
})
