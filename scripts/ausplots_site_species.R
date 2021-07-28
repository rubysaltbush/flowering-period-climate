ausplots_site_species <- cache_csv("data_cache/ausplots_site_species.csv", function() {
  #directly access species and site data from TERN AusPlots R package
  ausplots <- get_ausplots(site_info=FALSE, veg.vouchers=FALSE, veg.PI=TRUE)
  
  ###############PERCENT COVER CALCULATIONS#######################################
  #get frequency of unique species per unique site - the sum of these will be more
  #than the total hits at a site as multiple species can be recorded per hit
  ausplots_site_species <- as.data.frame(table(ausplots$veg.PI$herbarium_determination,
                                               ausplots$veg.PI$site_unique, useNA = "ifany"))
  ausplots_site_species <- ausplots_site_species[ausplots_site_species$Freq != 0,]
  colnames(ausplots_site_species) <- c("original_name", "site_unique", "freq")
  
  #get total transect hits for that site
  transect_points <- as.data.frame(table(ausplots$veg.PI$hits_unique, ausplots$veg.PI$site_unique))
  transect_points <- transect_points[transect_points$Freq != 0,]
  transect_points <- as.data.frame(table(transect_points$Var2))
  colnames(transect_points) <- c("site_unique", "total_hits")
  
  #join transect hits to species frequency data
  ausplots_site_species <- left_join(ausplots_site_species, transect_points, by = "site_unique")
  rm(transect_points)
  
  #remove sites with total_hits >10 more or 101 less than standard 1010 hits per site
  ausplots_site_species <- ausplots_site_species %>%
    dplyr::filter(ausplots_site_species$total_hits >= 909 & ausplots_site_species$total_hits <= 1020)
  
  #calculate percent cover by dividing freq by total hits per site - this will sum
  #to more than 100% per site as there can be more than one species per hit
  ausplots_site_species$percent_cover <- ausplots_site_species$freq / ausplots_site_species$total_hits*100
  
  #calculate proportional percent cover for all hits with a herbarium determination
  #first remove values without a herbarium determination (these are bare ground/litter at a site)
  ausplots_site_species <- dplyr::filter(ausplots_site_species, !is.na(ausplots_site_species$original_name))
  
  #create new data of percent_cover sums per site
  site_cover <- aggregate(ausplots_site_species$percent_cover, list(ausplots_site_species$site_unique), sum)
  colnames(site_cover) <- c("site_unique", "site_cover")
  ausplots_site_species <- left_join(ausplots_site_species, site_cover, by = "site_unique")
  rm(site_cover)
  
  #calculate proportional cover for each species per site, a value between 0-1
  #that represents a species' proportion of vegetation cover at a site
  ausplots_site_species$proportion <- ausplots_site_species$percent_cover/ausplots_site_species$site_cover
  
  #####################MATCHING NAMES TO AUSTRALIAN PLANT CENSUS##################
  #match names from ausplots data to names in the Australian Plant Census to be sure
  #that all taxa are currently accepted and will match taxon concepts used in trait data
  
  #get unique species names
  vegcover_names <- ausplots_site_species[complete.cases(ausplots_site_species$original_name),]
  vegcover_names <- as.data.frame(unique(vegcover_names$original_name))
  colnames(vegcover_names) <- "original_name"
  
  #patch names that won't match using apcnames package with names looked up manually
  #from the Australian Plant Census - most of these are Genus sp. names
  vegcover_names_patch <- read.csv("data_input/unmatched_taxa.csv") %>%
    dplyr::select(original_name, could_match) %>%
    filter(!is.na(could_match))
  
  vegcover_names <- vegcover_names %>%
    left_join(by="original_name", vegcover_names_patch) %>%
    mutate(
      original_name_patched = ifelse(!is.na(could_match), could_match, original_name),
      could_match = NULL
    )
  
  rm(vegcover_names_patch)
  
  #align the patched names to the Australian Plant Census and Australian Plant Name Index
  vegcover_names_aligned <- apcnames::align_taxa(vegcover_names$original_name_patched)
  
  #get updated taxonomy for all aligned names, make distinct and remove odd cases
  vegcover_taxa <- update_taxonomy(vegcover_names_aligned$aligned_name,
                                   output = "data_output/vegcover_taxa_1.csv")
  vegcover_taxa <- dplyr::distinct(vegcover_taxa)
  vegcover_taxa <- vegcover_taxa[vegcover_taxa$taxonRank != "[n/a]",]
  
  #remove var., subsp., etc so just species, genus or family level IDs are retained
  vegcover_taxa$canonicalName_subbed <-
    gsub("\\s(?:subsp|var|f|sect)\\.\\s.+$", "", vegcover_taxa$canonicalName, perl = TRUE)
  
  #re-update taxonomy with subspecies and variants removed
  vegcover_taxa_subbed <- update_taxonomy(vegcover_taxa$canonicalName_subbed,
                                          output = "data_output/vegcover_taxa_2.csv")
  vegcover_taxa_subbed <- dplyr::rename(vegcover_taxa_subbed, canonicalName_subbed = aligned_name)
  vegcover_taxa_subbed <- dplyr::distinct(vegcover_taxa_subbed)
  
  #join newly aligned taxa and taxonomic info to ausplots_site_species
  ausplots_site_species <- ausplots_site_species %>%
    left_join(by = "original_name", vegcover_names) %>%
    dplyr::rename(herbarium_determination = original_name, original_name = original_name_patched)
  rm(vegcover_names)
  
  vegcover_names_aligned <- dplyr::select(vegcover_names_aligned, original_name, aligned_name, source)
  vegcover_taxa <- dplyr::select(vegcover_taxa, aligned_name, canonicalName_subbed)
  vegcover_taxa_subbed <- dplyr::select(vegcover_taxa_subbed, canonicalName_subbed, canonicalName,
                                        scientificNameAuthorship, taxonomicStatus, taxonRank,
                                        family, subclass, taxonDistribution)
  
  ausplots_site_species <- ausplots_site_species %>%
    left_join(by = "original_name", vegcover_names_aligned) %>%
    left_join(by = "aligned_name", vegcover_taxa) %>%
    left_join(by = "canonicalName_subbed", vegcover_taxa_subbed)
  
  rm(vegcover_taxa)
  rm(vegcover_names_aligned)
  rm(vegcover_taxa_subbed)
  rm(taxonomic_resources, envir = globalenv())
  
  #select only columns of interest for future analyses
  ausplots_site_species <- dplyr::select(
    ausplots_site_species,
    herbarium_determination,
    canonicalName,
    site_unique,
    percent_cover,
    proportion,
    taxonomicStatus,
    taxonRank,
    family,
    subclass
  )
  
  ausplots_site_species
})
