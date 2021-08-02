#woodiness - Welch's t-tests of all species and then species by biome
ttests <- list()
ttests$woody <- t.test(species_all$monthsCount[species_all$woodiness == "woody"],
                       species_all$monthsCount[species_all$woodiness == "herbaceous"])
ttests$woody
#significant though hard to see in boxplot - herbaceous species slightly higher

#some species occur in multiple biomes - t-tests of woodiness vs herbaceousness
#should consider the species that occur in each biome, with Bonferroni
#corrections for multiple t-tests (alpha = 0.05/6)
species_biome <- site_species %>%
  dplyr::select(canonicalName, Biome) %>%
  dplyr::distinct() %>%
  dplyr::left_join(species_all, by = "canonicalName")

#per biome Welch's t-tests (okay if unequal variances)
ttests$woodydeserts <- t.test(species_biome$monthsCount[species_biome$woodiness == "woody" & species_biome$Biome == "Deserts and Xeric Shrublands"],
                              species_biome$monthsCount[species_biome$woodiness == "herbaceous" & species_biome$Biome == "Deserts and Xeric Shrublands"])
ttests$woodydeserts
ttests$woodymedit <- t.test(species_biome$monthsCount[species_biome$woodiness == "woody" & species_biome$Biome == "Mediterranean Forests, Woodlands and Scrub"],
                            species_biome$monthsCount[species_biome$woodiness == "herbaceous" & species_biome$Biome == "Mediterranean Forests, Woodlands and Scrub"])
ttests$woodymedit
ttests$woodytrop <- t.test(species_biome$monthsCount[species_biome$woodiness == "woody" & species_biome$Biome == "Tropical and Subtropical Grasslands, Savannas and Shrublands"],
                           species_biome$monthsCount[species_biome$woodiness == "herbaceous" & species_biome$Biome == "Tropical and Subtropical Grasslands, Savannas and Shrublands"])
ttests$woodytrop
ttests$woodyforest <- t.test(species_biome$monthsCount[species_biome$woodiness == "woody" & species_biome$Biome == "Temperate Broadleaf and Mixed Forest"],
                             species_biome$monthsCount[species_biome$woodiness == "herbaceous" & species_biome$Biome == "Temperate Broadleaf and Mixed Forest"])
ttests$woodyforest
ttests$woodymontane <- t.test(species_biome$monthsCount[species_biome$woodiness == "woody" & species_biome$Biome == "Montane Grasslands and Shrublands"],
                              species_biome$monthsCount[species_biome$woodiness == "herbaceous" & species_biome$Biome == "Montane Grasslands and Shrublands"])
ttests$woodymontane
ttests$woodygrass <- t.test(species_biome$monthsCount[species_biome$woodiness == "woody" & species_biome$Biome == "Temperate Grasslands, Savannas and Shrublands"],
                            species_biome$monthsCount[species_biome$woodiness == "herbaceous" & species_biome$Biome == "Temperate Grasslands, Savannas and Shrublands"])
ttests$woodygrass

#export t.test results
tresults <- data.frame(sapply(ttests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    x$statistic[1],
    x$parameter[1],
    p.value = x$p.value)
}))
tresults$rownames <- rownames(tresults)
#CAN I GET NOBS AND ADD AS COLUMN??

write_csv(tresults, "data_output/woodiness_ttest_results.csv")

# Graphs
species_all %>%
  dplyr::filter(woodiness == "woody" | woodiness == "herbaceous") %>%
  ggplot(aes(x = woodiness, y = monthsCount, fill = woodiness)) +
  geom_violin(scale = "count") +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  theme_pubr() +
  theme(legend.position = "none", plot.title = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(1, 12, by = 1), limits = c(1,12)) +
  ylab("Length of flowering period (months)") +
  xlab("") +
  scale_fill_viridis_d(direction = -1) +
  scale_x_discrete(labels = paste(c("Herbacous species", "Woody species"), "\n(N = ",
                                  c(nobs(species_all$monthsCount[species_all$woodiness == "herbaceous"]),
                                    nobs(species_all$monthsCount[species_all$woodiness == "woody"])),
                                  ")", "\n(mean = ",
                                  c(signif(mean(species_all$monthsCount[species_all$woodiness == "herbaceous"]), 2),
                                    signif(mean(species_all$monthsCount[species_all$woodiness == "woody"]), 2)),
                                  ")", sep = "")) +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14))
ggsave("figures/Fig S3 flowering period by woodiness.png", width = 6, height = 8)

#graph woodiness per biome
species_biome %>%
  dplyr::filter(woodiness == "woody" | woodiness == "herbaceous") %>%
  mutate(Biome = fct_reorder(Biome, Biome, .fun='table', .desc = TRUE)) %>% #display biomes in order of n
  ggplot(aes(x = woodiness, y = monthsCount, fill = woodiness)) +
  geom_violin(scale = "count") +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  theme_pubr() +
  theme(legend.position = "none", plot.title = element_text(size = 11)) +
  scale_y_continuous(breaks = seq(1, 12, by = 1), limits = c(1,12)) +
  ylab("Length of flowering period (months)") +
  xlab("") +
  facet_wrap(~Biome) +
  scale_fill_viridis_d(direction = -1)
ggsave("figures/Fig S3 flowering period by woodiness by biome.png", width = 13, height = 6)

rm(species_biome, tresults, ttests)
