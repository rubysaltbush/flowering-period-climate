#histograms of flowering period lengths in ausplots species, coloured by family group
site_species %>%
  dplyr::select(canonicalName, monthsCount, family_group) %>%
  distinct() %>%
  ggplot(aes(x = monthsCount, fill = family_group)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 12, by=1)) +
  colour_scales$fam +
  theme_pubr(legend = "right") +
  xlab("Length of flowering period (months)") +
  ylab("Number of species")
ggsave("figures/Fig 4 freq flowering periods in AusPlots species.png", width = 10, height = 4)

#frequency of flowering period lengths in ausplots species coloured by family, separated by biome
#high species richness biomes with high fixed scale
site_species %>%
  dplyr::filter(Biome %in% c("Deserts and Xeric Shrublands", "Mediterranean Forests, Woodlands and Scrub", "Tropical and Subtropical Grasslands, Savannas and Shrublands")) %>%
  mutate(Biome = fct_reorder(Biome, Biome, .fun='table', .desc = TRUE)) %>% #display biomes in order of n
  dplyr::select(canonicalName, family_group, monthsCount, woodiness, Biome) %>%
  dplyr::distinct() %>%
  ggplot(aes(x = monthsCount, fill = family_group)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 12, by=1)) +
  theme_pubr(legend = "right") +
  xlab("Length of flowering period (months)") +
  ylab("Number of species") +
  facet_wrap(~Biome) +
  colour_scales$fam
ggsave("figures/Fig 4 freq flowering periods in AusPlots species large biomes.png", width = 15, height = 4)

#lower species richness biomes with lower fixed scale
site_species %>%
  dplyr::filter(Biome %in% c("Montane Grasslands and Shrublands", "Temperate Broadleaf and Mixed Forest", "Temperate Grasslands, Savannas and Shrublands")) %>%
  mutate(Biome = fct_reorder(Biome, Biome, .fun='table', .desc = TRUE)) %>% #display biomes in order of n
  dplyr::select(canonicalName, family_group, monthsCount, woodiness, Biome) %>%
  dplyr::distinct() %>%
  ggplot(aes(x = monthsCount, fill = family_group)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 12, by=1)) +
  theme_pubr(legend = "right") +
  xlab("Length of flowering period (months)") +
  ylab("Number of species") +
  facet_wrap(~Biome) +
  colour_scales$fam
ggsave("figures/Fig 4 freq flowering periods in AusPlots species small biomes.png", width = 15, height = 4)
