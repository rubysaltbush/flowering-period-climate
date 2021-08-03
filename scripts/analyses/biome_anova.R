#ANOVA test to compare difference between flowering period lengths in biomes
anova <- list()
#first Bartlett test for homogeneity of variance (sample size very unequal)
anova$bartlett <- bartlett.test(monthsCount_CWM ~ Biome, data = sites)
anova$bartlett
#p value <0.05, unequal variances, use Welch's ANOVA
anova$welch <- oneway.test(monthsCount_CWM ~ Biome, data = sites, var.equal = FALSE)
anova$welch
#ANOVA significant, use Games-Howell post-hoc test
anova$posthoc <- sites %>%
  dplyr::select(monthsCount_CWM, Biome) %>%
  as.data.frame() %>%
  rstatix::games_howell_test(monthsCount_CWM ~ Biome, conf.level = 0.95, detailed = FALSE)
anova$posthoc

#output ANOVA results
write_csv(broom::glance(anova$welch), "data_output/anova_results.csv")
write_csv(anova$posthoc, "data_output/anova_posthoc_results.csv")

# Graphs

#chart of monthsCount CWM across all species in sites in a biome
sites %>%
  mutate(Biome = fct_reorder(Biome, Biome, .fun='table')) %>% #orders violin plot by number of sites
  ggplot(aes(x = Biome, y = monthsCount_CWM, fill = Biome, color = Biome)) +
  geom_violin() + #can add scale = "count" to scale violins by number of observations
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_y_continuous(breaks = seq(1, 12, by=1), limits=c(1,12)) +
  theme_pubr(legend = "none") + #gets rid of legend
  coord_flip() + # switches X and Y axis
  xlab("") +
  ylab("Community weighted mean length of flowering period (months)") +
  colour_scales$biome +
  colour_scales$biomecol
ggsave("figures/Fig 4 Length of flowering period by biome.png", width = 10, height = 3)

##flowering month by month##
##all species##
site_species %>%
  dplyr::group_by(site_unique) %>%
  dplyr::summarise(across(Jan:Dec, sum)) %>%
  dplyr::left_join(sites, by = "site_unique") %>%
  dplyr::select(site_unique:Dec, Biome) %>%
  dplyr::group_by(Biome) %>%
  dplyr::summarise(across(Jan:Dec, mean)) %>%
  tidyr::pivot_longer(cols = 2:13, names_to = "month", values_to = "mean_weight") %>%
  mutate(month = forcats::as_factor(month)) %>%
  ggplot(aes(x = month, y = mean_weight, colour = Biome, group = Biome)) +
  geom_point(aes(shape = Biome)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  ylab("Mean proportion species flowering per plot") +
  xlab("Month of flowering") +
  colour_scales$biomecol +
  theme_pubr(legend = "right")
ggsave("figures/Fig 4 mean weight spp flowering per biome per month.png", width = 10, height = 4)

rm(anova)
