# first read in biomes shapefile
biome_sf <- sf::st_read("data_input/ibra7_biomes.shp")
# make spatially valid
biome_sf <- sf::st_make_valid(biome_sf)
# calculate area of biomes
biome_area <- as.data.frame(biome_sf$biome)
biome_area$area <- sf::st_area(biome_sf)
biome_area$area_m2 <- as.numeric(biome_area$area)
biome_area <- dplyr::select(biome_area, biome = `biome_sf$biome`, area_m2)
#get number of analysis ausplots per biome
biomesites <- aggregate(sites$site_unique, list(sites$biome), length)
colnames(biomesites) <- c("biome", "no_sites")
biome_area <- biome_area %>%
  dplyr::left_join(biomesites, by = "biome")
biome_area[is.na(biome_area)] <- 0
rm(biomesites, biome_sf)

#correlate number of plots vs area of biome
#first make sure both columns numeric
biome_area$no_sites <- as.numeric(biome_area$no_sites)
#convert area to kilometres squared
biome_area$area_km2 <- biome_area$area_m2/1000000
#do regression
biomereg <- lm(biome_area$area_km2 ~ biome_area$no_sites)
summary(biomereg)
#plot biome area vs number of sites in each biome
ggplot(biome_area, aes(x = area_km2, y = no_sites)) +
  geom_point(size = 3, aes(colour = biome, shape = biome)) +
  ylab("Number of AusPlots") +
  xlab(expression("Biome area (km"^2*")")) +
  theme_pubr(legend = "right") +
  colour_scales$biomecolall +
  scale_shape_manual(values=c(15,16,17,18,15,17,18)) +
  geom_smooth(method = lm, colour = "black", se = FALSE) +
  theme(plot.title = element_text(size = 8)) +
  labs(title = paste("R² = ", signif(summary(biomereg)$r.squared, 2),
                     "    P = ", format.pval(summary(biomereg)$coef[2,4], 
                                             eps = .001, digits = 2)))
ggsave("figures/Fig S2 ausplots sampling by biome area.png", width = 9, height = 4)
rm(biome_area, biomereg)
