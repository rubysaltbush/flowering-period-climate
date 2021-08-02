##maps##
#map flowering time onto biome map, with matching colours to violin plots
#first read in simplified biome shapefile using sf
biome_bg <- sf::st_read("data_input/ibra7_biomes_background.shp")
#convert sites to an sf spatial object and assign CRS of WGS84
sites <- sites %>%
  dplyr::mutate(long = longitude, lat = latitude)
sites <- sf::st_as_sf(sites, coords = c("long","lat"))
st_crs(sites) <- 4326

#plot AusPlots sites included in analysis with biomes in background
ggplot() +
  geom_sf(data = biome_bg, aes(fill = biome), lwd = 0.02) + #map biomes
  colour_scales$biomeall + #colour biome according to predefined colour scale
  geom_sf(data = sites, shape = 1) +
  theme_void()
ggsave("figures/Fig 1 AusPlots by biome map.png", width = 11, height = 6)

#plot AusPlots sites coloured by flowering period length CWM with blank background
ggplot() +
  geom_sf(data = biome_bg, lwd = 0) + #map biomes
  geom_sf(data = arrange(sites, desc(monthsCount_CWM)), 
          aes(size = monthsCount_CWM, colour = monthsCount_CWM)) + #map flowering periods
  scale_size_continuous(name = "CWM length of flowering period (months)", breaks = seq(1, 12)) +
  scale_colour_viridis_b(name = "CWM length of flowering period (months)", breaks = seq(1, 12)) +
  theme_void() +
  guides(colour = guide_legend())
ggsave("figures/Fig S4 AusPlots sites by CWM flowering period.png", width = 11, height = 6)

rm(biome_bg)
