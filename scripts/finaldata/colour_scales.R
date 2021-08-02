##colour scales##
#assign fixed colour scale to biomes
colour_scales <- list()
my_colours <- c("#fb8072", "#bebada", "#fccde5", "#b3de69", "#fdb462", "#8dd3c7", "#80b1d3")
names(my_colours) <- c("Deserts and Xeric Shrublands",
                       "Mediterranean Forests, Woodlands and Scrub",
                       "Montane Grasslands and Shrublands",
                       "Temperate Broadleaf and Mixed Forest",
                       "Temperate Grasslands, Savannas and Shrublands",
                       "Tropical and Subtropical Grasslands, Savannas and Shrublands",
                       "Tropical and Subtropical Moist Broadleaf Forests")
colour_scales$biomeall <- scale_fill_manual(name = "Biome", values = my_colours)
colour_scales$biomecolall <- scale_colour_manual(name = "Biome", values = my_colours)
#drop unsampled biome so it doesn't show up on graphs
my_colours <- my_colours[1:6]
colour_scales$biome <- scale_fill_manual(name = "Biome", values = my_colours)
colour_scales$biomecol <- scale_colour_manual(name = "Biome", values = my_colours)

#assign fixed colour scale to families
my_colours <- c("#B76890", "#68A353", "#8282B3", "#E5874D", "#C25F5C",
                "#AD9A34", "#0EA287", "#ffecb3", "#000000")
names(my_colours) <- c("Fabaceae", "Poaceae", "Myrtaceae", "Asteraceae", "Proteaceae",
                         "Chenopodiaceae", "Cyperaceae", "Other families", "All families")
colour_scales$famall <- scale_fill_manual(name = "Family", values = my_colours)
colour_scales$famcolall <- scale_colour_manual(name = "Family", values = my_colours)
#drop all families so it doesn't show up on graphs where not relevant
my_colours <- my_colours[1:8]
colour_scales$fam <- scale_fill_manual(name = "Family", values = my_colours)
colour_scales$famcol <- scale_colour_manual(name = "Family", values = my_colours)
rm(my_colours)
