# species regressions
# define regressions to do
regressions_todo <- list(
  # eoo
  lmeoo_months = list(
    xdata = species_all$monthsCount,
    ydata = species_all$eoo_aus,
    xlabel = "Length of flowering period (months)",
    ylabel = "Species extent of occurrence (EOO)",
    output_path = "figures/regressions/flowering period vs eoo"
  )
)

# prep empty list for results and df for result table
speciesregs <- data.frame()
speciesregressions <- list()
# loop over regressions_todo, perform linear regression and output graphs
for (regression_name in names(regressions_todo)) {
  todo <- regressions_todo[[regression_name]]
  speciesregressions[[regression_name]] <- do_regression(
    xdata = todo$xdata,
    ydata = todo$ydata,
    xlabel = todo$xlabel,
    ylabel = todo$ylabel,
    output_path = todo$output_path
  )
  new_row <- broom::glance(speciesregressions[[regression_name]])
  new_row$slope <- speciesregressions[[regression_name]]$coefficients[[2]]
  new_row$regression_name <- regression_name
  speciesregs <- rbind(speciesregs, new_row)
}

write_csv(speciesregs, "data_output/species_regression_results.csv")

# add extra column to species_all to graph EOO with smaller numbers
species_all <- species_all %>%
  dplyr::mutate(eoo_aus_mill = eoo_aus/1000000)

# graph of monthsCount versus extent of occurrence (EOO)
ggplot(species_all, aes(y = eoo_aus_mill, x = monthsCount)) +
  geom_point(aes(alpha = 0.5)) +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  theme_pubr(legend = "none") +
  scale_x_continuous(breaks = seq(1, 12, by=1), limits=c(1,12)) +
  ylab("Extent of Occurrence (million km²)") +
  xlab("Species length of flowering period (months)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(speciesregressions$lmeoo_months)$r.squared, 2),
                     "    P = ", format.pval(summary(speciesregressions$lmeoo_months)$coef[2,4], eps = .001, digits = 2)))

ggsave("figures/Fig 5 species flowering period vs eoo.png", width = 5.6, height = 3.6)

rm(regression_name, regressions_todo, speciesregressions, speciesregs, new_row, todo)
