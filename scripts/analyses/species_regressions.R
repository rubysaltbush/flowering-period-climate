#species regressions
#define regressions to do
regressions_todo <- list(
  #eoo
  lmeoo_months = list(
    xdata = species_all$monthsCount,
    ydata = species_all$eoo_aus,
    xlabel = "Length of flowering period (months)",
    ylabel = "Species extent of occurrence (EOO)",
    output_path = "figures/regressions/flowering period vs eoo"
  )
)

speciesregs <- data.frame()
speciesregressions <- list()
for (regressionName in names(regressions_todo)) {
  todo <- regressions_todo[[regressionName]]
  speciesregressions[[regressionName]] <- do_regression(
    xdata = todo$xdata,
    ydata = todo$ydata,
    xlabel = todo$xlabel,
    ylabel = todo$ylabel,
    output_path = todo$output_path
  )
  speciesregs <- rbind(speciesregs, broom::glance(speciesregressions[[regressionName]]))
}
speciesregs$regressions <- names(regressions_todo)
write_csv(speciesregs, "data_output/species_regression_results.csv")

rm(regressionName, regressions_todo, speciesregressions, speciesregs)