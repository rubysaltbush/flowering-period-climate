#make list of all environmental variables to test in regressions and their corresponding plot labels
regressions_todo <- list(
  #mean annual temperature
  lmMATCWM = list(
    xdata = sites$MAT,
    ydata = sites$monthsCount_CWM,
    xlabel = "Mean Annual Temperature (ºC)",
    ylabel = "CWM flowering period (months)",
    output_path = "figures/regressions/CWM flowering period vs MAT"
  ),
  lmMATSNC = list(
    xdata = species_all$monthsCount,
    ydata = species_all$MAT_SNC,
    xlabel = "Species length of flowering period (months)",
    ylabel = "SNC mean annual temperature (ºC)",
    output_path = "figures/regressions/MAT SNC vs flowering period length"
  ),
  #mean annual precipitation
  lmMAPCWM = list(
    xdata = sites$MAP,
    ydata = sites$monthsCount_CWM,
    xlabel = "Mean Annual Precipitation (mm)",
    ylabel = "CWM flowering period (months)",
    output_path = "figures/regressions/CWM flowering period vs MAP"
  ),
  lmMAPSNC = list(
    xdata = species_all$monthsCount,
    ydata = species_all$MAP_SNC,
    xlabel = "Species length of flowering period (months)",
    ylabel = "SNC mean annual precipitation (mm)",
    output_path = "figures/regressions/MAP SNC vs flowering period length"
  ),
  #total annual precpitation log10
  lmlog10MAPCWM = list(
    xdata = sites$log10MAP,
    ydata = sites$monthsCount_CWM,
    xlabel = "Log10 of mean annual precipitation (mm)",
    ylabel = "CWM flowering period (months)",
    output_path = "figures/regressions/CWM flowering period vs log10MAP"
  ),
  lmlog10MAPSNC = list(
    xdata = species_all$monthsCount,
    ydata = species_all$log10MAP_SNC,
    xlabel = "Species length of flowering period (months)",
    ylabel = "SNC log10 mean annual precipitation (mm)",
    output_path = "figures/regressions/log10MAP SNC vs flowering period length"
  ),
  #precipitation predictability
  lmprecpredCWM = list(
    xdata = sites$prec_predictability,
    ydata = sites$monthsCount_CWM,
    xlabel = "Precipitation predictability",
    ylabel = "CWM flowering period (months)",
    output_path = "figures/regressions/CWM flowering period vs precipitation predictability"
  ),
  lmprecpredSNC = list(
    xdata = species_all$monthsCount,
    ydata = species_all$precpred_SNC,
    xlabel = "Species length of flowering period (months)",
    ylabel = "SNC precipitation predictability",
    output_path = "figures/regressions/precipitation predictability SNC vs flowering period length"
  ),
  #precipitation predictability - contingency
  lmpreccontCWM = list(
    xdata = sites$prec_cont,
    ydata = sites$monthsCount_CWM,
    xlabel = "Precipitation contingency",
    ylabel = "CWM flowering period (months)",
    output_path = "figures/regressions/CWM flowering period vs precipitation contingency"
  ),
  lmpreccontSNC = list(
    xdata = species_all$monthsCount,
    ydata = species_all$preccont_SNC,
    xlabel = "Species length of flowering period (months)",
    ylabel = "SNC precipitation contingency",
    output_path = "figures/regressions/precipitation contingency SNC vs flowering period length"
  ),
  #precipitation predictability - constancy
  lmprecconsCWM = list(
    xdata = sites$prec_const,
    ydata = sites$monthsCount_CWM,
    xlabel = "Precipitation constancy",
    ylabel = "CWM flowering period (months)",
    output_path = "figures/regressions/CWM flowering period vs precipitation constancy"
  ),
  lmprecconsSNC = list(
    xdata = species_all$monthsCount,
    ydata = species_all$precconst_SNC,
    xlabel = "Species length of flowering period (months)",
    ylabel = "SNC precipitation constancy",
    output_path = "figures/regressions/precipitation constancy SNC vs flowering period length"
  ),
  #temperature predictability
  lmtemppredCWM = list(
    xdata = sites$temp_predictability,
    ydata = sites$monthsCount_CWM,
    xlabel = "Temperature predictability",
    ylabel = "CWM flowering period (months)",
    output_path = "figures/regressions/CWM flowering period vs temperature predictability"
  ),
  lmtemppredSNC = list(
    xdata = species_all$monthsCount,
    ydata = species_all$temppred_SNC,
    xlabel = "Species length of flowering period (months)",
    ylabel = "SNC temperature predictability",
    output_path = "figures/regressions/temperature predictability SNC vs flowering period length"
  ),
  #temperature predictability - contingency
  lmtempcontCWM = list(
    xdata = sites$temp_cont,
    ydata = sites$monthsCount_CWM,
    xlabel = "Temperature contingency",
    ylabel = "CWM flowering period (months)",
    output_path = "figures/regressions/CWM flowering period vs temperature contingency"
  ),
  lmtempcontSNC = list(
    xdata = species_all$monthsCount,
    ydata = species_all$tempcont_SNC,
    xlabel = "Species length of flowering period (months)",
    ylabel = "SNC temperature contingency",
    output_path = "figures/regressions/temperature contingency SNC vs flowering period length"
  ),
  #temperature predictability - constancy
  lmtempconsCWM = list(
    xdata = sites$temp_const,
    ydata = sites$monthsCount_CWM,
    xlabel = "Temperature constancy",
    ylabel = "CWM flowering period (months)",
    output_path = "figures/regressions/CWM flowering period vs temperature constancy"
  ),
  lmtempconsSNC = list(
    xdata = species_all$monthsCount,
    ydata = species_all$tempconst_SNC,
    xlabel = "Species length of flowering period (months)",
    ylabel = "SNC temperature constancy",
    output_path = "figures/regressions/temperature constancy SNC vs flowering period length"
  )
)

regressions <- list()
regresults <- data.frame()
for (regressionName in names(regressions_todo)) {
  todo <- regressions_todo[[regressionName]]
  regressions[[regressionName]] <- do_regression(
    xdata = todo$xdata,
    ydata = todo$ydata,
    xlabel = todo$xlabel,
    ylabel = todo$ylabel,
    output_path = todo$output_path
  )
  regresults <- rbind(regresults, broom::glance(regressions[[regressionName]]))
}
regresults$regressions <- names(regressions_todo)

#add slope regression results
regresults$slope <- rbind(regressions$lmMATCWM$coef[[2]], regressions$lmMATSNC$coef[[2]],
                          regressions$lmMAPCWM$coef[[2]], regressions$lmMAPSNC$coef[[2]],
                          regressions$lmlog10MAPCWM$coef[[2]], regressions$lmlog10MAPSNC$coef[[2]],
                          regressions$lmprecpredCWM$coef[[2]], regressions$lmprecpredSNC$coef[[2]],
                          regressions$lmpreccontCWM$coef[[2]], regressions$lmpreccontSNC$coef[[2]],
                          regressions$lmprecconsCWM$coef[[2]], regressions$lmprecconsSNC$coef[[2]],
                          regressions$lmtemppredCWM$coef[[2]], regressions$lmtemppredSNC$coef[[2]],
                          regressions$lmtempcontCWM$coef[[2]], regressions$lmtempcontSNC$coef[[2]],
                          regressions$lmtempconsCWM$coef[[2]], regressions$lmtempconsSNC$coef[[2]])

#actually confused now about t statistics - there are multiple per lm???

write_csv(regresults, "data_output/CWM_regression_results.csv")

rm(regressionName, regresults, regressions, todo, regressions_todo)
