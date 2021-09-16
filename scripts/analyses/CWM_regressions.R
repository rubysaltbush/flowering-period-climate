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

#create list to store regression output
CWMregressions <- list()
#create data frame for regression results table
CWMregresults <- data.frame()

#loop over each climate variable, perform linear regression and output graphs and results
for (regression_name in names(regressions_todo)) {
  todo <- regressions_todo[[regression_name]]
  CWMregressions[[regression_name]] <- do_regression(
    xdata = todo$xdata,
    ydata = todo$ydata,
    xlabel = todo$xlabel,
    ylabel = todo$ylabel,
    output_path = todo$output_path
  )
  new_row <- broom::glance(CWMregressions[[regression_name]])
  new_row$slope <- CWMregressions[[regression_name]]$coefficients[[2]]
  new_row$regression_name <- regression_name
  CWMregresults <- rbind(CWMregresults, new_row)
}

write_csv(CWMregresults, "data_output/CWM_regression_results.csv")
rm(regression_name, todo, regressions_todo, new_row)

#multiple linear regression of significant climate variables
#first check for collinearity among climate variables
GGally::ggpairs(data = sites, columns = c(22, 25, 19, 16), 
                columnLabels = c("Mean Annual Temperature (ºC)", 
                                 "log10 Mean Annual Precipitation", 
                                 "Temperature predictability", 
                                 "Precipitation predictability")) + theme_pubr()
ggsave("figures/Fig S3 Pairwise correlation plot.png", width = 13, height = 9.2)

cor(sites$MAT, sites$log10MAP, use = "complete.obs")
cor(sites$MAT, sites$temp_predictability, use = "complete.obs")
cor(sites$MAT, sites$prec_predictability, use = "complete.obs")
cor(sites$log10MAP, sites$temp_predictability, use = "complete.obs")
cor(sites$log10MAP, sites$prec_predictability, use = "complete.obs")
cor(sites$temp_predictability, sites$prec_predictability, use = "complete.obs")
# correlations 0.07 - 0.71, below 0.8 so multicollinearity unlikely
# perform multiple regression
multiple_regression <- lm(monthsCount_CWM ~ MAT + log10MAP + temp_predictability + prec_predictability, data = sites)
summary(multiple_regression)
# duoble check for multicollinearity using Variance Inflation Factor (VIF)
car::vif(multiple_regression)
# VIF below 2.2 for all variables, no multicollinearity

##REGRESSION GRAPHS##
# plot of CWM MAT - higher p value for CWM than SNC
ggplot(sites, aes(x = MAT, y = monthsCount_CWM)) +
  geom_point(aes(colour = Biome, shape = Biome)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  theme_pubr(legend = "right") +
  scale_y_continuous(breaks = seq(1, 12, by=1), limits=c(1,12)) +
  colour_scales$biomecol +
  scale_shape_manual(values=c(15,16,17,18,15,17,18)) +
  xlab("Mean Annual Temperature (ºC)") +
  ylab("CWM length of flowering period (months)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(CWMregressions$lmMATCWM)$r.squared, 2),
                     "    Pmax = ", format.pval(summary(CWMregressions$lmMATCWM)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/Fig 3 CWM flowering period vs MAT with biomes.png", width = 10, height = 5)

# plot of CWM log10MAP - log10 transformation much better than raw values
ggplot(sites, aes(x = log10MAP, y = monthsCount_CWM)) +
  geom_point(aes(colour = Biome, shape = Biome)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  theme_pubr(legend = "right") +
  scale_y_continuous(breaks = seq(1, 12, by=1), limits=c(1,12)) +
   colour_scales$biomecol +
  scale_shape_manual(values=c(15,16,17,18,15,17,18)) +
  xlab("Log10 Mean Annual Precipitation (mm)") +
  ylab("CWM length of flowering period (months)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(CWMregressions$lmlog10MAPCWM)$r.squared, 2),
                     "    Pmax = ", format.pval(summary(CWMregressions$lmlog10MAPSNC)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/Fig 3 CWM flowering period vs log10MAP with biomes.png", width = 10, height = 5)

#pretty plot of CWM precipitation predictability
ggplot(sites, aes(x = prec_predictability, y = monthsCount_CWM)) +
  geom_point(aes(colour = Biome, shape = Biome)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  theme_pubr(legend = "right") +
  scale_y_continuous(breaks = seq(1, 12, by=1), limits=c(1,12)) +
  colour_scales$biomecol +
  scale_shape_manual(values=c(15,16,17,18,15,17,18)) +
  xlab("Precipitation predictability") +
  ylab("CWM length of flowering period (months)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(CWMregressions$lmprecpredCWM)$r.squared, 2),
                     "    Pmax = ", format.pval(summary(CWMregressions$lmprecpredSNC)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/Fig 3 CWM flowering period vs precipitation predictability with biomes.png", width = 10, height = 5)

#pretty plot of CWM temperature predictability
ggplot(sites, aes(x = temp_predictability, y = monthsCount_CWM)) +
  geom_point(aes(colour = Biome, shape = Biome)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  theme_pubr(legend = "right") +
  scale_y_continuous(breaks = seq(1, 12, by=1), limits=c(1,12)) +
  colour_scales$biomecol +
  scale_shape_manual(values=c(15,16,17,18,15,17,18)) +
  xlab("Temperature predictability") +
  ylab("CWM length of flowering period (months)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(CWMregressions$lmtemppredCWM)$r.squared, 2),
                     "    Pmax = ", format.pval(summary(CWMregressions$lmtemppredSNC)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/Fig 3 CWM flowering period vs temperature predictability with biomes.png", width = 10, height = 5)

#extract legend from below graph
ggplot(sites, aes(x = MAT, y = monthsCount_CWM)) +
  geom_point(aes(colour = Biome, shape = Biome)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  theme_pubr(legend = "bottom") +
  scale_y_continuous(breaks = seq(1, 12, by=1), limits=c(1,12)) +
  colour_scales$biomecol +
  scale_shape_manual(values=c(15,16,17,18,15,17,18)) +
  xlab("Mean Annual Temperature (ºC)") +
  ylab("CWM length of flowering period (months)") +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14)) +
  labs(title = paste("R² = ", signif(summary(CWMregressions$lmMATCWM)$r.squared, 2),
                     "    Pmax = ", format.pval(summary(CWMregressions$lmMATCWM)$coef[2,4], eps = .001, digits = 2)))
ggsave("figures/Fig 3 biome legend extract.png", width = 11.1, height = 6)

rm(CWMregressions, CWMregresults)
