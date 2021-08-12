# function to perform linear regressions on list of input data, export basic
# scatter plots (with line if significant) of data and qqplots of residuals

do_regression <- function(xdata, ydata, xlabel, ylabel, output_path) {
  lm_data <- lm(ydata ~ xdata)
  
  # plot residuals to check for normality - have to inspect visually
  
  png(paste(output_path, "residual histogram"), width = 7, height = 7, units = 'in', res = 300)
  hist(lm_data$residuals, xlab = paste("Residuals of", xlabel, "&", ylabel))
  
  dev.off()
  
  png(paste(output_path, "residual qqplot"), width = 7, height = 7, units = 'in', res = 300)
  qqnorm(lm_data$residuals, main = paste("QQ Plot of", xlabel, "&", ylabel))
  qqline(lm_data$residuals)
  
  dev.off()
  
  # assuming residuals okay, plot data and add regression line to plot only if
  # regression significant
  
  png(output_path, width = 7, height = 7, units = 'in', res = 300)
  
  plot(ydata ~ xdata,
       cex = 0.8,
       bty = "l",
       ylab = ylabel,
       xlab = xlabel,
       main = paste("R² = ", signif(summary(lm_data)$r.squared, 3),
                    "   P = ", format.pval(summary(lm_data)$coef[2,4], eps = .001, digits = 2)),
       cex.main = 1
  )
  
  if (summary(lm_data)$coefficients[2,4] < 0.05) {
    abline(lm_data)
  }
  
  dev.off()
  
  lm_data
}
