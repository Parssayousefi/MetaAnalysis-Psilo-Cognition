# data extracted from Fig4
# Assumptions:
# Standard Error to Standard Deviation: We used the provided standard errors to estimate the standard deviations of the differences between conditions.
# Combining Standard Errors: We combined the standard errors from the two conditions to estimate the standard error of the mean differences.
# Normal Distribution: We assumed that the differences between conditions are normally distributed.
# Independence of Measurements: We treated the standard errors as if they were from independent samples, which may not be accurate in a within-subjects design.



#  calculate Cohen's d for paired samples
cohens_d <- function(mean_diff, se_diff, N) {
  sd_diff <- se_diff * sqrt(N) # Estimate the SD from the SE
  d <- mean_diff / sd_diff # Calculate Cohen's d
  return(d)
}

# Sample size
N <- 12

# Means and standard errors
means_se <- list(
  'T0 Placebo' = list('Mean' = 7.33, 'SE' = 0.58),
  'T0 Med' = list('Mean' = 7.51, 'SE' = 0.79),
  'T0 High' = list('Mean' = 7.14, 'SE' = 0.53),
  'T100 Placebo' = list('Mean' = 7.55, 'SE' = 0.77),
  'T100 Med' = list('Mean' = 7.40, 'SE' = 0.93),
  'T100 High' = list('Mean' = 6.39, 'SE' = 0.75),
  'T360 Placebo' = list('Mean' = 7.81, 'SE' = 0.76),
  'T360 Med' = list('Mean' = 7.39, 'SE' = 0.64),
  'T360 High' = list('Mean' = 7.88, 'SE' = 0.56)
)

# Calculate mean differences and their SE
effect_sizes <- list()
for (timepoint in c('T0', 'T100', 'T360')) {
  mean_diff_med <- means_se[[paste(timepoint, 'Med', sep=' ')]]$Mean - means_se[[paste(timepoint, 'Placebo', sep=' ')]]$Mean
  se_diff_med <- sqrt(means_se[[paste(timepoint, 'Med', sep=' ')]]$SE^2 + means_se[[paste(timepoint, 'Placebo', sep=' ')]]$SE^2)
  effect_sizes[[paste(timepoint, 'Med', sep=' ')]] <- cohens_d(mean_diff_med, se_diff_med, N)
  
  mean_diff_high <- means_se[[paste(timepoint, 'High', sep=' ')]]$Mean - means_se[[paste(timepoint, 'Placebo', sep=' ')]]$Mean
  se_diff_high <- sqrt(means_se[[paste(timepoint, 'High', sep=' ')]]$SE^2 + means_se[[paste(timepoint, 'Placebo', sep=' ')]]$SE^2)
  effect_sizes[[paste(timepoint, 'High', sep=' ')]] <- cohens_d(mean_diff_high, se_diff_high, N)
}


effect_sizes
