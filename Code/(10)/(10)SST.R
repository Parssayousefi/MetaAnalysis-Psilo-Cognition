# data extracted from Fig4: Spatial span length
# Assumptions:
# Standard Error to Standard Deviation: We used the provided standard errors to estimate the standard deviations of the differences between conditions.
# Combining Standard Errors: We combined the standard errors from the two conditions to estimate the standard error of the mean differences.
# Normal Distribution: We assumed that the differences between conditions are normally distributed.
# Independence of Measurements: We treated the standard errors as if they were from independent samples, which may not be accurate in a within-subjects design.
#However, using Cohen's d for within subjects seems to be relatively robust. See:  https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/tdunpaired
#Accuracy is approximated by deviding the values by 9 (max value of the span length).)


#  calculate Cohen's d for paired samples
cohens_d <- function(mean_diff, se_diff, N) {
  sd_diff <- se_diff * sqrt(N) # Estimate the SD from the SE
  d <- mean_diff / sd_diff # Calculate Cohen's d
  return(d)
}

# Sample size
N <- 12

# assuming linear scaling of relative variability of the scores 
means_se <- list(
  'T0 Placebo' = list('Mean' = 7.33 / 9, 'SE' = 0.58 / 9),
  'T0 Med' = list('Mean' = 7.51 / 9, 'SE' = 0.79 / 9),
  'T0 High' = list('Mean' = 7.14 / 9, 'SE' = 0.53 / 9),
  'T100 Placebo' = list('Mean' = 7.55 / 9, 'SE' = 0.77 / 9),
  'T100 Med' = list('Mean' = 7.40 / 9, 'SE' = 0.93 / 9),
  'T100 High' = list('Mean' = 6.39 / 9, 'SE' = 0.75 / 9),
  'T360 Placebo' = list('Mean' = 7.81 / 9, 'SE' = 0.76 / 9),
  'T360 Med' = list('Mean' = 7.39 / 9, 'SE' = 0.64 / 9),
  'T360 High' = list('Mean' = 7.88 / 9, 'SE' = 0.56 / 9)
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
