# Figure 3F: Part B (median, 1st quartile, 3rd quartile)
# Psilo:   42.13, 36.71, 48.31
# Placebo: 35.17,  32.08, 45.22



#---RT---#
n_psilo <- 34  
n_placebo <- 34

median_psilo <- 42.13
iqr_psilo <- 48.31 - 36.71

median_placebo <- 35.17
iqr_placebo <- 45.22 - 32.08

# standard deviations
sd_psilo <- iqr_psilo / 1.35
sd_placebo <- iqr_placebo / 1.35


# pooled standard deviation
pooled_sd <- sqrt(((n_psilo - 1) * sd_psilo^2 + (n_placebo - 1) * sd_placebo^2) / (n_psilo + n_placebo - 2))

# Calculate Cohen's d using medians as a proxy for means
cohens_d <- (median_psilo - median_placebo) / pooled_sd

# Output Cohen's d
cohens_d

