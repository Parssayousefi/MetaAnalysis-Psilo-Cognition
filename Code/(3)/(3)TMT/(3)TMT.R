# Figure 3F: Part A (median, 1st quartile, 3rd quartile)
# Psilo:   19.83, 17.85, 24.79
# Placebo: 17.52,  16.53, 21.16




n_psilo <- 34  
n_placebo <- 34

median_psilo <- 19.83
iqr_psilo <- 24.79 - 17.85

median_placebo <- 17.52
iqr_placebo <- 21.16 - 16.53

# standard deviations
sd_psilo <- iqr_psilo / 1.35
sd_placebo <- iqr_placebo / 1.35


# pooled standard deviation
pooled_sd <- sqrt(((n_psilo - 1) * sd_psilo^2 + (n_placebo - 1) * sd_placebo^2) / (n_psilo + n_placebo - 2))

# Calculate Cohen's d using medians as a proxy for means
cohens_d <- (median_psilo - median_placebo) / pooled_sd

# Output Cohen's d
cohens_d
