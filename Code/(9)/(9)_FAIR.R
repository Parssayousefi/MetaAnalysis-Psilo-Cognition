
# LowDose
# Pre, 575.95(547.47-604.43)
# Post 496.84(468.35-522.15)

n = 16
# Define the means and standard errors for two groups
mean_pre_low <- 575.95  
mean_post_low <- 496.84 

se_pre_low <- (604.43 - 547.47) / 2  # average the two standard errors
se_post_low <- (522.15 - 468.35) / 2  # average the two standard errors


# Calculate standard deviations from standard errors
sd_pre_low <- se_pre_low * sqrt(n)
sd_post_low <- se_post_low * sqrt(n)

# Compute Cohen's d
# Calculate pooled standard deviation
s_pooled_low <- sqrt(((n-1)*sd_pre_low^2 + (n-1)*sd_post_low^2) / (2*n-2))

# Calculate Cohen's d
cohen_d_low <- (mean_post_low - mean_pre_low) / s_pooled_low

print(cohen_d_low)

#---Mid dose---#
# Define the means and standard errors for two groups
mean_pre_mid <- 534.81  
mean_post_mid <- 449.37 

se_pre_mid <- (569.62 - 503.16) / 2  # average the two standard errors
se_post_mid <- (481.01 - 417.72) / 2  # average the two standard errors


# Calculate standard deviations from standard errors
sd_pre_mid <- se_pre_mid * sqrt(n)
sd_post_mid <- se_post_mid * sqrt(n)

# Compute Cohen's d
# Calculate pooled standard deviation
s_pooled_mid <- sqrt(((n-1)*sd_pre_mid^2 + (n-1)*sd_post_mid^2) / (2*n-2))

# Calculate Cohen's d
cohen_d_mid <- (mean_post_mid - mean_pre_mid) / s_pooled_mid

print(cohen_d_mid)


#---High dose---#
# Define the means and standard errors for two groups
mean_pre_high <- 613.92  
mean_post_high <- 455.70 

se_pre_high <- (579.11-655.06) / 2  # average the two standard errors
se_post_high <- (420.89-490.51) / 2  # average the two standard errors


# Calculate standard deviations from standard errors
sd_pre_high <- se_pre_high * sqrt(n)
sd_post_high <- se_post_high * sqrt(n)

# Compute Cohen's d
# Calculate pooled standard deviation
s_pooled_high <- sqrt(((n-1)*sd_pre_high^2 + (n-1)*sd_post_high^2) / (2*n-2))

# Calculate Cohen's d
cohen_d_high <- (mean_post_high - mean_pre_high) / s_pooled_high

print(cohen_d_high)