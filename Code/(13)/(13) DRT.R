# Data from Fig2: Ketanserin group (mean +/- SEM)

# Define the means and standard errors
mean_pla_pla <- 0.71
se_pla_pla <- 0.07
mean_pla_psi <- 1.10
se_pla_psi <- 0.12

# Sample size
n <- 5

# Convert standard errors to standard deviations
sd_pla_pla <- se_pla_pla * sqrt(n)
sd_pla_psi <- se_pla_psi * sqrt(n)

# Calculate pooled standard deviation
pooled_sd <- sqrt((sd_pla_pla^2 + sd_pla_psi^2) / 2)

# Calculate Cohen's d
cohens_d <- round((mean_pla_psi - mean_pla_pla) / pooled_sd,2)

# Print Cohen's d
print(cohens_d)
