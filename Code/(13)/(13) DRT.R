# Figure 2:  mean(+/- se)
# Ketanserin:
# Pla-pla: 0.71 (0.06)
# Pla-psil: 1.08 (0.14)

# Risperidone:
# Pla-pla: 0.79 (0.07)
# Pla-psil: 1.15 (0.07)

# Haloperidol:
# Pla-pla: 0.75 (0.04)
# Pla-psil: 1.23 (0.17)

mean_pla_pla <- c(0.71, 0.79, 0.75)
se_pla_pla <- c(0.06, 0.07, 0.04)
mean_pla_psil <- c(1.08, 1.15, 1.23)
se_pla_psil <- c(0.14, 0.07, 0.17)

# Sample size
n <- 5

# Convert standard errors to standard deviations
sd_pla_pla <- se_pla_pla * sqrt(n)
sd_pla_psil <- se_pla_psil * sqrt(n)

# Calculate pooled standard deviation
pooled_sd <- sqrt((sum(sd_pla_pla^2) + sum(sd_pla_psil^2)) / (length(sd_pla_pla) + length(sd_pla_psil)))

# Calculate Cohen's d
cohens_d <- round((mean(mean_pla_psil) - mean(mean_pla_pla)) / pooled_sd, 2)

# Print Cohen's d
print(cohens_d)