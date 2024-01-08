#Data from Fig3
# Number of participants in within-subject design
total_n <- 40

# Means for placebo and psilocybin
placebo_means <- c(493.38, 517.88, 503.31, 512.58, 429.80, 493.38)
psilocybin_means <- c(470.86, 496.03, 490.73, 499.34, 421.19, 482.78)

# Calculate average mean for each condition
placebo_avg_rt <- mean(placebo_means)
psilocybin_avg_rt <- mean(psilocybin_means)

# Upper confidence limits for placebo and psilocybin
placebo_uppers <- c(516.56, 542.38, 525.17, 536.42, 450.99, 521.19)
psilocybin_uppers <- c(496.03, 523.18, 517.88, 529.14, 447.68, 512.58)

# Calculate standard error for each condition
placebo_se <- (placebo_uppers - placebo_means) / 1.96
psilocybin_se <- (psilocybin_uppers - psilocybin_means) / 1.96

# Calculate standard deviation for each condition
placebo_sd <- placebo_se * sqrt(total_n)
psilocybin_sd <- psilocybin_se * sqrt(total_n)

# Calculate average standard deviation for each condition
placebo_avg_sd <- mean(placebo_sd)
psilocybin_avg_sd <- mean(psilocybin_sd)

print(paste("Placebo average RT:", placebo_avg_rt, "SD:", placebo_avg_sd))
print(paste("Psilocybin average RT:", psilocybin_avg_rt, "SD:", psilocybin_avg_sd))

# Calculate Cohen's d (approximation)
cohens_d <- (psilocybin_avg_rt - placebo_avg_rt) / sqrt((placebo_avg_sd^2 + psilocybin_avg_sd^2) / 2)

print(paste("Cohen's d:", cohens_d))

