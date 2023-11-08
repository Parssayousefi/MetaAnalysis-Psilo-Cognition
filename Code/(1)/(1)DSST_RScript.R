# Data for Accuracy is extracted from Figure S2
# Data for Number of Attempted Trials is extracted from Figure S1

# --------------------- Accuracy (From Figure S2) ---------------------
# Function to calculate Cohen's d
calculate_cohens_d <- function(median_diff, sd1, sd2) {
  pooled_sd <- sqrt((sd1^2 + sd2^2) / 2)
  cohens_d <- median_diff / pooled_sd
  return(cohens_d)
}

# Function to estimate SD from IQR
estimate_sd_from_iqr <- function(iqr) {
  sd <- iqr / 1.35
  return(sd)
}

# Function to print Cohen's d
print_cohens_d <- function(dose, time, cohens_d) {
  print(paste("Cohen's d for", dose, "vs placebo (accuracy at", time, "):", cohens_d))
}

# Data for placebo and psilocybin groups (median and IQR)
data <- list(
  "2h" = list(
    "placebo" = list(median = 0.9827, iqr = c(0.9697, 0.9864)),
    "10mg" = list(median = 0.9859, iqr = c(0.9686, 1.00)),
    "20mg" = list(median = 0.9843, iqr = c(0.9632, 1.00)),
    "30mg" = list(median = 1.0000, iqr = c(0.89, 0.9987))
  ),
  "4h" = list(
    "placebo" = list(median = 0.98, iqr = c(0.97, 1.00)),
    "10mg" = list(median = 0.99, iqr = c(0.98, 1.00)),
    "20mg" = list(median = 0.9839, iqr = c(0.9634, 1.00)),
    "30mg" = list(median = 1.00, iqr = c(0.98, 1.00))
  ),
  "6h" = list(
    "placebo" = list(median = 0.98, iqr = c(0.98, 1.00)),
    "10mg" = list(median = 0.99, iqr = c(0.97, 1.00)),
    "20mg" = list(median = 0.9833, iqr = c(0.9683, 1.00)),
    "30mg" = list(median = 0.99, iqr = c(0.98, 1.00))
  )
)

# Calculate and print Cohen's d for each dose and time point
results <- list()
for (time in names(data)) {
  placebo_data <- data[[time]][["placebo"]]
  placebo_sd <- estimate_sd_from_iqr(diff(unlist(placebo_data$iqr)))
  
  for (dose in c("10mg", "20mg", "30mg")) {
    psilocybin_data <- data[[time]][[dose]]
    psilocybin_sd <- estimate_sd_from_iqr(diff(unlist(psilocybin_data$iqr)))
    median_diff <- psilocybin_data$median - placebo_data$median
    
    cohens_d <- calculate_cohens_d(median_diff, placebo_sd, psilocybin_sd)
    results[[paste(dose, time, sep = "_")]] <- cohens_d
    print_cohens_d(dose, time, cohens_d)
  }
}

# The results are stored in a list for further processing if needed
results


# --------------------- Number of Attempted Trials (From Figure S1) ---------------------

# Post-treatment median and SD for number of attempted trials
# Placebo
median_post_placebo_attempts <- 68.90  
sd_post_placebo_attempts <- (75.30 - 62.20) / 1.35  

# 10mg Psilocybin
median_post_10mg_attempts <- 59.76  
sd_post_10mg_attempts <- (72.87 - 50.61) / 1.35  

# 20mg Psilocybin
median_post_20mg_attempts <- 49.39  
sd_post_20mg_attempts <- (59.15 - 37.50) / 1.35  

# 30mg Psilocybin
median_post_30mg_attempts <- 40.85  
sd_post_30mg_attempts <- (52.13 - 33.23) / 1.35  

# median differences for number of attempted trials
median_diff_10mg_vs_placebo_attempts <- median_post_10mg_attempts - median_post_placebo_attempts
median_diff_20mg_vs_placebo_attempts <- median_post_20mg_attempts - median_post_placebo_attempts
median_diff_30mg_vs_placebo_attempts <- median_post_30mg_attempts - median_post_placebo_attempts

# Pooled SD for number of attempted trials
pooled_sd_10mg_vs_placebo_attempts <- sqrt((sd_post_placebo_attempts^2 + sd_post_10mg_attempts^2) / 2)
pooled_sd_20mg_vs_placebo_attempts <- sqrt((sd_post_placebo_attempts^2 + sd_post_20mg_attempts^2) / 2)
pooled_sd_30mg_vs_placebo_attempts <- sqrt((sd_post_placebo_attempts^2 + sd_post_30mg_attempts^2) / 2)

# Cohen's d for number of attempted trials
cohens_d_10mg_vs_placebo_attempts <- median_diff_10mg_vs_placebo_attempts / pooled_sd_10mg_vs_placebo_attempts
cohens_d_20mg_vs_placebo_attempts <- median_diff_20mg_vs_placebo_attempts / pooled_sd_20mg_vs_placebo_attempts
cohens_d_30mg_vs_placebo_attempts <- median_diff_30mg_vs_placebo_attempts / pooled_sd_30mg_vs_placebo_attempts

# Print Cohen's d for number of attempted trials
print(paste("Cohen's d for 10mg vs placebo (attempts): ", cohens_d_10mg_vs_placebo_attempts))
print(paste("Cohen's d for 20mg vs placebo (attempts): ", cohens_d_20mg_vs_placebo_attempts))
print(paste("Cohen's d for 30mg vs placebo (attempts): ", cohens_d_30mg_vs_placebo_attempts))
