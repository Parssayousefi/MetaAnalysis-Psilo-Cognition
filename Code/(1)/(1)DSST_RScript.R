# Data for Accuracy is extracted from Figure S2
# Data for Number of Attempted Trials is extracted from Figure S1

# --------------------- Accuracy (From Figure S2) ---------------------

# Post-treatment median and SD for accuracy
# Placebo
median_post_placebo_acc <- 98.2703  
sd_post_placebo_acc <- (98.6486 - 96.9730) / 1.35  

# 10mg Psilocybin
median_post_10mg_acc <- 98.5946  
sd_post_10mg_acc <- (100 - 96.8649) / 1.35  

# 20mg Psilocybin
median_post_20mg_acc <- 98.4324  
sd_post_20mg_acc <- (100.00 - 96.3243) / 1.35  

# 30mg Psilocybin
median_post_30mg_acc <- 100.00  
sd_post_30mg_acc <- (99.87 - 88.99) / 1.35  

# median differences for accuracy
median_diff_10mg_vs_placebo_acc <- median_post_10mg_acc - median_post_placebo_acc
median_diff_20mg_vs_placebo_acc <- median_post_20mg_acc - median_post_placebo_acc
median_diff_30mg_vs_placebo_acc <- median_post_30mg_acc - median_post_placebo_acc

# Pooled SD for accuracy
pooled_sd_10mg_vs_placebo_acc <- sqrt((sd_post_placebo_acc^2 + sd_post_10mg_acc^2) / 2)
pooled_sd_20mg_vs_placebo_acc <- sqrt((sd_post_placebo_acc^2 + sd_post_20mg_acc^2) / 2)
pooled_sd_30mg_vs_placebo_acc <- sqrt((sd_post_placebo_acc^2 + sd_post_30mg_acc^2) / 2)

# Cohen's d for accuracy
cohens_d_10mg_vs_placebo_acc <- median_diff_10mg_vs_placebo_acc / pooled_sd_10mg_vs_placebo_acc
cohens_d_20mg_vs_placebo_acc <- median_diff_20mg_vs_placebo_acc / pooled_sd_20mg_vs_placebo_acc
cohens_d_30mg_vs_placebo_acc <- median_diff_30mg_vs_placebo_acc / pooled_sd_30mg_vs_placebo_acc

# Print Cohen's d for accuracy
print(paste("Cohen's d for 10mg vs placebo (accuracy): ", cohens_d_10mg_vs_placebo_acc))
print(paste("Cohen's d for 20mg vs placebo (accuracy): ", cohens_d_20mg_vs_placebo_acc))
print(paste("Cohen's d for 30mg vs placebo (accuracy): ", cohens_d_30mg_vs_placebo_acc))

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
