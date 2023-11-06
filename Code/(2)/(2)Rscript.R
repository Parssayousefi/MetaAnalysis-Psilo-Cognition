



#----Spatial span test---#
#Data from Figure4
#-----SPAN LENGTH------#

n <- 8

# Placebo:
mean_post_placebo_span <- 7.80
sem_post_placebo_span <- (8.36 - 7.28) / (2 * sqrt(n))
sd_post_placebo_span <- sem_post_placebo_span * sqrt(n)

# Psilocybin:
mean_post_psilocybin_span <- 6.79
sem_post_psilocybin_span <- (7.25 - 6.20) / (2 * sqrt(n))
sd_post_psilocybin_span <- sem_post_psilocybin_span * sqrt(n)

# Calculate mean differences
mean_diff_span <- mean_post_psilocybin_span - mean_post_placebo_span

# Calculate pooled standard deviation
pooled_sd_span <- sqrt((sd_post_placebo_span^2 + sd_post_psilocybin_span^2) / 2)

# Calculate Cohen's d
cohens_d_span <- mean_diff_span / pooled_sd_span

# Print Cohen's d for span length
print(cohens_d_span)


#-----ACCURACY------#
# Total number of trials
total_trials <- 84
# Convert the number of incorrect responses to accuracy
mean_acc_post_placebo <- (total_trials - 10.95) / total_trials * 100
mean_acc_post_psilocybin <- (total_trials - 12.46) / total_trials * 100

# Standard Deviation for Accuracy
# (Assuming that the standard error doesn't change significantly when converting from incorrect responses to accuracy)
sd_acc_post_placebo <- (14.69 - 7.41) / (2 * sqrt(n)) * sqrt(n)
sd_acc_post_psilocybin <- (14.43 - 10.75) / (2 * sqrt(n)) * sqrt(n)

# Calculate mean differences in accuracy
mean_diff_ACC <- mean_acc_post_psilocybin - mean_acc_post_placebo

# Calculate pooled standard deviation for accuracy
pooled_sd_ACC <- sqrt((sd_acc_post_placebo^2 + sd_acc_post_psilocybin^2) / 2)

# Calculate Cohen's d for accuracy
cohens_d_ACC <- mean_diff_ACC / pooled_sd_ACC

# Print the result for accuracy
print(cohens_d_ACC)



#----Attentional object racking---#
#Data from Figure3B
#Accuracy calcualted as nr of dots tracked/8(max nr of dots)

total_dots <- 8

# means are  represented as accuracy percentages
accuracy_placebo <- (2.57 / total_dots) * 100
sem_range_placebo <- ((3.13 - 2.87) / 2) / total_dots * 100  
n_placebo <- 8

accuracy_psilocybin <- (1.62 / total_dots) * 100
sem_range_psilocybin <- ((1.96 - 1.28) / 2) / total_dots * 100  
n_psilocybin <- 8

#  Standard Deviation from SEM (
sd_placebo <- sem_range_placebo * sqrt(n_placebo)
sd_psilocybin <- sem_range_psilocybin * sqrt(n_psilocybin)

# pooled standard deviation
s_pooled <- sqrt(((n_placebo - 1) * sd_placebo^2 + (n_psilocybin - 1) * sd_psilocybin^2) / (n_placebo + n_psilocybin - 2))

# Cohen's d
d <- (accuracy_psilocybin - accuracy_placebo) / s_pooled


print(paste("Cohen's d:", d))


