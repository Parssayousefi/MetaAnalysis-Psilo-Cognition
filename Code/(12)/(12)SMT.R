#Spatial memory test#
#--- not accounted for within subejcts design#
#--- Response Time (RT) ----#
# n = 19
# Data from Table S3 for Response Time: 

# Table S3: (Mean +/- SEM)
# Response time: 
#   Psilo: 1352.71 (± 142.36)
# Placebo: 1396.93 (± 131.59)

n_rt <- 19

# Given data for response time
mean_rt_placebo <- 1396.93
mean_rt_psilo <- 1352.71
sem_rt_placebo <- 131.59
sem_rt_psilo <- 142.36

# Calculate standard deviations for RT, assuming SEM = SD / sqrt(n)
sd_rt_placebo <- sem_rt_placebo * sqrt(n_rt)
sd_rt_psilo <- sem_rt_psilo * sqrt(n_rt)

# Calculate pooled standard deviation for RT
sd_pooled_rt <- sqrt((sd_rt_placebo^2 + sd_rt_psilo^2) / 2)

# Calculate Cohen's d for response time
cohens_d_rt <- (mean_rt_psilo - mean_rt_placebo) / sd_pooled_rt

print(cohens_d_rt)

#---- Accuracy (Acc) ----#
# Data from Table S3 for Accuracy: 

# Table S3: (Mean +/- SEM)
# total correct (out of 60 pictures):
#   Psilo: 41.15(+/-1.81)
# Placebo: 48.67(+/-1.72)

n_acc <- 19  # Sample size for accuracy

# Total number of trials for accuracy
total_trials_acc <- 60

# Means and SEMs for total correct responses for accuracy
total_correct_acc_psilo <- 41.15
sem_acc_psilo <- 1.81
total_correct_acc_placebo <- 48.67
sem_acc_placebo <- 1.72

# Convert total correct responses to accuracies
accuracy_psilo <- total_correct_acc_psilo / total_trials_acc
accuracy_placebo <- total_correct_acc_placebo / total_trials_acc

# Convert SEMs to SDs for accuracy
sd_acc_psilo <- sem_acc_psilo * sqrt(n_acc)
sd_acc_placebo <- sem_acc_placebo * sqrt(n_acc)

# Calculate the pooled standard deviation for accuracy
sd_pooled_acc <- sqrt((sd_acc_psilo^2 + sd_acc_placebo^2) / 2)

# Calculate Cohen's d for accuracy
cohens_d_acc <- (accuracy_psilo - accuracy_placebo) / sd_pooled_acc

print(cohens_d_acc)
print(cohens_d_rt)



