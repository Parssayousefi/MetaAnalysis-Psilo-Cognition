

# data extracted from Figure 4&5:
#RT

# means and se for RT
mean_placebo_rt <- 723.46
mean_psilo_rt <- 992.72
sem_placebo_rt <- 27
sem_psilo_rt <- 88.84

#  pooled sd RT
n <- 16  # number of participants
sd_pooled_rt <- sqrt((sem_placebo_rt^2 + sem_psilo_rt^2) / 2) * sqrt(n)

# Cohen's d for RT
d_rt <- (mean_psilo_rt - mean_placebo_rt) / sd_pooled_rt


#ACC

#  number of errors (out of 48 trials)
errors_placebo <- 1.67
errors_psilo <- 3.97
trials <- 48

# nr of errors to accuracy 
accuracy_placebo <- (trials - errors_placebo) / trials * 100
accuracy_psilo <- (trials - errors_psilo) / trials * 100

# SE for accuracy converted from errors
sem_placebo_acc <- 0.4 / trials * 100
sem_psilo_acc <- 0.87 / trials * 100

# pooled sd acc   
sd_pooled_acc <- sqrt((sem_placebo_acc^2 + sem_psilo_acc^2) / 2) * sqrt(n)

# Calculate Cohen's d for accuracy
d_acc <- (accuracy_psilo - accuracy_placebo) / sd_pooled_acc

# Output 
d_rt
d_acc
