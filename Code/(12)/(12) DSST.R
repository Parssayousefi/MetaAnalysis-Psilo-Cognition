
# n =22
#---DSST ----#
#Data from Table S3: 

#ACCURACY (% correct)
# Placebo (mean +/- SEM): 98.74 ± 0.28
# Psilo(mean +/- SEM): 98.81 ± 0.27



# Given data
mean_placebo <- 98.74
mean_psilo <- 98.81
sem_placebo <- 0.28
sem_psilo <- 0.27
n <- 19

# pooled SEM
pooled_sem <- sqrt((sem_placebo^2 + sem_psilo^2) / 2)

# Cohen's d
cohens_d <- (mean_psilo - mean_placebo) / pooled_sem
cohens_d
