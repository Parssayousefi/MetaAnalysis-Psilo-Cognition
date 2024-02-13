
# n =22
#---DSST ----#
#--- not accounted for within subejcts design#
#Data from Table S3: 

#ACCURACY (% correct)
# Placebo (mean +/- SEM): 98.74 ± 0.28
# Psilo(mean +/- SEM): 98.81 ± 0.27



# Given data
mean_placebo = 98.74
mean_psilo = 98.81
sem_placebo = 0.28
sem_psilo = 0.27
n = 19

# Convert SEM to SD
sd_placebo = sem_placebo * (n ** 0.5)
sd_psilo = sem_psilo * (n ** 0.5)

# Pooled standard deviation (assuming equal sample sizes for simplicity)
pooled_sd = ((sd_placebo ** 2 + sd_psilo ** 2) / 2) ** 0.5

# Cohen's d
cohens_d = (mean_psilo - mean_placebo) / pooled_sd
cohens_d
