

#REACTION TIME
#-------------#
#We assume a correlation (r) of 0.5 between 2-back and 0-back scores
#Data:
#Figure S6 Response Times: 
#2-back (1st quartile, median, 3rd quartile)
#Placebo: 413.88, 446.17, 595.69
#10mg: 456.94, 553.83, 588.52
#20mg: 562.20, 614.83, 704.55
#30mg: 627.99, 694.98, 807.42

##0-back (1st quartile, median, 3rd quartile)
#Placebo: 385.97, 444.44, 477.58
#10mg: 395.71, 424.95, 467.84
#20mg: 413.26, 463.94, 545.81
#30mg: 450.29, 471.74, 510.72

# Function to calculate Cohen's d
calculate_cohens_d <- function(n1, sd1, med1, n2, sd2, med2) {
  s_pooled <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1 + n2 - 2))
  cohen_d <- (med2 - med1) / s_pooled
  return(cohen_d)
}

# Function to calculate SD for difference scores with correlation
calculate_sd_diff_with_correlation <- function(sd_2back, sd_0back, r) {
  return(sqrt(sd_2back^2 + sd_0back^2 - 2 * r * sd_2back * sd_0back))
}

# Assuming a correlation (r) of 0.5 between 2-back and 0-back conditions
r = 0.5


# Placebo group 
n_pla = 19
#2back
med_pla_2 = 446.17
sd_pla_2 = (595.69 - 413.88) / 1.35
#0back
med_pla_0 = 444.44
sd_pla_0 = (477.58 - 385.97) / 1.35

med_diff_pla = 446.17 - 444.44
sd_pla_diff = calculate_sd_diff_with_correlation(sd_pla_2, sd_pla_0, r)

# 10mg group
#2back
n_10mg = 19
sd_10mg = (588.52 - 456.94) / 1.35
med_10mg = 553.83
#0back
med_10mg_0 = 424.95
sd_10mg_0 = (467.84 - 395.71) / 1.35

med_diff_10mg = 553.83 - 424.95
sd_10mg_diff = calculate_sd_diff_with_correlation(sd_10mg, sd_10mg_0, r)


cohen_d_10mg_diff = calculate_cohens_d(n_pla, sd_pla_diff, med_diff_pla, n_10mg, sd_10mg_diff, med_diff_10mg)


# 20mg group
n_20mg = 18
#2back
sd_20mg_2 = (704.55 - 562.20) / 1.35
med_20mg_2 = 614.83
#0back
med_20mg_0 = 463.94
sd_20mg_0 = (545.81 - 413.26) / 1.35

med_diff_20mg = 614.83 - 463.94
sd_20mg_diff = calculate_sd_diff_with_correlation(sd_20mg_2, sd_20mg_0, r)

cohen_d_20mg_diff = calculate_cohens_d(n_pla, sd_pla_diff, med_diff_pla, n_20mg, sd_20mg_diff, med_diff_20mg)

# 30mg group
n_30mg = 18
#2back
sd_30mg_2 = (807.42 - 627.99) / 1.35
med_30mg_2 = 694.98
#0back
med_30mg_0 = 471.74
sd_30mg_0 = (510.72 - 450.29) / 1.35

med_diff_30mg = 694.98 - 471.74
sd_30mg_diff = calculate_sd_diff_with_correlation(sd_30mg_2, sd_30mg_0, r)


cohen_d_30mg_diff = calculate_cohens_d(n_pla, sd_pla_diff, med_diff_pla, n_30mg, sd_30mg_diff, med_diff_30mg)



print(paste("Cohen's d for 10mg using difference scores: ", cohen_d_10mg_diff))
print(paste("Cohen's d for 20mg using difference scores: ", cohen_d_20mg_diff))
print(paste("Cohen's d for 30mg using difference scores: ", cohen_d_30mg_diff))
