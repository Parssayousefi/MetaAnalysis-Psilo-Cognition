
#REACTION TIME
#-------------#

#Data:
#Figure S6 Response Times: 2-back (1st quartile, median, 3rd quartile)
#Placebo: 413.88, 446.17, 595.69
#10mg: 456.94, 553.83, 588.52
#20mg: 562.20, 614.83, 704.55
#30mg: 627.99, 694.98, 807.42


# Function to calculate Cohen's d
calculate_cohens_d <- function(n1, sd1, med1, n2, sd2, med2) {
  s_pooled <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1 + n2 - 2))
  cohen_d <- (med2 - med1) / s_pooled
  return(cohen_d)
}

# Placebo group
n_pla = 19
med_pla = 446.17
sd_pla = (595.69 - 413.88) / 1.35

# 10mg group
n_10mg = 19
sd_10mg = (588.52 - 456.94) / 1.35
med_10mg = 553.83
cohen_d_10mg = calculate_cohens_d(n_pla, sd_pla, med_pla, n_10mg, sd_10mg, med_10mg)
print(paste("Cohen's d for 10mg: ", cohen_d_10mg))

# 20mg group
n_20mg = 18
sd_20mg = (704.55 - 562.20) / 1.35
med_20mg = 614.83
cohen_d_20mg = calculate_cohens_d(n_pla, sd_pla, med_pla, n_20mg, sd_20mg, med_20mg)
print(paste("Cohen's d for 20mg: ", cohen_d_20mg))

# 30mg group
n_30mg = 18
sd_30mg = (807.42 - 627.99) / 1.35
med_30mg = 694.98
cohen_d_30mg = calculate_cohens_d(n_pla, sd_pla, med_pla, n_30mg, sd_30mg, med_30mg)
print(paste("Cohen's d for 30mg: ", cohen_d_30mg))


