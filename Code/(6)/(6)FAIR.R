


# data extracted from Table2

calculate_cohens_d <- function(mean1, mean2, sem1, sem2, n) {
  sd1 <- sem1 * sqrt(n)
  sd2 <- sem2 * sqrt(n)
  sd_pooled <- sqrt(((n - 1) * sd1^2 + (n - 1) * sd2^2) / (2 * n - 2))
  
  d <- (mean2 - mean1) / sd_pooled
  return(d)
}

# data
n <- 8
placebo_mean_performance <- 480.6
placebo_sem_performance <- 33.4
performance_data <- data.frame(
  mean = c(456.5, 455.8, 260.7, 258.0),
  sem = c(25.8, 31.9, 25.0, 42.0),
  dose = c('Very low dose', 'Low dose', 'Mid dose', 'High dose')
)

placebo_mean_quality <- 0.943
placebo_sem_quality <- 0.014
quality_data <- data.frame(
  mean = c(0.959, 0.961, 0.927, 0.910),
  sem = c(0.014, 0.004, 0.029, 0.025),
  dose = c('Very low dose', 'Low dose', 'Mid dose', 'High dose')
)

# Calculate Cohen's d 
cohens_d_performance_vld <- calculate_cohens_d(placebo_mean_performance, performance_data$mean[1], 
                                               placebo_sem_performance, performance_data$sem[1], n)
cohens_d_performance_ld <- calculate_cohens_d(placebo_mean_performance, performance_data$mean[2], 
                                              placebo_sem_performance, performance_data$sem[2], n)
cohens_d_performance_md <- calculate_cohens_d(placebo_mean_performance, performance_data$mean[3], 
                                              placebo_sem_performance, performance_data$sem[3], n)
cohens_d_performance_hd <- calculate_cohens_d(placebo_mean_performance, performance_data$mean[4], 
                                              placebo_sem_performance, performance_data$sem[4], n)

cohens_d_quality_vld <- calculate_cohens_d(placebo_mean_quality, quality_data$mean[1], 
                                           placebo_sem_quality, quality_data$sem[1], n)
cohens_d_quality_ld <- calculate_cohens_d(placebo_mean_quality, quality_data$mean[2], 
                                          placebo_sem_quality, quality_data$sem[2], n)
cohens_d_quality_md <- calculate_cohens_d(placebo_mean_quality, quality_data$mean[3], 
                                          placebo_sem_quality, quality_data$sem[3], n)
cohens_d_quality_hd <- calculate_cohens_d(placebo_mean_quality, quality_data$mean[4], 
                                          placebo_sem_quality, quality_data$sem[4], n)

#  combine  results  labels 
performance_results <- data.frame(
  dose = c('Very low dose', 'Low dose', 'Mid dose', 'High dose'),
  cohens_d = c(cohens_d_performance_vld, cohens_d_performance_ld, cohens_d_performance_md, cohens_d_performance_hd)
)

quality_results <- data.frame(
  dose = c('Very low dose', 'Low dose', 'Mid dose', 'High dose'),
  cohens_d = c(cohens_d_quality_vld, cohens_d_quality_ld, cohens_d_quality_md, cohens_d_quality_hd)
)

# Output
print(performance_results)
print(quality_results)
