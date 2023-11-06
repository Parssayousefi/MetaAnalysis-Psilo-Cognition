#data from supplementary material. We used the RT for correct responses--#
median_baseline <- 2808.90
se_baseline <- 407.15
median_one_week <- 1984.71
se_one_week <- 132.90
median_four_weeks <- 1970.10
se_four_weeks <- 132.68
N <- 22

#  SE to SD
sd_baseline <- se_baseline * sqrt(N)
sd_one_week <- se_one_week * sqrt(N)
sd_four_weeks <- se_four_weeks * sqrt(N)

# Function Cohen's d | negative = decrease in RT, positive = increase in RT
calculate_cohens_d <- function(mean1, sd1, mean2, sd2) {
  sd_pooled <- sqrt(((N - 1) * sd1^2 + (N - 1) * sd2^2) / (2 * N - 2))
  d <- (mean2 - mean1) / sd_pooled
  return(d)
}

# Cohen's d  baseline vs 1 week
cohens_d_baseline_vs_one_week <- calculate_cohens_d(median_baseline, sd_baseline, median_one_week, sd_one_week)

# Cohen's d  baseline vs 4 weeks
cohens_d_baseline_vs_four_weeks <- calculate_cohens_d(median_baseline, sd_baseline, median_four_weeks, sd_four_weeks)

# the Cohen's d values
list(
  cohens_d_baseline_vs_one_week = cohens_d_baseline_vs_one_week,
  cohens_d_baseline_vs_four_weeks = cohens_d_baseline_vs_four_weeks
)