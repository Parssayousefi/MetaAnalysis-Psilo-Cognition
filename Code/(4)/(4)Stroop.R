stroop_data<- read.csv("dep1606_stroop.csv") 


#DATA PRIVIDED BY AUTHORS

# number of  participants
n <- length(unique(stroop_data$subject))
print(n)

#------RT-----#
#positive values mean increased RT

# Function  Cohen's d
calculate_cohens_d <- function(baseline_group, post_group) {
  n_baseline <- length(baseline_group)
  n_post <- length(post_group)
  mean_baseline <- mean(baseline_group, na.rm = TRUE)
  mean_post <- mean(post_group, na.rm = TRUE)
  sd_baseline <- sd(baseline_group, na.rm = TRUE)
  sd_post <- sd(post_group, na.rm = TRUE)
  
  # print("baseline:")
  # print(mean(baseline_group, na.rm = TRUE))
  # print("post")
  # print(mean(post_group, na.rm = TRUE))

  sd_pooled <- sqrt(((n_baseline - 1) * sd_baseline^2 + (n_post - 1) * sd_post^2) / (n_baseline + n_post - 2))
  
  d <- (mean_post - mean_baseline) / sd_pooled
  return(d)
}


#  Cohen's d for each comparison and each reaction time type

# Filter reaction times based on session
rt_baseline_word <- stroop_data$rt.col.incong[stroop_data$session == 0]
rt_one_week_word <- stroop_data$rt.col.incong[stroop_data$session == 1]
rt_four_weeks_word <- stroop_data$rt.col.incong[stroop_data$session == 2]

rt_baseline_ink <- stroop_data$rt.ink.incong[stroop_data$session == 0]
rt_one_week_ink <- stroop_data$rt.ink.incong[stroop_data$session == 1]
rt_four_weeks_ink <- stroop_data$rt.ink.incong[stroop_data$session == 2]

#  Cohen's d for word and ink-naming incongruent trials
cohens_d_one_week_word <- calculate_cohens_d(rt_baseline_word, rt_one_week_word)
cohens_d_four_weeks_word <- calculate_cohens_d(rt_baseline_word, rt_four_weeks_word)

cohens_d_one_week_ink <- calculate_cohens_d(rt_baseline_ink, rt_one_week_ink)
cohens_d_four_weeks_ink <- calculate_cohens_d(rt_baseline_ink, rt_four_weeks_ink)

# Average Cohen's d across word and ink-naming trials
cohens_d_average_one_week <- (cohens_d_one_week_word + cohens_d_one_week_ink) / 2
cohens_d_average_four_weeks <- (cohens_d_four_weeks_word + cohens_d_four_weeks_ink) / 2

# averaged Cohen's d values
list(
  cohens_d_average_one_week = cohens_d_average_one_week,
  cohens_d_average_four_weeks = cohens_d_average_four_weeks
)


#----Accuracy----#
#negative values here mean decreased accuracy#

# Filter  hit rates based on session for word incongruent trials
hr_baseline_word <- stroop_data$hr.col.incong[stroop_data$session == 0]
hr_one_week_word <- stroop_data$hr.col.incong[stroop_data$session == 1]
hr_four_weeks_word <- stroop_data$hr.col.incong[stroop_data$session == 2]

# Filter  hit rates based on session for ink incongruent 
hr_baseline_ink <- stroop_data$hr.ink.incong[stroop_data$session == 0]
hr_one_week_ink <- stroop_data$hr.ink.incong[stroop_data$session == 1]
hr_four_weeks_ink <- stroop_data$hr.ink.incong[stroop_data$session == 2]

#  Cohen's d for word and ink incongruent
cohens_d_one_week_word <- calculate_cohens_d(hr_one_week_word, hr_baseline_word)
cohens_d_four_weeks_word <- calculate_cohens_d(hr_four_weeks_word, hr_baseline_word)

cohens_d_one_week_ink <- calculate_cohens_d(hr_one_week_ink, hr_baseline_ink)
cohens_d_four_weeks_ink <- calculate_cohens_d(hr_four_weeks_ink, hr_baseline_ink)

# Average Cohen's d across word and ink 
cohens_d_average_one_week_hr <- (cohens_d_one_week_word + cohens_d_one_week_ink) / 2
cohens_d_average_four_weeks_hr <- (cohens_d_four_weeks_word + cohens_d_four_weeks_ink) / 2

# averaged Cohen's d  hit rates
list(
  cohens_d_average_one_week_hr = cohens_d_average_one_week_hr,
  cohens_d_average_four_weeks_hr = cohens_d_average_four_weeks_hr
)




