

#Attentional Blink Task, cohens d for accuracy and reaction time
#raw data is in the form of 2 separate files, one for the placebo group and one for the psilocybin group

library(dplyr)
library(readxl)
library(tidyr) 
library(effsize)

# Load the data from the files
AB_Placebo <- read.csv("Code/(3)/(3)AB/(3)AB_Placebo_comb.csv")
AB_Psilo <- read.csv("Code/(3)/(3)AB/(3)AB_Psilo_comb.csv")

# Function to preprocess dataset
preprocess_dataset <- function(data, num_subjects, rows_per_subject, lags_to_include) {
  # Rename the first column to "Subject"
  names(data)[1] <- "Subject"
  names(data) <- as.character(names(data))
  # Assign subject IDs
  data$Subject <- rep(1:num_subjects, each = rows_per_subject)
  # Filter based on specified Lag values
  data <- data %>%
    filter(Lag %in% lags_to_include)
  return(data)
}
num_subjects <- 31
rows_per_subject <- 140
lags_to_include <- c(2, 3,4, 7)

AB_Psilo <- preprocess_dataset(AB_Psilo, num_subjects, rows_per_subject, lags_to_include)
AB_Placebo <- preprocess_dataset(AB_Placebo, num_subjects, rows_per_subject, lags_to_include)

# Display the head of the processed Placebo group as an example
head(AB_Placebo)
names(AB_Psilo)
names(AB_Placebo)

str(AB_Psilo)


acc_per_subj_psilo <- AB_Psilo %>%
  filter(Correcto.T1 == 1) %>%  # Filter rows where Correcto.T1 is 1
  group_by(Subject) %>%  # Group data by Subject
  summarize(
    Correct_T2_Count = sum(Correcto.T2, na.rm = TRUE),  # Sum Correcto.T2 for each group
    Accuracy = Correct_T2_Count / 140  # Calculate accuracy
  ) %>%
  rename(Accuracy_Per_Subject = Accuracy)  # Rename column for clarity

# Print the resulting dataframe
print(acc_per_subj_psilo, n=31)


acc_per_subj_placebo <- AB_Placebo %>%
  filter(Correcto.T1 == 1) %>%  # Filter rows where Correcto.T1 is 1
  group_by(Subject) %>%  # Group data by Subject
  summarize(
    Correct_T2_Count = sum(Correcto.T2, na.rm = TRUE),  # Sum Correcto.T2 for each group
    Accuracy = Correct_T2_Count / 140  # Calculate accuracy
  ) %>%
  rename(Accuracy_Per_Subject = Accuracy)  # Rename column for clarity

print(mean(acc_per_subj_placebo$Accuracy_Per_Subject))
print(mean(acc_per_subj_psilo$Accuracy_Per_Subject))



calculate_accuracy_difference <- function(data) {
  data %>%
    filter(Correcto.T1 == 1) %>%
    group_by(Subject, Lag) %>%
    summarize(
      Accuracy = sum(Correcto.T2, na.rm = TRUE) / 140,
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = Lag, values_from = Accuracy) %>%
    mutate(
      Mean_Acc_234 = rowMeans(select(., c(`2`, `3`, `4` )), na.rm = TRUE),
      Acc_7 = `7`,
      Diff =   Acc_7 - Mean_Acc_234
    ) %>%
    select(Subject, Mean_Acc_234, Acc_7, Diff)
}

# Apply the function to both the Psilocybin and Placebo datasets
acc_diff_psilo <- calculate_accuracy_difference(AB_Psilo)
acc_diff_placebo <- calculate_accuracy_difference(AB_Placebo)

# Print the results
print(mean(acc_diff_psilo$Diff))
print(mean(acc_diff_placebo$Diff))

# Merge the data for difference score calculation
merged_data <- merge(acc_diff_psilo, acc_diff_placebo, by="Subject", suffixes = c("_psilo", "_placebo"))

head(merged_data)

# Calculate the difference scores for the accuracy differences
merged_data$diff_scores_acc <- merged_data$Diff_psilo - merged_data$Diff_placebo

# Calculate mean and standard deviation of the difference scores
mean_diff <- mean(merged_data$diff_scores_acc)
sd_diff <- sd(merged_data$diff_scores_acc)

# Calculate Cohen's d
cohens_d_ACC <- mean_diff / sd_diff

# Print Cohen's d
print(cohens_d_ACC)

#--------RT------
# Define the function to calculate reaction time differences
calculate_rt_difference_t2 <- function(data) {
  data %>%
    mutate(Reaction.Time.T2 = Reaction.Time.T2 * 1000) %>%  # Convert RT to milliseconds
    filter(Correcto.T1 == 1 & Correcto.T2 == 1) %>% 
    group_by(Subject, Lag) %>%
    summarize(MeanRT_T2 = mean(Reaction.Time.T2, na.rm = TRUE), .groups = 'drop') %>% 
    pivot_wider(names_from = Lag, values_from = MeanRT_T2) %>%
    mutate(Mean_RT_234 = ifelse(is.na(`2`) | is.na(`3`) | is.na(`4`), NA_real_, 
                                rowMeans(cbind(`2`, `3`, `4`), na.rm = TRUE)),
           RT_Diff = `7` - Mean_RT_234) %>%
    select(Subject, Mean_RT_234, `7`, RT_Diff) 
}

# Apply the function to both datasets
RT_diff_psilo <- calculate_rt_difference_t2(AB_Psilo)
RT_diff_placebo <- calculate_rt_difference_t2(AB_Placebo)

# Calculate the mean RT differences for both groups
mean_rt_diff_psilo <- mean(RT_diff_psilo$RT_Diff, na.rm = TRUE)
mean_rt_diff_placebo <- mean(RT_diff_placebo$RT_Diff, na.rm = TRUE)

print(mean_rt_diff_psilo)
print(mean_rt_diff_placebo)

# Calculate squared deviations from the mean for each dataset
RT_diff_psilo$sq_dev = (RT_diff_psilo$RT_Diff - mean_rt_diff_psilo)^2
RT_diff_placebo$sq_dev = (RT_diff_placebo$RT_Diff - mean_rt_diff_placebo)^2


# Calculate Cohen's d using the effectsize package
cohens_d_result <- cohen.d(RT_diff_psilo$RT_Diff, RT_diff_placebo$RT_Diff)

# Print the result
print(cohens_d_result)
