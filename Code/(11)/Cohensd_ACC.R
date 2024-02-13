

#Raw Data from authors

library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation

# Load the data from Excel files
df_placebo <- read_excel("Code/(11)/Acc_final_Placebo.xlsx")
df_psilo <- read_excel("Code/(11)/Acc_final_Psilo.xlsx")


# Select only the NoGo trial columns for each condition
nogo_columns <- c("NeutralFear", "NeutralSad", "NeutralHappy", "NeutralAngry")

# We assume the NoGo columns have the same names in both datasets and represent the NoGo trials
df_placebo_nogo <- df_placebo %>% select(participant, all_of(nogo_columns))
df_psilo_nogo <- df_psilo %>% select(participant, all_of(nogo_columns))

# Merge the NoGo datasets by participant number
df_merged_nogo <- merge(df_placebo_nogo, df_psilo_nogo, by = "participant", suffixes = c("_placebo", "_psilo"))

# Calculate the average NoGo trial accuracy for Placebo and Psilocybin
df_merged_nogo <- df_merged_nogo %>%
  rowwise() %>%
  mutate(
    avg_nogo_placebo = mean(c_across(ends_with("_placebo"))),
    avg_nogo_psilo = mean(c_across(ends_with("_psilo")))
  ) %>%
  ungroup()

# Calculate the difference in NoGo trial accuracy for each participant
df_merged_nogo <- df_merged_nogo %>%
  mutate(
    acc_diff_nogo = avg_nogo_psilo - avg_nogo_placebo
  )

# Function to calculate Cohen's d for paired samples
cohens_d <- function(data_diff) {
  # Calculate the mean of the differences
  mean_diff <- mean(data_diff)
  # Calculate the standard deviation of the differences
  sd_diff <- sd(data_diff)
  # Calculate the effect size
  d <- mean_diff / sd_diff
  return(d)
}

# Calculate Cohen's d for NoGo trials using the difference scores
d_nogo <- cohens_d(df_merged_nogo$acc_diff_nogo)

# Output the result
print(d_nogo)
