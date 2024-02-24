# Load the required libraries
library(readxl)
library(dplyr)
library(stringr)
library(effsize)

# Load the data from Excel files
df_placebo <- read_excel("Code/(11)/Acc_final_Placebo.xlsx")
df_psilo <- read_excel("Code/(11)/Acc_final_Psilo.xlsx")

# Define NoGo and Go trial columns
nogo_columns <- c("NeutralFear", "NeutralSad", "NeutralHappy", "NeutralAngry")
go_columns <- c("FearNeut", "SadNeut", "HappyNeut", "AngryNeut")

# Select and merge the relevant columns for both conditions
df_placebo_trials <- df_placebo %>% select(participant, all_of(nogo_columns), all_of(go_columns))
df_psilo_trials <- df_psilo %>% select(participant, all_of(nogo_columns), all_of(go_columns))
df_merged_trials <- merge(df_placebo_trials, df_psilo_trials, by = "participant", suffixes = c("_placebo", "_psilo"))

# Calculate the average NoGo and Go trial accuracy for Placebo and Psilocybin
df_diffs <- df_merged_trials %>%
  rowwise() %>%
  mutate(
    avg_nogo_placebo = mean(c_across(ends_with("_placebo"))),
    avg_go_placebo = mean(c_across(str_c(go_columns, "_placebo"))),
    avg_nogo_psilo = mean(c_across(ends_with("_psilo"))),
    avg_go_psilo = mean(c_across(str_c(go_columns, "_psilo"))),
    diff_nogo_go_placebo = avg_nogo_placebo - avg_go_placebo,
    diff_nogo_go_psilo = avg_nogo_psilo - avg_go_psilo,
    acc_diff_nogo_go = diff_nogo_go_psilo - diff_nogo_go_placebo
  ) %>%
  ungroup()

# Print the first few rows to check the output
print(head(df_diffs))

# Function to calculate Cohen's d for paired samples
cohens_d <- function(data_diff) {
  mean_diff <- mean(data_diff)
  sd_diff <- sd(data_diff)
  d <- mean_diff / sd_diff
  return(d)
}

# Calculate Cohen's d for the NoGo - Go trial differences between the conditions
d_nogo_go <- cohens_d(df_diffs$acc_diff_nogo_go)

# Output the result
print(d_nogo_go)
