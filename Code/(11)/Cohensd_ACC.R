

#Raw Data from authors

library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation

# Load the data from Excel files
df_placebo <- read_excel("Acc_final_Placebo.xlsx")
df_psilo <- read_excel("Acc_final_Psilo.xlsx")

# Merge the datasets by participant number
df_merged <- merge(df_placebo, df_psilo, by = "participant", suffixes = c("_placebo", "_psilo"))

# Calculate the difference in accuracy between Psilocybin and Placebo for each participant
df_merged <- df_merged %>%
  mutate(acc_diff = `AVGcorrect  for subj` - `AVG for subj`)

# Function to calculate Cohen's d for paired samples
cohens_d <- function(data_diff) {
  mean_diff <- mean(data_diff)
  sd_diff <- sd(data_diff)
  d <- mean_diff / sd_diff
  return(d)
}

# Calculate Cohen's d
d <- cohens_d(df_merged$acc_diff)

# Output the result
print(d)

