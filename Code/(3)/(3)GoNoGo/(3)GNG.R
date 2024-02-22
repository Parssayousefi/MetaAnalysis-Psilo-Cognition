library(dplyr)
library(readr)
library(effsize)
library(stringr)


# Load the data from the file
file_path1 <- "Code/(3)/(3)GoNoGo/(3)GoNoGo_Placebo_comb_datafile.txt"
GoNoGoPlacebo <- read.table(file_path1, sep = "\t", header = TRUE, fill = TRUE)

file_path2 <- "Code/(3)/(3)GoNoGo/(3)GoNoGo_Psilo_comb_datafile.txt"
GoNoGoPsilo <- read.table(file_path2, sep = "\t", header = TRUE, fill = TRUE)

# Count rows with specific text in a column
print(nrow(GoNoGoPsilo))


# Clean the SubjectNumber for GoNoGoPlacebo
GoNoGoPlacebo$SubjectNumber <- as.integer(str_extract(GoNoGoPlacebo$PARTICIPANT, "[0-9]+"))

# Clean the SubjectNumber for GoNoGoPsilo
GoNoGoPsilo$SubjectNumber <- as.integer(str_extract(GoNoGoPsilo$PARTICIPANT, "[0-9]+"))

print(length(unique(GoNoGoPlacebo$SubjectNumber)))
print(length(unique(GoNoGoPsilo$SubjectNumber)))



#make the values nummeric
GoNoGoPlacebo$ACCURACY <- as.numeric(GoNoGoPlacebo$ACCURACY)
GoNoGoPlacebo$RT_REL_STIM_ONSET <- as.numeric(GoNoGoPlacebo$RT_REL_STIM_ONSET)
GoNoGoPsilo$ACCURACY <- as.numeric(GoNoGoPsilo$ACCURACY)
GoNoGoPsilo$RT_REL_STIM_ONSET <- as.numeric(GoNoGoPsilo$RT_REL_STIM_ONSET)



# remove outliers in a specific column of a data frame based on IQR
remove_outliers_df <- function(data, column_name, factor = 1.5) {
  # Check if the column_name exists in the data frame
  if (!column_name %in% colnames(data)) {
    stop("The specified column name is not present in the data frame.")
  }
  
  # Compute the first and third quartiles (Q1 and Q3)
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  
  # Calculate the Interquartile Range (IQR)
  IQR <- Q3 - Q1
  
  # Define the lower and upper bounds for outliers
  lower_bound <- Q1 - factor * IQR
  upper_bound <- Q3 + factor * IQR
  
  # Filter out the outliers
  outliers <- data[data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, ]
  cleaned_data <- data[data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound, ]
  cat("Number of outliers:", nrow(outliers), "\n")
  
  return(cleaned_data)
}

#exclude outliers
GoNoGoPlacebo <- remove_outliers_df(GoNoGoPlacebo, "RT_REL_STIM_ONSET")
GoNoGoPsilo <- remove_outliers_df(GoNoGoPsilo, "RT_REL_STIM_ONSET")


# Calculate difference scores for the Placebo group
placebo_diff_scores <- GoNoGoPlacebo %>%
  group_by(SubjectNumber) %>%
  summarise(
    MeanRT_NoGo = mean(RT_REL_STIM_ONSET[CURRENT_TRIAL == "NG"], na.rm = TRUE),
    MeanRT_Go = mean(RT_REL_STIM_ONSET[CURRENT_TRIAL == "G"], na.rm = TRUE),
    MeanACC_NoGo = mean(ACCURACY[CURRENT_TRIAL == "NG"], na.rm = TRUE),
    MeanACC_Go = mean(ACCURACY[CURRENT_TRIAL == "G"], na.rm = TRUE)
  ) %>%
  mutate(
    Diff_MeanRT = MeanRT_NoGo - MeanRT_Go,
    Diff_MeanACC = MeanACC_NoGo - MeanACC_Go
  )
print(placebo_diff_scores, n = 34)

# Calculate difference scores for the Psilo group
psilo_diff_scores <- GoNoGoPsilo %>%
  group_by(SubjectNumber) %>%
  summarise(
    MeanRT_NoGo = mean(RT_REL_STIM_ONSET[CURRENT_TRIAL == "NG"], na.rm = TRUE),
    MeanRT_Go = mean(RT_REL_STIM_ONSET[CURRENT_TRIAL == "G"], na.rm = TRUE),
    MeanACC_NoGo = mean(ACCURACY[CURRENT_TRIAL == "NG"], na.rm = TRUE),
    MeanACC_Go = mean(ACCURACY[CURRENT_TRIAL == "G"], na.rm = TRUE)
  ) %>%
  mutate(
    Diff_MeanRT = MeanRT_NoGo - MeanRT_Go,
    Diff_MeanACC = MeanACC_NoGo - MeanACC_Go
  )
print(psilo_diff_scores, n=34)

# Merge the difference scores by SubjectNumber
merged_diff_scores <- merge(placebo_diff_scores, psilo_diff_scores, by = "SubjectNumber", suffixes = c("_Placebo", "_Psilo"))

# Calculate Cohen's d for the difference scores (Psilo - Placebo)
merged_diff_scores$Diff_MeanRT_Psilo_Placebo <- merged_diff_scores$Diff_MeanRT_Psilo - merged_diff_scores$Diff_MeanRT_Placebo
merged_diff_scores$Diff_MeanACC_Psilo_Placebo <- merged_diff_scores$Diff_MeanACC_Psilo - merged_diff_scores$Diff_MeanACC_Placebo

# Calculate the standard deviation of the difference scores for accuracy
sd_diff_acc <- sd(merged_diff_scores$Diff_MeanACC_Psilo_Placebo, na.rm = TRUE)

# Calculate the mean of the difference scores for accuracy
mean_diff_acc <- mean(merged_diff_scores$Diff_MeanACC_Psilo_Placebo, na.rm = TRUE)

# Compute Cohen's d for within-subjects design
cohens_d_acc_within <- mean_diff_acc / sd_diff_acc

print(cohens_d_acc_within)
