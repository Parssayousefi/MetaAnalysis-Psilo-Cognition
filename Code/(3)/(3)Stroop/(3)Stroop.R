
#---Original Data from the authors used---#
#---- Calculations are only for incongruent -congruent trials--#

library(dplyr)
library(stringr)

# Load the data from the file
file_path1 <- "Code/(3)/(3)Stroop/(3)Stroop_Placebo_comb_datafile.txt"
StroopPlacebo <- read.table(file_path1, sep = ";", header = TRUE)

file_path2 <- "Code/(3)/(3)Stroop/(3)Stroop_Psilo_comb_datafile.txt"
 StroopPsilo <- read.table(file_path2, sep = ";", header = TRUE)


# Count rows with specific text in a column
print(nrow(StroopPsilo))

# Clean the SubjectNumber
StroopPlacebo$SubjectNumber <- as.integer(str_extract(StroopPlacebo$Sub_id, "[0-9]+"))
StroopPsilo$SubjectNumber <- as.integer(str_extract(StroopPsilo$Sub_id, "[0-9]+"))

print(length(unique(StroopPlacebo$SubjectNumber)))
print(length(unique(StroopPsilo$SubjectNumber)))


#make the values nummeric
StroopPlacebo$Accuracy <- as.numeric(StroopPlacebo$Accuracy)
StroopPlacebo$RT <- as.numeric(StroopPlacebo$RT)

StroopPsilo$Accuracy <- as.numeric(StroopPsilo$Accuracy)
StroopPsilo$RT <- as.numeric(StroopPsilo$RT)


#function to remove outliers
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
StroopPlacebo <- remove_outliers_df(StroopPlacebo, "RT")
StroopPsilo <- remove_outliers_df(StroopPsilo, "RT")

# Calculate the mean and SD for RT and ACC for the placebo group
placebo_diff_scores <- StroopPlacebo %>%
  group_by(SubjectNumber) %>%
  summarise(
    MeanRT_Incongruent = mean(RT[congruent == 0], na.rm = TRUE),
    MeanRT_Congruent = mean(RT[congruent == 1], na.rm = TRUE),
    MeanACC_Incongruent = mean(Accuracy[congruent == 0], na.rm = TRUE),
    MeanACC_Congruent = mean(Accuracy[congruent == 1], na.rm = TRUE)
  ) %>%
  mutate(
    Diff_MeanRT = MeanRT_Incongruent - MeanRT_Congruent,
    Diff_MeanACC = MeanACC_Incongruent - MeanACC_Congruent
  )

print(placebo_diff_scores$Diff_MeanRT)

# Calculate the mean and SD for RT and ACC for the psilo group
psilo_diff_scores <- StroopPsilo %>%
  group_by(SubjectNumber) %>%
  summarise(
    MeanRT_Incongruent = mean(RT[congruent == 0], na.rm = TRUE),
    MeanRT_Congruent = mean(RT[congruent == 1], na.rm = TRUE),
    MeanACC_Incongruent = mean(Accuracy[congruent == 0], na.rm = TRUE),
    MeanACC_Congruent = mean(Accuracy[congruent == 1], na.rm = TRUE)
  ) %>%
  mutate(
    Diff_MeanRT = MeanRT_Incongruent - MeanRT_Congruent,
    Diff_MeanACC = MeanACC_Incongruent - MeanACC_Congruent
  )



# Merge the data frames
merged_diff_scores <- merge(placebo_diff_scores, psilo_diff_scores, by = "SubjectNumber", suffixes = c("_Placebo", "_Psilo"))

# Calculate Cohen's d for the difference scores
mean_diff_rt <- mean(merged_diff_scores$Diff_MeanRT_Psilo - merged_diff_scores$Diff_MeanRT_Placebo, na.rm = TRUE)
sd_diff_rt <- sd(merged_diff_scores$Diff_MeanRT_Psilo - merged_diff_scores$Diff_MeanRT_Placebo, na.rm = TRUE)

mean_diff_acc <- mean(merged_diff_scores$Diff_MeanACC_Psilo - merged_diff_scores$Diff_MeanACC_Placebo, na.rm = TRUE)
sd_diff_acc <- sd(merged_diff_scores$Diff_MeanACC_Psilo - merged_diff_scores$Diff_MeanACC_Placebo, na.rm = TRUE)

cohens_d_rt <- mean_diff_rt / sd_diff_rt
cohens_d_acc <- mean_diff_acc / sd_diff_acc

print(cohens_d_rt)
print(cohens_d_acc)