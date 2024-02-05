
#---Original Data from the authors used---#
#---- Calculations are only for incongruent trials--#

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


#only incongruent trials
StroopPlacebo_Incongruent <- StroopPlacebo[StroopPlacebo$congruent == 0, ]
subject_means_Placebo <- StroopPlacebo_Incongruent %>%
  group_by(SubjectNumber) %>%
  summarise(
    MeanRT = mean(RT, na.rm = TRUE),
    SDRT = sd(RT, na.rm = TRUE),
    MeanACC = mean(Accuracy, na.rm = TRUE),
    SDACC = sd(Accuracy, na.rm = TRUE)
  )
print(subject_means_Placebo, n = 34)

StroopPsilo_Incongruent <- StroopPsilo[StroopPsilo$congruent == 0, ]
subject_means_Psilo <- StroopPsilo_Incongruent %>%
  group_by(SubjectNumber) %>%
  summarise(
    MeanRT = mean(RT, na.rm = TRUE),
    SDRT = sd(RT, na.rm = TRUE),
    MeanACC = mean(Accuracy, na.rm = TRUE),
    SDACC = sd(Accuracy, na.rm = TRUE)
  )
print(subject_means_Psilo$MeanRT)
print(subject_means_Placebo$MeanRT)

# Merge the datasets by SubjectNumber
merged_data <- merge(subject_means_Placebo, subject_means_Psilo, by = "SubjectNumber", suffixes = c("_Placebo", "_Psilo"))


# Calculate difference scores
merged_data$Diff_MeanRT <- merged_data$MeanRT_Psilo - merged_data$MeanRT_Placebo
merged_data$Diff_MeanACC <- merged_data$MeanACC_Psilo - merged_data$MeanACC_Placebo

# Calculate mean and SD for difference scores
mean_diff_rt <- mean(merged_data$Diff_MeanRT, na.rm = TRUE)
sd_diff_rt <- sd(merged_data$Diff_MeanRT, na.rm = TRUE)

mean_diff_acc <- mean(merged_data$Diff_MeanACC, na.rm = TRUE)
sd_diff_acc <- sd(merged_data$Diff_MeanACC, na.rm = TRUE)

# Approximate Cohen's d for RT and ACC (not the standard method)
cohens_d_rt_approx <- mean_diff_rt / sd_diff_rt
cohens_d_acc_approx <- mean_diff_acc / sd_diff_acc

print(cohens_d_rt_approx)
print(cohens_d_acc_approx)
