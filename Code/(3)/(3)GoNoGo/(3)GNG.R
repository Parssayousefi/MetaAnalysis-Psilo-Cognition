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



# only No-Go trials
GoNoGoPlacebo_NG <- GoNoGoPlacebo[GoNoGoPlacebo$CURRENT_TRIAL == "NG", ]
subject_means_placebo <- GoNoGoPlacebo_NG %>%
  group_by(SubjectNumber) %>%
  summarise(
    MeanRT = mean(RT_REL_STIM_ONSET, na.rm = TRUE),
    SDRT = sd(RT_REL_STIM_ONSET, na.rm = TRUE),
    MeanACC = mean(ACCURACY, na.rm = TRUE),
    SDACC = sd(ACCURACY, na.rm = TRUE)
  )
print(subject_means_placebo, n = 34)

# only No-Go trials
GoNoGoPsilo_NG <- GoNoGoPsilo[GoNoGoPsilo$CURRENT_TRIAL == "NG", ]

subject_means_psilo <- GoNoGoPsilo_NG %>%
  group_by(SubjectNumber) %>%
  summarise(
    MeanRT = mean(RT_REL_STIM_ONSET, na.rm = TRUE),
    SDRT = sd(RT_REL_STIM_ONSET, na.rm = TRUE),
    MeanACC = mean(ACCURACY, na.rm = TRUE),
    SDACC = sd(ACCURACY, na.rm = TRUE) )

print(subject_means_psilo, n=34)

# Merge the datasets by SubjectNumber
merged_data <- merge(subject_means_placebo, subject_means_psilo, by = "SubjectNumber", suffixes = c("_Placebo", "_Psilo"))

print(merged_data$Diff_MeanRT)
print(merged_data$Diff_MeanACC)

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
