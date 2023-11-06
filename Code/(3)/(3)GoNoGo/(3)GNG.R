library(dplyr)
library(readr)
library(effsize)


# Load the data from the file
file_path1 <- "(3)GoNoGo_Placebo_comb_datafile.txt"
GoNoGoPlacebo <- read.table(file_path1, sep = "\t", header = TRUE, fill = TRUE)


file_path2 <- "(3)GoNoGo_Psilo_comb_datafile.txt"
GoNoGoPsilo <- read.table(file_path2, sep = "\t", header = TRUE, fill = TRUE)


#make the values nummeric
GoNoGoPlacebo$ACCURACY <- as.numeric(GoNoGoPlacebo$ACCURACY)
GoNoGoPlacebo$RT <- as.numeric(GoNoGoPlacebo$RT_REL_STIM_ONSET)

GoNoGoPsilo$ACCURACY <- as.numeric(GoNoGoPsilo$ACCURACY)
GoNoGoPsilo$RT_REL_STIM_ONSET <- as.numeric(GoNoGoPsilo$RT_REL_STIM_ONSET)


# +remove outliers in a specific column of a data frame based on IQR
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
  cleaned_data <- data[data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound, ]
  
  return(cleaned_data)
}

#exclude outliers
GoNoGoPlacebo <- remove_outliers_df(GoNoGoPlacebo, "RT_REL_STIM_ONSET")
GoNoGoPsilo <- remove_outliers_df(GoNoGoPsilo, "RT_REL_STIM_ONSET")

#calculate mean RT and ACC + SD
meanRtPlacebo = mean(GoNoGoPlacebo$RT_REL_STIM_ONSET, na.rm = TRUE)
SDRtPlacebo = sd(GoNoGoPlacebo$RT_REL_STIM_ONSET, na.rm = TRUE)

meanACCPlacebo = mean(GoNoGoPlacebo$ACCURACY,na.rm = TRUE)
SDACCPlacebo = sd(GoNoGoPlacebo$ACCURACY,na.rm = TRUE)

meanRtPsilo = mean(GoNoGoPsilo$RT_REL_STIM_ONSET, na.rm = TRUE)
SDRtPsilo = sd(GoNoGoPsilo$RT_REL_STIM_ONSET, na.rm = TRUE)

meanACCPsilo = mean(GoNoGoPsilo$ACCURACY,na.rm = TRUE)
SDACCPsilo = sd(GoNoGoPsilo$RT_REL_STIM_ONSET, na.rm = TRUE)



cat("mean accuracy placebo:",meanACCPlacebo, "\n")
cat("SD accuracy placebo:",SDACCPlacebo, "\n")
cat("mean RT placebo:",meanRtPlacebo, "\n")
cat("SD RT placebo:",SDRtPlacebo, "\n")



cat("mean accuracy Psilocybin:",meanACCPsilo, "\n")
cat("SD accuracy Psilocybin:",SDACCPsilo, "\n")
cat("mean RT Psilocybin:",meanRtPsilo, "\n")
cat("SD RT Psilocybin:",SDRtPsilo, "\n")


#Cohens d
dRT <- (meanRtPsilo-meanRtPlacebo)/((SDRtPlacebo+SDRtPsilo)/2)
cat("Cohen's d RT:",dRT)

dACC <- (meanACCPsilo-meanACCPlacebo)/((SDACCPlacebo+SDACCPsilo)/2)
cat("Cohen's d ACC:",dACC)
      
      


