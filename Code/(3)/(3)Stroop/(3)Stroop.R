
#---Original Data from the authors used---#
#---- Calculations are across incongruent & congruent trials--#

library(dplyr)

# Load the data from the file
file_path1 <- "Code/(3)/(3)Stroop/(3)Stroop_Placebo_comb_datafile.txt"
StroopPlacebo <- read.table(file_path1, sep = ";", header = TRUE)

file_path2 <- "Code/(3)/(3)Stroop/(3)Stroop_Psilo_comb_datafile.txt"
StroopPsilo <- read.table(file_path2, sep = ";", header = TRUE)


#clean data(many headers are in the dataset)
StroopPlacebo <- StroopPlacebo %>%
  filter(RT != "RT")
StroopPsilo <- StroopPsilo %>%
  filter(RT != "RT")

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
  cleaned_data <- data[data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound, ]
  
  return(cleaned_data)
}


#exclude outliers
StroopPlacebo <- remove_outliers_df(StroopPlacebo, "RT")
StroopPsilo <- remove_outliers_df(StroopPsilo, "RT")

#calculate mean RT and ACC + SD
meanRtPlacebo = mean(StroopPlacebo$RT)
SDRtPlacebo = sd(StroopPlacebo$RT)

meanACCPlacebo = mean(StroopPlacebo$Accuracy)
SDACCPlacebo = sd(StroopPlacebo$Accuracy)


meanRtPsilo = mean(StroopPsilo$RT)
SDRtPsilo = sd(StroopPsilo$RT)

meanACCPsilo = mean(StroopPsilo$Accuracy)
SDACCPsilo = sd(StroopPsilo$RT)



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




