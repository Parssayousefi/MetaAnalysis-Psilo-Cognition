library(dplyr)
library(readxl)


# Load the data from the file

AB_Placebo <- read.csv("(3)AB_Placebo_comb.csv")


AB_Psilo <- read.csv("(3)AB_Psilo_comb.csv")

head(AB_Psilo)


#1.Psilo group

#1.1 sort data
names(AB_Psilo)[1] <- "Subject"

# Define the number of times to repeat the operation
num_repeats <- 31 #because there are 31 subjects in this gouprp, see folder!

# Define the number of rows to change at a time
rows_at_a_time <- 140

# Loop over the number of repeats
for (i in 1:num_repeats) {
  # Calculate the start and end row indices for each repeat
  start_row <- (i - 1) * rows_at_a_time + 1
  end_row <- i * rows_at_a_time
  
  # Change the value in the specified rows of the "Subject" column
  AB_Psilo[start_row:end_row, "Subject"] <- i
}


#1.2 calculate the accuracy (Number of correct T2 detections when T1 was correctly detected) / 140 trials)

# Subset the data frame to include only the rows where Correcto.T1 is 1
df_subset_psilo <- AB_Psilo[AB_Psilo$Correcto.T1 == 1, ]

# Calculate the number of correct T2 detections per subject
correct_detections_psilo <- with(df_subset_psilo, tapply(Correcto.T2, Subject, sum))

# Calculate the accuracy per subject
accuracy_psilo <- correct_detections_psilo / 140

# Create a new data frame with the subject IDs and their accuracies
acc_per_subj_psilo <- data.frame(
  Subject = names(accuracy_psilo),
  Accuracy = accuracy_psilo
)




#2.1. sort data Placebo group
names(AB_Placebo)[1] <- "Subject"

# Define the number of times to repeat the operation
num_repeats <- 31 #because there are 31 subjects in this gouprp, see folder!

# Define the number of rows to change at a time
rows_at_a_time <- 140

# Loop over the number of repeats
for (i in 1:num_repeats) {
  # Calculate the start and end row indices for each repeat
  start_row <- (i - 1) * rows_at_a_time + 1
  end_row <- i * rows_at_a_time
  
  # Change the value in the specified rows of the "Subject" column
  AB_Placebo[start_row:end_row, "Subject"] <- i
}

#2.2 calculate the accuracy (Number of correct T2 detections when T1 was correctly detected) / 140 trials)

# Subset the data frame to include only the rows where Correcto.T1 is 1
df_subset_placebo <- AB_Placebo[AB_Placebo$Correcto.T1 == 1, ]

# Calculate the number of correct T2 detections per subject
correct_detections_placebo <- with(df_subset_placebo, tapply(Correcto.T2, Subject, sum))

# Calculate the accuracy per subject
accuracy_placebo <- correct_detections_placebo / 140

# Create a new data frame with the subject IDs and their accuracies
acc_per_subj_placebo <- data.frame(
  Subject = names(accuracy_placebo),
  Accuracy = accuracy_placebo
)


#3. t-test for accuracy 

# Perform paired t-test
result <- t.test(acc_per_subj_psilo$Accuracy, acc_per_subj_placebo$Accuracy, paired = TRUE)

# Print the t-test results
print(result)


#4. Effect size ACC
mean_diff <- mean(acc_per_subj_psilo$Accuracy - acc_per_subj_placebo$Accuracy)
pooled_sd <- sqrt((sd(acc_per_subj_psilo$Accuracy)^2 + sd(acc_per_subj_placebo$Accuracy)^2) / 2)
cohens_d <- mean_diff / pooled_sd

# (Cohen's d ACC)
print(cohens_d)



#--------RT------
# Calculate the mean reaction time for T1 and T2 per subject for the Psilo group
mean_rt_t1_psilo <- with(AB_Psilo[AB_Psilo$Correcto.T1 == 1 & AB_Psilo$Correcto.T2 == 1, ], tapply(Reaction.Time.T1, Subject, mean))
mean_rt_t2_psilo <- with(AB_Psilo[AB_Psilo$Correcto.T1 == 1 & AB_Psilo$Correcto.T2 == 1, ], tapply(Reaction.Time.T2, Subject, mean))

# Create a new data frame with the subject IDs and their mean reaction times
rt_per_subj_psilo <- data.frame(
  Subject = names(mean_rt_t1_psilo),
  MeanRT_T1 = mean_rt_t1_psilo,
  MeanRT_T2 = mean_rt_t2_psilo,
  MeanRT_T1T2_psilo= (mean_rt_t1_psilo +  mean_rt_t2_psilo) /2
)

# Calculate the mean reaction time for T1 and T2 per subject for the Placebo group
mean_rt_t1_placebo <- with(AB_Placebo[AB_Placebo$Correcto.T1 == 1 & AB_Placebo$Correcto.T2 == 1, ], tapply(Reaction.Time.T1, Subject, mean))
mean_rt_t2_placebo <- with(AB_Placebo[AB_Placebo$Correcto.T1 == 1 & AB_Placebo$Correcto.T2 == 1, ], tapply(Reaction.Time.T2, Subject, mean))

# Create a new data frame with the subject IDs and their mean reaction times
rt_per_subj_placebo <- data.frame(
  Subject = names(mean_rt_t1_placebo),
  MeanRT_T1 = mean_rt_t1_placebo,
  MeanRT_T2 = mean_rt_t2_placebo,
  MeanRT_T1T2_placebo= (mean_rt_t1_placebo +  mean_rt_t2_placebo) /2
)

# Perform  t-tests for T1 and T2
result_t1 <- t.test(rt_per_subj_psilo$MeanRT_T1, rt_per_subj_placebo$MeanRT_T1, paired = TRUE)
result_t2 <- t.test(rt_per_subj_psilo$MeanRT_T2, rt_per_subj_placebo$MeanRT_T2, paired = TRUE)

# Print the t-test results
print(result_t1)
print(result_t2)

# Calculate Cohen's d for T1
mean_diff_t1 <- mean(rt_per_subj_psilo$MeanRT_T1 - rt_per_subj_placebo$MeanRT_T1)
pooled_sd_t1 <- sqrt((sd(rt_per_subj_psilo$MeanRT_T1)^2 + sd(rt_per_subj_placebo$MeanRT_T1)^2) / 2)
cohens_d_t1 <- mean_diff_t1 / pooled_sd_t1

# Calculate Cohen's d for T2
mean_diff_t2 <- mean(rt_per_subj_psilo$MeanRT_T2 - rt_per_subj_placebo$MeanRT_T2)
pooled_sd_t2 <- sqrt((sd(rt_per_subj_psilo$MeanRT_T2)^2 + sd(rt_per_subj_placebo$MeanRT_T2)^2) / 2)
cohens_d_t2 <- mean_diff_t2 / pooled_sd_t2

# Print Cohen's d
print(cohens_d_t1)
print(cohens_d_t2)

# Perform  t-tests for Mean of T1 & T2
result_t1t2 <- t.test(rt_per_subj_psilo$MeanRT_T1T2_psilo, rt_per_subj_placebo$MeanRT_T1T2_placebo, paired = TRUE)
print(result_t1t2)
# Calculate Cohen's d for mean of T1 & T2
mean_diff_meanT1T2 <- mean(rt_per_subj_psilo$MeanRT_T1T2_psilo - rt_per_subj_placebo$MeanRT_T1T2_placebo)
pooled_sd_t1t2 <- sqrt((sd(rt_per_subj_psilo$MeanRT_T1T2_psilo)^2 + sd(rt_per_subj_placebo$MeanRT_T1T2_placebo)^2) / 2)
cohens_d_t1t2 <- mean_diff_meanT1T2 / pooled_sd_t1t2

print(cohens_d_t1t2)

#----extra---#
# Calculate the overall mean and SD for accuracy and RT for the Psilocybin group
overall_mean_acc_psilo <- mean(acc_per_subj_psilo$Accuracy, na.rm = TRUE)
overall_sd_acc_psilo <- sd(acc_per_subj_psilo$Accuracy, na.rm = TRUE)
overall_mean_rt_psilo <- mean(rt_per_subj_psilo$MeanRT_T1T2_psilo, na.rm = TRUE)
overall_sd_rt_psilo <- sd(rt_per_subj_psilo$MeanRT_T1T2_psilo, na.rm = TRUE)

cat("Psilocybin Group - Mean Accuracy:", overall_mean_acc_psilo, "\n")
cat("Psilocybin Group - SD Accuracy:", overall_sd_acc_psilo, "\n")
cat("Psilocybin Group - Mean RT:", overall_mean_rt_psilo, "\n")
cat("Psilocybin Group - SD RT:", overall_sd_rt_psilo, "\n")

# Calculate the overall mean and SD for accuracy and RT for the Placebo group
overall_mean_acc_placebo <- mean(acc_per_subj_placebo$Accuracy, na.rm = TRUE)
overall_sd_acc_placebo <- sd(acc_per_subj_placebo$Accuracy, na.rm = TRUE)
overall_mean_rt_placebo <- mean(rt_per_subj_placebo$MeanRT_T1T2_placebo, na.rm = TRUE)
overall_sd_rt_placebo <- sd(rt_per_subj_placebo$MeanRT_T1T2_placebo, na.rm = TRUE)

cat("Placebo Group - Mean Accuracy:", overall_mean_acc_placebo, "\n")
cat("Placebo Group - SD Accuracy:", overall_sd_acc_placebo, "\n")
cat("Placebo Group - Mean RT:", overall_mean_rt_placebo, "\n")
cat("Placebo Group - SD RT:", overall_sd_rt_placebo, "\n")

