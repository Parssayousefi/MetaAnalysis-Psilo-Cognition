# Data from Fig3:
predrug_means <- c(LVF_100 = 55.319, RVF_100 = 42.553, LVF_800 = 19.149, RVF_800 = 36.170)
predrug_whiskers <- c(LVF_100 = 87.234, RVF_100 = 82.979, LVF_800 = 74.468, RVF_800 = 89.362)
postdrug_means <- c(LVF_100 = 113.380, RVF_100 = 107.042, LVF_800 = 3.521, RVF_800 = 39.437)
postdrug_whiskers <- c(LVF_100 = 180.986, RVF_100 = 193.662, LVF_800 = 83.803, RVF_800 = 111.268)

# Calculate the SDs
predrug_sds <- predrug_whiskers - predrug_means
postdrug_sds <- postdrug_whiskers - postdrug_means

# Function to calculate pooled SD
pooled_sd <- function(sd1, sd2) {
  sqrt((sd1^2 + sd2^2) / 2)
}

# Function to calculate Cohen's d
cohens_d <- function(mean1, mean2, sd1, sd2) {
  (mean2 - mean1) / pooled_sd(sd1, sd2)
}

# Calculate Cohen's d for each SOA, averaged across LVF and RVF
cohens_d_100 <- (cohens_d(predrug_means["LVF_100"], postdrug_means["LVF_100"], predrug_sds["LVF_100"], postdrug_sds["LVF_100"]) +
                   cohens_d(predrug_means["RVF_100"], postdrug_means["RVF_100"], predrug_sds["RVF_100"], postdrug_sds["RVF_100"])) / 2

cohens_d_800 <- (cohens_d(predrug_means["LVF_800"], postdrug_means["LVF_800"], predrug_sds["LVF_800"], postdrug_sds["LVF_800"]) +
                   cohens_d(predrug_means["RVF_800"], postdrug_means["RVF_800"], predrug_sds["RVF_800"], postdrug_sds["RVF_800"])) / 2

# Calculate Cohen's d averaged across SOAs
cohens_d_overall_soa <- (cohens_d_100 + cohens_d_800) / 2

# Calculate one overall Cohen's d
mean_differences <- postdrug_means - predrug_means
pooled_sds <- mapply(pooled_sd, predrug_sds, postdrug_sds)
overall_cohens_d <- sum(mean_differences / pooled_sds) / length(mean_differences)

# results
list(
  Cohens_d_100 = cohens_d_100,
  Cohens_d_800 = cohens_d_800,
  Cohens_d_overall_SOA = cohens_d_overall_soa,
  Overall_Cohens_d = overall_cohens_d
)
