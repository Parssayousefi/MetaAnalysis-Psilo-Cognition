#Data from Fig3

#  number of participants within subject design
total_n <- 40 #see paper section Data processing and analyses -> Emo GNG

placebo_means <- c(493.38, 517.88, 503.31, 512.58, 429.80, 493.38)
placebo_uppers <- c(516.56, 542.38, 525.17, 536.42, 450.99, 521.19)

psilocybin_means <- c(470.86, 496.03, 490.73, 499.34, 421.19, 482.78)
psilocybin_uppers <- c(496.03, 523.18, 517.88, 529.14, 447.68, 512.58)

placebo_avg_rt <- mean(placebo_means)
psilocybin_avg_rt <- mean(psilocybin_means)

placebo_se <- (placebo_uppers - placebo_means) / 1.96
psilocybin_se <- (psilocybin_uppers - psilocybin_means) / 1.96

# calculate SD from SE
placebo_sd <- placebo_se * sqrt(total_n)
psilocybin_sd <- psilocybin_se * sqrt(total_n)

placebo_avg_sd <- mean(placebo_sd)
psilocybin_avg_sd <- mean(psilocybin_sd)

print(paste("Placebo average RT:", placebo_avg_rt, "SD:", placebo_avg_sd))
print(paste("Psilocybin average RT:", psilocybin_avg_rt, "SD:", psilocybin_avg_sd))

# differences for each condition + SD
difference_sd <- sd(psilocybin_means- placebo_means)

# use the SD of the differences for Cohen's d calculation
cohens_d <- (psilocybin_avg_rt- placebo_avg_rt) / difference_sd

print(paste("Cohen's d:", cohens_d))
