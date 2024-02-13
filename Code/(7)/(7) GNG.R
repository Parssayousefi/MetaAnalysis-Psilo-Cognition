

#partial eta^2 extracted from text
#within subject design
# Function to convert eta squared to Cohen's d
convert_eta2_to_cohen_d <- function(eta_squared) {
  cohen_d <- 2 * sqrt(eta_squared / (1 - eta_squared))
  return(cohen_d)
}

#---RT--#
eta_squared_RT <- 0.378

cohen_d_result_RT <- convert_eta2_to_cohen_d(eta_squared_RT)
print(cohen_d_result_RT)


#---Error rates---#
eta_squared_ErrorRate= 0.5381

#multiply with -1 to account for the fact that 1-error rate = accuracy
cohen_d_result_ErrorRate <- -1*(convert_eta2_to_cohen_d(eta_squared_ErrorRate))

print(cohen_d_result_ErrorRate)