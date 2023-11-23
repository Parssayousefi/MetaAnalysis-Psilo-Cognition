
###############################################################################################
##########      ANALYSIS CODE FOR UPDATES to Meta Parsa            [ 2023-11-11 ]  ############
##########      custom workflow, packaging and plotting routine                    ############
##########           by                                                            ############
##########      Ralph C. Alexander Rippe                                           ############
##########                                                                         ############
##########      Please acknowledge appropriately when distributing                 ############
###############################################################################################

# setwd("C:/Users/prsyu/OneDrive/Bidlung/University/M.S. Leiden University/M.S. Neuroscience (Research)/Internship Groningen/Statistical Analysis/Meta playground")
library('readxl')
library('metafor')
source("transform_functions.r")
source("https://raw.githubusercontent.com/talgalili/R-code-snippets/master/boxplot.with.outlier.label.r")
datafile <- "Overview_Studies.xlsx"

currentdate <- Sys.Date()
dir.create(paste(currentdate,sep=""))
sink(file = paste(currentdate,"/","MetaSet ",format(Sys.Date(),format="%y%m%d")," Series v2 Initialize.txt",sep=""), 
     append = F, type = "output", split = T)



# READ DATA and prepare for analysis
data_low          <- as.data.frame(read_excel(datafile, sheet= "Low Dose"))
data_mid          <- as.data.frame(read_excel(datafile, sheet= "Mid Dose"))
data_high         <- as.data.frame(read_excel(datafile, sheet= "High Dose"))

data              <- rbind(
                     data_low[,  c("ID",          "CognitiveFunction", "N_final", 
                                   "ES_final_RT", "ES_final_ACC",       "ES_final_other", 
                                        "Timepoint (min)",    "Dose range (Hi, Mid, Low, MD)",
                                   "PlotLabel",   "DoseLabel")],
                     data_mid[,  c("ID",          "CognitiveFunction", "N_final", 
                                   "ES_final_RT", "ES_final_ACC",       "ES_final_other", 
                                        "Timepoint (min)",    "Dose range (Hi, Mid, Low, MD)",
                                   "PlotLabel",   "DoseLabel")],
                     data_high[, c("ID",          "CognitiveFunction", "N_final", 
                                   "ES_final_RT", "ES_final_ACC",       "ES_final_other", 
                                        "Timepoint (min)",    "Dose range (Hi, Mid, Low, MD)",
                                   "PlotLabel",   "DoseLabel")]
                     ) # rbind

for (j in c(3,4,5,6,7))  data[,j] <- as.numeric(data[,j])
data$timing                       <- 0
data$timing[data[,7] > 180]       <- 1 #post 180 = 1
table(data$timing)                      # 28 pre 180min, 12 post 180 min
data$micro                        <- ifelse(data[,8]=="Micro", TRUE, FALSE)
data$dosage_cat_3                 <- c(rep(1, times = nrow(data_low)),
                                       rep(2, times = nrow(data_mid)),
                                       rep(3, times = nrow(data_high)))
data$dosage_cat_2                 <- c(rep(1, times = nrow(data_low)),
                                       rep(2, times = (nrow(data_mid) + nrow(data_high))))
data <- data[,-c(8)]

data <- subset(data, !is.na(ES_final_RT) | !is.na(ES_final_ACC))
length(unique(data$ID))

# add the variance of the Cohen's d to the dataset
# https://stats.stackexchange.com/questions/144084/variance-of-cohens-d-statistic
# https://stats.stackexchange.com/questions/495015/what-is-the-formula-for-the-standard-error-of-cohens-d
data$ES_var_RT         <- NA
data$ES_var_ACC        <- NA
data$ES_var_other      <- NA
splitfactor            <- 2
for (i in 1:nrow(data)) {
  N_sum                <-  data$N_final[i] 
  N_prod               <- (data$N_final[i]/splitfactor)*(data$N_final[i]/splitfactor)
  data$ES_var_RT[i]    <- (N_sum / N_prod) + (data$ES_final_RT[i]^2 / (2*N_sum)) # or Vdrm=(1/n+drm2/2n)2(1-r) should r be known
  data$ES_var_ACC[i]   <- (N_sum / N_prod) + (data$ES_final_ACC[i]^2 / (2*N_sum))
  data$ES_var_other[i] <- (N_sum / N_prod) + (data$ES_final_other[i]^2 / (2*N_sum))
}


cat("\n******************************\nThe dataset after completion\n******************************\n")
print(data)

 sink()






####################################################################################################################
####################################################################################################################
#######################
#######################   CODE FOR RT measures starts here
#######################


sink(file = paste(currentdate,"/","RT data ",format(Sys.Date(),format="%y%m%d")," Series v2 models RT.txt",sep=""), 
     append = F, type = "output", split = T)

sel_RT  <- !is.na(data$ES_final_RT)
data_RT <- data[sel_RT,]

nrow(data_RT)
length(table(data_RT$ID))
table(data_RT$timing)

cat("\n******************************\nRandom effects model RT without moderator(s)\n******************************\n")
# start with random effect, High i^2 = 78%, significant Q
mod_all_plain <- rma(yi     = ES_final_RT, 
                     vi     = ES_var_RT, 
                     data   = data_RT,
                     method = "DL")
summary(mod_all_plain)

mod_all_ml <- rma.mv(yi     = ES_final_RT,
                     V      = ES_var_RT,
                     random = ~ 1 | ID, 
                     data   = data_RT)
summary(mod_all_ml)



 ### UPDATED I^2 here #######
W <- diag(1/mod_all_ml$vi)
X <- model.matrix(mod_all_ml)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(mod_all_ml$sigma2) / (sum(mod_all_ml$sigma2) + (mod_all_ml$k-mod_all_ml$p)/sum(diag(P)))


# big left ESs are the microdosing studies!
png(paste(currentdate,"/","data_RT ALL Forest plot no moderator.png",sep=""), 
    width  = 750, 
    height = 600)

forest.rma(mod_all_ml,
           data        = data_RT, 
           header      = T,
           addfit      = T,
           showweights = T,
           addcrea     = T,
           annotate    = T,
           xlim        = c(-12, 10), 
           col         = 'red',
           order       = order(data_RT$ID),
           slab        = paste("(", data_RT$ID, ")",data_RT$PlotLabel, data_RT$CognitiveFunction, data_RT$DoseLabel, sep = '  '))

dev.off()







cat("\n******************************\nResiduals raw and standardized\n******************************\n")
(res_mod_all_plain <- rstandard(mod_all_plain))  # no large (> |3|) z-residuals
(res_mod_all_ml    <- rstandard(mod_all_ml))     # no large (> |3|) z-residuals

cat("\n******************************\nRank test\n******************************\n")
ranktest(mod_all_plain)     # no asymmetry detected


cat("\n******************************\nTrim and Fill\n******************************\n")
# No missing studies detected: redundant due to n.s. ranktest, but provided for completeness
(ran.tf.left  <- trimfill(mod_all_ml, 
                          side       = "left",
                          estimator  = "R0",
                          maxiter    = 1000))
(ran.tf.right <- trimfill(mod_all_ml, 
                          side       = "right",
                          estimator  = "R0",
                          maxiter    = 1000))


cat("\n******************************\nfail Safe numbers\n******************************\n")
# huge FSN, thus no suggested asymmetry
fsn(yi     = ES_final_RT, 
    vi     = ES_var_RT,
    data   = data_RT,
    type   = "Rosenthal")            # 429 needed to go down to zero ES
fsn(yi     = ES_final_RT,
    vi     = ES_var_RT,
    data   = data_RT,
    type   = "Orwin",
    target = 0.1)                    # 164 needed to go down to d_pooled = 0.1
fsn(yi     = ES_final_RT,
    vi     = ES_var_RT,
    data   = data_RT,
    type   = "Rosenberg")            # 252 needed to go down to non-sign ES


png(paste(currentdate,"/","data_RT ALL boxplot for outliers.png",sep=""), width = 300, height = 500)
boxplot.with.outlier.label(res_mod_all_ml$z,
                           label_name = data_RT$ID)
dev.off()


png(paste(currentdate,"/","data_RT ALL Funnel.png",sep=""), width = 500, height = 500)
funnel(mod_all_ml,
       yaxis            = "vi",
       legend           = T,
       level            = c(95, 99), 
       shade            = c("white", "gray65"))
dev.off()


png(paste(currentdate,"/","data_RT ALL Residual barplot.png",sep=""), 
    width  = 600, 
    height = 500)
zresids <- rstandard(mod_all_ml)$z
barplot(t(zresids),
        beside     = T, 
        names.arg  = data_RT[,1],
        cex.names  = 0.8,
        horiz      = T,
        las        = 2)
dev.off()




cat("\n******************************\nLeave out microdosings\n******************************\n")

data_RT_filtered <- data_RT[data_RT$micro == FALSE, ]
mod_all_ml.filt  <- rma.mv(yi     = ES_final_RT,
                           V      = ES_var_RT,
                           random = ~ 1 | ID, 
                           data   = data_RT_filtered)
summary(mod_all_ml.filt)                          
# 
# data_RT_filtered$ES_final_RT <- round(data_RT_filtered$ES_final_RT, 3)
# data_RT_filtered$ES_var_RT <- round(data_RT_filtered$ES_var_RT, 3)

 ### UPDATED I^2 here #######
W <- diag(1/mod_all_ml.filt$vi)
X <- model.matrix(mod_all_ml.filt)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(mod_all_ml.filt$sigma2) / (sum(mod_all_ml.filt$sigma2) + (mod_all_ml.filt$k-mod_all_ml.filt$p)/sum(diag(P)))



cat("\n******************************\nResiduals raw and standardized\n******************************\n")
(res_mod_all_ml.filt    <- rstandard(mod_all_ml.filt))  # no large (> |3|) z-residuals

cat("\n******************************\nRank test\n******************************\n")
ranktest(mod_all_ml.filt)     # no asymmetry detected


cat("\n******************************\nfail Safe numbers\n******************************\n")
# huge FSN, thus no suggested asymmetry
fsn(yi     = ES_final_RT, 
    vi     = ES_var_RT, 
    data   = data_RT_filtered, 
    type   = "Rosenthal")            # 468 needed to go down to zero ES
fsn(yi     = ES_final_RT, 
    vi     = ES_var_RT, 
    data   = data_RT_filtered, 
    type   = "Orwin", 
    target = 0.1)                    # 177 needed to go down to d_pooled = 0.1
fsn(yi     = ES_final_RT, 
    vi     = ES_var_RT, 
    data   = data_RT_filtered, 
    type   = "Rosenberg")            # 310 needed to go down to non-sign ES


png(paste(currentdate,"/","data_RT No MICRO boxplot for outliers.png",sep=""), 
    width  = 300,  
    height = 500)
boxplot.with.outlier.label(res_mod_all_ml.filt$z, 
                           label_name = data$ID)
dev.off()


png(paste(currentdate,"/","data_RT No MICRO Funnel.png",sep=""), 
    width  = 500,
    height = 500)
funnel(mod_all_ml.filt,
       yaxis            = "vi",
       legend           = T,
       level            = c(95, 99), 
       shade            = c("white", "gray65"))
dev.off()



png(paste(currentdate,"/","data_RT No MICRO Forest.png",sep=""), 
    width  = 700,
    height = 500)
forest.rma(mod_all_ml.filt,
           data        = data_RT_filtered, 
           header      = T,
           addfit      = T,
           showweights = T,
           addcrea     = T,
           annotate    = T,
           xlim        = c(-12, 10), 
           col         = 'red',
           order       = order(data_RT_filtered$ID),
           slab        = paste("(", data_RT_filtered$ID, ")", data_RT_filtered$PlotLabel, data_RT_filtered$CognitiveFunction, data_RT_filtered$DoseLabel, sep = '  '))

dev.off()


png(paste(currentdate,"/","data_RT NO MICRO Residual barplot.png",sep=""), 
    width  = 600,
    height = 500)
zresids <- rstandard(mod_all_ml.filt)$z
barplot(t(zresids),
        beside     = T, 
        names.arg  = data_RT_filtered[,1],
        cex.names  = 0.8,
        horiz      = T,
        las        = 2)

dev.off()


cat("\n******************************\nModeration by Dosage after removal of microdosings\n******************************\n")
# moderation by Cognitive functioning Category does not solve the residual heterogeneity
# all function category effect sizes are non-sign now
(mod <- rma.mv(yi     = ES_final_RT, # low vs mid vs high
               V      = ES_var_RT, 
               random = ~ 1 | ID, 
               mods   = ~factor(dosage_cat_3), 
               data   = data_RT_filtered))

(mod <- rma.mv(yi     = ES_final_RT, #low vs mid&high
               V      = ES_var_RT, 
               random = ~ 1 | ID, 
               mods   = ~factor(dosage_cat_2), 
               data   = data_RT_filtered))


cat("\n******************************\nModeration by Timing\n******************************\n")
# moderation by Timing does not solve the residual heterogeneity
# all function category effect sizes are non-sign now
(mod <- rma.mv(yi     = ES_final_RT,
               V      = ES_var_RT, 
               random = ~ 1 | ID, 
               mods   = ~factor(timing), 
               data   = data_RT_filtered))



cat("\n******************************\nModeration by Cognitive Functioning Category\n******************************\n")
# moderation by Cognitive functioning Category does not solve the residual heterogeneity
# all function category effect sizes are non-sign now
(mod <- rma.mv(yi     = ES_final_RT,
               V      = ES_var_RT, 
               random = ~ 1 | ID, 
               mods   = ~factor(CognitiveFunction), 
               data   = data_RT_filtered))



cat("\n******************************\nLeave out microdosings and MID/HIGH\n******************************\n")

data_RT_filtered_low <- data_RT[data_RT$micro == FALSE & data_RT$dosage_cat_2 == 1, ]
mod_all_ml.filt.low <- rma.mv(yi     = ES_final_RT,
                              V      = ES_var_RT,
                              random = ~ 1 | ID, 
                              data   = data_RT_filtered_low)
summary(mod_all_ml.filt.low)                           # NO MORE HETEROGENEITY!




sink()








