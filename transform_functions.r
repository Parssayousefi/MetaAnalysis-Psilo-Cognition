

# FUNCTIONS
## Fisher transform
fisherZ    <- function(r){0.5*log((1+r)/(1-r))}
invFisherZ <- function(z){ tanh(z) }


## effect size handlers
r_handler <- function(x){
  es = fisherZ(x[,'Value_SV'])
  sv = 1 / (x[,'Ntotal_SV'] -3)
  return(c(es, sv))
}

beta_handler <- function(x){
 # note: this function handles the standardized beta form a regression analysis. the esc_beta function assumes that the beta refers to the coefficient for a binary grouping variables, as you would have in a indepdendent samples t-test for example.
 # beta is thus treated as a (partial) r. 
  return(r_handler(x))
}

B_handler <- function(x){
  # tmp = esc_B(data$Value_SV, grp1n=data$Ntotal_SV, grp2n=data$Ntotal_SV, sdy=data$SDNonDel_SV, es.type= "r")
  es = data$Value_SV #* sx/sy
  es =  fisherZ(es)
  es = NA
  sv = 1 / (x[,'Ntotal_SV'] -3)
  return(c(es, sv)) #TODO: the SD is not corect yet, but it is really needed!
}
           
X_handler <- function(x){
  es = sqrt(x[,'Value_SV'] / x[,'Ntotal_SV'])   # covert to r  (phi coefficient)
  es = fisherZ(es)
  sv = 1 / (x[,'Ntotal_SV']-3) 
  return(c(es, sv))
}

Xp_handler <- function(x){
  es = sqrt(qchisq(p=x[,'P_SV'], df=1, lower.tail=FALSE) / x[,'Ntotal_SV'])
  es = fisherZ(es)
  sv = 1 / (x[,'Ntotal_SV']-3) 
  return(c(es, sv))
}

Fisherp_handler <- function(x){
  #handle as a chi-square test with only a p-value
  return(Xp_handler(x))
}

OR_handler <- function(x){
  n1 = x[,'Ntotal_SV']/2
  n2 = n1
  if(!is.na(x[,'Ndel_SV']) &&  !is.na(x[,'Nnondel_SV'])) {
    n1=x[,'Ndel_SV'] ; n2 = x[,'Nnondel_SV']
  }
  if(!is.na(x[,'Nma_SV']) &&  !is.na(x[,'Nnonma_SV'])) {
    n1=x[,'Nma_SV'] ; n2 = x[,'Nnonma_SV']
  } 
  d  = log(x[,'Value_SV'])*sqrt(3)/ pi  # convert to d  (equation from Borenstein et al)
  a  = (n1+n2)^2 / (n1*n2)
  es = d / sqrt(d^2 + a)                # covert to r   (equation from Borenstein et al)
  es = fisherZ(es)                     # convert to FisherZ
  sv = 1 / (x[,'Ntotal_SV']-3) 
  return(c(es, sv))
}


AOR_handler <- function(x){
  #handle as OR
  return(OR_handler(x))
}

d_handler <- function(x){
  n1 = x[,'Ntotal_SV']/2
  n2 = n1
  if(!is.na(x[,'Ndel_SV']) &&  !is.na(x[,'Nnondel_SV'])) {
    n1=x[,'Ndel_SV'] ; n2 = x[,'Nnondel_SV']
  }
  if(!is.na(x[,'Nma_SV']) &&  !is.na(x[,'Nnonma_SV'])) {
    n1=x[,'Nma_SV'] ; n2 = x[,'Nnonma_SV']
  } 
  a  = (n1+n2)^2 / (n1*n2)
  es = x[,'Value_SV'] / sqrt(x[,'Value_SV']^2 + a) #convert to r  , equation from borenstein et al
  es = fisherZ(es)
  sv = 1 / (x[,'Ntotal_SV']-3) 
  return(c(es, sv))
}

MplusSD_handler <- function(x){
  n1=NA; n2=NA; s1=NA; s2=NA; m1=NA; m2=NA
  n1 =  x[,'Ntotal_SV']/2
  n2 =  x[,'Ntotal_SV']/2
  if(!is.na(x[,'Ndel_SV']) &&  !is.na(x[,'Nnondel_SV'])) {
    n1=x[,'Ndel_SV'] ; n2 = x[,'Nnondel_SV']
    m1 = x[,'MeanDel_SV'] ; m2=  x[,'MeanNonDel_SV']
    s1=x[,'SDDel_SV'] ; s2 = x[,'SDNonDel_SV']
  }
  if(!is.na(x[,'Nma_SV']) &&  !is.na(x[,'Nnonma_SV'])) {
    n1=x[,'Nma_SV'] ; n2 = x[,'Nnonma_SV']
    m1 = x[,'MeanMa_SV'] ; m2=  x[,'MeanNonMa_SV']
    s1=x[,'SDMa_SV'] ; s2 = x[,'SDNonMa_SV']} 
  sp = sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2 ) / (n1+n2-2))
  d  = abs(m1-m2) / sp  #d
  es = d / sqrt(d^2 + 4)  # convert to r
  es = fisherZ(es)
  sv = 1 / (x[,'Ntotal_SV']-3) 
  return(c(es,sv) )
}

t_handler <- function(x){
  # assuming "pooled" independent samples t-test
  if(is.na(x[,'Value_SV'])){ 
    return(MplusSD_handler(x))
    }
  else{
    #  t = diff/se, se = diff/t
    #  d = diff/sp
    #  se = sp * sqrt(1/n1 + 1/n2)
    #  sp = se/sqrt(1/n1 + 1/n2)  
    #  sp = (diff/t) / sqrt(1/n1 + 1/n2)
    #  d = diff/sp = diff / ( (diff/t) / sqrt(1/n1 + 1/n2)) = sqrt(1/n1 + 1/n2) * t
    #  split ntotal in two equal groups, however, if nsubgroups available use that instead 
    n1 =  x[,'Ntotal_SV']/2
    n2 =  x[,'Ntotal_SV']/2
    if(!is.na(x[,'Ndel_SV']) &&  !is.na(x[,'Nnondel_SV'])) {n1=x[,'Ndel_SV'] ; n2 = x[,'Nnondel_SV']}
    if(!is.na(x[,'Nma_SV']) &&  !is.na(x[,'Nnonma_SV'])) {n1=x[,'Nma_SV'] ; n2 = x[,'Nnonma_SV']}
    d  =  sqrt((1/n1) + (1/n2)) * x[,'Value_SV']   # convert to d
    a  = (n1+n2)^2 / (n1*n2)
    es = d / sqrt(d^2 + a)  # convert to r
    es = fisherZ(es)
    sv = 1 / (x[,'Ntotal_SV']-3) 
    return(c(es,sv) )
  }
}

percentages_handler<- function(x){
  if(!is.na(x[,'Nma_SV'])){
    a = x[,'PercDELinMA'] * x[,'Nma_SV']; b = x[,'PercDELinCONTROL'] * x[,'Nnonma_SV']
    c = x[,'Nma_SV'] - a; d =  x[,'Nnonma_SV'] - b
  }
  if(!is.na(x[,'Ndel_SV'])) {
    a = x[,'PercMAinDEL'] * x[,'Ndel_SV']; b = x[,'PercMAinCONTROL'] * x[,'Nnondel_SV']
    c = x[,'Ndel_SV'] - a; d =  x[,'Nnondel_SV'] - b
  }
  lor = abs( log((a*d) / (b*c) ))# always make positive, take direction into account later 
  es  = lor*sqrt(3) / pi  # convert lor to d
  es  = es / sqrt(es^2 + 4)                # covert to r
  es  = fisherZ(es)                     # convert to FisherZ
  sv  = 1 / (x[,'Ntotal_SV']-3) 
  return(c(es, sv)) 
}

default_handler <- function(x){
  warning(paste("oh no! Handler not yet implemented for: " , x['Statistic_SV']))
  return(c(NA,NA))
}


##convert effect sizes
func <- function(x){
  tmp <- switch(x[1, "Statistic_SV"],
                     "d"                                                  = d_handler(x),
                     "r"                                                  = r_handler(x),
                     "β (standardized)"                                   = beta_handler(x),
                     "β"                                                  = beta_handler(x),
                     "b"                                                  = B_handler(x),
                     "b (unstandardized from negative binomial models)"   = default_handler(x),
                     "b (negative binomial regression coefficient)"       = default_handler(x),
                     "X"                                                  = X_handler(x),
                     "Chi-test, only p-value given"                       = Xp_handler(x),
                     "Fisher's test, only p-value given"                  = Fisherp_handler(x),
                     "OR"                                                 = OR_handler(x),
                     "AOR"                                                = AOR_handler(x),
                     "Percentages"                                        = percentages_handler(x),
                     "M + SD"                                             = MplusSD_handler(x),
                     "t"                                                  = t_handler(x),
                     "SEM, auteurs gemaild voor univariate statistieken." = default_handler(x),
                     default_handler(x)
  )
  tmp[1] =  (-1+2*(x[,'Direction_SV']%%2) ) * abs(tmp[1])  # odd numbers in x[, 'Direction_SV' ] indicate a positive direction
return(tmp)
}

#' Calculate I-squared values and variance distribution for multilevel meta-analysis models
#'
#' This function calculates values of \eqn{I^2} and the variance distribution for multilevel meta-analysis
#' models fitted with \code{\link[metafor]{rma.mv}}.
#'
#'
#' @usage mlm.variance.distribution(x)
#'
#' @param x An object of class \code{rma.mv}. Must be a multilevel model with two random effects (three-level meta-analysis model).
#'
#' @details This function estimates the distribution of variance in a three-level meta-analysis
#' model (fitted with the \code{\link[metafor]{rma.mv}} function). The share of variance attributable to
#' sampling error, within and between-cluster heterogeneity is calculated,
#' and an estimate of \eqn{I^2} (total and for Level 2 and Level 3) is provided. The function uses the formula by
#' Cheung (2014) to estimate the variance proportions attributable to each model component and to derive the \eqn{I^2} estimates.
#'
#'
#' @references
#'
#' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/mlma.html}{Chapter 12}.
#'
#'Cheung, M. W. L. (2014). Modeling dependent effect sizes with three-level meta-analyses: a structural equation modeling approach. \emph{Psychological Methods, 19}(2), 211.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @aliases var.comp
#'
#' @import ggplot2
#' @importFrom stats model.matrix
#'
#' @return Returns a data frame containing the results. A plot summarizing the variance distribution and \eqn{I^2} values can be generated using \code{plot}.
#'
#' @export mlm.variance.distribution
#' @export var.comp
#'
#' @examples
#' # Use dat.konstantopoulos2011 from the "metafor" package
#' library(metafor)
#'
#' # Build Multilevel Model (Three Levels)
#' m = rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011)
#'
#' # Calculate Variance Distribution
#' mlm.variance.distribution(m)
#'
#' # Use alias 'var.comp' and 'Chernobyl' data set
#' data("Chernobyl")
#' m2 = rma.mv(yi = z, V = var.z, data = Chernobyl, random = ~ 1 | author/es.id)
#' res = var.comp(m2)
#'
#' # Print results
#' res
#'
#' # Generate plot
#' plot(res)



mlm.variance.distribution = var.comp = function(x){

  m = x

  # Check class
  if (!(class(m)[1] %in% c("rma.mv", "rma"))){
    stop("x must be of class 'rma.mv'.")
  }

  # Check for three level model
  if (m$sigma2s != 2){
    stop("The model you provided does not seem to be a three-level model. This function can only be used for three-level models.")
  }

  # Check for right specification (nested model)
  if (sum(grepl("/", as.character(m$random[[1]]))) < 1){
    stop("Model must contain nested random effects. Did you use the '~ 1 | cluster/effect-within-cluster' notation in 'random'? See ?metafor::rma.mv for more details.")
  }

  # Get variance diagonal and calculate total variance
  n = m$k.eff
  vector.inv.var = 1/(diag(m$V))
  sum.inv.var = sum(vector.inv.var)
  sum.sq.inv.var = (sum.inv.var)^2
  vector.inv.var.sq = 1/(diag(m$V)^2)
  sum.inv.var.sq = sum(vector.inv.var.sq)
  num = (n-1)*sum.inv.var
  den = sum.sq.inv.var - sum.inv.var.sq
  est.samp.var = num/den

  # Calculate variance proportions
  level1=((est.samp.var)/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level2=((m$sigma2[2])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level3=((m$sigma2[1])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)

  # Prepare df for return
  Level=c("Level 1", "Level 2", "Level 3")
  Variance=c(level1, level2, level3)
  df.res=data.frame(Variance)
  colnames(df.res) = c("% of total variance")
  rownames(df.res) = Level
  I2 = c("---", round(Variance[2:3], 2))
  df.res = as.data.frame(cbind(df.res, I2))

  totalI2 = Variance[2] + Variance[3]


  # Generate plot
  df1 = data.frame("Level" = c("Sampling Error", "Total Heterogeneity"),
                  "Variance" = c(df.res[1,1], df.res[2,1]+df.res[3,1]),
                  "Type" = rep(1,2))

  df2 = data.frame("Level" = rownames(df.res),
                   "Variance" = df.res[,1],
                   "Type" = rep(2,3))

  df = as.data.frame(rbind(df1, df2))


  g = ggplot(df, aes(fill=Level, y=Variance, x=as.factor(Type))) +
    coord_cartesian(ylim = c(0,1), clip = "off") +
    geom_bar(stat="identity", position="fill", width = 1, color="black") +
    scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color="black"),
          axis.line.y = element_blank(),
          axis.title.y=element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_line(lineend = "round"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm"),
          axis.ticks.length=unit(.25, "cm"),
          plot.margin = unit(c(1,3,1,1), "lines")) +
    scale_fill_manual(values = c("darkseagreen3", "deepskyblue3", "darkseagreen2",
                                 "deepskyblue1", "deepskyblue2")) +

    # Add Annotation

    # Total Variance
    annotate("text", x = 1.5, y = 1.05,
             label = paste("Total Variance:",
                           round(m$sigma2[1]+m$sigma2[2]+est.samp.var, 3)), size = 6) +

    # Sampling Error
    annotate("text", x = 1, y = (df[1,2]/2+df[2,2])/100,
             label = paste("Sampling Error Variance: \n", round(est.samp.var, 3)), size = 7) +

    # Total I2
    annotate("text", x = 1, y = ((df[2,2])/100)/2-0.02,
             label = bquote("Total"~italic(I)^2*":"~.(round(df[2,2],2))*"%"), size = 7) +
    annotate("text", x = 1, y = ((df[2,2])/100)/2+0.05,
             label = paste("Variance not attributable \n to sampling error: \n", round(m$sigma2[1]+m$sigma2[2],3)), size = 7) +

    # Level 1
    annotate("text", x = 2, y = (df[1,2]/2+df[2,2])/100, label = paste("Level 1: \n",
                                                                       round(df$Variance[3],2), "%", sep=""), size = 7) +

    # Level 2
    annotate("text", x = 2, y = (df[5,2]+(df[4,2]/2))/100,
             label = bquote(italic(I)[Level2]^2*":"~.(round(df[4,2],2))*"%"), size = 7) +

    # Level 3
    annotate("text", x = 2, y = (df[5,2]/2)/100,
             label = bquote(italic(I)[Level3]^2*":"~.(round(df[5,2],2))*"%"), size = 7)

  returnlist = list(results = df.res,
                    totalI2 = totalI2,
                    plot = g)
  class(returnlist) = c("mlm.variance.distribution", "list")

  return(invisible(returnlist))

  returnlist

}



