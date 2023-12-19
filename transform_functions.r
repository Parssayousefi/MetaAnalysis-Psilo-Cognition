

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
