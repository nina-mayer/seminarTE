rm(list=ls())
library(plyr)
library(survival)
require(xtable)

## load data and define important columns

source("data_teachers.R")
cols <- c(3:7)

#' Used to de-trend X_it
time_trends = function() {
  ## Covariates
  X = matrix(0, nrow=0, ncol=42)

  X = rbind(X, ddply(teachers_full, .(year), summarize, mean(avgteachsal))[, 2])
  X = rbind(X, ddply(teachers_full, .(year), summarize, mean(avginstrucsal))[, 2])
  X = rbind(X, ddply(teachers_full, .(year), summarize, mean(studteachratio))[, 2])
  X = rbind(X, ddply(teachers_full, .(year), summarize, mean(nonwageppexpend))[, 2])
  X = rbind(X, ddply(teachers_full, .(year), summarize, mean(South))[, 2])

  rownames(X) = colnames(teachers_full)[cols]
  B_trend = matrix(0, nrow=0, ncol=1)
  for(j in 1:nrow(X)) {
    Xj = X[j, ]
    z = 1:ncol(X)
    B_trend = rbind(B_trend, coef(lm(Xj ~ z))[2])
  }
  rownames(B_trend) = rownames(X)
  

  return(B_trend)
}

#' Main function.
#' adoption = data frame with treatment adoption data (state, when)
#' @return Matrix with propensity scores (P(I_1|..)) for every state.
#' 
synth_fisher = function(adoption, vars=c("nonwageppexpend", "studteachratio"), only.CT=FALSE, verbose=FALSE) {
  
  # fits Xt  ~ t to take out time-effect.
  X_trend = time_trends()
  
  # Week format
  week = sapply(adoption$time, function(s) {
    yr = as.numeric(strsplit(as.character(s), "/")[[1]][2])
    mo = as.numeric(strsplit(as.character(s), "/")[[1]][1])
    12*(yr-1959) + mo   # baseline is 1959.
  })
  
  #' Define covariates.
  #' 
  #' X_ij = covariate j of unit i at time of treatment.
  #' X_ij is defined in terms of "1959 values" where we difference out common time trends.
  #' 
  X = matrix(0, nrow=0, ncol=5)
  colnames(X) = colnames(teachers_full)[cols]
  stopifnot(all(vars %in% colnames(X)))
  
  # Adjust Xit for time trends.
  for(i in 1:nrow(teachers_treat)) {
    yr  = as.numeric(strsplit(as.character(adoption[i,]$time), "/")[[1]][2])
    st = as.character(adoption[i, ]$state)
    # risk_set = AllStates[which(week >= week[i])]
    
    # st_data = subset(smoking, state==st & year==yr)[, c(3:7, 9)]
    x_it = as.numeric(subset(teachers_full, State==st & year==yr)[, cols])
    x_it = x_it - as.numeric(X_trend) * (yr - 1959) 
    
    X = rbind(X, x_it)
  }
  rownames(X) = NULL
  # Update adoption data.
  adoption = cbind(adoption, X)
  head(adoption)
  # state    when  cigsale  lnincome age15to24    retprice unemploy     dems
  # Alabama 05/2004 145.1858  9.765749 0.1755933 -44.3444687 6.510136 84.80464
  
  # Change from (01/1990) -> week format
  adoption$time = week
  head(adoption)
  #' state    when  cigsale  lnincome  age15to24     retprice  unemploy     dems
  #' Alabama  293  145.1858  9.765749  0.1755933  -44.3444687  6.510136   84.8046
  #' ...
  
  status = rep(1, nrow(teachers_treat))
  status[which(teachers_treat$state=="NE")] = 0
  adoption$event = status
  
  surv = with(adoption, Surv(time, event))
  f = as.formula(paste("surv ~ ", paste(vars, collapse="+")))
  
  out = coxph(f, data=adoption)
  if(verbose) {
    print(sprintf("## Model ##"))
    print(sprintf("AIC = %.2f", AIC(out)))
    print(summary(out))
    print("## ##")
  }
  #
  var_ids = as.numeric(sapply(vars, function(v) which(colnames(adoption)==v)))
  
  X = as.matrix(adoption[, var_ids])
  stopifnot(all(names(coef(out)) == colnames(X)))
  
  # hats.
  yhat = exp(X %*% as.numeric(coef(out)))
  ps_hat = yhat / sum(yhat)
  rownames(ps_hat) = teachers_treat$state
  if(only.CT) {
    i = which(rownames(ps_hat)=="CT")
    return(c(ps_hat[i, ], AIC(out)))
  }
  
  ord = rev(order(ps_hat))
  M = data.frame(a = rep(0, 11))
  # matrix(0, nrow=13, ncol=6)
  for(j in 1:3) {
    j1 = 11 * (j-1) + 1
    j_index = ord[seq(j1, j1 + 10)]
    ps_j = round(as.numeric(ps_hat[j_index]), 4)
    names_j = rownames(ps_hat)[j_index]
    
    M = cbind(M, data.frame(State=names_j, PS=ps_j))
  }
  
  M$a = NULL
  rownames(M) = NULL
  
  as.matrix(M)
}

synth_fisher(teachers_treat)


#' Try all possible combinations of models with X1, ... X6
#' 
#' @return Kx3 matrix that contains (pvalue, AIC, #vars) at each row
#' 
single_DATA_analysis = function(adoption, verbose=FALSE) {
  # out = synth_fisher(adoption, vars = c("lnincome", "retprice"), FALSE)
  
  ## All models
  print("- Checking all models with 1-5 variables...")
  pvalues = matrix(0, nrow=0, ncol=3)
  colnames(pvalues) = c("pvalue", "AIC", "vars")
  
  for(num_var in 1:5) {
    # All num_var models
    models = t(combn(colnames(teachers_full)[cols], num_var))
    for(i in 1:nrow(models)) {
      m = models[i, ]
      # print(sprintf("Checking model"))
      # print(m)
      out = synth_fisher(adoption, vars = m, only.CT = TRUE)
      pvalues = rbind(pvalues, c(out, num_var))
    }
  }
  
  rownames(pvalues) = NULL
  #' pvalues = MATRIX (K x 3)
  #'    pvalue  AIC  #vars.
  #'         ....
  return(as.data.frame(pvalues))
}


paper_analysis = function() {
  # single_DATA_analysis(adoption_Data_2, colnames(smoking)[cols])
  all_xnames = colnames(teachers_full)[cols]
  out = synth_fisher(teachers_treat, vars=c("nonwageppexpend", "studteachratio"),verbose=T)
  out
  xtable(out, include.rownames=FALSE)
  
  ## Single data analysis
  pvals = single_DATA_analysis(teachers_treat)
  pvals[which.min(pvals$AIC), ]
}

paper_analysis()
