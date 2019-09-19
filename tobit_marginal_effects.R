tobit_marginal_effects = function(model, x, dummies_indices=c()) {
  # written by dr Rafal Wozniak, Faculty of Economic Sciences, University of Warsaw
  # 2019-04-17
  #
  # arguments:
  # ------------------
  # model - model estimated by censReg function
  # x - characteristics marginal effects are calculated for
  # comments:
  # ------------------
  # Please make sure that the vector contains one for the constant term.
  
  # check if model is of class 'censReg'
  
  
  # estimates without ln(Sigma)
  beta_hat = as.vector(model$estimate[-length(model$estimate)])
  sigma_hat = model$estimate[length(model$estimate)]
  
  # Marginal Effects: Probability Uncensored
  me_prob_uncens = dnorm(t(x)%*%beta_hat/exp(sigma_hat))%*%beta_hat/exp(sigma_hat)
  
  # Marginal Effects: Unconditional Expected Value
  me_uncond_evalue = pnorm(t(x)%*%beta_hat/exp(sigma_hat))%*%beta_hat
  
  # Marginal Effects: Conditional on being Uncensored
  # Xbeta_hat/Sigma xBdS
  xBdS = t(x)%*%beta_hat/exp(sigma_hat)
  me_cond_uncens = (1-dnorm(xBdS)/pnorm(xBdS)*(xBdS+dnorm(xBdS)/pnorm(xBdS)))%*%beta_hat
  
  # dummy variables indices
  dvi = dummies_indices
  if(length(dvi)>0){ 
    x0 = x
    x0[dvi] = 0
    x1 = x0
    
    for(i in 1:length(dvi)) {
      x1[dvi[i]] = 1
      # Marginal Effects: Probability Uncensored
      me_prob_uncens[i] = pnorm(t(x1)%*%beta_hat/exp(sigma_hat))-pnorm(t(x0)%*%beta_hat/exp(sigma_hat))
      
      # Marginal Effects: Conditional on being Uncensored
      # Xbeta_hat/Sigma xBdS
      xBdS1 = t(x1)%*%beta_hat/exp(sigma_hat)
      xBdS0 = t(x0)%*%beta_hat/exp(sigma_hat)
      me_cond_uncens[i] = (t(x1)%*%beta_hat+exp(sigma_hat)*dnorm(xBdS1)/pnorm(xBdS1))-
        (t(x0)%*%beta_hat+exp(sigma_hat)*dnorm(xBdS0)/pnorm(xBdS0))
      
      # Marginal Effects: Unconditional Expected Value
      me_uncond_evalue[i] = (t(x1)%*%beta_hat+exp(sigma_hat)*dnorm(xBdS1)/pnorm(xBdS1))*pnorm(xBdS1)-
        (t(x0)%*%beta_hat+exp(sigma_hat)*dnorm(xBdS0)/pnorm(xBdS0))*pnorm(xBdS0)
      x1[dvi[i]] = 0
    }
  }
  
  # Marginal effects brief table
  varnames = names(model$estimate[-length(model$estimate)])
  names(beta_hat) = varnames
  me = cbind(beta_hat, t(me_uncond_evalue), t(me_cond_uncens), t(me_prob_uncens), x)
  colnames(me) <- c("y*", "E(y|x)", "E(y|x,y>0)", "Pr(y>0|x)", "at X=")
  
  if(length(dvi)>0) {
    rn = rownames(me)
    for(i in 1:length(dvi)) {
      rn[dvi[i]] <- paste(rn[dvi[i]], "!", sep="")
    }
    rownames(me) = rn
  }
  cat("\n Marginal effects of the tobit model \n")
  cat(" ----------------------------------- \n")
  show(me[-1,])
  if(length(dvi)>0){
    cat("(!) indicates marginal effect was calculated for discrete change of dummy variable from 0 to 1")
  }
  
  me = me[-1,]
  return(me)
}