# Function for estimating VIF as a function of c-statistic and
# prevalence of treatment.
#
# THIS SOFTWARE IS PROVIDED FOR ILLUSTRATIVE PURPOSES AND COMES WITH ABSOLUTELY
#
# NO WARRANTY.
#
# Austin, P. C. (2021). Informing power and sample size calculations when
# using inverse probability of treatment weighting using the propensity score.
# Statistics in Medicine, 2, 1-14. https://doi.org/10.1002/sim.9176

library(rms)

vif <- function(auc, prev.treat){
  # auc: c-statistic of PS model.
  # prev.treat: Prevalence of treatment.
  #set.seed(1)
  
  N <- 1e7
  # Population size.
  
  ################################################################################
  # Generate covariate.
  ################################################################################
  
  x <- rnorm(N)
  # Assume standardized so that it is standard normal.
  
  ################################################################################
  # Generate beta for PS model.
  ################################################################################
  
  # auc = phi(sigma*beta/sqrt(2))
  
  beta.ps <- sqrt(2) * qnorm(auc)
  
  ################################################################################
  # Determine intercept for PS model for desired prevalance of treatment.
  ################################################################################
  
  bias <- 1
  lower.int <- -20
  upper.int <- 20
  
  while (bias > 0.0001){
    
    beta0.treat <- (lower.int + upper.int)/2
    
    # Generate treatment status for each subject.
    
    logit.treat <- beta0.treat + beta.ps*x
    p.treat <- exp(logit.treat)/(1 + exp(logit.treat))
    treat <- rbinom(N,1,p.treat)
    emp.prev.treat <- mean(treat)
    bias <- abs(prev.treat - emp.prev.treat)
    
    if (emp.prev.treat > prev.treat) {
      upper.int <- beta0.treat
    } else {
      lower.int <- beta0.treat
    }
    
    remove(logit.treat,p.treat,emp.prev.treat)
  }
  
  remove(bias,lower.int,upper.int)
  
  ################################################################################
  # Compute weights
  ################################################################################
  
  psm <- lrm(treat ~ x)
  auc.emp <- psm$stats["C"]
  ps <- exp(psm$linear.predictor)/(1 + exp(psm$linear.predictor))
  iptw <- (treat/ps) + (1-treat)/(1-ps)
  att <- treat + (1-treat)*(ps/(1-ps))
  ow <- treat*(1-ps) + (1-treat)*ps
  mw <- treat*pmin(ps,1-ps)/ps + (1-treat)*pmin(ps,1-ps)/(1-ps)
  entropy <- treat*(-ps*log(ps) - (1-ps)*log(1-ps))/ps +
    (1-treat)*(-ps*log(ps) - (1-ps)*log(1-ps))/(1-ps)
  
  remove(x,beta.ps,psm,ps)
  
  ################################################################################
  # Compute VIFs
  ################################################################################
  
  N1 <- sum(treat)
  N0 <- N - N1
  
  VIF.iptw <- (N1*N0/N) * (sum(treat*iptw*iptw)/((sum(treat*iptw))^2) +
                             sum((1-treat)*iptw*iptw)/((sum((1-treat)*iptw))^2) )
  
  VIF.att <- (N1*N0/N) * (sum(treat*att*att)/((sum(treat*att))^2) +
                            sum((1-treat)*att*att)/((sum((1-treat)*att))^2) )
  
  VIF.ow <- (N1*N0/N) * (sum(treat*ow*ow)/((sum(treat*ow))^2) +
                           sum((1-treat)*ow*ow)/((sum((1-treat)*ow))^2) )
  
  VIF.mw <- (N1*N0/N) * (sum(treat*mw*mw)/((sum(treat*mw))^2) +
                           sum((1-treat)*ow*ow)/((sum((1-treat)*ow))^2) )
  
  VIF.entropy <- (N1*N0/N) * (sum(treat*entropy*entropy)/((sum(treat*entropy))^2) +
                                sum((1-treat)*ow*ow)/((sum((1-treat)*ow))^2) )
  
  remove(N1,N0,iptw,att,ow,mw,entropy)
  
  return(c(AUC        = auc,
           AUC_emp    = auc.emp,
           Prev_treat = prev.treat,
           VIF_IPTW   = VIF.iptw,
           VIF_ATT    = VIF.att,
           VIF_OW     = VIF.ow,
           VIF_MW     = VIF.mw,
           VIF_entropy= VIF.entropy)
         )
}

vif(0.8, .7)

