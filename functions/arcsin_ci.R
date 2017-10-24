arcsin.ci <- function(fit, alpha = 0.05, t = NULL) {
  
  #- Extract necessary values from survfit object
  surv.est  <- fit$surv
  sigma     <- fit$std.err #This is sigma, not the SE of S(t)
  surv.time <- fit$time
  
  #- Get limits element wise
  lims <- data.frame(time = fit$time, surv = surv.est,  sigma = fit$std.err)
  
  lims$LL <- sin(sapply(asin(sqrt(surv.est)), function(x) max(0, x)) - 0.5 * qnorm(1 - alpha / 2) * sigma * 
            sqrt(surv.est/ (1 - surv.est)))^2
  
  lims$UL <- sin(sapply(asin(sqrt(surv.est)), function(x) min(pi / 2, x)) +  0.5 * qnorm(1 - alpha / 2) * sigma * 
          sqrt(surv.est / (1 - surv.est)))^2
  
  return(round(lims[which(fit$n.censor == 0), ], 3))
}
