# Classes and methods for using apsrtable for spatial autoregressive models
#   provided in the spdep package by Roger Bivand.
#
#   September 2012
#   Greg Macfarlane, Georgia Tech

#==============================================================================
# Spatial Auto Regressive/Spatial Durbin 
#==============================================================================
# Create class for the summary method.
setClass("summary.sarlm", 
         representation("$\\rho$" = "numeric",
                        "$p(\\rho)$" = "numeric",
                        "$N$" = "numeric",
                        "AIC" = "numeric",
                        "\\mathcal{L}" = "numeric")
         )

# Fix the coef method for sarlm summary classes. 
#   - coef(summmary) should provide table of coefficients.
#setMethod("coef", "apsrtableSummary.sarlm", function(object) object$coefficients )
setMethod("coef", "summary.sarlm", function(object) object$coefficients )

# Model summary statistics
setMethod("modelInfo", "summary.sarlm", function(x){
  env <- sys.parent()
  digits <- evalq(digits, envir=env)
  model.info <- list(
    # Number of observations
    "$N$" = length(x$fitted.values),
    # Akaike Information Criterion
    "AIC" = formatC(AIC(x), format="f", digits=digits),
    # R-squared
    #"$R^2$"= formatC(x$r.squared, format="f", digits=digits),
    # Log-likelihood for linear model
    #"$\\log(\\mathcal{L}_{LM})$" = formatC(unname(x$LR1$estimate[2]),
    #                                     format="f", digits=digits),
    # Log-likelihood
    "$\\log(\\mathcal{L})$" = formatC(x$LL, format="f", digits=digits)
    # LR Test statistic
    #"LRT (vs LM)" = formatC(unname(x$LR1$statistic[1]), format="f", digits=digits),
    # test p-value
    #"$p$(LRT)" = formatC(unname(x$LR1$p.value[1]), format="f", digits=digits)
  )
  class(model.info) <- "model.info"
  return(model.info)
})

# Get the right coefficients from the mlogit summary
"apsrtableSummary.sarlm" <- function (x){
  s <- summary(x)
  
  # calculate R^2
  mss <- sum((x$fitted.values - mean(x$fitted.values))^2)
  rss <- sum(x$residuals^2)
  s$r.squared <- mss/(mss + rss)
  
  # Get rho for SAR and SDM, place in coef table
  if(s$type == "lag" | s$type == "mixed"){
    s$rholine <- c(unname(s$rho), s$rho.se, unname(s$rho/s$rho.se),
                   unname(2 * (1 - pnorm(abs(s$rho/s$rho.se)))))
    s$Coef <- rbind(s$rholine, s$Coef)
    rownames(s$Coef)[1] <- "rho"
  }
  # Get lambda for SEM, place in coef table
  if(s$type == "error"){
    s$lambdaline <- c(unname(s$lambda), s$lambda.se, unname(s$lambda/s$lambda.se),
                      unname(2 * (1 - pnorm(abs(s$lambda/s$lambda.se)))))
    s$Coef <- rbind(s$Coef, s$lambdaline)
    rownames(s$Coef)[nrow(s$Coef)] <- "lambda"
  }
  s$coefficients <- s$Coef
  return(s)
}

#==============================================================================
# Update LM for better comparison
#==============================================================================
# Model summary statistics
setMethod("modelInfo", "summary.lm", function(x){
  env <- sys.parent()
  digits <- evalq(digits, envir=env)
  model.info <- list(
    # Number of observations
    "$N$" = length(x$residuals),
    # Akaike Information Criterion
    "AIC" = formatC(x$AIC, format="f", digits=digits),
    # Log-Likelihood
    "$\\log(\\mathcal{L})$" = formatC(x$LL, format="f", digits=digits)
    # R-squared
    #"$R^2$" = formatC(x$r.squared, format="f", digits=digits)
  )
  class(model.info) <- "model.info"
  return(model.info)
})

# Get the right coefficients from the mlogit summary
"apsrtableSummary.lm" <- function (x){
  s <- summary(x)
  s$AIC <- AIC(x)
  s$LL <- logLik(x)
  return(s)
}

