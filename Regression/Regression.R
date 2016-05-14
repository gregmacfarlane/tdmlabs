
## ----setup, echo=FALSE---------------------------------------------------
opts_knit$set(width = 60)


## ----loaddata, echo=TRUE-------------------------------------------------
load("VehicleData.Rdata")


## ----factor, echo=TRUE---------------------------------------------------
Vehicles$MSARAIL <- factor(Vehicles$MSACAT, levels = c(4,1,2,3),  
                           labels = c(" Not in MSA", " >1 M w/ rail", 
                                      " >1 M w/o rail", " <1 M"))


## ----model, echo=TRUE----------------------------------------------------
Vehicles$lBESTMILE <- ifelse(Vehicles$BESTMILE > 1, log(Vehicles$BESTMILE),
                             0)
model1.lm <- lm(lBESTMILE ~ MSARAIL + EPATMPG, data = Vehicles)
summary(model1.lm)


## ----matrixsolution, echo=TRUE-------------------------------------------
X <- model.matrix(~lBESTMILE + MSARAIL + EPATMPG, data = Vehicles)
y <- X[,2]
X <- X[,-2]
# Parameter Estimates
XTX <- solve(t(X) %*% X)
Beta <- XTX %*% t(X) %*% y
# Mean squared error
e <- y - X%*%Beta
s <- mean(e^2)
# Standard errors
SE <- sqrt(diag(s * XTX))
# output table
cbind(Beta, SE, Beta/SE)


## ----linearlik, echo=TRUE------------------------------------------------
linear.lik <- function(theta, y, X){
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)
}


## ----maxlik--------------------------------------------------------------
linear.MLE <- nlm(f=linear.lik, p=c(1,1,1,1,1,1), 
                    hessian=TRUE, y=y, X=X)
par <- linear.MLE$estimate
se <- sqrt(diag(solve(linear.MLE$hessian)))
t <- par/se
pval <- 2 * (1 - pt(abs(t), nrow(X) - ncol(X)))
cbind(par, se, t, pval)


