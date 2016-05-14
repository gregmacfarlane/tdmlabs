# Newton Maximization Algorithm

OneDNewton <- function(Function, initial=0,
                       tolerance=1e-5, maxiterations=10, verbose=TRUE){
  x <- initial
  if(verbose) outtable <- list()
  for(i in 1:maxiterations){
    x1 <- x - eval(Function)/eval(D(Function, "x"))
    if(verbose){
      outtable[[i]] <- cbind(i, x1, abs(x1-x))
    }
    if(abs(x1 - x) < tolerance) break
    x <- x1
  }
  
  if(verbose) print(matrix(unlist(outtable), nrow=i, byrow=TRUE, 
                           dimnames=list(1:i, c("Iteration", "Solution", "Improvement"))
                           )
                    )
  if(i==maxiterations)print("WARNING: Algorithm did not converge")
  return(list(solution = x1, iterations = i, improvement=abs(x1-x)))
}


NDNewton <- function(Functions, Initial, tolerance=1e-5, maxiterations=10, 
                     verbose=TRUE){
  # length check
  if(length(Functions) != length(Initial)){
    stop("Need an initial value for every variable")
  }
  # assign variables
  numvars <- length(Initial)  
  list2env(Initial, .GlobalEnv)
  if(verbose) outtable <- list()  
  
  #Calculate Jacobian
  J <- t(sapply(Functions, function(ex) lapply(names(Initial),
                                        function(arg) D(ex, arg) ) ) )
  
  for(it in 1:maxiterations){
    Je <- matrix(sapply(J, function(mat) eval(mat)),
                 nrow=numvars, ncol=numvars, byrow=FALSE)    
    y = unlist(Initial) -  solve(Je) %*% sapply(Functions, eval)
    if(norm(y, type="I") < tolerance) break
    
    for(i in 1:length(Initial)){
      Initial[[i]] <- y[i]
    }
    list2env(Initial, .GlobalEnv)
  }
  

  outputlist <- list(Iterations = it, Improve = norm(y, type="I"), Solution = y)
  return(outputlist)  
}





