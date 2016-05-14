# Lecture notes
system <- matrix(c(1,-0.005,0,
                   1,0,-0.02,
                   0,1,1), nrow=3, ncol=3, byrow=TRUE)

solutionF <- function(system, V){
  
  rhs <- c(15,10, V)
  return(solve(system, rhs))
  
}



smallcosts <- matrix(c(15, 0.005, 10, 0.02), nrow=2, byrow=TRUE)


smallue <- function(Vb, V, costfunctions, info=FALSE){
  volumes <- vector("numeric", length=nrow(costfunctions))
  
  # put in volume constraints; these constraints make only one unknown
  volumes[1] <- Vb
  volumes[2] <- V - Vb
  
  # compute travel time on each link
  times <- rowSums(costfunctions * cbind(1, volumes))
  
  # compute path travel times
  
  traveltimes <- times 
  
  # User Equilibrium condition:
  #   At equilibrium, all available paths will have precisely
  #   equal travel times. Want to iterate until minimum is acheived.
  #   (if all equal, max(traveltimes) - min(traveltimes) = 0)
  stat <- max(traveltimes) - min(traveltimes)
  
  
  if(info==TRUE){
    return(list("Link Volumes"= volumes,
                "Link Times" = times,
                "Path Times" = traveltimes))
  }else{
    return(stat)
  }
}