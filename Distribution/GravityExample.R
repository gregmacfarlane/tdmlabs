# Gravity model function

P <- c(100,200,100)
A <- c(200, 50, 150)
triptime <- matrix(c(2,5,4,
                     5,2,3,
                     4,3,2), 
                   nrow=3, ncol=3)

b <- 1


observed <- matrix(c(80,5,15,80,40,80,40,5,55), nrow=3, byrow=TRUE)

gravityModel <- function(P, A, triptime, b=1, tol = 0.01){
  
  ASTAR = A
  DIFFERENCE = Inf
  TRIPS1 = matrix(rep(0, 9), nrow=3, ncol=3)
  
  for(i in 1:100){
    if(DIFFERENCE < tol) break
    
    TRIPS <- P%o%ASTAR * triptime^-b/
      colSums(ASTAR * triptime^-b)
    
    ASTAR <- ASTAR * A/colSums(TRIPS)
    
    DIFFERENCE <- sum(abs( TRIPS1 - TRIPS ))
    TRIPS1 <- TRIPS
    #print(TRIPS)
  }
  
  print(i)
  return(TRIPS)
}



ranjaniGM <- function(P, A, triptime, b=0.4, tol = 0.01){
  
  ASTAR = A
  DIFFERENCE = Inf
  TRIPS1 = matrix(rep(0, 9), nrow=3, ncol=3)
  
  for(i in 1:100){
    if(DIFFERENCE < tol) break
    
    TRIPS <- P%o%A * triptime^-b/
      colSums(ASTAR * triptime^-b)
    
    
    ASTAR <- ASTAR * A/colSums(TRIPS)
    
    DIFFERENCE <- sum(abs( TRIPS1 - TRIPS ))
    TRIPS1 <- TRIPS
  }
  
  return(TRIPS)
}


