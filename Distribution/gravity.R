# Function to execute a gravity model using the convergence method
#  	given in Meyer and Miller, chapter 5, problem 6.
gravityModel <- function(productions, attractions, cost.matrix, b, TOLERANCE) {
  
  # Input Checks
  # ==================================================================
  
  # Check that productions and attractions are the same size
  if(length(productions) != length(attractions))
    stop("The production and attraction vectors do not match.")
  
  # Make sure b > 0
  if(b <= 0)
    stop("The cost-sensitivity coefficient b must be positive.")
  
  # Tolerance > 1
  if(TOLERANCE <= 0)
    stop("The tolerance must be positive.")
  
  
  # Initialize convergence conditions
  # ==================================================================
  # run until difference between Trips, Trips1 fall below tolerance 
  DIFFERENCE = Inf;
  TRIPS  = matrix(rep(NA, length(productions)*length(attractions)),
                  nrow=length(productions), ncol=length(attractions))
  TRIPS1 = matrix(rep(0, length(productions)*length(attractions)),
                  nrow=length(productions), ncol=length(attractions))
  ASTAR = attractions
  
  
  # Program Loop
  # ==================================================================
  for(iteration in 1:100){ # maximum 100 iterations
    # check convergence
    if(DIFFERENCE < TOLERANCE) break
    
    # Calculate Trips from i to j
    for(i in 1:length(productions)){
      for(j in 1:length(attractions)){
        TRIPS[i,j] = ( productions[i] * ASTAR[j] * ( cost.matrix[i,j] )^-b ) /
          ( sum(ASTAR * ( cost.matrix[i,] )^-b ));
      }
    }
    
    # Update attractions
    ASTAR1 = ASTAR * ( attractions / colSums( TRIPS ))
    ASTAR = ASTAR1
    
    # Calculate convergence condition
    DIFFERENCE = sum( abs( TRIPS1 - TRIPS ))
    TRIPS1 = TRIPS
    
  }
  #print(paste("Iterations: ", iteration, sep=""))
  return(TRIPS)
}


# Create a cost matrix based on distance and transfer assumptions

costMatrix <- function(distance, transfer) {
  # Graph of distances between stations
  d.matrix <- matrix(c(Inf,1,3,4,2,4,3,3,4,
                       1,Inf,2,3,1,3,2,2,3,
                       3,2,Inf,1,1,3,2,2,3,
                       4,3,1,Inf,2,4,3,3,4,
                       2,1,1,2,Inf,2,1,1,2,
                       4,3,3,4,2,Inf,1,3,4,
                       3,2,2,3,1,1,Inf,2,3,
                       3,2,2,3,1,3,2,Inf,1,
                       4,3,3,4,2,4,3,1,Inf),
                     nrow=9, ncol=9, TRUE)
  # Matrix indicating whether a tranfer penalty should be applied
  t.matrix <- matrix(c(0, 0, 0, 0, 0, 1, 1, 1, 1,
                       0, 0, 0, 0, 0, 1, 1, 1, 1,
                       0, 0, 0, 0, 0, 1, 1, 1, 1,
                       0, 0, 0, 0, 0, 1, 1, 1, 1,
                       0, 0, 0, 0, 0, 0, 0, 0, 0,
                       1, 1, 1, 1, 0, 0, 0, 0, 0,
                       1, 1, 1, 1, 0, 0, 0, 0, 0,
                       1, 1, 1, 1, 0, 0, 0, 0, 0,
                       1, 1, 1, 1, 0, 0, 0, 0, 0),
                     nrow=9,ncol=9, TRUE)
  # return weighted sum of both matrices
  return(distance*d.matrix + transfer*t.matrix)
}


