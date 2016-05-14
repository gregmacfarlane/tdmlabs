Distribution Lab Solution
========================================================

We have a model with three zones. The productions and attractions vectors, as well as the cost matrix, are given below.

```r
P <- c(100, 200, 100)
A <- c(200, 50, 150)
costmatrix <- matrix(c(2, 5, 4, 5, 2, 3, 4, 3, 2), nrow = 3, byrow = TRUE)
observed <- matrix(c(80, 5, 15, 80, 40, 80, 40, 5, 55), nrow = 3, byrow = TRUE)
```


The script to compute the gravity model is given below.


```r
gravityModel <- function(P, A, triptime, b = 1, tol = 0.01) {
    
    ASTAR = A
    DIFFERENCE = Inf
    TRIPS1 = matrix(rep(0, 9), nrow = 3, ncol = 3)
    
    for (i in 1:100) {
        if (DIFFERENCE < tol) 
            break
        
        TRIPS <- P %o% ASTAR * triptime^-b/colSums(ASTAR * triptime^-b)
        
        ASTAR <- ASTAR * A/colSums(TRIPS)
        
        DIFFERENCE <- sum(abs(TRIPS1 - TRIPS))
        TRIPS1 <- TRIPS
        # print(TRIPS)
    }
    
    return(TRIPS)
}
```


Let's find the optimum value of $b$ by minimizing an absolute error objective function.


```r
minimumObjective <- function(b, P, A, triptime, observed) {
    predicted <- gravityModel(P, A, triptime, b, tol = 0.01)
    stat <- sum(abs(predicted - observed))
    return(stat)
}

saebest <- nlm(f = minimumObjective, p = 1, P = P, A = A, triptime = costmatrix, 
    observed = observed)
saebest
```

```
## $minimum
## [1] 13.15
## 
## $estimate
## [1] 1.329
## 
## $gradient
## [1] 6.864
## 
## $code
## [1] 2
## 
## $iterations
## [1] 8
```


According to this minimization process, the best is $b=1.3293$ if we judge by sum of absolute error. We can use this estimate to predict the number of trips after we make the improvements to the network:


```r
# Original trips
gravityModel(P, A, costmatrix, saebest$estimate)
```

```
##       [,1]   [,2]  [,3]
## [1,] 80.13  3.362 16.50
## [2,] 80.00 38.351 81.65
## [3,] 39.87  8.287 51.85
```

```r

# What if improvment?
improvements <- matrix(c(2, 3, 4, 3, 2, 3, 4, 3, 2), nrow = 3, byrow = TRUE)
gravityModel(P, A, improvements, saebest$estimate)
```

```
##       [,1]  [,2]  [,3]
## [1,] 71.18  8.16 20.66
## [2,] 96.79 32.61 70.61
## [3,] 32.04  9.23 58.73
```

