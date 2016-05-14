# Class Notes
library(spdep)
library(ggmap
# Load Data
load("./HomePrices.Rdata")
str(ClassPointsLL)

# Get Stamen map using our points as a bouding box.
Atlanta <- get_map(bbox(ClassPointsLL), source='stamen',
                   maptype='toner')

AtlantaMap <- ggmap(Atlanta, extent="device")


# Make spatial weights matrix by 50 nearest
points.knn <- knn2nb(knearneigh(ClassPointsLL, k=50))
dists  <- lapply(nbdists(points.knn, ClassPointsLL), function(x) x)
dists.inv <- lapply(dists, function(x) 1/(x+0.01))
W <- nb2listw(points.knn, glist=dists.inv, style="W")

# Calculate average neighbor distance for each observation
ClassPointsLL$meanNB <- unlist(lapply(dists, mean))

# Calculate Moran's I test statistic
price.mI <- moran.test(price.ols$residuals, W)
price.mI

ClassPointsLL$OLSresid <- price.ols$residuals
AtlantaMap + geom_point(aes(x=X, y=Y, color=cut(OLSresid, 
                            breaks=quantile(OLSresid,
                                            probs=seq(0,1,0.2)))), 
                        data = ClassPointsLL@data) + 
  scale_color_brewer(palette="RdBu", type="div", name= "Residual")

# Spatial models
price.sar <- lagsarlm(formula(price.ols), listw=W, data=ClassPointsLL@data)
price.sem <- errorsarlm(formula(price.ols), listw=W, data=ClassPointsLL@data)
price.sdm <- lagsarlm(formula(price.ols), listw=W, type="mixed", 
                      data=ClassPointsLL@data)


# Effects

# Need to get the trace of the powers of the weights matrix to simulate the effects
# you don't have to understand it to use it; come to me if you have a need for understanding.
Wmat <- as(as_dgRMatrix_listw(W), "CsparseMatrix") # convert to actual matrix object instead of list
trWmat <- trW(Wmat) # calculate trace of powers of W

# simulate the effects (you should use more than 10 draws to get accurate t-stats)
impacts.sar <- impacts(price.sar, tr=trWmat, R= 10)
summary(impacts.sar, zstats=TRUE)
