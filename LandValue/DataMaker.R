# Data Maker
# Public data from the FCTAO
library(sp)
library(spdep)
library(rgdal)

load("~/Resiliency/Data/PUBLICDATA.RData")


vars <- c("X2012", "X", "Y", "INCOME2010", "PCTWHITE", "NEAREST_STATION",
          "Acres", "EffAge", "Rmtot", "Rmbed", "Fixbath")

stations <- c("Midtown", "North Avenue","Georgia State", "Civic Center",
              "Peachtree Center", "Five Points", "Garnett", "West End",
              "Inman Park-Reynoldstown", "King Memorial",
              "Ashby", "Dome/GWCC/Philips/CNN", "Oakland City", "Vine City")

ClassPoints <- MyPoints [ MyPoints@data$NEAREST_STATION %in% stations,
                               vars]
ClassPoints <- ClassPoints[ complete.cases(ClassPoints@data),]
ClassPoints <- ClassPoints [ sample(nrow(ClassPoints), 2500, replace=FALSE),] 
ClassPointsLL <- spTransform(ClassPoints, 
                             CRSobj=CRS("+proj=longlat + datum=WGS84"))
ClassPointsLL$X <- ClassPointsLL@coords[,1]
ClassPointsLL$Y <- ClassPointsLL@coords[,2]

save(ClassPointsLL, file="./HomePrices.Rdata")
