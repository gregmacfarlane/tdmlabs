Vehicles <- read.csv("~/Downloads/Ascii/Ascii/VEHV2PUB.CSV")
require("gdata")
Vehicles$GSCOST <- unknownToNA(Vehicles$GSCOST, -9.0)
Vehicles$BESTMILE <- unknownToNA(Vehicles$BESTMILE, -9.0)
Vehicles$EIADMPG <- unknownToNA(Vehicles$EIADMPG, -9.0)
Vehicles$MSACAT <- unknownToNA(Vehicles$MSACAT, -9)
Vehicles$HTEEMPDN <- unknownToNA(Vehicles$HTEEMPDN, -9)
Vehicles$HTPPOPDN <- unknownToNA(Vehicles$HTPPOPDN, -9)


Vehicles$LOG.BESTMILE <- ifelse(BESTMILE < 1 , 0,  log(BESTMILE))

Vehicles$HHFAMINC <- unknownToNA(Vehicles$HHFAMINC, -9)
Vehicles$HHFAMINC <- unknownToNA(Vehicles$HHFAMINC, -8)
Vehicles$HHFAMINC <- unknownToNA(Vehicles$HHFAMINC, -7)

Vehicles$HHINCOME <- ifelse(Vehicles$HHFAMINC == 18, 100, 
                   ifelse(Vehicles$HHFAMINC == 17, 90, Vehicles$HHFAMINC * 5 -2.5)
) 


make.american <- c("01","02","03","06","07","09",10,12,13,14,18,19,20,21,22,23,24,29)
make.european <- c(30,32,34,39,42,43,45,47,50,51,62,65)
make.asian <- c(25,35,37,38,41,48,49,52,53,54,55,58,59,63,64)
make.motorcycle <- c(71,72,73,76)

Vehicles$ORIGIN <- rep(0, length(Vehicles$MAKECODE))
Vehicles$ORIGIN[which(Vehicles$MAKECODE %in% make.american == TRUE)] <- 1
Vehicles$ORIGIN[which(Vehicles$MAKECODE %in% make.european == TRUE)] <- 2
Vehicles$ORIGIN[which(Vehicles$MAKECODE %in% make.asian == TRUE)] <- 3
Vehicles$ORIGIN[which(Vehicles$MAKECODE %in% make.motorcycle == TRUE)] <- 4

save(Vehicles, file="VehicleData.Rdata")

Vehicles$ORIGIN <- factor(Vehicles$ORIGIN, levels = c(0,1,2,3,4), 
                          labels = c(" Unkown", " American", 
                                     " European", " Asian", " Motorcycle"))

Vehicles$MSARAIL <- factor(Vehicles$MSACAT, levels = c(4,1,2,3),  
                           labels = c(" Not in MSA", " >1 M w/ rail", 
                                      " >1 M w/o rail", " <1 M"))