Day <- read.csv("~/Downloads/Ascii/Ascii/DAYV2PUB.CSV")
HHold <- read.csv("~/Downloads/Ascii/Ascii/HHV2PUB.CSV")

# Only urban vehicles
Day <- Day[which(Day$URBAN != 4), ]
HH <- HH[which(HH$URBAN != 4), ]

# count how many work trips each household makes in a day
day.hbw <- Day[which(Day$TRIPPURP == "HBW"), ]
n.hbw <- aggregate(day.hbw$HOUSEID, list(day.hbw$HOUSEID), length)
HH <- merge(HH, n.hbw, by.x = "HOUSEID", by.y = "Group.1", all.x = TRUE)
colnames(HH)[ncol(HH)] <- "HBWTrips"
HH$HBWTrips[is.na(HH$HBWTrips)] <- 0

# count how many HBO trips each household makes
hbo.types = c("HBO", "HBOSHOP", "HBOSOCREC")
day.hbo <- Day[which(Day$TRIPPURP %in% hbo.types), ]
n.hbo <- aggregate(day.hbo$HOUSEID, list(day.hbo$HOUSEID), length)
HH <- merge(HH, n.hbo, by.x = "HOUSEID", by.y = "Group.1", all.x = TRUE)
colnames(HH)[ncol(HH)] <- "HBOTrips"
HH$HBOTrips[is.na(HH$HBOTrips)] <- 0

# and NHB trips
day.nhb <- Day[which(Day$TRIPPURP == "NHB"), ]
n.nhb <- aggregate(day.nhb$HOUSEID, list(day.nhb$HOUSEID), length)
HH <- merge(HH, n.nhb, by.x = "HOUSEID", by.y = "Group.1", all.x = TRUE)
colnames(HH)[ncol(HH)] <- "NHBTrips"
HH$NHBTrips[is.na(HH$NHBTrips)] <- 0
TripGen <- HH


save(TripGen, file="./TripGen.Rdata")