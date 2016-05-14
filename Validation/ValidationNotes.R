library(ggplot2)
library(foreign)

lm_eqn <- function(m) {

  l <- list(a = format(coef(m)[1], digits = 2),
      b = format(abs(coef(m)[2]), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 3));

  if (coef(m)[2] >= 0)  {
    eq <- gsub("a", l$a, "$y = a + b x, r^2 = r2$")
    eq <- gsub("b", l$b, eq)
    eq <- gsub("r2", l$r2, eq)
  } else {
    eq <- gsub("a", l$a, "$y = a - b x, r^2 = r2$")
    eq <- gsub("b", l$b, eq)
    eq <- gsub("r2", l$r2, eq)
  }

  as.character(as.expression(eq));                 
}


Highways <- read.csv("COMPARE_MODEL_OBS_VOL_BY_AADT_ID.CSV")
Highways <- Highways[ Highways$ObsAWDT != 0,]

modelall <- lm(ModelAWDT ~ ObsAWDT, data = Highways)

AWDTall <- ggplot(data = Highways, aes(y=ModelAWDT, x=ObsAWDT)) + geom_point()
AWDTall <- AWDTall +  annotate("text", x = 50000, y = 150000,  
                                    label = lm_eqn(modelall))
AWDTall +  stat_smooth(method="loess") + theme_bw() +
  xlab("Observed") + ylab("Actual")

types <- c("Freeways", "Multilane", "Arterials", "Rural")
freeways.ft <- c(31, 32)
multi.ft <- 11
arterials.ft <- c(2, 42)
rural.ft <- 22
types.no <- c(freeways.ft, multi.ft, arterials.ft, rural.ft)
T_Highways <- Highways[ Highways$FT %in% types.no,]
T_Highways$FT <- ifelse(T_Highways$FT == 32, 31, T_Highways$FT)
T_Highways$FT <- ifelse(T_Highways$FT == 42, 2, T_Highways$FT)
T_Highways$FTf <- factor(T_Highways$FT, labels=c("Arterials", "Multilane", 
                                                 "Rural", "Freeways"))

modelslist <- list()

for(i in types){
  modelslist[[i]] <- lm(ModelAWDT ~ ObsAWDT, 
                        data = T_Highways[T_Highways$FTf == i,])
}

AWDTmatch <- ggplot(data = T_Highways, aes(y=ModelAWDT, x=ObsAWDT, col=FTf)) + 
  geom_point(size=6, alpha=0.5)
AWDTmatch +  stat_smooth(method=lm) + theme_bw() + 
  facet_grid(.~ FTf, scales="free") + 
  xlab("Observed") + ylab("Actual")


BusesRidership <- read.csv("BusRoutes.csv")
TransitInfo <- read.dbf("~/4_ModeChoice/Mo/Boardings/Base2009_2_Route.dbf")

TransitInfo$MNAME <- gsub("[^0-9]", "", TransitInfo$NAME)

TransitInfo <- merge(TransitInfo, BusesRidership, by.x = "MNAME", by.y = "Route")


