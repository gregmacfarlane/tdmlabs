# need to clean the model output file to join to Shapefile

library(foreign)
library(plyr)
#Need to get link volumes from OD file
ODvolumes <- read.dbf("~/Desktop/SaltLakeCity/V7_103759/Base2009_Demo/4_ModeChoice/Mo/Boardings/Base2009_2_OD_Station_Detail.dbf")
#Need to get frequencies from PA file
PAvolumes <- read.dbf("~/Desktop/SaltLakeCity/V7_103759/Base2009_Demo/4_ModeChoice/Mo/Boardings/Base2009_1_PA_Station.dbf")
# only care about rail modes and some variables
ODvolumes <- ODvolumes[ ODvolumes$MODE > 6, c("SEQ1", "NAME", "A", "DIST", "D_TOT_VOL")]
PAvolumes <- PAvolumes[ PAvolumes$MODE > 6, c("NAME", "PK_FREQ", "OK_FREQ")]
# join frequencies from PA to volumes from OD
Volumes <- join(ODvolumes, PAvolumes, by="NAME", type="left", match = "first")

# calculate revenue miles on each section of rail
# mi * (min/hr / min/train * hr/day) = train-mi/day
Volumes$RevMi <- Volumes$DIST * ((60/ Volumes$PK_FREQ)*6 + # six peak hours
                                 (60/ Volumes$OK_FREQ)*10) # ten off-peak hours
# $7 per revenue mile
# $/mile * miles/riders = $/rider
Volumes$INVperRID <- 7 * (Volumes$RevMi/Volumes$D_TOT_VOL)


# also make a unique ID to join with ArcGIS
#links are identified by beginning and ending, want to slide A to create B
lagpad <- function(x, k=1) {
  i<-is.vector(x)
  if(is.vector(x)) x<-matrix(x) else x<-matrix(x,nrow(x))
  if(k>0) {
    x <- rbind(matrix(rep(NA, k*ncol(x)),ncol=ncol(x)), matrix(x[1:(nrow(x)-k),], ncol=ncol(x)))
  }
  else {
    x <- rbind(matrix(x[(-k+1):(nrow(x)),], ncol=ncol(x)),matrix(rep(NA, -k*ncol(x)),ncol=ncol(x)))
  }
  if(i) x[1:length(x)] else x
}
Volumes$B <- lagpad(Volumes$A, k=-1)

# ID = A.B
Volumes$ID <- paste(Volumes$A, Volumes$B, sep=".")

write.dbf(Volumes["DIST" > 0,], file="CrunchedVolumes.dbf")
