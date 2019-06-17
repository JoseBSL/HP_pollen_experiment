# Creating Raw data table of pollen count

#IPOMOEA_PURPUREA_focal| SOME and ERSA
#ippu-some
IPPU_focal_SOME_non_focal = matrix(c(137, 136, 141, 612, 116, 359), nrow=3, ncol=2) 
colnames(IPPU_focal_SOME_non_focal) <- c("IPPU_pollen","SOME_pollen")
IPPU_focal_SOME_non_focal=data.frame(IPPU_focal_SOME_non_focal)
library(dplyr)
IPPU_focal_SOME_non_focal_mean <- IPPU_focal_SOME_non_focal %>%summarise_all(funs(mean))
IPPU_focal_SOME_non_focal_mean$IPPU <- IPPU_focal_SOME_non_focal_mean$IPPU_pollen/sum(IPPU_focal_SOME_non_focal_mean[1,])
IPPU_focal_SOME_non_focal_mean$SOME <- 1-IPPU_focal_SOME_non_focal_mean$IPPU
IPPU_focal_SOME_non_focal %>%summarise_all(funs(sum))

barplot(as.numeric(IPPU_focal_SOME_non_focal_mean[1,c(3:4)]))




#Calculate percentage by hand no time...







#ippu-ersa
IPPU_focal_ERSA_non_focal = matrix(c(21, 14, 46, 33, 334, 280), nrow=3, ncol=2) 
colnames(IPPU_focal_ERSA_non_focal) <- c("IPPU_pollen","ERSA_pollen")
IPPU_focal_ERSA_non_focal=data.frame(IPPU_focal_ERSA_non_focal)
library(dplyr)
IPPU_focal_ERSA_non_focal %>%summarise_all(funs(mean))
IPPU_focal_ERSA_non_focal %>%summarise_all(funs(sum))

