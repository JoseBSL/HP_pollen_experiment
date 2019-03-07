#In this script I'm going to perform GLM between Hp effect and the traits

#load data
hp <- read.csv("Data/y.csv")
hp_mean <- read.csv("Data/y_mean_scale.csv")

#Now I'm using the HP effect calculated in Analysis_mantel_test
#But maybe I could use later effect sizes...

#Now I'm going to match all the traits with these data.frames
traits_all <- read.csv("Data/traits_all.csv", sep=",")
#I create a column with equal code of species in order to merge
traits_all$species <- c("BROL", "BRRA", "CAAN", "ERSA", "IPAQ", "IPPU", 
                        "PEIN", "SIAL", "SOLY", "SOME") 

#The column of selfing rate I'm going to add SI values



#colname to merge
colnames(traits_all)[2] <- "Species"

#I use the mean effect for each treatment, later maybe I come back and use the other
data <- merge(hp_mean, traits_all, by="Species")



