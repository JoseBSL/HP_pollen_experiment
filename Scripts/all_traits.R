#In this script I´m going to prepare the full data.frame of traits for analyses
#First I load an existing table of traits and then I complete it

#load data
traits_all <- read.csv("Data/tab.csv", sep="", stringsAsFactors = F)
#remove useless columns 
traits_all <- traits_all[ ,-c(1,2,4,7,10,12)]

#Making numerical the stigma type
traits_all[2] <- 1
traits_all[2] <- as.numeric(traits_all$stigma_type)
traits_all[1:4,2] <- 0

#Incompatibility instead of using binary for it I´m going to use the percetage of selfing 
#Obtained with hand self pollination

#Load csv with selfing rates (It was done on the script "Analysis_mantel_test)

selfing <- read.csv("Data/selfing_rate.csv")

selfing=selfing[,-1]
colnames(selfing)[2]<- "Selfing_rate"
#Ordering alphabetically to merge with traits_all
selfing <- selfing[order(selfing$Species), ]
#Ordering too traits all
traits_all <- traits_all[order(traits_all$species),]

#Overwrite column with new values (numerical ones instead of categorical)
traits_all[,3] <- selfing[,2]
colnames(traits_all)[3]<- "Selfing_rate"


#Add other numerical trais


