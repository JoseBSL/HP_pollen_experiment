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
morphometry <- read.csv("Data/species_traits.csv")
morphometry <- morphometry[, -c(4:15)]

#Now I have to remove the numbers after "_" 

measurement  <- str_split_fixed(as.character(morphometry$measurement), "_", 3)
measurement <- as.data.frame(measurement)
measurement$new <- paste(measurement$V1, "_", measurement$V2 )
morphometry[,2] <- measurement[,4] 
#Aggregate of different variables 
morphometry <- dcast(species + measurement ~ ., value.var = "um", fun.aggregate = mean, data = morphometry, na.rm= TRUE)
#Some grammar mistakes, at least they seem to be homogeneous among species

#Fix colnames
colnames(morphometry)[3] <- "um"

#First I create the dataframes then I order and finally I insert them in the trait_all data.frame
stigma <-morphometry[grep("stigma", morphometry$measurement),] 
#Stigma area 
stigma_area <-stigma[grep("area", stigma$measurement),] 
#Stigma length
stigma_lenght <-stigma[grep("length", stigma$measurement),] 
#Stigma surface
stigma_surface <-stigma[grep("surface", stigma$measurement),] 
#Stigma width
stigma_width <-stigma[grep("width", stigma$measurement),] 




