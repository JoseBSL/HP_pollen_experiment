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
measurement$new <- paste(measurement$V1,"_", measurement$V2 )
morphometry[,2] <- measurement[,4] 
#Aggregate of different variables 
morphometry <- dcast(species + measurement ~ ., value.var = "um", fun.aggregate = mean, data = morphometry, na.rm= TRUE)
#Some grammar mistakes, at least they seem to be homogeneous among species

#Fix colnames
colnames(morphometry)[3] <- "um"

#First I create the dataframes then I order and finally I insert them in the trait_all data.frame

#Stigma
stigma <-morphometry[grep("stigma", morphometry$measurement),] 
#Stigma area (square micrometers, only unit like this)
stigma_area <-stigma[grep("area", stigma$measurement),] 
#Stigma length
stigma_lenght <-stigma[grep("length", stigma$measurement),] 
#Stigma surface
stigma_surface <-stigma[grep("surface", stigma$measurement),] 
#Stigma width
stigma_width <-stigma[grep("width", stigma$measurement),] 


#Style
style <-morphometry[grep("style", morphometry$measurement),] 
#Style length
style_length <-style[grep("length", style$measurement),] 
#Style width
style_width <-style[grep("width", style$measurement),] 


#Ovary
ovary <-morphometry[grep("ovary", morphometry$measurement),] 

#Ovary width
ovary_width <-ovary[grep("width", ovary$measurement),] 
#Ovary width
ovary_length <-ovary[grep("legth", ovary$measurement),] 
ovary_length$measurement <- gsub('ovary _ legth', 'ovary _ length', ovary_length$measurement)

#Ok now all the traits are ready!! Lets add it to traits_all.

traits_all$stigma_area <- stigma_area$um
traits_all$stigma_length <- stigma_lenght$um
traits_all$stigma_surface <- stigma_surface$um
traits_all$stigma_width <- stigma_width$um
traits_all$style_length <- stigma_lenght$um
traits_all$style_width <- style_width$um
traits_all$ovary_width <- ovary_width$um
traits_all$ovary_length <- ovary_length$um


#write.csv(traits_all, "Data/traits_all.csv")

#Checking for correlations

library(dplyr)

length <- filter(morphometry, measurement=="stigma _ length" & species=="SOME")
width <- filter(morphometry, measurement=="stigma _ width" & species=="SOME")
area <- filter(morphometry, measurement=="stigma _ area" & species=="SOME")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="SOME")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="SOME")
s_length <- filter(morphometry, measurement=="style _ length" & species=="SOME")
s_width <- filter(morphometry, measurement=="style _ width" & species=="SOME")

s_width<- s_width[1:15,]

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)



length <- filter(morphometry, measurement=="stigma _ length" & species=="CAAN")
width <- filter(morphometry, measurement=="stigma _ width" & species=="CAAN")
area <- filter(morphometry, measurement=="stigma _ area" & species=="CAAN")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="CAAN")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="CAAN")
s_length <- filter(morphometry, measurement=="style _ length" & species=="CAAN")
s_width <- filter(morphometry, measurement=="style _ width" & species=="CAAN")
s_width<- s_width[1:15,]

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)



length <- filter(morphometry, measurement=="stigma _ length" & species=="SOLY")
width <- filter(morphometry, measurement=="stigma _ width" & species=="SOLY")
area <- filter(morphometry, measurement=="stigma _ area" & species=="SOLY")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)


length <- filter(morphometry, measurement=="stigma _ length" & species=="PEIN")
width <- filter(morphometry, measurement=="stigma _ width" & species=="PEIN")
area <- filter(morphometry, measurement=="stigma _ area" & species=="PEIN")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)


length <- filter(morphometry, measurement=="stigma _ length" & species=="BRRA")
width <- filter(morphometry, measurement=="stigma _ width" & species=="BRRA")
area <- filter(morphometry, measurement=="stigma _ area" & species=="BRRA")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)

length <- filter(morphometry, measurement=="stigma _ length" & species=="BROL")
width <- filter(morphometry, measurement=="stigma _ width" & species=="BROL")
area <- filter(morphometry, measurement=="stigma _ area" & species=="BROL")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)

length <- filter(morphometry, measurement=="stigma _ length" & species=="SIAL")
width <- filter(morphometry, measurement=="stigma _ width" & species=="SIAL")
area <- filter(morphometry, measurement=="stigma _ area" & species=="SIAL")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)


length <- filter(morphometry, measurement=="stigma _ length" & species=="ERSA")
width <- filter(morphometry, measurement=="stigma _ width" & species=="ERSA")
area <- filter(morphometry, measurement=="stigma _ area" & species=="ERSA")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)


length <- filter(morphometry, measurement=="stigma _ length" & species=="IPAQ")
width <- filter(morphometry, measurement=="stigma _ width" & species=="IPAQ")
area <- filter(morphometry, measurement=="stigma _ area" & species=="IPAQ")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)

length <- filter(morphometry, measurement=="stigma _ length" & species=="IPPU")
width <- filter(morphometry, measurement=="stigma _ width" & species=="IPPU")
area <- filter(morphometry, measurement=="stigma _ area" & species=="IPPU")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)







ovary_width <- filter(morphometry, measurement=="ovary _ width")
ovary_length <- filter(morphometry, measurement=="ovary _ legth")#I maintain the spelling mistake...

cor.test(ovary_length$um, ovary_width$um)




