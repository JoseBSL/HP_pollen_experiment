#Here I test for correlation between stigma size and total pollen deposited on the stigma

total_pollen <- read.csv("Data/Csv/total_pollen.csv")

total_pollen=total_pollen[,-1]
colnames(total_pollen)[3] <- "total_pollen"
head(total_pollen)

#Now I load the trait table to add stigma size

traits_all <- read.csv("Data/Csv/traits_all.csv", sep=",")

stigma_area <- traits_all[,c(2,10)]
str(stigma_area)

stigma_area$species=as.character(stigma_area$species)
stigma_width <- traits_all[,c(2,13)]
stigma_width$species=as.character(stigma_width$species)



str(total_pollen)
total_pollen$focal=as.character(total_pollen$focal)
unique(total_pollen$focal)
total_pollen$focal[total_pollen$focal=="Capsicum"] <- "Capsicum annuum"
total_pollen$focal[total_pollen$focal=="Eggplant"] <- "Solanum melongena"
total_pollen$focal[total_pollen$focal=="Morning glory"] <- "Ipomoea purpurea"
total_pollen$focal[total_pollen$focal=="Tomato"] <- "Solanum lycopersicum"
total_pollen$focal[total_pollen$focal=="Petunia"] <- "Petunia integrifolia"
total_pollen$focal[total_pollen$focal=="Rocket"] <- "Eruca versicaria"
total_pollen$focal[total_pollen$focal=="Pak choi"] <- "Brassica rapa"
total_pollen$focal[total_pollen$focal=="Wild cabbage"] <- "Brassica oleracea"
total_pollen$focal[total_pollen$focal=="White mustard"] <- "Sinapis alba"
total_pollen$focal[total_pollen$focal=="Water morning glory"] <- "Ipomoea aquatica"
str(total_pollen)
colnames(total_pollen)[1] <- "species"

test <- merge(total_pollen, stigma_area, by="species")

cor.test(test$total_pollen, test$stigma_area, method=c("pearson"))

test <- merge(total_pollen, stigma_width, by="species")
cor.test(test$total_pollen, test$stigma_width, method=c("pearson"))

 
