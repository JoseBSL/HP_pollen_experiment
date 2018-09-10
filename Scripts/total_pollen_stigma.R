
library(ggplot2)
library(reshape2)


#All species (2 measurements per focal species. 40 days took me to measure all)
focal_total_pollen <- as.matrix(read.table("Data/total_pollen_matrix_focal.csv", header=TRUE, sep = ",",
                                                row.names = 1,
                                                as.is=TRUE))
focal_total_pollen <- melt(focal_total_pollen)
focal_total_pollen <- focal_total_pollen[complete.cases(focal_total_pollen), ]
focal_total_pollen$A <- "focal_pollen"
non_focal_total_pollen <- as.matrix(read.table("Data/total_pollen_matrix_non_focal.csv", header=TRUE, sep = ",",
                                                    row.names = 1,
                                                    as.is=TRUE))

non_focal_total_pollen <- melt(non_focal_total_pollen)
non_focal_total_pollen <- non_focal_total_pollen[complete.cases(non_focal_total_pollen), ]
non_focal_total_pollen$A <- "non_focal_pollen"

total_pollen <- rbind(focal_total_pollen,non_focal_total_pollen)
colnames(total_pollen) <- c("focal", "non_focal", "ratio", "variable")
#Changes spp code to common names

data.frame(total_pollen, stringsAsFactors = F)
total_pollen$focal <- as.character(total_pollen$focal)
total_pollen$focal[total_pollen$focal=="CAAN"] <- "Capsicum"
total_pollen$focal[total_pollen$focal=="SOLY"] <- "Tomato"
total_pollen$focal[total_pollen$focal=="SOME"] <- "Eggplant"
total_pollen$focal[total_pollen$focal=="PEIN"] <- "Petunia"
total_pollen$focal[total_pollen$focal=="SIAL"] <- "White mustard"
total_pollen$focal[total_pollen$focal=="ERSA"] <- "Rocket"
total_pollen$focal[total_pollen$focal=="BROL"] <- "Wild cabbage"
total_pollen$focal[total_pollen$focal=="BRRA"] <- "Pak choi"
total_pollen$focal[total_pollen$focal=="IPAQ"] <- "Water morning glory"
total_pollen$focal[total_pollen$focal=="IPPU"] <- "Morning glory"

total_pollen$non_focal <- as.character(total_pollen$non_focal)
total_pollen$non_focal[total_pollen$non_focal=="CAAN"] <- "Capsicum"
total_pollen$non_focal[total_pollen$non_focal=="SOLY"] <- "Tomato"
total_pollen$non_focal[total_pollen$non_focal=="SOME"] <- "Eggplant"
total_pollen$non_focal[total_pollen$non_focal=="PEIN"] <- "Petunia"
total_pollen$non_focal[total_pollen$non_focal=="SIAL"] <- "White mustard"
total_pollen$non_focal[total_pollen$non_focal=="ERSA"] <- "Rocket"
total_pollen$non_focal[total_pollen$non_focal=="BROL"] <- "Wild cabbage"
total_pollen$non_focal[total_pollen$non_focal=="BRRA"] <- "Pak choi"
total_pollen$non_focal[total_pollen$non_focal=="IPAQ"] <- "Water morning glory"
total_pollen$non_focal[total_pollen$non_focal=="IPPU"] <- "Morning glory"

total_pollen$spp <- paste(total_pollen$focal,total_pollen$non_focal, sep="-")

write.csv(total_pollen, "Data/total_pollen.csv")
write.csv(total_pollen, "Rmd/Data/total_pollen.csv")


ggplot(total_pollen, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+theme(axis.text.x=element_text(angle=60,hjust=1))

#Brassicaceae
#Brassicaceae pollen ratio plot

total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen_brassicaceae <- total_pollen[total_pollen$focal!="Morning glory" & total_pollen$focal!="Water morning glory"& total_pollen$focal!="Eggplant"&
                                            total_pollen$focal!="Tomato" & total_pollen$focal!="Petunia"& total_pollen$focal!="Capsicum",  ]

write.csv(total_pollen_brassicaceae, "Rmd/Data/total_pollen_brassicaceae.csv")

ggplot(total_pollen_brassicaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="Brassicaceae", x="", y="Pollen mix ratio") + 
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  scale_fill_manual(values=c("grey10", "grey60"))
#Solanaceae pollen ratio plot

total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen_solanaceae <- total_pollen[total_pollen$focal!="Morning glory" & total_pollen$focal!="Water morning glory"& total_pollen$focal!="Wild cabbage"&
                                          total_pollen$focal!="Pak choi" & total_pollen$focal!="White mustard"& total_pollen$focal!="Rocket",  ]

#write.csv(total_pollen_solanaceae, "Rmd/Data/total_pollen_solanaceae.csv")
ggplot(total_pollen_solanaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="Solanaceae", x="", y="Pollen mix ratio") +
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  scale_fill_manual(values=c("grey10", "grey60"))

#Convolvulaceae

total_pollen <- read.csv("Data/total_pollen.csv")

total_pollen_convolvulaceae <- total_pollen[total_pollen$focal!="Capsicum" & total_pollen$focal!="Eggplant"& total_pollen$focal!="Wild cabbage"&
                                               total_pollen$focal!="Pak choi" & total_pollen$focal!="White mustard"
                                              & total_pollen$focal!="Rocket"& total_pollen$focal!="Tomato"& total_pollen$focal!="Petunia",  ]





#write.csv(total_pollen_convolvulaceae, "Rmd/Data/total_pollen_convolvulaceae.csv")


ggplot(total_pollen_convolvulaceae, aes(x=spp, y=ratio, fill=variable)) + 
  geom_bar(stat='identity', position='dodge') +labs(title="Convolvulaceae", x="", y="Pollen mix ratio") + 
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  scale_fill_manual(values=c("grey10", "grey60"))

