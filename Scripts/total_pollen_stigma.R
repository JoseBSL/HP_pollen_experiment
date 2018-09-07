
library(ggplot2)
library(reshape2)


#All species (2 measurements per focal species. 40 days took me to measure all)
focal_pollen_percentage <- as.matrix(read.table("Data/total_pollen_matrix_focal.csv", header=TRUE, sep = ";",
                                                row.names = 1,
                                                as.is=TRUE))
focal_pollen_percentage <- melt(focal_pollen_percentage)
focal_pollen_percentage <- focal_pollen_percentage[complete.cases(focal_pollen_percentage), ]
focal_pollen_percentage$A <- "focal_pollen"
non_focal_pollen_percentage <- as.matrix(read.table("Data/total_pollen_matrix_non_focal.csv", header=TRUE, sep = ";",
                                                    row.names = 1,
                                                    as.is=TRUE))

non_focal_pollen_percentage <- melt(non_focal_pollen_percentage)
non_focal_pollen_percentage <- non_focal_pollen_percentage[complete.cases(non_focal_pollen_percentage), ]
non_focal_pollen_percentage$A <- "non_focal_pollen"

pollen_ratios <- rbind(focal_pollen_percentage,non_focal_pollen_percentage)
colnames(pollen_ratios) <- c("focal", "non_focal", "ratio", "variable")
#Changes spp code to common names

data.frame(pollen_ratios, stringsAsFactors = F)
pollen_ratios$focal <- as.character(pollen_ratios$focal)
pollen_ratios$focal[pollen_ratios$focal=="CAAN"] <- "Capsicum"
pollen_ratios$focal[pollen_ratios$focal=="SOLY"] <- "Tomato"
pollen_ratios$focal[pollen_ratios$focal=="SOME"] <- "Eggplant"
pollen_ratios$focal[pollen_ratios$focal=="PEIN"] <- "Petunia"
pollen_ratios$focal[pollen_ratios$focal=="SIAL"] <- "White mustard"
pollen_ratios$focal[pollen_ratios$focal=="ERSA"] <- "Rocket"
pollen_ratios$focal[pollen_ratios$focal=="BROL"] <- "Wild cabbage"
pollen_ratios$focal[pollen_ratios$focal=="BRRA"] <- "Pak choi"
pollen_ratios$focal[pollen_ratios$focal=="IPAQ"] <- "Water morning glory"
pollen_ratios$focal[pollen_ratios$focal=="IPPU"] <- "Morning glory"

pollen_ratios$non_focal <- as.character(pollen_ratios$non_focal)
pollen_ratios$non_focal[pollen_ratios$non_focal=="CAAN"] <- "Capsicum"
pollen_ratios$non_focal[pollen_ratios$non_focal=="SOLY"] <- "Tomato"
pollen_ratios$non_focal[pollen_ratios$non_focal=="SOME"] <- "Eggplant"
pollen_ratios$non_focal[pollen_ratios$non_focal=="PEIN"] <- "Petunia"
pollen_ratios$non_focal[pollen_ratios$non_focal=="SIAL"] <- "White mustard"
pollen_ratios$non_focal[pollen_ratios$non_focal=="ERSA"] <- "Rocket"
pollen_ratios$non_focal[pollen_ratios$non_focal=="BROL"] <- "Wild cabbage"
pollen_ratios$non_focal[pollen_ratios$non_focal=="BRRA"] <- "Pak choi"
pollen_ratios$non_focal[pollen_ratios$non_focal=="IPAQ"] <- "Water morning glory"
pollen_ratios$non_focal[pollen_ratios$non_focal=="IPPU"] <- "Morning glory"

pollen_ratios$spp <- paste(pollen_ratios$focal,pollen_ratios$non_focal, sep="-")

write.csv(pollen_ratios, "Data/total_pollen.csv")


ggplot(total_pollen, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+theme(axis.text.x=element_text(angle=60,hjust=1))

#Brassicaceae
#Brassicaceae pollen ratio plot

pollen_ratios <- read.csv("Data/total_pollen.csv")
total_pollen_brassicaceae <- total_pollen[total_pollen$focal!="Morning glory" & total_pollen$focal!="Water morning glory"& total_pollen$focal!="Eggplant"&
                                            total_pollen$focal!="Tomato" & total_pollen$focal!="Petunia"& total_pollen$focal!="Capsicum",  ]

#write.csv(pollen_ratios_brassicaceae, "Rmd/Data/pollen_ratios_brassicaceae.csv")

ggplot(total_pollen_brassicaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="Brassicaceae", x="", y="Pollen mix ratio") + 
  theme(axis.text.x=element_text(angle=60,hjust=1))+geom_hline(yintercept = 50)+
  scale_fill_manual(values=c("grey10", "grey60"))+ ylim(0,100)
#Solanaceae pollen ratio plot

total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen_solanaceae <- total_pollen[total_pollen$focal!="Morning glory" & total_pollen$focal!="Water morning glory"& total_pollen$focal!="Wild cabbage"&
                                          total_pollen$focal!="Pak choi" & total_pollen$focal!="White mustard"& total_pollen$focal!="Rocket",  ]

#write.csv(pollen_ratios_solanaceae, "Rmd/Data/pollen_ratios_solanaceae.csv")
ggplot(total_pollen_solanaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="Solanaceae", x="", y="Pollen mix ratio") +
  theme(axis.text.x=element_text(angle=60,hjust=1))+geom_hline(yintercept = 50)+
  scale_fill_manual(values=c("grey10", "grey60"))+ ylim(0,100)

#Convolvulaceae

total_pollen <- read.csv("Data/total_pollen.csv")

total_pollen_convolvulaceae <- total_pollen[total_pollen$focal!="Capsicum" & total_pollen$focal!="Eggplant"& total_pollen$focal!="Wild cabbage"&
                                               total_pollen$focal!="Pak choi" & total_pollen$focal!="White mustard"
                                              & total_pollen$focal!="Rocket"& total_pollen$focal!="Tomato"& total_pollen$focal!="Petunia",  ]


#write.csv(pollen_ratios_convolvulaceae, "Rmd/Data/pollen_ratios_convolvulaceae.csv")


ggplot(total_pollen_convolvulaceae, aes(x=spp, y=ratio, fill=variable)) + 
  geom_bar(stat='identity', position='dodge') +labs(title="Convolvulaceae", x="", y="Pollen mix ratio") + 
  theme(axis.text.x=element_text(angle=60,hjust=1))+geom_hline(yintercept = 50)+
  scale_fill_manual(values=c("grey10", "grey60"))+ ylim(0,100)

