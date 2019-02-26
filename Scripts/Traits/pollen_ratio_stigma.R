
library(ggplot2)
library(reshape2)

#All species (2 measurements per focal species. 40 days took me to measure all)
focal_pollen_percentage <- as.matrix(read.table("Data/pollen_ratios_matrix.csv", header=TRUE, sep = ";",
                              row.names = 1,
                              as.is=TRUE))
focal_pollen_percentage <- melt(focal_pollen_percentage)
focal_pollen_percentage <- focal_pollen_percentage[complete.cases(focal_pollen_percentage), ]
focal_pollen_percentage$A <- "focal_pollen"
non_focal_pollen_percentage <- as.matrix(read.table("Data/pollen_ratios_matrix_non_focal.csv", header=TRUE, sep = ";",
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

write.csv(pollen_ratios, "Data/pollen_ratios.csv")


ggplot(pollen_ratios, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+theme(axis.text.x=element_text(angle=60,hjust=1))

#Brassicaceae
#Brassicaceae pollen ratio plot

pollen_ratios <- read.csv("Data/pollen_ratios.csv")
pollen_ratios_brassicaceae <- pollen_ratios[pollen_ratios$focal!="Morning glory" & pollen_ratios$focal!="Water morning glory"& pollen_ratios$focal!="Eggplant"&
                               pollen_ratios$focal!="Tomato" & pollen_ratios$focal!="Petunia"& pollen_ratios$focal!="Capsicum",  ]

#write.csv(pollen_ratios_brassicaceae, "Rmd/Data/pollen_ratios_brassicaceae.csv")

ggplot(pollen_ratios_brassicaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="A) Brassicaceae") +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.title = element_blank()) + scale_fill_grey(labels=c("Recipient","Donor")) + 
  geom_hline(yintercept = 50) + xlab("") + ylab("Pollen Ratio")


#Solanaceae pollen ratio plot

pollen_ratios <- read.csv("Data/pollen_ratios.csv")
pollen_ratios_solanaceae <- pollen_ratios[pollen_ratios$focal!="Morning glory" & pollen_ratios$focal!="Water morning glory"& pollen_ratios$focal!="Wild cabbage"&
                                 pollen_ratios$focal!="Pak choi" & pollen_ratios$focal!="White mustard"& pollen_ratios$focal!="Rocket",  ]

#write.csv(pollen_ratios_solanaceae, "Rmd/Data/pollen_ratios_solanaceae.csv")

ggplot(pollen_ratios_solanaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="B) Solanaceae") +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.title = element_blank()) + scale_fill_grey(labels=c("Recipient","Donor")) + 
  geom_hline(yintercept = 50) + xlab("") + ylab("Pollen Ratio")


#Convolvulaceae

pollen_ratios <- read.csv("Data/pollen_ratios.csv")

pollen_ratios$spp <- paste(pollen_ratios$focal,pollen_ratios$non_focal)
pollen_ratios_convolvulaceae <- pollen_ratios[pollen_ratios$focal!="Capsicum" & pollen_ratios$focal!="Eggplant"& pollen_ratios$focal!="Wild cabbage"&
                                 pollen_ratios$focal!="Pak choi" & pollen_ratios$focal!="White mustard"
                               & pollen_ratios$focal!="Rocket"& pollen_ratios$focal!="Tomato"& pollen_ratios$focal!="Petunia",  ]

#write.csv(pollen_ratios_convolvulaceae, "Rmd/Data/pollen_ratios_convolvulaceae.csv")
#save.image("Manuscript_draft/pollen_ratio.RData")
#Now the angle is right

ggplot(pollen_ratios_convolvulaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="C) Convolvulaceae") +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.title = element_blank()) + scale_fill_grey(labels=c("Recipient","Donor")) + 
  geom_hline(yintercept = 50) + xlab("") + ylab("Pollen Ratio")
