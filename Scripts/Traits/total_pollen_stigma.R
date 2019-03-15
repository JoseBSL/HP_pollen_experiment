
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
total_pollen$focal[total_pollen$focal=="CAAN"] <- "C. annuum"
total_pollen$focal[total_pollen$focal=="SOLY"] <- "S. lycopersicum"
total_pollen$focal[total_pollen$focal=="SOME"] <- "S. melongena"
total_pollen$focal[total_pollen$focal=="PEIN"] <- "P. integrifolia"
total_pollen$focal[total_pollen$focal=="SIAL"] <- "S. alba"
total_pollen$focal[total_pollen$focal=="ERSA"] <- "E. sativa"
total_pollen$focal[total_pollen$focal=="BROL"] <- "B. oleracea"
total_pollen$focal[total_pollen$focal=="BRRA"] <- "B. rapa"
total_pollen$focal[total_pollen$focal=="IPAQ"] <- "I. aquatica"
total_pollen$focal[total_pollen$focal=="IPPU"] <- "I. purpurea"

total_pollen$non_focal <- as.character(total_pollen$non_focal)
total_pollen$non_focal[total_pollen$non_focal=="CAAN"] <- "C. annuum"
total_pollen$non_focal[total_pollen$non_focal=="SOLY"] <- "S. lycopersicum"
total_pollen$non_focal[total_pollen$non_focal=="SOME"] <- "S. melongena"
total_pollen$non_focal[total_pollen$non_focal=="PEIN"] <- "P. integrifolia"
total_pollen$non_focal[total_pollen$non_focal=="SIAL"] <- "S. alba"
total_pollen$non_focal[total_pollen$non_focal=="ERSA"] <- "E. sativa"
total_pollen$non_focal[total_pollen$non_focal=="BROL"] <- "B. oleracea"
total_pollen$non_focal[total_pollen$non_focal=="BRRA"] <- "B. rapa"
total_pollen$non_focal[total_pollen$non_focal=="IPAQ"] <- "I. aquatica"
total_pollen$non_focal[total_pollen$non_focal=="IPPU"] <- "I. purpurea"

total_pollen$spp <- paste(total_pollen$focal,total_pollen$non_focal, sep="-")

write.csv(total_pollen, "Data/total_pollen.csv")
write.csv(total_pollen, "Rmd/Data/total_pollen.csv")


ggplot(total_pollen, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+theme(axis.text.x=element_text(angle=60,hjust=1))

#Brassicaceae
#Brassicaceae pollen ratio plot

total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen_brassicaceae <- total_pollen[total_pollen$focal!="I. purpurea" & total_pollen$focal!="I. aquatica"& total_pollen$focal!="S. melongena"&
                                            total_pollen$focal!="S. lycopersicum" & total_pollen$focal!="P. integrifolia"& total_pollen$focal!="C. annuum",  ]

write.csv(total_pollen_brassicaceae, "Rmd/Data/total_pollen_brassicaceae.csv")

ggplot(total_pollen_brassicaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="Brassicaceae", x="", y="Pollen mix ratio") + 
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  scale_fill_manual(values=c("grey10", "grey60"))
#Solanaceae pollen ratio plot

total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen_solanaceae <- total_pollen[total_pollen$focal!="I. purpurea" & total_pollen$focal!="I. aquatica"& total_pollen$focal!="B. oleracea"&
                                          total_pollen$focal!="B. rapa" & total_pollen$focal!="S. alba"& total_pollen$focal!="E. sativa",  ]

#write.csv(total_pollen_solanaceae, "Rmd/Data/total_pollen_solanaceae.csv")
ggplot(total_pollen_solanaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="Solanaceae", x="", y="Pollen mix ratio") +
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  scale_fill_manual(values=c("grey10", "grey60"))

#Convolvulaceae

total_pollen <- read.csv("Data/total_pollen.csv")

total_pollen_convolvulaceae <- total_pollen[total_pollen$focal!="C. annuum" & total_pollen$focal!="S. melongena"& total_pollen$focal!="B. oleracea"&
                                               total_pollen$focal!="B. rapa" & total_pollen$focal!="S. alba"
                                              & total_pollen$focal!="E. sativa"& total_pollen$focal!="S. lycopersicum"& total_pollen$focal!="P. integrifolia",  ]





#write.csv(total_pollen_convolvulaceae, "Rmd/Data/total_pollen_convolvulaceae.csv")


ggplot(total_pollen_convolvulaceae, aes(x=spp, y=ratio, fill=variable)) + 
  geom_bar(stat='identity', position='dodge') +labs(title="Convolvulaceae", x="", y="Pollen mix ratio") + 
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  scale_fill_manual(values=c("grey10", "grey60"))

#Now I combine both focal and non focal for the same treatment
#to create a data.frame where I will test for correlation with stigma size
#library

library(reshape2)

total_pollen <- dcast(focal + non_focal  ~ ., value.var = "ratio", fun.aggregate = sum,
      data = total_pollen, na.rm= TRUE)

#write.csv(total_pollen, "Data/total_pollen.csv")
save.image("Manuscript_draft/Data/total_pollen.RData")
