
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

#data.frame(pollen_ratios, stringsAsFactors = F)
pollen_ratios$focal <- as.character(pollen_ratios$focal)
pollen_ratios$focal[pollen_ratios$focal=="CAAN"] <- "C. annuum"
pollen_ratios$focal[pollen_ratios$focal=="SOLY"] <- "S. lycopersicum"
pollen_ratios$focal[pollen_ratios$focal=="SOME"] <- "S. melongena"
pollen_ratios$focal[pollen_ratios$focal=="PEIN"] <- "P. integrifolia"
pollen_ratios$focal[pollen_ratios$focal=="SIAL"] <- "S. alba"
pollen_ratios$focal[pollen_ratios$focal=="ERSA"] <- "E. sativa"
pollen_ratios$focal[pollen_ratios$focal=="BROL"] <- "B. oleracea"
pollen_ratios$focal[pollen_ratios$focal=="BRRA"] <- "B. rapa"
pollen_ratios$focal[pollen_ratios$focal=="IPAQ"] <- "I. aquatica"
pollen_ratios$focal[pollen_ratios$focal=="IPPU"] <- "I. purpurea"

pollen_ratios$non_focal <- as.character(pollen_ratios$non_focal)
pollen_ratios$non_focal[pollen_ratios$non_focal=="CAAN"] <- "C. annuum"
pollen_ratios$non_focal[pollen_ratios$non_focal=="SOLY"] <- "S. lycopersicum"
pollen_ratios$non_focal[pollen_ratios$non_focal=="SOME"] <- "S. melongena"
pollen_ratios$non_focal[pollen_ratios$non_focal=="PEIN"] <- "P. integrifolia"
pollen_ratios$non_focal[pollen_ratios$non_focal=="SIAL"] <- "S. alba"
pollen_ratios$non_focal[pollen_ratios$non_focal=="ERSA"] <- "E. sativa"
pollen_ratios$non_focal[pollen_ratios$non_focal=="BROL"] <- "B. oleracea"
pollen_ratios$non_focal[pollen_ratios$non_focal=="BRRA"] <- "B. rapa"
pollen_ratios$non_focal[pollen_ratios$non_focal=="IPAQ"] <- "I. aquatica"
pollen_ratios$non_focal[pollen_ratios$non_focal=="IPPU"] <- "I. purpurea"

pollen_ratios$spp <- paste(pollen_ratios$focal,pollen_ratios$non_focal, sep="-")

write.csv(pollen_ratios, "Data/pollen_ratios.csv")


ggplot(pollen_ratios, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+theme(axis.text.x=element_text(angle=60,hjust=1))

#Brassicaceae
#Brassicaceae pollen ratio plot

pollen_ratios <- read.csv("Data/pollen_ratios.csv")
pollen_ratios_brassicaceae <- pollen_ratios[pollen_ratios$focal!="I. purpurea" & pollen_ratios$focal!="I. aquatica"& pollen_ratios$focal!="S. melongena"&
                               pollen_ratios$focal!="S. lycopersicum" & pollen_ratios$focal!="P. integrifolia"& pollen_ratios$focal!="C. annuum",  ]

#write.csv(pollen_ratios_brassicaceae, "Rmd/Data/pollen_ratios_brassicaceae.csv")

ggplot(pollen_ratios_brassicaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge',width=0.5) +labs(title="A) Brassicaceae") +
  theme(axis.text.x=element_text(angle=45,hjust=1, face="italic"), legend.title = element_blank()) + scale_fill_grey(labels=c("Recipient","Donor")) + 
  geom_hline(yintercept = 50) + xlab("") + ylab("Pollen Ratio")+ylim(0, 100)


#Solanaceae pollen ratio plot

pollen_ratios <- read.csv("Data/pollen_ratios.csv")
pollen_ratios_solanaceae <- pollen_ratios[pollen_ratios$focal!="I. purpurea" & pollen_ratios$focal!="I. aquatica"& pollen_ratios$focal!="B. oleracea"&
                                 pollen_ratios$focal!="B. rapa" & pollen_ratios$focal!="S. alba"& pollen_ratios$focal!="E. sativa",  ]

#write.csv(pollen_ratios_solanaceae, "Rmd/Data/pollen_ratios_solanaceae.csv")

ggplot(pollen_ratios_solanaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge',width=0.5) +labs(title="B) Solanaceae") +
  theme(axis.text.x=element_text(angle=45,hjust=1, face="italic"), legend.title = element_blank()) + scale_fill_grey(labels=c("Recipient","Donor")) + 
  geom_hline(yintercept = 50) + xlab("") + ylab("Pollen Ratio")+ylim(0, 100)


#Convolvulaceae

pollen_ratios <- read.csv("Data/pollen_ratios.csv")

#pollen_ratios$spp <- paste(pollen_ratios$focal,pollen_ratios$non_focal)
pollen_ratios_convolvulaceae <- pollen_ratios[pollen_ratios$focal!="C. annuum" & pollen_ratios$focal!="S. melongena"& pollen_ratios$focal!="B. oleracea"&
                                 pollen_ratios$focal!="B. rapa" & pollen_ratios$focal!="S. alba"
                               & pollen_ratios$focal!="E. sativa"& pollen_ratios$focal!="S. lycopersicum"& pollen_ratios$focal!="P. integrifolia",  ]

#write.csv(pollen_ratios_convolvulaceae, "Rmd/Data/pollen_ratios_convolvulaceae.csv")
save.image("Manuscript_draft/pollen_ratio.RData")
#Now the angle is right

ggplot(pollen_ratios_convolvulaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge',width=0.25) +labs(title="C) Convolvulaceae") +
  theme(axis.text.x=element_text(angle=45,hjust=1, face="italic"), legend.title = element_blank()) + scale_fill_grey(labels=c("Recipient","Donor")) + 
  geom_hline(yintercept = 50) + xlab("") + ylab("Pollen Ratio")+ylim(0, 100)


mean(pollen_ratios_brassicaceae$ratio[pollen_ratios_brassicaceae$variable=="non_focal_pollen"])
mean(pollen_ratios_solanaceae$ratio[pollen_ratios_solanaceae$variable=="focal_pollen"])
mean(pollen_ratios_convolvulaceae$ratio[pollen_ratios_convolvulaceae$variable=="focal_pollen"])

#Making Convolvulaceae plot same size

a<- rbind(pollen_ratios_convolvulaceae,pollen_ratios_convolvulaceae)

a[9:16,4]<-0
str(a)
a$spp=as.character(a$spp)
a[9:16,6]<-" "
ggplot(a, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="C) Convolvulaceae") +
  theme(axis.text.x=element_text(angle=45,hjust=1, face="italic"), legend.title = element_blank()) + scale_fill_grey(labels=c("Recipient","Donor")) + 
  geom_hline(yintercept = 50) + xlab("") + ylab("Pollen Ratio")


#I'm going to prepare the data in another format so I can plot it nicely
#Brassicaceae
pollen_ratios <- read.csv("Data/pollen_ratios.csv")
pollen_ratios_brassicaceae <- pollen_ratios[pollen_ratios$focal!="I. purpurea" & pollen_ratios$focal!="I. aquatica"& pollen_ratios$focal!="S. melongena"&
                                              pollen_ratios$focal!="S. lycopersicum" & pollen_ratios$focal!="P. integrifolia"& pollen_ratios$focal!="C. annuum",  ]
ersa_ippu <- c(99,1)
brol_ippu <- c(63,37)
brra_ippu <- c(99,1)
sial_ipaq <- c(99,1)
sial_some <- c(73,27)
ersa_pein <- c(67,33)
brra_soly <- c(83,17)
brol_caan <- c(57,43)

a<- data.frame(ersa_ippu,brol_ippu,brra_ippu,sial_ipaq,sial_some,ersa_pein,brra_soly,brol_caan)
a=as.matrix(a)
rownames(a)<- c("Recipient","Donor")
ratio_bra=a
barplot(ratio_bra, hor=T,col=coul ,space=0.8, border="white",srt=80,las=1,cex.axis=0.7)                                                                                              
saveRDS(ratio_bra,"Rmd/Data/ratio_bra.RData")
#Solanaceae
pollen_ratios <- read.csv("Data/pollen_ratios.csv")
pollen_ratios_solanaceae <- pollen_ratios[pollen_ratios$focal!="I. purpurea" & pollen_ratios$focal!="I. aquatica"& pollen_ratios$focal!="B. oleracea"&
                                            pollen_ratios$focal!="B. rapa" & pollen_ratios$focal!="S. alba"& pollen_ratios$focal!="E. sativa",  ]

caan_ippu <- c(95,5)
some_ipaq <- c(74,26)
pein_ipaq <- c(49,51)
soly_ipaq <- c(84,16)
some_ersa <- c(46,54)
pein_ersa <- c(49,51)
caan_sial <- c(38,62)
soly_brra <- c(49,51)
a<- data.frame(caan_ippu,some_ipaq,pein_ipaq,soly_ipaq,some_ersa,pein_ersa,caan_sial,soly_brra)
a=as.matrix(a)
row.names(a)<- c("Recipient", "Donor")
ratio_sol=a
barplot(ratio_sol, hor=T,col=coul ,space=0.8, border="white",srt=80,las=1,cex.axis=0.7)                                                                                              
saveRDS(ratio_sol,"Rmd/Data/ratio_sol.RData")

#Convolvulaceae

pollen_ratios <- read.csv("Data/pollen_ratios.csv")

#pollen_ratios$spp <- paste(pollen_ratios$focal,pollen_ratios$non_focal)
pollen_ratios_convolvulaceae <- pollen_ratios[pollen_ratios$focal!="C. annuum" & pollen_ratios$focal!="S. melongena"& pollen_ratios$focal!="B. oleracea"&
                                                pollen_ratios$focal!="B. rapa" & pollen_ratios$focal!="S. alba"
                                              & pollen_ratios$focal!="E. sativa"& pollen_ratios$focal!="S. lycopersicum"& pollen_ratios$focal!="P. integrifolia",  ]

ippu_some<- c(33,67) 
ipaq_caan<- c(31,69)
ippu_ersa<- c(19,81)
ipaq_sial<- c(29,71)
a=data.frame(ippu_some,ipaq_caan,ippu_ersa,ipaq_sial)
a=as.matrix(a)
rownames(a) <- c("Recipient", "Donor")
ratio_con=a
barplot(ratio_con, hor=T,col=coul ,space=0.8, border="white",srt=80,las=1,cex.axis=0.7)                                                                                              
saveRDS(ratio_con,"Rmd/Data/ratio_con.RData")
