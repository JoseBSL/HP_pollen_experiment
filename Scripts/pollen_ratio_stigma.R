
library(ggplot2)

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
pollen_ratios$spp <- paste(pollen_ratios$focal,pollen_ratios$non_focal)

ggplot(pollen_ratios, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge')

#Brassicaceae
#Brassicaceae pollen ratio plot
pollen_ratios_brassicaceae <- pollen_ratios[pollen_ratios$focal!="IPPU" & pollen_ratios$focal!="IPAQ"& pollen_ratios$focal!="SOME"&
                               pollen_ratios$focal!="SOLY" & pollen_ratios$focal!="PEIN"& pollen_ratios$focal!="CAAN",  ]

write.csv(pollen_ratios_brassicaceae, "Rmd/Data/pollen_ratios_brassicaceae.csv")

ggplot(pollen_ratios_brassicaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="Brassicaceae")

#Solanaceae pollen ratio plot

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
pollen_ratios$spp <- paste(pollen_ratios$focal,pollen_ratios$non_focal)

pollen_ratios_solanaceae <- pollen_ratios[pollen_ratios$focal!="IPPU" & pollen_ratios$focal!="IPAQ"& pollen_ratios$focal!="BROL"&
                                 pollen_ratios$focal!="BRRA" & pollen_ratios$focal!="SIAL"& pollen_ratios$focal!="ERSA",  ]

write.csv(pollen_ratios_solanaceae, "Rmd/Data/pollen_ratios_solanaceae.csv")

ggplot(pollen_ratios_solanaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="Solanaceae")



#Convolvulaceae

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
pollen_ratios$spp <- paste(pollen_ratios$focal,pollen_ratios$non_focal)
pollen_ratios_convolvulaceae <- pollen_ratios[pollen_ratios$focal!="CAAN" & pollen_ratios$focal!="SOME"& pollen_ratios$focal!="BROL"&
                                 pollen_ratios$focal!="BRRA" & pollen_ratios$focal!="SIAL"
                               & pollen_ratios$focal!="ERSA"& pollen_ratios$focal!="SOLY"& pollen_ratios$focal!="PEIN",  ]

write.csv(pollen_ratios_convolvulaceae, "Rmd/Data/pollen_ratios_convolvulaceae.csv")

ggplot(pollen_ratios_convolvulaceae, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +labs(title="Convolvulaceae")
