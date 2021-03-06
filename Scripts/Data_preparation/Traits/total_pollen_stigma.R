
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

coul = c("#999999","#779799")
ggplot(total_pollen, aes(x=spp, y=ratio, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+theme(axis.text.x=element_text(angle=60,hjust=1))

#Brassicaceae
#Brassicaceae pollen ratio plot

total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen_brassicaceae <- total_pollen[total_pollen$focal!="I. purpurea" & total_pollen$focal!="I. aquatica"& total_pollen$focal!="S. melongena"&
                                            total_pollen$focal!="S. lycopersicum" & total_pollen$focal!="P. integrifolia"& total_pollen$focal!="C. annuum",  ]

#write.csv(total_pollen_brassicaceae, "Rmd/Data/total_pollen_brassicaceae.csv")

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


#Trying to obtain results per family
total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen=total_pollen[,-1]
total_pollen$focal=as.character(total_pollen$focal)
total_pollen$non_focal=as.character(total_pollen$non_focal)

total_pollen$focal[total_pollen$focal=="C. annuum"] <- "SOL"
total_pollen$focal[total_pollen$focal=="S. lycopersicum"] <- "SOL"
total_pollen$focal[total_pollen$focal=="S. melongena"] <- "SOL"
total_pollen$focal[total_pollen$focal=="P. integrifolia"] <- "SOL"
total_pollen$focal[total_pollen$focal=="S. alba"] <- "BRA"
total_pollen$focal[total_pollen$focal=="E. sativa"] <- "BRA"
total_pollen$focal[total_pollen$focal=="B. oleracea"] <- "BRA"
total_pollen$focal[total_pollen$focal=="B. rapa"] <- "BRA"
total_pollen$focal[total_pollen$focal=="I. aquatica"] <- "CON"
total_pollen$focal[total_pollen$focal=="I. purpurea"] <- "CON"

total_pollen$non_focal[total_pollen$non_focal=="C. annuum"] <- "SOL"
total_pollen$non_focal[total_pollen$non_focal=="S. lycopersicum"] <- "SOL"
total_pollen$non_focal[total_pollen$non_focal=="S. melongena"] <- "SOL"
total_pollen$non_focal[total_pollen$non_focal=="P. integrifolia"] <- "SOL"
total_pollen$non_focal[total_pollen$non_focal=="S. alba"] <- "BRA"
total_pollen$non_focal[total_pollen$non_focal=="E. sativa"] <- "BRA"
total_pollen$non_focal[total_pollen$non_focal=="B. oleracea"] <- "BRA"
total_pollen$non_focal[total_pollen$non_focal=="B. rapa"] <- "BRA"
total_pollen$non_focal[total_pollen$non_focal=="I. aquatica"] <- "CON"
total_pollen$non_focal[total_pollen$non_focal=="I. purpurea"] <- "CON"

#Solanaceae with copnvolvulaceae pollen
sol<- subset(total_pollen, focal=="SOL")
sol_con<- subset(sol, non_focal=="CON")

sol_con_focal<-mean(sol_con$ratio[sol_con$variable=="focal_pollen"])
sol_con_non_focal<-mean(sol_con$ratio[sol_con$variable=="non_focal_pollen"])

#Solanaceae with brassicaceae pollen
sol<- subset(total_pollen, focal=="SOL")
sol_bra<- subset(sol, non_focal=="BRA")

sol_bra_focal<-mean(sol_bra$ratio[sol_bra$variable=="focal_pollen"])
sol_bra_non_focal<-mean(sol_bra$ratio[sol_bra$variable=="non_focal_pollen"])

#Convolvulaceae with solanaceae pollen
con<- subset(total_pollen, focal=="CON")
con_sol<- subset(con, non_focal=="SOL")

con_sol_focal<-mean(con_sol$ratio[con_sol$variable=="focal_pollen"])
con_sol_non_focal<-mean(con_sol$ratio[con_sol$variable=="non_focal_pollen"])

#Convolvulaceae with brassicaceae pollen
con<- subset(total_pollen, focal=="CON")
con_bra<- subset(con, non_focal=="BRA")

con_bra_focal<-mean(con_bra$ratio[con_bra$variable=="focal_pollen"])
con_bra_non_focal<-mean(con_bra$ratio[con_bra$variable=="non_focal_pollen"])

focal_pollen<- c(sol_con_focal,sol_bra_focal,con_sol_focal,con_bra_focal)
non_focal_pollen <- c(sol_con_non_focal,sol_bra_non_focal,con_sol_non_focal,con_bra_non_focal)

#brassicaceae with solanaceae pollen
bra<- subset(total_pollen, focal=="BRA")
bra_sol<- subset(bra, non_focal=="SOL")

bra_sol_focal<-mean(bra_sol$ratio[bra_sol$variable=="focal_pollen"])
bra_sol_non_focal<-mean(bra_sol$ratio[bra_sol$variable=="non_focal_pollen"])

#brassicaceae with convolvulaceae pollen
bra<- subset(total_pollen, focal=="BRA")
bra_con<- subset(bra, non_focal=="CON")

bra_con_focal<-mean(bra_con$ratio[bra_con$variable=="focal_pollen"])
bra_con_non_focal<-mean(bra_con$ratio[bra_con$variable=="non_focal_pollen"])


focal_pollen<- c(sol_con_focal,sol_bra_focal,con_sol_focal,con_bra_focal,bra_sol_focal,bra_con_focal)
non_focal_pollen <- c(sol_con_non_focal,sol_bra_non_focal,con_sol_non_focal,con_bra_non_focal,bra_sol_non_focal,bra_con_non_focal)

focal_pollen_fam<-c("SOL","SOL","CON","CON","BRA","BRA")
non_focal_pollen_fam<-c("CON","BRA","SOL","BRA","SOL","CON")
data_1<- data.frame(focal_pollen,non_focal_pollen,focal_pollen_fam,non_focal_pollen_fam)

pollen<- c(sol_con_focal,sol_con_non_focal, sol_bra_focal,sol_bra_non_focal)
treatment<- c("sol_con","sol_con","sol_bra","sol_bra")
b<-c("focal","non_focal","focal","non_focal")
d<- data.frame(pollen,treatment,b)
ggplot(d, aes(x = factor(treatment), fill = (pollen))) +
  geom_bar()

barplot(d, col=coul , border="white", xlab="group",ylim=c(0,2000))


#create color palette:
library(RColorBrewer)
coul = c("#999999","#779799")

sol_con <- c(sol_con_focal,sol_con_non_focal)
sol_bra<- c(sol_bra_focal,sol_bra_non_focal)
con_sol<- c(con_sol_focal,con_sol_non_focal)
con_bra<- c(con_bra_focal,con_bra_non_focal)
bra_con<-c(bra_con_focal,bra_con_non_focal)
bra_sol<-c(bra_sol_focal,bra_sol_non_focal)
focal<- c("focal","non_focal")
a<-data.frame(bra_con,bra_sol,con_bra,con_sol,sol_bra,sol_con)

a=as.matrix(a)
row.names(a)<- focal
barplot(a, col=coul , border="white", xlab="group",ylim=c(0,2000))
data_percentage=apply(a, 2, function(x){x*100/sum(x,na.rm=T)})
par(mar=c(7,10,2,5),xpd=T)

saveRDS(data_percentage,"Rmd/Data/per.RData")
barplot(a, hor=T,col=coul ,space=0.8, border="white",srt=80,las=1,cex.axis=0.7,cex.names=0.6,names=c("Brassicaceae-Convolvulaceae",
 "Brassicaceae-Solanaceae","Convolvulaceae-Brassicaceae","Convolvulaceae-Solanaceae","Solanaceae-Convolvulaceae","Solanaceae-Brassicaceae"))

legend(legend=c("recipient","donor"),bty="n",fill = c("#999999","#779799"))

#Prepare same style plot pollen ratios but with pollen quantities...

#Brassicaceae
total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen_brassicaceae <- total_pollen[total_pollen$focal!="I. purpurea" & total_pollen$focal!="I. aquatica"& total_pollen$focal!="S. melongena"&
                                            total_pollen$focal!="S. lycopersicum" & total_pollen$focal!="P. integrifolia"& total_pollen$focal!="C. annuum",  ]

ersa_ippu <- c(1308,8)
brol_ippu <- c(90,20)
brra_ippu <- c(193,2)
sial_ipaq <- c(1617,19)
sial_some <- c(3054,1096)
ersa_pein <- c(621,321)
brra_soly <- c(1114,246)
brol_caan <- c(281,209)

a<- data.frame(ersa_ippu,brol_ippu,brra_ippu,sial_ipaq,sial_some,ersa_pein,brra_soly,brol_caan)
a=as.matrix(a)
row.names(a) <- c("Recipient","Donor")
total_brol <- a
barplot(total_brol, hor=T,col=coul ,space=0.8, border="white",srt=80,las=1,cex.axis=0.7)                                                                                              

#Convolvulaceae

total_pollen <- read.csv("Data/total_pollen.csv")

total_pollen_convolvulaceae <- total_pollen[total_pollen$focal!="C. annuum" & total_pollen$focal!="S. melongena"& total_pollen$focal!="B. oleracea"&
                                              total_pollen$focal!="B. rapa" & total_pollen$focal!="S. alba"
                                            & total_pollen$focal!="E. sativa"& total_pollen$focal!="S. lycopersicum"& total_pollen$focal!="P. integrifolia",  ]
ippu_some <- c(138,362)
ipaq_caan <- c(35,85)
ippu_ersa <- c(27,216)
ipaq_sial <- c(42,170)

a<- data.frame(ippu_some,ipaq_caan, ippu_ersa, ipaq_sial)
a=as.matrix(a)
row.names(a) <- c("Recipient","Donor")
total_brol <- a
barplot(total_brol, hor=T,col=coul ,space=0.8, border="white",srt=80,las=1,cex.axis=0.7)                                                                                              

#Solanaceae

total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen_solanaceae <- total_pollen[total_pollen$focal!="I. purpurea" & total_pollen$focal!="I. aquatica"& total_pollen$focal!="B. oleracea"&
                                          total_pollen$focal!="B. rapa" & total_pollen$focal!="S. alba"& total_pollen$focal!="E. sativa",  ]

caan_ippu <- c(258,11)
some_ipaq <- c(64,26)
pein_ipaq <- c(23,20)
soly_ipaq <- c(56,10)
some_ersa <- c(1837,2231)
pein_brol <- c(332,376)
caan_sial <- c(304,450)
soly_brra <- c(164,208)

a<- data.frame(caan_ippu,some_ipaq,pein_ipaq,soly_ipaq,some_ersa,pein_brol,caan_sial,soly_brra)
a=as.matrix(a)
row.names(a) <- c("Recipient","Donor")
total_brol <- a
barplot(total_brol, hor=T,col=coul ,space=0.8, border="white",srt=80,las=1,cex.axis=0.7)                                                                                              


#Total pollen
total_pollen <- read.csv("Data/total_pollen.csv")
total_pollen <- total_pollen[,-1]
colnames(total_pollen)[2] <- "non_focal"
colnames(total_pollen)[3] <- "pollen_per_stigma"


ggplot(total_pollen, aes(x=spp, y=pollen_per_stigma, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+theme(legend.title = element_blank(),axis.text.x=element_text(angle=60,hjust=1))+
  scale_fill_manual(values=c("#999999","#779799"),labels = c("Recipient","Donor"))



ggplot(total_pollen, aes(x=spp, y=pollen_per_stigma, fill=variable,width=.9)) +
  geom_bar(stat='identity', position='dodge')+theme_minimal()+
  theme(legend.title = element_blank(),axis.text.x=element_text(angle=60,hjust=1,face="italic"))+
  scale_fill_manual(values=c("#999999","#779799"),labels = c("Recipient","Donor"))+labs(x = "",y="Pollen on stigma")



total_pollen <- write.csv(total_pollen, "Data/total_pollen.csv")

a <- total_pollen[1:20,]
b <- total_pollen[21:40,]

ab <- cbind(a,b)
colnames(ab)[3] <- "focal_pollen"
ab <- ab[,-c(4,5,6,7,8,10,11)]
colnames(ab)[4] <- "non_focal_pollen"
colnames(ab)[5] <- "focal_non_focal_pollen"
write.csv(ab, "Data/total_pollen_table_supp_mat.csv")
saveRDS(ab, "Thesis_Chapter_1/Tables/total_pollen.rds")
