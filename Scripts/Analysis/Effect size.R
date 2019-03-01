#In this script I'm going to calculate the effect sizes and plot them.
#Moreover I will compare differences among species and among families

#loadlibrary
#install.packages("effsize")
library(effsize)
library(dplyr)
library(ggplot2)
library(ggpubr)

load("seed_set&scaled_seed_set.RData")

#Preparing for loop to clean dataframe and select columns of interest
species_list <- list(soly, some, pein, caan, ersa, brra, sial, brol, ippu, ipaq)
i <- NULL
y <- NULL

for (i in species_list){
  colnames(i)<- c("Species", "Treatment", "Treatment_number", "Seed_set", "Scale_seed")
  i <- filter(i, Treatment!="RARA 50%", Treatment!="COSA 50%", Treatment!="Self", Treatment!="Control",Treatment!="Flower Control",
              Treatment!="FC", Treatment!="FLOWER CONTROL", Treatment!="control", Treatment!="flower control", Treatment!="self", Treatment!="SELF",
              Treatment!="CONTROL")
  i <-i[-grep("100", i$Treatment),] 
  
  y <- rbind(y, i)
}

#Now I prepare effect sizes for SOLY


####
#SOLANACEAE
####
#SOLY

#First we subset the long data frame for our species of interest
soly_seeds <- subset(y, Species=="SOLY")
str(soly_seeds)

#We order alphabetically to be able to replicate exactly the same for all the species
soly_seeds <- soly_seeds[order(soly_seeds$Treatment, soly_seeds$Seed_set), ]
#Subset cross for Cohen's d on the loop
soly_cross <- subset(soly_seeds, Treatment=="CROSS")

#Just checking a first example of how it would be
soly_ippu <- subset(soly_seeds, Treatment=="IPPU 50%")
soly_ipaq <- subset(soly_seeds, Treatment=="IPAQ 50%")
a <- cohen.d(soly_ippu$Seed_set, soly_cross$Seed_set)
b <- cohen.d(soly_ipaq$Seed_set, soly_cross$Seed_set)

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(soly_seeds$Treatment))

b <- NULL
x <- NULL
for (i in species){
  a<-cohen.d(soly_seeds$Seed_set[soly_seeds$Treatment==i], soly_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

#Now we convert the list to a data frame to plot it
lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa", "C. annuum", "S. lycopersicum.", "E. sativa", "I. aquatica", "I. purpurea",
            "P. integrifolia", "S. alba", "S. melongena")

Family <- c("B", "B", "S", "P", "B", "C", "C", "S", "B", "S")
soly_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(soly_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(soly_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(soly_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


#PEIN

pein_seeds <- subset(y, Species=="PEIN")
#We order alphabetically to be able to replicate exactly the same for all the species
pein_seeds <- pein_seeds[order(pein_seeds$Treatment, pein_seeds$Seed_set), ]
pein_cross <- subset(pein_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(pein_seeds$Treatment))
b <- NULL
x <- NULL
a <- NULL
for (i in species){
  a<-cohen.d(pein_seeds$Seed_set[pein_seeds$Treatment==i], pein_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa", "C. annuum", "P. integrifolia", "E. sativa", "I. aquatica", "I. purpurea",
               "S. alba", "S. lycopersicum", "S. melongena")

Family <- c("B", "B", "S", "P", "B", "C", "C", "S", "B", "S")
pein_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(pein_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(pein_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(pein_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


#CAAN

caan_seeds <- subset(y, Species=="CAAN")

#We order alphabetically to be able to replicate exactly the same for all the species
caan_seeds <- caan_seeds[order(caan_seeds$Treatment, caan_seeds$Seed_set), ]
caan_cross <- subset(caan_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(caan_seeds$Treatment))
b <- NULL
x <- NULL
a <- NULL
for (i in species){
  a<-cohen.d(caan_seeds$Seed_set[caan_seeds$Treatment==i], caan_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa", "C. annuum", "E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. alba", "S. lycopersicum", "S. melongena")

Family <- c("B", "B", "P", "B", "C", "C", "S", "B","S", "S")
caan_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(caan_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(caan_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(caan_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#SOME

some_seeds <- subset(y, Species=="SOME")

#We order alphabetically to be able to replicate exactly the same for all the species
some_seeds <- some_seeds[order(some_seeds$Treatment, some_seeds$Seed_set), ]
some_cross <- subset(some_seeds, Treatment=="CROSS")
#We lack a treatment I duplicate with one spp of the same family
#I decide what to do later, probably best leave it in blank as I have done
some_ippu <- subset(some_seeds, Treatment=="IPPU 50%")

some_ippu$Treatment[some_ippu$Treatment=="IPPU 50%"] <- "IPAQ 50%"
some_ipaq <- some_ippu[,1:5]
some_ipaq$Seed_set <- NA
some_seeds <- rbind(some_seeds, some_ipaq) 


#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(some_seeds$Treatment))
b <- NULL
x <- NULL
a <- NULL
for (i in species){
  a<-cohen.d(some_seeds$Seed_set[some_seeds$Treatment==i], some_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa", "C. annuum", "S. melongena", "E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. alba", "S. lycopersicum")

Family <- c("B", "B", "S", "P", "B", "C","C", "S", "B","S")
some_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(some_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(some_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(some_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


####
#BRASSICACEAE
####

#BROL
brol_seeds <- subset(y, Species=="BROL")

#We order alphabetically to be able to replicate exactly the same for all the species
brol_seeds <- brol_seeds[order(brol_seeds$Treatment, brol_seeds$Seed_set), ]
brol_cross <- subset(brol_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(brol_seeds$Treatment))
b <- NULL
x <- NULL
a <- NULL
for (i in species){
  a<-cohen.d(brol_seeds$Seed_set[brol_seeds$Treatment==i], brol_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. rapa","C. annuum","B. oleracea", "E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. alba", "S. lycopersicum","S. melongena")

Family <- c("B", "S", "P", "B", "C","C", "S", "B","S", "S")
brol_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(brol_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(brol_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(brol_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#BRRA
brra_seeds <- subset(y, Species=="BRRA")

#We order alphabetically to be able to replicate exactly the same for all the species
brra_seeds <- brra_seeds[order(brra_seeds$Treatment, brra_seeds$Seed_set), ]
brra_cross <- subset(brra_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(brra_seeds$Treatment))
b <- NULL
x <- NULL
a <- NULL
for (i in species){
  a<-cohen.d(brra_seeds$Seed_set[brra_seeds$Treatment==i], brra_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","C. annuum","B. rapa", "E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. alba", "S. lycopersicum","S. melongena")

Family <- c("B", "S", "P", "B", "C","C", "S", "B","S", "S")
brra_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(brra_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(brra_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(brra_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#SIAL
sial_seeds <- subset(y, Species=="SIAL")

#We order alphabetically to be able to replicate exactly the same for all the species
sial_seeds <- sial_seeds[order(sial_seeds$Treatment, sial_seeds$Seed_set), ]
sial_cross <- subset(sial_seeds, Treatment=="Cross")

#We lack a treatment I duplicate with one spp of the same family
#I decide what to do later, probably best leave it in blank as I have done
sial_ippu <- subset(sial_seeds, Treatment=="IPPU 50%")

sial_ippu$Treatment[sial_ippu$Treatment=="IPPU 50%"] <- "IPAQ 50%"
sial_ipaq <- sial_ippu[,1:5]
sial_ipaq$Seed_set <- NA
sial_seeds <- rbind(sial_seeds, sial_ipaq) 

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(sial_seeds$Treatment))
b <- NULL
x <- NULL
a <- NULL
for (i in species){
  a<-cohen.d(sial_seeds$Seed_set[sial_seeds$Treatment==i], sial_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa","C. annuum", "S. alba","E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. lycopersicum","S. melongena")

Family <- c("B", "B", "S", "P","B", "C","C", "S","S", "S")
sial_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(sial_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(sial_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(sial_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#ERSA
ersa_seeds <- subset(y, Species=="ERSA")

#We order alphabetically to be able to replicate exactly the same for all the species
ersa_seeds <- ersa_seeds[order(ersa_seeds$Treatment, ersa_seeds$Seed_set), ]
ersa_cross <- subset(ersa_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(ersa_seeds$Treatment))
b <- NULL
x <- NULL
a <- NULL
for (i in species){
  a<-cohen.d(ersa_seeds$Seed_set[ersa_seeds$Treatment==i], ersa_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa","C. annuum","E. sativa", "I. aquatica","I. purpurea", "P. integrifolia","S. alba",
               "S. lycopersicum","S. melongena")

Family <- c("B", "B", "S", "P","C","C", "S","B","S", "S")
ersa_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(ersa_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(ersa_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(ersa_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

####
#CONVOLVULACEAE
####


#IPPU
ippu_seeds <- subset(y, Species=="IPPU")

#We order alphabetically to be able to replicate exactly the same for all the species
ippu_seeds <- ippu_seeds[order(ippu_seeds$Treatment, ippu_seeds$Seed_set), ]
ippu_cross <- subset(ippu_seeds, Treatment=="cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(ippu_seeds$Treatment))
b <- NULL
x <- NULL
a <- NULL
for (i in species){
  a<-cohen.d(ippu_seeds$Seed_set[ippu_seeds$Treatment==i], ippu_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa","C. annuum","I. purpurea","E. sativa", "I. aquatica", "P. integrifolia","S. alba",
               "S. lycopersicum","S. melongena")

Family <- c("B", "B", "S", "P","B","C", "S","B","S", "S")
ippu_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(ippu_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(ippu_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(ippu_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#IPAQ
ipaq_seeds <- subset(y, Species=="IPAQ")

#We order alphabetically to be able to replicate exactly the same for all the species
ipaq_seeds <- ipaq_seeds[order(ipaq_seeds$Treatment, ipaq_seeds$Seed_set), ]
ipaq_cross <- subset(ipaq_seeds, Treatment=="cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(ipaq_seeds$Treatment))
b <- NULL
x <- NULL
a <- NULL
for (i in species){
  a<-cohen.d(ipaq_seeds$Seed_set[ipaq_seeds$Treatment==i], ipaq_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa","C. annuum","I. aquatica","E. sativa", "I. purpurea", "P. integrifolia","S. alba",
               "S. lycopersicum","S. melongena")

Family <- c("B", "B", "S", "P","B","C", "S","B","S", "S")
ipaq_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(ipaq_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(ipaq_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(ipaq_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")