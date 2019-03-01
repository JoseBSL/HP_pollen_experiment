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
#I decide what to do later, probably best leave it in blank
some_ippu <- subset(some_seeds, Treatment=="IPPU 50%")

some_ippu$Treatment[some_ippu$Treatment=="IPPU 50%"] <- "IPAQ 50%"
some_ipaq <- some_ippu[,1:5]
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