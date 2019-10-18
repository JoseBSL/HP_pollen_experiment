#In this script I'm going to calculate the effect sizes and plot them.
#Moreover I will compare differences among species and among families

#loadlibrary
#i#install.packages("effsize")
library(effsize)
library(dplyr)
library(ggplot2)
library(ggpubr)
#install.packages("effsize")

load("Data/seed_set&scaled_seed_set.RData")
#Reading correct file of SIAL and SOME is lacking one Treatment for both in the old file
sial <- read.csv("Data/species_seed_set/SIAL_seed_set.csv", sep=";")
sial$Scale_seed <- scale(sial$Seed.production)
some <- read.csv("Data/species_seed_set/SOME_seed_set.csv", sep=";")
some$Scale_seed <- scale(some$seed_set)
some<- some[,-4]

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

####
#SOLANACEAE
####
#SOLY

#First we subset the long data frame for our species of interest
soly_seeds <- subset(y, Species=="SOLY")
#Solanaceae
soly_seeds$Family[soly_seeds$Treatment=="CROSS"] <- "A"
soly_seeds$Family[soly_seeds$Treatment=="SOME 50%"] <- "S. lycopersicum"
soly_seeds$Family[soly_seeds$Treatment=="CAAN 50%"] <- "S. lycopersicum"
soly_seeds$Family[soly_seeds$Treatment=="PEIN 50%"] <- "S. lycopersicum"

#Brassicaceae
soly_seeds$Family[soly_seeds$Treatment=="BRRA 50%"] <- "S. lycopersicum"
soly_seeds$Family[soly_seeds$Treatment=="SIAL 50%"] <- "S. lycopersicum"
soly_seeds$Family[soly_seeds$Treatment=="ERSA 50%"] <- "S. lycopersicum"
soly_seeds$Family[soly_seeds$Treatment=="BROL 50%"] <- "S. lycopersicum"

#Convolvulaceae
soly_seeds$Family[soly_seeds$Treatment=="IPAQ 50%"] <- "S. lycopersicum"
soly_seeds$Family[soly_seeds$Treatment=="IPPU 50%"] <- "S. lycopersicum"

soly_cross <- subset(soly_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(soly_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(soly_seeds$Seed_set[soly_seeds$Family==i], soly_cross$Seed_set,hedges.correction=TRUE)
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

soly_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(soly_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")


soly_effect_size <- subset(soly_effect_size, Family=="S. lycopersicum")

#SOME

#First we subset the long data frame for our species of interest
some_seeds <- subset(y, Species=="SOME")
#Solanaceae
some_seeds$Family[some_seeds$Treatment=="CROSS"] <- "A"
some_seeds$Family[some_seeds$Treatment=="SOLY 50%"] <- "S. melongena"
some_seeds$Family[some_seeds$Treatment=="CAAN 50%"] <- "S. melongena"
some_seeds$Family[some_seeds$Treatment=="PEIN 50%"] <- "S. melongena"

#Brassicaceae
some_seeds$Family[some_seeds$Treatment=="BRRA 50%"] <- "S. melongena"
some_seeds$Family[some_seeds$Treatment=="SIAL 50%"] <- "S. melongena"
some_seeds$Family[some_seeds$Treatment=="ERSA 50%"] <- "S. melongena"
some_seeds$Family[some_seeds$Treatment=="BROL 50%"] <- "S. melongena"

#Convolvulaceae
some_seeds$Family[some_seeds$Treatment=="IPAQ 50%"] <- "S. melongena"
some_seeds$Family[some_seeds$Treatment=="IPPU 50%"] <- "S. melongena"

some_cross <- subset(some_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(some_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(some_seeds$Seed_set[some_seeds$Family==i], some_cross$Seed_set,hedges.correction=TRUE)
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

some_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(some_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")


some_effect_size <- subset(some_effect_size, Family=="S. melongena")

all <- rbind(some_effect_size, soly_effect_size)

#CAAN

#First we subset the long data frame for our species of interest
caan_seeds <- subset(y, Species=="CAAN")
#Solanaceae
caan_seeds$Family[caan_seeds$Treatment=="CROSS"] <- "A"
caan_seeds$Family[caan_seeds$Treatment=="SOLY 50%"] <- "C. annuum"
caan_seeds$Family[caan_seeds$Treatment=="SOME 50%"] <- "C. annuum"
caan_seeds$Family[caan_seeds$Treatment=="PEIN 50%"] <- "C. annuum"

#Brassicaceae
caan_seeds$Family[caan_seeds$Treatment=="BRRA 50%"] <- "C. annuum"
caan_seeds$Family[caan_seeds$Treatment=="SIAL 50%"] <- "C. annuum"
caan_seeds$Family[caan_seeds$Treatment=="ERSA 50%"] <- "C. annuum"
caan_seeds$Family[caan_seeds$Treatment=="BROL 50%"] <- "C. annuum"

#Convolvulaceae
caan_seeds$Family[caan_seeds$Treatment=="IPAQ 50%"] <- "C. annuum"
caan_seeds$Family[caan_seeds$Treatment=="IPPU 50%"] <- "C. annuum"

caan_cross <- subset(caan_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(caan_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(caan_seeds$Seed_set[caan_seeds$Family==i], caan_cross$Seed_set,hedges.correction=TRUE)
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

caan_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(caan_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")


caan_effect_size <- subset(caan_effect_size, Family=="C. annuum")

all <- rbind(all, caan_effect_size)

#PEIN

#First we subset the long data frame for our species of interest
pein_seeds <- subset(y, Species=="PEIN")
#Solanaceae
pein_seeds$Family[pein_seeds$Treatment=="CROSS"] <- "A"
pein_seeds$Family[pein_seeds$Treatment=="SOLY 50%"] <- "P. integrifolia"
pein_seeds$Family[pein_seeds$Treatment=="SOME 50%"] <- "P. integrifolia"
pein_seeds$Family[pein_seeds$Treatment=="CAAN 50%"] <- "P. integrifolia"

#Brassicaceae
pein_seeds$Family[pein_seeds$Treatment=="BRRA 50%"] <- "P. integrifolia"
pein_seeds$Family[pein_seeds$Treatment=="SIAL 50%"] <- "P. integrifolia"
pein_seeds$Family[pein_seeds$Treatment=="ERSA 50%"] <- "P. integrifolia"
pein_seeds$Family[pein_seeds$Treatment=="BROL 50%"] <- "P. integrifolia"
pein
#Convolvulaceae
pein_seeds$Family[pein_seeds$Treatment=="IPAQ 50%"] <- "P. integrifolia"
pein_seeds$Family[pein_seeds$Treatment=="IPPU 50 %"] <- "P. integrifolia"

pein_cross <- subset(pein_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(pein_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(pein_seeds$Seed_set[pein_seeds$Family==i], pein_cross$Seed_set,hedges.correction=TRUE)
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

pein_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(pein_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")


pein_effect_size <- subset(pein_effect_size, Family=="P. integrifolia")
str(pein_effect_size)
all <- rbind(all, pein_effect_size)
#Effect sizes for solanaceae species done
all_solanaceae <- all

p1<- ggplot(all_solanaceae, aes(Family,Cohen_d, size=10)) + theme_bw(base_size=10)
p1 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +
  geom_errorbar(show.legend=FALSE, aes(x = Family, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(Family)), width = 0.2)+
  scale_color_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#BROL

#First we subset the long data frame for our species of interest
brol_seeds <- subset(y, Species=="BROL")
#Solanaceae
brol_seeds$Family[brol_seeds$Treatment=="PEIN 50%"] <- "B. oleracea"
brol_seeds$Family[brol_seeds$Treatment=="SOLY 50%"] <- "B. oleracea"
brol_seeds$Family[brol_seeds$Treatment=="SOME 50%"] <- "B. oleracea"
brol_seeds$Family[brol_seeds$Treatment=="CAAN 50%"] <- "B. oleracea"

#Brassicaceae
brol_seeds$Family[brol_seeds$Treatment=="BRRA 50%"] <- "B. oleracea"
brol_seeds$Family[brol_seeds$Treatment=="SIAL 50%"] <- "B. oleracea"
brol_seeds$Family[brol_seeds$Treatment=="ERSA 50%"] <- "B. oleracea"
brol_seeds$Family[brol_seeds$Treatment=="Cross"] <- "A"

#Convolvulaceae
brol_seeds$Family[brol_seeds$Treatment=="IPAQ 50%"] <- "B. oleracea"
brol_seeds$Family[brol_seeds$Treatment=="IPPU 50%"] <- "B. oleracea"

brol_cross <- subset(brol_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(brol_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(brol_seeds$Seed_set[brol_seeds$Family==i], brol_cross$Seed_set,hedges.correction=TRUE)
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


brol_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(brol_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")
brol_effect_size <- subset(brol_effect_size, Family=="B. oleracea")

#BRRA

#First we subset the long data frame for our species of interest
brra_seeds <- subset(y, Species=="BRRA")
#Solanaceae
brra_seeds$Family[brra_seeds$Treatment=="PEIN 50%"] <- "B. rapa"
brra_seeds$Family[brra_seeds$Treatment=="SOLY 50%"] <- "B. rapa"
brra_seeds$Family[brra_seeds$Treatment=="SOME 50%"] <- "B. rapa"
brra_seeds$Family[brra_seeds$Treatment=="CAAN 50%"] <- "B. rapa"

#Brassicaceae
brra_seeds$Family[brra_seeds$Treatment=="BROL 50%"] <- "B. rapa"
brra_seeds$Family[brra_seeds$Treatment=="SIAL 50%"] <- "B. rapa"
brra_seeds$Family[brra_seeds$Treatment=="ERSA 50%"] <- "B. rapa"
brra_seeds$Family[brra_seeds$Treatment=="Cross"] <- "A"

#Convolvulaceae
brra_seeds$Family[brra_seeds$Treatment=="IPAQ 50%"] <- "B. rapa"
brra_seeds$Family[brra_seeds$Treatment=="IPPU 50%"] <- "B. rapa"

brra_cross <- subset(brra_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(brra_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(brra_seeds$Seed_set[brra_seeds$Family==i], brra_cross$Seed_set,hedges.correction=TRUE)
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

brra_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(brra_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")
brra_effect_size <- subset(brra_effect_size, Family=="B. rapa")

all_brassicaceae <- rbind(brol_effect_size, brra_effect_size)

#SIAL

#First we subset the long data frame for our species of interest
sial_seeds <- subset(y, Species=="SIAL")
#Solanaceae
sial_seeds$Family[sial_seeds$Treatment=="PEIN 50%"] <- "S. alba"
sial_seeds$Family[sial_seeds$Treatment=="SOLY 50%"] <- "S. alba"
sial_seeds$Family[sial_seeds$Treatment=="SOME 50%"] <- "S. alba"
sial_seeds$Family[sial_seeds$Treatment=="CAAN 50%"] <- "S. alba"

#Brassicaceae
sial_seeds$Family[sial_seeds$Treatment=="BROL 50%"] <- "S. alba"
sial_seeds$Family[sial_seeds$Treatment=="BRRA 50%"] <- "S. alba"
sial_seeds$Family[sial_seeds$Treatment=="ERSA 50%"] <- "S. alba"
sial_seeds$Family[sial_seeds$Treatment=="Cross"] <- "A"

#Convolvulaceae
sial_seeds$Family[sial_seeds$Treatment=="IPAQ 50%"] <- "S. alba"
sial_seeds$Family[sial_seeds$Treatment=="IPPU 50%"] <- "S. alba"

sial_cross <- subset(sial_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(sial_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(sial_seeds$Seed_set[sial_seeds$Family==i], sial_cross$Seed_set,hedges.correction=TRUE)
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

sial_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(sial_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")
sial_effect_size <- subset(sial_effect_size, Family=="S. alba")

all_brassicaceae <- rbind(all_brassicaceae, sial_effect_size)

#ERSA

#First we subset the long data frame for our species of interest
ersa_seeds <- subset(y, Species=="ERSA")
#Solanaceae
ersa_seeds$Family[ersa_seeds$Treatment=="PEIN 50%"] <- "E. sativa"
ersa_seeds$Family[ersa_seeds$Treatment=="SOLY 50%"] <- "E. sativa"
ersa_seeds$Family[ersa_seeds$Treatment=="SOME 50%"] <- "E. sativa"
ersa_seeds$Family[ersa_seeds$Treatment=="CAAN 50%"] <- "E. sativa"

#Brassicaceae
ersa_seeds$Family[ersa_seeds$Treatment=="BROL 50%"] <- "E. sativa"
ersa_seeds$Family[ersa_seeds$Treatment=="BRRA 50%"] <- "E. sativa"
ersa_seeds$Family[ersa_seeds$Treatment=="SIAL 50%"] <- "E. sativa"
ersa_seeds$Family[ersa_seeds$Treatment=="Cross"] <- "A"

#Convolvulaceae
ersa_seeds$Family[ersa_seeds$Treatment=="IPAQ 50%"] <- "E. sativa"
ersa_seeds$Family[ersa_seeds$Treatment=="IPPU 50%"] <- "E. sativa"

ersa_cross <- subset(ersa_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(ersa_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(ersa_seeds$Seed_set[ersa_seeds$Family==i], ersa_cross$Seed_set,hedges.correction=TRUE)
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

ersa_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(ersa_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")
ersa_effect_size <- subset(ersa_effect_size, Family=="E. sativa")

all_brassicaceae <- rbind(all_brassicaceae, ersa_effect_size)


p1<- ggplot(all_brassicaceae, aes(Family,Cohen_d, size=10)) + theme_bw(base_size=10)
p1 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +
  geom_errorbar(show.legend=FALSE, aes(x = Family, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(Family)), width = 0.2)+
  scale_color_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


#IPPU

#First we subset the long data frame for our species of interest
ippu_seeds <- subset(y, Species=="IPPU")
#Solanaceae
ippu_seeds$Family[ippu_seeds$Treatment=="PEIN 50%"] <- "I. purpurea"
ippu_seeds$Family[ippu_seeds$Treatment=="SOLY 50%"] <- "I. purpurea"
ippu_seeds$Family[ippu_seeds$Treatment=="SOME 50%"] <- "I. purpurea"
ippu_seeds$Family[ippu_seeds$Treatment=="CAAN 50%"] <- "I. purpurea"

#Brassicaceae
ippu_seeds$Family[ippu_seeds$Treatment=="BROL 50%"] <- "I. purpurea"
ippu_seeds$Family[ippu_seeds$Treatment=="BRRA 50%"] <- "I. purpurea"
ippu_seeds$Family[ippu_seeds$Treatment=="SIAL 50%"] <- "I. purpurea"
ippu_seeds$Family[ippu_seeds$Treatment=="ERSA 50%"] <- "I. purpurea"

#Convolvulaceae
ippu_seeds$Family[ippu_seeds$Treatment=="IPAQ 50%"] <- "I. purpurea"
ippu_seeds$Family[ippu_seeds$Treatment=="cross"] <- "A"

ippu_cross <- subset(ippu_seeds, Treatment=="cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(ippu_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(ippu_seeds$Seed_set[ippu_seeds$Family==i], ippu_cross$Seed_set,hedges.correction=TRUE)
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

ippu_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(ippu_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")
ippu_effect_size <- subset(ippu_effect_size, Family=="I. purpurea")


#IPAQ

#First we subset the long data frame for our species of interest
ipaq_seeds <- subset(y, Species=="IPAQ")
#Solanaceae
ipaq_seeds$Family[ipaq_seeds$Treatment=="PEIN 50%"] <- "I. aquatica"
ipaq_seeds$Family[ipaq_seeds$Treatment=="SOLY 50%"] <- "I. aquatica"
ipaq_seeds$Family[ipaq_seeds$Treatment=="SOME 50%"] <- "I. aquatica"
ipaq_seeds$Family[ipaq_seeds$Treatment=="CAAN 50%"] <- "I. aquatica"

#Brassicaceae
ipaq_seeds$Family[ipaq_seeds$Treatment=="BROL 50%"] <- "I. aquatica"
ipaq_seeds$Family[ipaq_seeds$Treatment=="BRRA 50%"] <- "I. aquatica"
ipaq_seeds$Family[ipaq_seeds$Treatment=="SIAL 50%"] <- "I. aquatica"
ipaq_seeds$Family[ipaq_seeds$Treatment=="ERSA 50%"] <- "I. aquatica"

#Convolvulaceae
ipaq_seeds$Family[ipaq_seeds$Treatment=="IPPU 50%"] <- "I. aquatica"
ipaq_seeds$Family[ipaq_seeds$Treatment=="cross"] <- "A"

ipaq_cross <- subset(ipaq_seeds, Treatment=="cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(ipaq_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(ipaq_seeds$Seed_set[ipaq_seeds$Family==i], ipaq_cross$Seed_set,hedges.correction=TRUE)
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

ipaq_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(ipaq_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")
ipaq_effect_size <- subset(ipaq_effect_size, Family=="I. aquatica")


all_convolvulaceae <- rbind(ippu_effect_size, ipaq_effect_size)


p1<- ggplot(all_convolvulaceae, aes(Family,Cohen_d, size=10)) + theme_bw(base_size=10)
p1 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +
  geom_errorbar(show.legend=FALSE, aes(x = Family, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(Family)), width = 0.2)+
  scale_color_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


#Now I order it alphabetically each data.frame

all_solanaceae$Family <- as.character(all_solanaceae$Family)
all_convolvulaceae$Family <- as.character(all_convolvulaceae$Family)
all_brassicaceae$Family <- as.character(all_brassicaceae$Family)

all_solanaceae <- all_solanaceae %>% arrange(desc(Family))
all_convolvulaceae <- all_convolvulaceae %>% arrange(desc(Family))
all_brassicaceae <- all_brassicaceae %>% arrange(desc(Family))

all<- rbind(all_solanaceae,all_convolvulaceae, all_brassicaceae)
saveRDS(all, "Data/effect_size_all.RData")
saveRDS(all, "Manuscript_draft/Data/effect_size_all.RData")

all$Family_1 <- "S"
all$Family_1[1:4]<- "B" 
all$Family_1[5:6]<- "C"

all$Family <- factor(all$Family, levels = all$Family)
p1<- ggplot(all, aes(Family,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(axis.text.y = element_text(face = "italic"))
p1 + geom_point(show.legend = FALSE,aes(color=factor(Family_1))) +
  geom_errorbar(show.legend=FALSE, aes(x = Family, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(Family_1)), width = 0.2)+
  scale_color_manual("Family",values=c( "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c( "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Hedges' g") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#save.image("Manuscript_draft/effect_size_species/effect_size_total.RData")
traits_all <- read.csv("Data/traits_all.csv", sep=",")
traits_all$species <- c("BROL", "BRRA", "CAAN", "ERSA", "IPAQ", "IPPU", 
                        "PEIN", "SIAL", "SOLY", "SOME") 
colnames(traits_all)[2] <- "Species"
traits_all=traits_all[,-1]
all$Family <- c("SOME", "SOLY", "PEIN", "CAAN", "IPPU", "IPAQ", "SIAL","ERSA", "BRRA", "BROL")
colnames(all)[1] <- "Species"
all <- all[order(all$Species),]
all_t <-merge(all, traits_all, by="Species")
si_index <- readRDS("Data/si_index.RData")
str(all_t)
all_t$si_index <- si_index
all_t$stigma_type <- as.character(all_t$stigma_type)
p1<- ggplot(all_t, aes(Species,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(axis.text.y = element_text(face = "italic"))
p1 + geom_point(show.legend = FALSE,aes(color=factor(stigma_type))) +
  geom_errorbar(show.legend=FALSE, aes(x = Species, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(stigma_type)), width = 0.2)+
  scale_color_manual("Family",values=c( "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c( "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Hedges' g") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

#Pollen size

all_t_ordered <- all_t[order(all_t$mean_ovules),]
all_t_ordered$Species <- factor(all_t_ordered$Species, levels = all_t_ordered$Species)
p1<- ggplot(all_t_ordered, aes(mean_ovules,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(axis.text.y = element_text(face = "italic"))
p1 + geom_point(show.legend = FALSE,aes(color=factor(stigma_type))) +
  geom_errorbar(show.legend=FALSE, aes(x = mean_ovules, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(stigma_type)), width = 0.2)+
  scale_color_manual("Family",values=c( "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c( "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Hedges' g") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

p1<- ggplot(all_t_ordered, aes(stigma_width,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(axis.text.y = element_text(face = "italic"))
p1 + geom_point(show.legend = FALSE,aes(color=factor(stigma_width))) +
  geom_errorbar(show.legend=FALSE, aes(x = stigma_width, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(stigma_width)), width = 0.2)+
  scale_fill_manual("Family",values=c( ))+
  xlab("Treatments") + ylab("Hedges' g") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


p1<- ggplot(all_t_ordered, aes(si_index,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(axis.text.y = element_text(face = "italic"))
p1 + geom_point(show.legend = FALSE,aes(color=factor(si_index))) +
  geom_errorbar(show.legend=FALSE, aes(x = si_index, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(si_index)), width = 0.2)+
  scale_fill_manual("Family",values=c( ))+
  xlab("Treatments") + ylab("Hedges' g") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")