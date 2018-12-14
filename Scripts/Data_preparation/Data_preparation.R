
#Preparing a long format dataframe for analysis
#I found some errors of labelling inconsistencies across species, I'm going to fix them

library(dplyr)

pein_seed_set_final <- read.csv("Rmd/Data/pein_seed_set_final.csv")
pein_seed_set_final=pein_seed_set_final[,-1]

unique(pein_seed_set_final$Species)
unique(pein_seed_set_final$Treatment)
head(pein_seed_set_final)
pein_seed_set_final$Scaled_seed_set=scale(pein_seed_set_final$Seed.production)
str(pein_seed_set_final)
pein_seed_set_final$Species=as.character(pein_seed_set_final$Species)
pein_seed_set_final$Species[pein_seed_set_final$Species=="PEIN"] <- "Petunia integrifolia"


soly_seed_set_final <- read.csv("Rmd/Data/soly_seed_set_final.csv")
soly_seed_set_final=soly_seed_set_final[,-1]
unique(soly_seed_set_final$Species)
unique(soly_seed_set_final$Treatment)
soly_seed_set_final$Scaled_seed_set=scale(soly_seed_set_final$Seed.production)
soly_seed_set_final$Species=as.character(soly_seed_set_final$Species)
soly_seed_set_final$Species[soly_seed_set_final$Species=="SOLY"] <- "Solanum lycopersicum"

Species=rbind(pein_seed_set_final,soly_seed_set_final)

some_seed_set_final <- read.csv("Rmd/Data/some_seed_set_final.csv")
some_seed_set_final=some_seed_set_final[,-1]
unique(some_seed_set_final$Species)
#Fix this
unique(some_seed_set_final$Treatment)
some_seed_set_final$Scaled_seed_set=scale(some_seed_set_final$Seed.production)
some_seed_set_final$Species=as.character(some_seed_set_final$Species)
some_seed_set_final$Species[some_seed_set_final$Species=="SOME"] <- "Solanum melongena"
some_seed_set_final$Species[some_seed_set_final$Species=="SOME "] <- "Solanum melongena"

Species=rbind(Species,some_seed_set_final)


caan_seed_set_final <- read.csv("Rmd/Data/caan_seed_set_final.csv")
caan_seed_set_final=caan_seed_set_final[,-1]
unique(caan_seed_set_final$Species)
unique(caan_seed_set_final$Treatment)
caan_seed_set_final$Scaled_seed_set=scale(caan_seed_set_final$Seed.production)
caan_seed_set_final$Species=as.character(caan_seed_set_final$Species)
caan_seed_set_final$Species[caan_seed_set_final$Species=="CAAN"] <- "Capsicum annuum"

Species=rbind(Species,caan_seed_set_final)

brol_seed_set_final <- read.csv("Rmd/Data/brol_seed_set_final.csv")
brol_seed_set_final=brol_seed_set_final[,-1]
unique(brol_seed_set_final$Species)
unique(brol_seed_set_final$Treatment)
brol_seed_set_final$Scaled_seed_set=scale(brol_seed_set_final$Seed.production)
brol_seed_set_final$Species=as.character(brol_seed_set_final$Species)
brol_seed_set_final$Species[brol_seed_set_final$Species=="BROL"] <- "Brassica oleracea"

Species=rbind(Species,brol_seed_set_final)

brra_seed_set_final <- read.csv("Rmd/Data/brra_seed_set_final.csv")
brra_seed_set_final=brra_seed_set_final[,-1]
unique(brra_seed_set_final$Species)
unique(brra_seed_set_final$Treatment)
brra_seed_set_final$Scaled_seed_set=scale(brra_seed_set_final$Seed.production)
brra_seed_set_final$Species=as.character(brra_seed_set_final$Species)
brra_seed_set_final$Species[brra_seed_set_final$Species=="BRRA"] <- "Brassica rapa"

Species=rbind(Species,brra_seed_set_final)

sial_seed_set_final <- read.csv("Rmd/Data/sial_seed_set_final.csv")
sial_seed_set_final=sial_seed_set_final[,-1]
unique(sial_seed_set_final$Species)
unique(sial_seed_set_final$Treatment)
#For this species I lack a treatment with IPAQ pollen
head(sial_seed_set_final)
add_row(sial_seed_set_final, Species="SIAL", Treatment="Ipomoea aquatica",Treatment.number=c(1:10),
        Seed.production=rep(NA,10), Family="Convolvulaceae")

sial_seed_set_final=rbind(sial_seed_set_final,ipaq_sial)
sial_seed_set_final$Scaled_seed_set=scale(sial_seed_set_final$Seed.production)
sial_seed_set_final$Species=as.character(sial_seed_set_final$Species)
sial_seed_set_final$Species[sial_seed_set_final$Species=="SIAL"] <- "Sinapis alba"


Species=rbind(Species,sial_seed_set_final)
unique(Species$Species)


ersa_seed_set_final <- read.csv("Rmd/Data/ersa_seed_set_final.csv")
ersa_seed_set_final=ersa_seed_set_final[,-1]
unique(ersa_seed_set_final$Species)
unique(ersa_seed_set_final$Treatment)
ersa_seed_set_final$Scaled_seed_set=scale(ersa_seed_set_final$Seed.production)
ersa_seed_set_final$Species=as.character(ersa_seed_set_final$Species)
ersa_seed_set_final$Species[ersa_seed_set_final$Species=="ERSA"] <- "Eruca vesicaria"

Species=rbind(Species,ersa_seed_set_final)

ipaq_seed_set_final <- read.csv("Rmd/Data/ipaq_seed_set_final.csv")
ipaq_seed_set_final=ipaq_seed_set_final[,-1]
unique(ipaq_seed_set_final$Species)
unique(ipaq_seed_set_final$Treatment)
ipaq_seed_set_final$Scaled_seed_set=scale(ipaq_seed_set_final$Seed.production)
ipaq_seed_set_final$Species=as.character(ipaq_seed_set_final$Species)
ipaq_seed_set_final$Species[ipaq_seed_set_final$Species=="IPAQ"] <- "Ipomoea aquatica"

Species=rbind(Species,ipaq_seed_set_final)


ippu_seed_set_final <- read.csv("Rmd/Data/ippu_seed_set_final.csv")
ippu_seed_set_final=ippu_seed_set_final[,-1]
unique(ippu_seed_set_final$Species)
unique(ippu_seed_set_final$Treatment)
ippu_seed_set_final$Scaled_seed_set=scale(ippu_seed_set_final$Seed.production)
ippu_seed_set_final$Species=as.character(ippu_seed_set_final$Species)
ippu_seed_set_final$Species[ippu_seed_set_final$Species=="IPPU"] <- "Ipomoea purpurea"

Species=rbind(Species,ippu_seed_set_final)
unique(Species$Species)
unique(Species$Treatment)
str(Species)
#Now I have the 10 spp. I call all the treatments in a similar way.
Species$Treatment=as.character(Species$Treatment)
Species$Treatment[Species$Treatment=="CROSS"] <-"Cross"
Species$Treatment[Species$Treatment=="SELF"] <-"Self"
Species$Treatment[Species$Treatment=="CONTROL"] <-"Control"
Species$Treatment[Species$Treatment=="FLOWER CONTROL"] <-"Flower control"
Species$Treatment[Species$Treatment=="Flower Control"] <-"Flower control"
Species$Treatment[Species$Treatment=="FC"] <-"Flower control"

#write.csv(Species,"Data/species_sed_set")
