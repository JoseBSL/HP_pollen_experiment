#In this script I'm going to create the plot for 100% pollen treatments
#JUST FOR SOLANACEAE SPECIES

soly  <- read.csv("Data/species_seed_set/soly_seed_set.csv", sep=";", stringsAsFactors = F)
some  <- read.csv("Data/species_seed_set/some_seed_set.csv", sep=";", stringsAsFactors = F)
pein  <- read.csv("Data/species_seed_set/pein_seed_set.csv", sep=";", stringsAsFactors = F)
caan  <- read.csv("Data/species_seed_set/caan_seed_set.csv", sep=";", stringsAsFactors = F)


#Making all columns numerical ones. If not the loop doesn´t run
soly[3:5] <- lapply(soly[3:5], as.numeric)
some[3:5] <- lapply(some[3:5], as.numeric)
caan[3:5] <- lapply(caan[3:5], as.numeric)



species_list <- list(soly, some, caan)
i <- NULL
y <- NULL

for (i in species_list){
  colnames(i)<- c("Species", "Treatment", "Treatment_number", "Seed_set", "Scale_seed")
  i <- filter(i, Treatment!="RARA 50%", Treatment!="COSA 50%",
              Treatment!="Cross", Treatment!="Self", Treatment!="Control",Treatment!="Flower Control",
              Treatment!="FC", Treatment!="CROSS", Treatment!="FLOWER CONTROL", Treatment!="control",
              Treatment!="cross", Treatment!="flower control", Treatment!="self", Treatment!="SELF",
              Treatment!="CONTROL")
  i <-i[-grep("50", i$Treatment),] 
  
  y <- rbind(y, i)
}
