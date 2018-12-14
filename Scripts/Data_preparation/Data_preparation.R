#I found some erros I'm going to fix them

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

rbind(pein_seed_set_final,soly_seed_set_final)