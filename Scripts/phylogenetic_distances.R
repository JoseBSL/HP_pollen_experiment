
#Chloroplast matrix with phylogenetic distances (RBCL)

all_species_rbcl<-read.csv("data/species_matrix_phylogenetic_distance_rbcl.csv", sep = ";", header = F)
View(all_species_rbcl)
species<- all_species_rbcl$V1
rownames(all_species_rbcl) <- species
all_species_rbcl<- all_species_rbcl[,-1]
colnames(all_species_rbcl) <- species

