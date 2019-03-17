#In this script I prepare the plot of 100% pollen
#Because they are just few treatments that gave seeds
#I created manually

#First for Brassicaceae specie
Family <- c(rep("Brassicaceae",9)) 
Species <- c("B. oleracea", rep("B. rapa", 5), "S. alba", "E. sativa","E. sativa")
Donor <- c("C. annuum", rep("B. oleracea",2), "S. lycopersicum",rep("B. oleracea",3),
           "C. annuum","C. annuum") 
Seeds <-  c(5, 2, 13, 1, 7, 5, 7, 6, 1)

dat_bra <- data.frame(Family, Species, Donor, Seeds)

#Solanaceae species
Family <- c(rep("Solanaceae",4)) 
Species <- c("S. lycopersicum", "S. melongena", "C. annuum", "C. annuum")
Donor <- c("S. alba", "P. integrifolia", "S. alba", "E. sativa")
Seeds <-  c(3, 36, 127,3)

dat_sol <- data.frame(Family, Species, Donor, Seeds)

dat_all <- rbind(dat_bra,dat_sol)
saveRDS(dat_all, "Manuscript_draft/Data/dat_all.RData")
