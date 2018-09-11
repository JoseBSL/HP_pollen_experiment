
#Load library

library(stringr)
library(cowplot)

t <- read.csv("Data/species_traits.csv", header=T)

#data formatting
t <- t[,-(4:15)]
t$measurement <- as.character(t$measurement)
d<- data.frame(str_split_fixed(t$measurement, "_", 3))
colnames(d) <- c("a", "b", "c")
t <- cbind(t, d)
t$trait <- paste(t$a,t$b)
t <- t[, -c(2,4,5,6)]
t$trait <- as.character(t$trait)
t$species_common <- NA
t$family <- NA

#Add common names
t$species_common[t$species=="SOLY"] <- "Tomato"
t$species_common[t$species=="SOME"] <- "Eggplant"
t$species_common[t$species=="PEIN"] <- "Petunia"
t$species_common[t$species=="CAAN"] <- "Capsicum"
t$species_common[t$species=="BROL"] <- "Wild cabbage"
t$species_common[t$species=="SIAL"] <- "White mustard"
t$species_common[t$species=="BRRA"] <- "Pak choi"
t$species_common[t$species=="ERSA"] <- "Rocket"
t$species_common[t$species=="IPAQ"] <- "Water morning glory"
t$species_common[t$species=="IPPU"] <- "Morning glory"

#Add family

#Add common names
t$family[t$species=="SOLY"] <- "Solanaceae"
t$family[t$species=="SOME"] <- "Solanaceae"
t$family[t$species=="PEIN"] <- "Solanaceae"
t$family[t$species=="CAAN"] <- "Solanaceae"
t$family[t$species=="BROL"] <- "Brassicaceae"
t$family[t$species=="SIAL"] <- "Brassicaceae"
t$family[t$species=="BRRA"] <- "Brassicaceae"
t$family[t$species=="ERSA"] <- "Brassicaceae"
t$family[t$species=="IPAQ"] <- "Convolvulaceae"
t$family[t$species=="IPPU"] <- "Convolvulaceae"


t_stigma_area <- t[t$trait=="style length" | t$trait=="style legth",]

t_stigma_area_brassicaceae <- t_stigma_area[t_stigma_area$species=="BROL" | 
t_stigma_area$species=="BRRA"| t_stigma_area$species=="SIAL"| t_stigma_area$species=="ERSA" | t_stigma_area$species=="IPPU" ,]

t_stigma_area_solanaceae <- t_stigma_area[t_stigma_area$species=="SOLY" | 
                                            t_stigma_area$species=="SOME"| t_stigma_area$species=="PEIN"| t_stigma_area$species=="CAAN"| t_stigma_area$species=="IPAQ"  ,]

t_stigma_area_convolvulaceae <- t_stigma_area[t_stigma_area$species=="IPAQ" | 
                                                t_stigma_area$species=="IPPU",]


write.csv(t_stigma_area, "Rmd/Data/t_stigma_area.csv")
write.csv(t_stigma_area_brassicaceae, "Rmd/Data/t_stigma_area_brassicaceae.csv")
write.csv(t_stigma_area_solanaceae, "Rmd/Data/t_stigma_area_solanaceae.csv")
write.csv(t_stigma_area_convolvulaceae, "Rmd/Data/t_stigma_area_convolvulaceae.csv")

###brassicaceae

theme_set(theme_gray())
a <- ggplot(t_stigma_area_brassicaceae, aes(x = um, fill = species)) +
  geom_density(alpha = 0.7) +
  theme(legend.position = c(0.8, 0.8))

cowplot::plot_grid(a, labels = "AUTO")

###solanaceae

theme_set(theme_gray())
a <- ggplot(t_stigma_area_solanaceae, aes(x = um, fill = species)) +
  geom_density(alpha = 0.7) +
  theme(legend.position = c(0.8, 0.8))

cowplot::plot_grid(a, labels = "AUTO")

###convolvulaceae

theme_set(theme_gray())
a <- ggplot(t_stigma_area_convolvulaceae, aes(x = um, fill = species)) +
  geom_density(alpha = 0.7) +
  theme(legend.position = c(0.8, 0.8))

cowplot::plot_grid(a, labels = "AUTO")

###All colour per species

theme_set(theme_gray())
a <- ggplot(t_stigma_area, aes(x = um, fill = species)) +
  geom_density(alpha = 0.7) +
  theme(legend.position = c(0.8, 0.8))

cowplot::plot_grid(a, labels = "AUTO")

###All Area per family

theme_set(theme_gray())
a <- ggplot(t_stigma_area, aes(x = um, fill = family)) +
  geom_density(alpha = 0.7) +
  theme(legend.position = c(0.8, 0.8))

cowplot::plot_grid(a, labels = "AUTO")




