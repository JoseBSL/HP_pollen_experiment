
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


t$species_common_names <- ifelse(t$species=="SOME", t$species=="SOLY", "Eggplant", "Tomato", "")

t_stigma_area <- t[t$trait=="style length" | t$trait=="style legth",]


theme_set(theme_gray())
a <- ggplot(t, aes(x = um, fill = species)) +
  geom_density(alpha = 0.7) +
  theme(legend.position = c(0.8, 0.8))

cowplot::plot_grid(a, labels = "AUTO")











