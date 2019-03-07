#In this script I'm going to perform GLM between Hp effect and the traits

#load data
hp <- read.csv("Data/y.csv")
hp_mean <- read.csv("Data/y_mean_scale.csv")

#Now I'm using the HP effect calculated in Analysis_mantel_test
#But maybe I could use later effect sizes...

#Now I'm going to match all the traits with these data.frames
traits_all <- read.csv("Data/traits_all.csv", sep=",")
#I create a column with equal code of species in order to merge
traits_all$species <- c("BROL", "BRRA", "CAAN", "ERSA", "IPAQ", "IPPU", 
                        "PEIN", "SIAL", "SOLY", "SOME") 

#The column of selfing rate I'm going to add SI index values
#Seeds or fruits produced by self divided by cross
#REF (Becerra & Lloyd 1992)

#For this I load the data of seed set

#Load seed set data for 10 species
soly  <- read.csv("Data/species_seed_set/soly_seed_set.csv", sep=";", stringsAsFactors = F)
some  <- read.csv("Data/species_seed_set/some_seed_set.csv", sep=";", stringsAsFactors = F)
pein  <- read.csv("Data/species_seed_set/pein_seed_set.csv", sep=";", stringsAsFactors = F)
caan  <- read.csv("Data/species_seed_set/caan_seed_set.csv", sep=";", stringsAsFactors = F)
ersa  <- read.csv("Data/species_seed_set/ersa_seed_set.csv", sep=";", stringsAsFactors = F)
brra  <- read.csv("Data/species_seed_set/brra_seed_set.csv", sep=";", stringsAsFactors = F)
sial  <- read.csv("Data/species_seed_set/sial_seed_set.csv", sep=";", stringsAsFactors = F)
brol  <- read.csv("Data/species_seed_set/brol_seed_set.csv", sep=";", stringsAsFactors = F)
ippu  <- read.csv("Data/species_seed_set/ippu_seed_set.csv", sep=";", stringsAsFactors = F)
ipaq  <- read.csv("Data/species_seed_set/ipaq_seed_set.csv", sep=";", stringsAsFactors = F)

a_ippu<- mean(ippu[ippu$treatment=="self", "seed.set"])
b_ippu<- mean(ippu[ippu$treatment=="cross", "seed.set"])
z_ippu <- (a_ippu/b_ippu) #[1] -173.6842 This means that selfin produced 173% more seeds

a_ipaq<- mean(ipaq[ipaq$treatment=="self", "seed_set"])
b_ipaq<- mean(ipaq[ipaq$treatment=="cross", "seed_set"])
z_ipaq <- (a_ipaq/b_ipaq) #

a_brol<- mean(brol[brol$Treatment=="Self", "Seed.production"])
b_brol<- mean(brol[brol$Treatment=="Cross", "Seed.production"])
z_brol <- (a_brol/b_brol) 

a_brra<- mean(brra[brra$Treatment=="Self", "Seed.production"])
b_brra<- mean(brra[brra$Treatment=="Cross", "Seed.production"])
z_brra <- (a_brra/b_brra) 

a_sial<- mean(sial[sial$Treatment=="Self", "Seed.production"])
b_sial<- mean(sial[sial$Treatment=="Cross", "Seed.production"])
z_sial <- (a_sial/b_sial) 

a_ersa<- mean(ersa[ersa$Treatment=="Self", "seed.production"])
b_ersa<- mean(ersa[ersa$Treatment=="Cross", "seed.production"])
z_ersa <- (a_ersa/b_ersa) 

a_soly<- mean(soly[soly$Treatment=="SELF", "seed_set"])
b_soly<- mean(soly[soly$Treatment=="CROSS", "seed_set"])
z_soly <- (a_soly/b_soly) 

a_some<- mean(some[some$Treatment=="SELF", "seed_set"])
b_some<- mean(some[some$Treatment=="CROSS", "seed_set"])
z_some <- (a_some/b_some)

a_pein<- mean(pein[pein$Treatment=="SELF", "Seed.production"])
b_pein<- mean(pein[pein$Treatment=="CROSS", "Seed.production"])
z_pein <- (a_pein/b_pein)


a_caan<- mean(caan[caan$treatment=="SELF", "seed_set"])
b_caan<- mean(caan[caan$treatment=="CROSS", "seed_set"])
z_caan <- (a_caan/b_caan)


traits_all$si_index <- c(z_brol, z_brra, z_caan, z_ersa, z_ipaq, z_ippu, 
                         z_pein, z_sial, z_soly, z_some) 

#colname to merge
colnames(traits_all)[2] <- "Species"


traits_all$compatibility[traits_all$Species=="BROL"] <- 0
traits_all$compatibility[traits_all$Species=="BRRA"] <- 0
traits_all$compatibility[traits_all$Species=="SIAL"] <- 100
traits_all$compatibility[traits_all$Species=="ERSA"] <- 2
traits_all$compatibility[traits_all$Focal=="CAAN"] <- 64
traits_all$compatibility[traits_all$Focal=="SOLY"] <- 48
traits_all$compatibility[traits_all$Focal=="SOME"] <- 100
traits_all$compatibility[traits_all$Focal=="PEIN"] <- 26
traits_all$compatibility[traits_all$Focal=="IPAQ"] <- 75
traits_all$compatibility[traits_all$Focal=="IPPU"] <- 100

traits_all$compatibility=as.numeric(traits_all$compatibility)




#I use the mean effect for each treatment, later maybe I come back and use the other
hp_mean_sp <- dcast(Species ~ ., value.var = "Scale_seed", fun.aggregate = mean, data = hp_mean, na.rm= TRUE)
colnames(hp_mean_sp) <- c("Species","Scale_seed")


data <- merge(hp_mean_sp, traits_all, by="Species")
data <- data[,-c(3)]

model1=lm(Scale_seed~si_index, data=data)
summary(model1)

model1=lm(Scale_seed~Selfing_rate, data=data)
summary(model1)

model1=lm(Scale_seed~pollen_size, data=data)
summary(model1)

model1=lm(Scale_seed~mean_pollen_anther, data=data)
summary(model1)

model1=lm(Scale_seed~mean_ovules, data=data)
summary(model1)

model1=lm(Scale_seed~pollen_ovule_ratio, data=data)
summary(model1)

model1=lm(Scale_seed~stigma_area, data=data)
summary(model1)

model1=lm(Scale_seed~stigma_length, data=data)
summary(model1)

model1=lm(Scale_seed~stigma_surface, data=data)
summary(model1)

model1=lm(Scale_seed~stigma_width, data=data)
summary(model1)

model1=lm(Scale_seed~style_length, data=data)
summary(model1)

model1=lm(Scale_seed~style_width, data=data)
summary(model1)

model1=lm(Scale_seed~ovary_width, data=data)
summary(model1)

model1=lm(Scale_seed~ovary_length, data=data)
summary(model1)

model1=lm(Scale_seed~si_index, data=data)
summary(model1)

model1=lm(Scale_seed~compatibility, data=data)
summary(model1)

#Now let see what happens without groping

data <- merge(hp_mean, traits_all, by="Species")
data <- data[,-c(2,4)]

model1=lm(Scale_seed~si_index, data=data)
summary(model1)

model1=lm(Scale_seed~Selfing_rate, data=data)
summary(model1)

model1=lm(Scale_seed~pollen_size, data=data)
summary(model1)

model1=lm(Scale_seed~mean_pollen_anther, data=data)
summary(model1)

model1=lm(Scale_seed~mean_ovules, data=data)
summary(model1)

model1=lm(Scale_seed~pollen_ovule_ratio, data=data)
summary(model1)

model1=lm(Scale_seed~stigma_area, data=data)
summary(model1)

model1=lm(Scale_seed~stigma_length, data=data)
summary(model1)

model1=lm(Scale_seed~stigma_surface, data=data)
summary(model1)

model1=lm(Scale_seed~stigma_width, data=data)
summary(model1)

model1=lm(Scale_seed~style_length, data=data)
summary(model1)

model1=lm(Scale_seed~style_width, data=data)
summary(model1)

model1=lm(Scale_seed~ovary_width, data=data)
summary(model1)

model1=lm(Scale_seed~ovary_length, data=data)
summary(model1)

model1=lm(Scale_seed~si_index, data=data)
summary(model1)

model1=lm(Scale_seed~compatibility, data=data)
summary(model1)

#Same result
#After simple linear models we add random effects

a <- data[68,] 
data <- rbind(data,a)
summary(data$Species)
data$indv<- seq.int(1:10)
summary(data)
#Analysis with random factor of individuals
model2=lme(Scale_seed~Selfing_rate, data=data, random=~1|indv)
summary(model2)

model2=lme(Scale_seed~pollen_size, data=data, random=~1|indv)
summary(model2)

model2=lme(Scale_seed~si_index, data=data, random=~1|indv)
summary(model2)

model2=lme(Scale_seed~compatibility, data=data, random=~1|indv)
summary(model2)

