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
z_ippu <- 100-(a_ippu/b_ippu*100) #[1] -173.6842 This means that selfin produced 173% more seeds

a_ipaq<- mean(ipaq[ipaq$treatment=="self", "seed_set"])
b_ipaq<- mean(ipaq[ipaq$treatment=="cross", "seed_set"])
z_ipaq <- 100-(a_ipaq/b_ipaq*100) #

a_brol<- mean(brol[brol$Treatment=="Self", "Seed.production"])
b_brol<- mean(brol[brol$Treatment=="Cross", "Seed.production"])
z_brol <- 100-(a_brol/b_brol*100) 

a_brra<- mean(brra[brra$Treatment=="Self", "Seed.production"])
b_brra<- mean(brra[brra$Treatment=="Cross", "Seed.production"])
z_brra <- 100-(a_brra/b_brra*100) 

a_sial<- mean(sial[sial$Treatment=="Self", "Seed.production"])
b_sial<- mean(sial[sial$Treatment=="Cross", "Seed.production"])
z_sial <- 100-(a_sial/b_sial*100) 

a_ersa<- mean(ersa[ersa$Treatment=="Self", "seed.production"])
b_ersa<- mean(ersa[ersa$Treatment=="Cross", "seed.production"])
z_ersa <- 100-(a_ersa/b_ersa*100) 

a_soly<- mean(soly[soly$Treatment=="SELF", "seed_set"])
b_soly<- mean(soly[soly$Treatment=="CROSS", "seed_set"])
z_soly <- 100-(a_soly/b_soly*100) 

a_some<- mean(some[some$Treatment=="SELF", "seed_set"])
b_some<- mean(some[some$Treatment=="CROSS", "seed_set"])
z_some <- 100-(a_some/b_some*100)

a_pein<- mean(pein[pein$Treatment=="SELF", "Seed.production"])
b_pein<- mean(pein[pein$Treatment=="CROSS", "Seed.production"])
z_pein <- 100-(a_pein/b_pein*100)


a_caan<- mean(caan[caan$treatment=="SELF", "seed_set"])
b_caan<- mean(caan[caan$treatment=="CROSS", "seed_set"])
z_caan <- 100-(a_caan/b_caan*100)


traits_all$si_index <- c(z_brol, z_brra, z_caan, z_ersa, z_ipaq, z_ippu, 
                         z_pein, z_sial, z_soly, z_some) 


#colname to merge
colnames(traits_all)[2] <- "Species"

#I use the mean effect for each treatment, later maybe I come back and use the other
data <- merge(hp_mean, traits_all, by="Species")



