#In this script I´m going to prepare the full data.frame of traits for analyses
#First I load an existing table of traits and then I complete it

#load data
traits_all <- read.csv("Data/tab.csv", sep="", stringsAsFactors = F)
#remove useless columns 
traits_all <- traits_all[ ,-c(1,2,4,7,10,12)]

#Making numerical the stigma type
traits_all[2] <- 1
traits_all[2] <- as.numeric(traits_all$stigma_type)
traits_all[1:4,2] <- 0

#Incompatibility instead of using binary for it I´m going to use the percetage of selfing 
#Obtained with hand self pollination

#Load csv with selfing rates (It was done on the script "Analysis_mantel_test)

selfing <- read.csv("Data/selfing_rate.csv")

selfing=selfing[,-1]
colnames(selfing)[2]<- "Selfing_rate"
#Ordering alphabetically to merge with traits_all
selfing <- selfing[order(selfing$Species), ]
#Ordering too traits all
traits_all <- traits_all[order(traits_all$species),]

#Overwrite column with new values (numerical ones instead of categorical)
traits_all[,3] <- selfing[,2]
colnames(traits_all)[3]<- "Selfing_rate"

#Add other numerical trais
morphometry <- read.csv("Data/species_traits.csv")
morphometry <- morphometry[, -c(4:15)]

#Now I have to remove the numbers after "_" 

measurement  <- str_split_fixed(as.character(morphometry$measurement), "_", 3)
measurement <- as.data.frame(measurement)
measurement$new <- paste(measurement$V1,"_", measurement$V2 )
morphometry[,2] <- measurement[,4] 
#Aggregate of different variables 
#morphometry <- dcast(species + measurement ~ ., value.var = "um", fun.aggregate = mean, data = morphometry, na.rm= TRUE)
#Some grammar mistakes, at least they seem to be homogeneous among species

#Fix colnames
colnames(morphometry)[3] <- "um"

#First I create the dataframes then I order and finally I insert them in the trait_all data.frame
#Cleaning outliers too
#Stigma
stigma <-morphometry[grep("stigma", morphometry$measurement),] 
#STIGMATIC AREA
#SOLY
stigma_soly <- subset(stigma, species=="SOLY")
#Stigma area (square micrometers, only unit like this)
stigma_area_soly <-stigma_soly[grep("area", stigma$measurement),] 
boxplot(stigma_area_soly$um)
stigma_area_soly<- stigma_area_soly[complete.cases(stigma_area_soly), ]

#SOME
stigma_some <- subset(stigma, species=="SOME")
#Stigma area (square micrometers, only unit like this)
stigma_area_some <-stigma_some[grep("area", stigma$measurement),] 
boxplot(stigma_area_some$um)
stigma_area_some<- stigma_area_some[complete.cases(stigma_area_some), ]
str(stigma_area_some)
stigma_area_some[stigma_area_some$um> 2000000,]<-NA
#PEIN
stigma_pein <- subset(stigma, species=="PEIN")
stigma_area_pein <-stigma_pein[grep("area", stigma$measurement),] 
stigma_area_pein<- stigma_area_pein[complete.cases(stigma_area_pein), ]

boxplot(stigma_area_pein$um)
#CAAN
stigma_caan <- subset(stigma, species=="CAAN")
stigma_area_caan <-stigma_caan[grep("area", stigma$measurement),] 
boxplot(stigma_area_caan$um)
stigma_area_caan<- stigma_area_caan[complete.cases(stigma_area_caan), ]
str(stigma_area_caan)
stigma_area_caan[stigma_area_caan$um> 1600000,]<-NA
#BROL
stigma_brol <- subset(stigma, species=="BROL")
stigma_area_brol <-stigma_brol[grep("area", stigma$measurement),] 
boxplot(stigma_area_brol$um)
summary(stigma_area_brol$um)
stigma_area_brol<- stigma_area_brol[complete.cases(stigma_area_brol), ]
#BRRA
stigma_brra <- subset(stigma, species=="BRRA")
stigma_area_brra <-stigma_brra[grep("area", stigma$measurement),] 
boxplot(stigma_area_brra$um)
summary(stigma_area_brra$um)
stigma_area_brra<- stigma_area_brra[complete.cases(stigma_area_brra), ]
stigma_area_brra[stigma_area_brra$um> 500000,]<-NA
#SIAL
stigma_sial <- subset(stigma, species=="SIAL")
stigma_area_sial <-stigma_sial[grep("area", stigma$measurement),] 
boxplot(stigma_area_sial$um)
summary(stigma_area_sial$um)
stigma_area_sial<- stigma_area_sial[complete.cases(stigma_area_sial), ]
stigma_area_sial[stigma_area_sial$um< 2000,]<-NA
#ERSA
stigma_ersa <- subset(stigma, species=="ERSA")
stigma_area_ersa <-stigma_ersa[grep("area", stigma$measurement),] 
boxplot(stigma_area_ersa$um)
summary(stigma_area_ersa$um)
stigma_area_ersa<- stigma_area_ersa[complete.cases(stigma_area_ersa), ]
stigma_area_ersa[stigma_area_ersa$um< 3000,]<-NA
#IPAQ
stigma_ipaq <- subset(stigma, species=="IPAQ")
stigma_area_ipaq <-stigma_ipaq[grep("area", stigma$measurement),] 
boxplot(stigma_area_ipaq$um)
summary(stigma_area_ipaq$um)
stigma_area_ipaq<- stigma_area_ipaq[complete.cases(stigma_area_ipaq), ]
#IPPU
stigma_ippu <- subset(stigma, species=="IPPU")
stigma_area_ippu <-stigma_ippu[grep("area", stigma$measurement),] 
boxplot(stigma_area_ippu$um)
summary(stigma_area_ippu$um)
stigma_area_ippu<- stigma_area_ippu[complete.cases(stigma_area_ippu), ]
stigma_area_ippu[stigma_area_ippu$um< 7000,]<-NA

stigma_area_all <- rbind(stigma_area_brol,stigma_area_brra,stigma_area_caan,stigma_area_ersa,stigma_area_ipaq,
      stigma_area_ippu,stigma_area_pein,stigma_area_sial,stigma_area_soly,stigma_area_some)
stigma_area_all<- stigma_area_all[complete.cases(stigma_area_all), ]

boxplot(stigma_area_all$um~stigma_area_all$species)


stigma_area_all <- dcast(species + measurement ~ ., value.var = "um", fun.aggregate = mean, data = stigma_area_all, na.rm= TRUE)
colnames(stigma_area_all)[3] <- "stigma_area" 

#STIGMA LENGTH
stigma_length <-stigma[grep("length", stigma$measurement),] 

#SOLY
stigma_length_soly <-stigma_soly[grep("length", stigma$measurement),] 
boxplot(stigma_length_soly$um)
stigma_length_soly <- stigma_length_soly[complete.cases(stigma_length_soly),]
#SOME
stigma_length_some <-stigma_some[grep("length", stigma$measurement),] 
boxplot(stigma_length_some$um)
stigma_length_some <- stigma_length_some[complete.cases(stigma_length_some),]
#PEIN
stigma_length_pein <-stigma_pein[grep("length", stigma$measurement),] 
boxplot(stigma_length_pein$um)
stigma_length_pein <- stigma_length_pein[complete.cases(stigma_length_pein),]
#CAAN
stigma_length_caan <-stigma_caan[grep("length", stigma$measurement),] 
boxplot(stigma_length_caan$um)
stigma_length_caan <- stigma_length_caan[complete.cases(stigma_length_caan),]
stigma_length_caan[stigma_length_caan$um>3000,]<-NA
#BROL
stigma_length_brol <-stigma_brol[grep("length", stigma$measurement),] 
boxplot(stigma_length_brol$um)
stigma_length_brol <- stigma_length_brol[complete.cases(stigma_length_brol),]
stigma_length_brol[stigma_length_brol$um>5000,]<-NA
#BRRA
stigma_length_brra <-stigma_brra[grep("length", stigma$measurement),] 
boxplot(stigma_length_brra$um)
stigma_length_brra <- stigma_length_brra[complete.cases(stigma_length_brra),]
#SIAL
stigma_length_sial <-stigma_sial[grep("length", stigma$measurement),] 
boxplot(stigma_length_sial$um)
stigma_length_sial <- stigma_length_sial[complete.cases(stigma_length_sial),]
#ERSA
stigma_length_ersa <-stigma_ersa[grep("length", stigma$measurement),] 
boxplot(stigma_length_ersa$um)
stigma_length_ersa <- stigma_length_ersa[complete.cases(stigma_length_ersa),]
#IPPU
stigma_length_ippu <-stigma_ippu[grep("length", stigma$measurement),] 
boxplot(stigma_length_ippu$um)
stigma_length_ippu <- stigma_length_ippu[complete.cases(stigma_length_ippu),]
#IPAQ
stigma_length_ipaq <-stigma_ipaq[grep("length", stigma$measurement),] 
boxplot(stigma_length_ipaq$um)
stigma_length_ipaq <- stigma_length_ipaq[complete.cases(stigma_length_ipaq),]
stigma_length_ipaq <- stigma_length_ipaq[stigma_length_ipaq$um>5000,]<- NA

stigma_length_all <- rbind(stigma_length_brol,stigma_length_brra,stigma_length_caan,stigma_length_ersa,stigma_length_ipaq,
                         stigma_length_ippu,stigma_length_pein,stigma_length_sial,stigma_length_soly,stigma_length_some)
stigma_length_all<- stigma_length_all[complete.cases(stigma_length_all), ]

boxplot(stigma_length_all$um~stigma_length_all$species)





#STIGMA WIDTH
stigma_width <-stigma[grep("width", stigma$measurement),] 
#SOLY
stigma_width_soly <-stigma_soly[grep("width", stigma$measurement),] 
boxplot(stigma_width_soly$um)
stigma_width_soly <- stigma_width_soly[complete.cases(stigma_width_soly),]
#SOME
stigma_width_some <-stigma_some[grep("width", stigma$measurement),] 
boxplot(stigma_width_some$um)
stigma_width_some <- stigma_width_some[complete.cases(stigma_width_some),]
#PEIN
stigma_width_pein <-stigma_pein[grep("width", stigma$measurement),] 
boxplot(stigma_width_pein$um)
stigma_width_pein <- stigma_width_pein[complete.cases(stigma_width_pein),]
#CAAN
stigma_width_caan <-stigma_caan[grep("width", stigma$measurement),] 
boxplot(stigma_width_caan$um)
summary(stigma_width_caan$um)
stigma_width_caan <- stigma_width_caan[complete.cases(stigma_width_caan),]
#BROL
stigma_width_brol <-stigma_brol[grep("width", stigma$measurement),] 
boxplot(stigma_width_brol$um)
summary(stigma_width_brol$um)
stigma_width_brol <- stigma_width_brol[complete.cases(stigma_width_brol),]
#BRRA
stigma_width_brra <-stigma_brra[grep("width", stigma$measurement),] 
boxplot(stigma_width_brra$um)
summary(stigma_width_brra$um)
stigma_width_brra <- stigma_width_brra[complete.cases(stigma_width_brra),]
#SIAL
stigma_width_sial <-stigma_sial[grep("width", stigma$measurement),] 
boxplot(stigma_width_sial$um)
summary(stigma_width_sial$um)
stigma_width_sial <- stigma_width_sial[complete.cases(stigma_width_sial),]
stigma_width_sial[stigma_width_sial$um>2000,]<- NA
#ERSA
stigma_width_ersa <-stigma_ersa[grep("width", stigma$measurement),] 
boxplot(stigma_width_ersa$um)
summary(stigma_width_ersa$um)
stigma_width_ersa <- stigma_width_ersa[complete.cases(stigma_width_ersa),]
#IPAQ
stigma_width_ipaq <-stigma_ipaq[grep("width", stigma$measurement),] 
boxplot(stigma_width_ipaq$um)
summary(stigma_width_ipaq$um)
stigma_width_ipaq <- stigma_width_ipaq[complete.cases(stigma_width_ipaq),]
#IPPU
stigma_width_ippu <-stigma_ippu[grep("width", stigma$measurement),] 
boxplot(stigma_width_ippu$um)
summary(stigma_width_ippu$um)
stigma_width_ippu <- stigma_width_ippu[complete.cases(stigma_width_ippu),]


stigma_width_all <- rbind(stigma_width_brol,stigma_width_brra,stigma_width_caan,stigma_width_ersa,stigma_width_ipaq,
                           stigma_width_ippu,stigma_width_pein,stigma_width_sial,stigma_width_soly,stigma_width_some)
stigma_width_all<- stigma_width_all[complete.cases(stigma_width_all), ]

boxplot(stigma_width_all$um~stigma_width_all$species)


#STIGMA SURFACE
stigma_surface <-stigma[grep("surface", stigma$measurement),] 
#SOLY
stigma_surface_soly <-stigma_soly[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_soly$um)
stigma_surface_soly <- stigma_surface_soly[complete.cases(stigma_surface_soly),]
#SOME
stigma_surface_some <-stigma_some[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_some$um)
summary(stigma_surface_some$um)
stigma_surface_some <- stigma_surface_some[complete.cases(stigma_surface_some),]
#CAAN
stigma_surface_caan <-stigma_caan[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_caan$um)
summary(stigma_surface_caan$um)
stigma_surface_caan <- stigma_surface_caan[complete.cases(stigma_surface_caan),]
#PEIN
stigma_surface_pein <-stigma_pein[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_pein$um)
summary(stigma_surface_pein$um)
stigma_surface_pein <- stigma_surface_pein[complete.cases(stigma_surface_pein),]
#BROL
stigma_surface_brol <-stigma_brol[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_brol$um)
summary(stigma_surface_brol$um)
stigma_surface_brol <- stigma_surface_brol[complete.cases(stigma_surface_brol),]
#BRRA
stigma_surface_brra <-stigma_brra[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_brra$um)
summary(stigma_surface_brra$um)
stigma_surface_brra <- stigma_surface_brra[complete.cases(stigma_surface_brra),]
#SIAL
stigma_surface_sial <-stigma_sial[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_sial$um)
summary(stigma_surface_sial$um)
stigma_surface_sial <- stigma_surface_sial[complete.cases(stigma_surface_sial),]
#ERSA
stigma_surface_ersa <-stigma_ersa[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_ersa$um)
summary(stigma_surface_ersa$um)
stigma_surface_ersa <- stigma_surface_ersa[complete.cases(stigma_surface_ersa),]
#IPAQ
stigma_surface_ipaq <-stigma_ipaq[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_ipaq$um)
summary(stigma_surface_ipaq$um)
stigma_surface_ipaq <- stigma_surface_ipaq[complete.cases(stigma_surface_ipaq),]
#IPPU
stigma_surface_ippu <-stigma_ippu[grep("surface", stigma$measurement),] 
boxplot(stigma_surface_ippu$um)
summary(stigma_surface_ippu$um)
stigma_surface_ippu <- stigma_surface_ippu[complete.cases(stigma_surface_ippu),]



stigma_surface_all <- rbind(stigma_surface_brol,stigma_surface_brra,stigma_surface_caan,stigma_surface_ersa,stigma_surface_ipaq,
                          stigma_surface_ippu,stigma_surface_pein,stigma_surface_sial,stigma_surface_soly,stigma_surface_some)
stigma_surface_all<- stigma_surface_all[complete.cases(stigma_surface_all), ]

boxplot(stigma_surface_all$um~stigma_surface_all$species)


#Style
style <-morphometry[grep("style", morphometry$measurement),] 
#STYLE LENGTH
style_length <-style[grep("length", style$measurement),] 
#SOLY
style_length_soly <- subset(style, species=="SOLY")
style_length_soly <-style_length_soly[grep("length", style$measurement),] 
boxplot(style_length_soly$um)
summary(style_length_soly$um)
style_length_soly <- style_length_soly[complete.cases(style_length_soly),]
#SOME
style_length_some <- subset(style, species=="SOME")
str(style_length_some)
style_length_some <- subset(style_length_some, measurement=="style _ length")
boxplot(style_length_some$um)
summary(style_length_some$um)
style_length_some <- style_length_some[complete.cases(style_length_some),]
#PEIN
style_length_pein <- subset(style, species=="PEIN")
style_length_pein <- subset(style_length_pein, measurement=="style _ length")
boxplot(style_length_pein$um)
summary(style_length_pein$um)
style_length_pein <- style_length_pein[complete.cases(style_length_pein),]
#CAAN
style_length_caan <- subset(style, species=="CAAN")
style_length_caan <- subset(style_length_caan, measurement=="style _ length")
boxplot(style_length_caan$um)
summary(style_length_caan$um)
style_length_caan <- style_length_caan[complete.cases(style_length_caan),]
#BROL
style_length_brol <- subset(style, species=="BROL")
style_length_brol <- subset(style_length_brol, measurement=="style _ length")
boxplot(style_length_brol$um)
summary(style_length_brol$um)
style_length_brol <- style_length_brol[complete.cases(style_length_brol),]
#BRRA
style_length_brra <- subset(style, species=="BRRA")
style_length_brra <- subset(style_length_brra, measurement=="style _ length")
boxplot(style_length_brra$um)
summary(style_length_brra$um)
style_length_brra <- style_length_brra[complete.cases(style_length_brra),]
#SIAL
style_length_sial <- subset(style, species=="SIAL")
style_length_sial <- subset(style_length_sial, measurement=="style _ length")
boxplot(style_length_sial$um)
summary(style_length_sial$um)
style_length_sial <- style_length_sial[complete.cases(style_length_sial),]
#ERSA
style_length_ersa <- subset(style, species=="ERSA")
style_length_ersa <- subset(style_length_ersa, measurement=="style _ length")
boxplot(style_length_ersa$um)
summary(style_length_ersa$um)
style_length_ersa <- style_length_ersa[complete.cases(style_length_ersa),]
#IPAQ
style_length_ipaq <- subset(style, species=="IPAQ")
style_length_ipaq <- subset(style_length_ipaq, measurement=="style _ length")
boxplot(style_length_ipaq$um)
summary(style_length_ipaq$um)
style_length_ipaq <- style_length_ipaq[complete.cases(style_length_ipaq),]
#IPPU
style_length_ippu <- subset(style, species=="IPPU")
style_length_ippu <- subset(style_length_ippu, measurement=="style _ length")
boxplot(style_length_ippu$um)
summary(style_length_ippu$um)
style_length_ippu <- style_length_ippu[complete.cases(style_length_ippu),]


style_length_all <- rbind(style_length_brol,style_length_brra,style_length_caan,style_length_ersa,style_length_ipaq,
                            style_length_ippu,style_length_pein,style_length_sial,style_length_soly,style_length_some)
style_length_all<- style_length_all[complete.cases(style_length_all), ]

boxplot(style_length_all$um~style_length_all$species)


#Style width
style_width <-style[grep("width", style$measurement),] 

#SOLY
style_width_soly <- subset(style, species=="SOLY")
style_width_soly <-style_width_soly[grep("width", style$measurement),] 
boxplot(style_width_soly$um)
summary(style_width_soly$um)
style_width_soly <- style_width_soly[complete.cases(style_width_soly),]
style_width_soly[style_width_soly$um>1000,]<- NA
#SOME
style_width_some <- subset(style, species=="SOME")
str(style_width_some)
style_width_some <- subset(style_width_some, measurement=="style _ width")
boxplot(style_width_some$um)
summary(style_width_some$um)
style_width_some[style_width_some$um>1500,]<-NA
style_width_some <- style_width_some[complete.cases(style_width_some),]
#PEIN
style_width_pein <- subset(style, species=="PEIN")
style_width_pein <- subset(style_width_pein, measurement=="style _ width")
boxplot(style_width_pein$um)
summary(style_width_pein$um)
style_width_pein <- style_width_pein[complete.cases(style_width_pein),]
#CAAN
style_width_caan <- subset(style, species=="CAAN")
style_width_caan <- subset(style_width_caan, measurement=="style _ width")
boxplot(style_width_caan$um)
summary(style_width_caan$um)
style_width_caan <- style_width_caan[complete.cases(style_width_caan),]
#BROL
style_width_brol <- subset(style, species=="BROL")
style_width_brol <- subset(style_width_brol, measurement=="style _ width")
boxplot(style_width_brol$um)
summary(style_width_brol$um)
style_width_brol <- style_width_brol[complete.cases(style_width_brol),]
style_width_brol[style_width_brol$um>1500,]<- NA
#BRRA
style_width_brra <- subset(style, species=="BRRA")
style_width_brra <- subset(style_width_brra, measurement=="style _ width")
boxplot(style_width_brra$um)
summary(style_width_brra$um)
style_width_brra <- style_width_brra[complete.cases(style_width_brra),]
#SIAL
style_width_sial <- subset(style, species=="SIAL")
style_width_sial <- subset(style_width_sial, measurement=="style _ width")
boxplot(style_width_sial$um)
summary(style_width_sial$um)
style_width_sial <- style_width_sial[complete.cases(style_width_sial),]
#ERSA
style_width_ersa <- subset(style, species=="ERSA")
style_width_ersa <- subset(style_width_ersa, measurement=="style _ width")
boxplot(style_width_ersa$um)
summary(style_width_ersa$um)
style_width_ersa <- style_width_ersa[complete.cases(style_width_ersa),]
#IPAQ
style_width_ipaq <- subset(style, species=="IPAQ")
style_width_ipaq <- subset(style_width_ipaq, measurement=="style _ width")
boxplot(style_width_ipaq$um)
summary(style_width_ipaq$um)
style_width_ipaq <- style_width_ipaq[complete.cases(style_width_ipaq),]
#IPPU
style_width_ippu <- subset(style, species=="IPPU")
style_width_ippu <- subset(style_width_ippu, measurement=="style _ width")
boxplot(style_width_ippu$um)
summary(style_width_ippu$um)
style_width_ippu <- style_width_ippu[complete.cases(style_width_ippu),]
style_width_ippu[style_width_ippu$um>5000,]<- NA

style_width_all <- rbind(style_width_brol,style_width_brra,style_width_caan,style_width_ersa,style_width_ipaq,
                            style_width_ippu,style_width_pein,style_width_sial,style_width_soly,style_width_some)
style_width_all<- style_width_all[complete.cases(style_width_all), ]

boxplot(style_width_all$um~style_width_all$species)

#Ovary
ovary <-morphometry[grep("ovary", morphometry$measurement),] 
#SOLY
ovary_width_soly <- subset(ovary, species=="SOLY")
ovary_width_soly <-ovary_width_soly[grep("width", ovary$measurement),] 
boxplot(ovary_width_soly$um)
summary(ovary_width_soly$um)
ovary_width_soly <- ovary_width_soly[complete.cases(ovary_width_soly),]
#SOME
ovary_width_some <- subset(ovary, species=="SOME")
str(ovary_width_some)
ovary_width_some <- subset(ovary_width_some, measurement=="ovary _ width")
boxplot(ovary_width_some$um)
summary(ovary_width_some$um)
ovary_width_some <- ovary_width_some[complete.cases(ovary_width_some),]
#PEIN
ovary_width_pein <- subset(ovary, species=="PEIN")
ovary_width_pein <- subset(ovary_width_pein, measurement=="ovary _ width")
boxplot(ovary_width_pein$um)
summary(ovary_width_pein$um)
ovary_width_pein <- ovary_width_pein[complete.cases(ovary_width_pein),]
#CAAN
ovary_width_caan <- subset(ovary, species=="CAAN")
ovary_width_caan <- subset(ovary_width_caan, measurement=="ovary _ width")
boxplot(ovary_width_caan$um)
summary(ovary_width_caan$um)
ovary_width_caan <- ovary_width_caan[complete.cases(ovary_width_caan),]
ovary_width_caan[ovary_width_caan$um<500,]<-NA
#BROL
ovary_width_brol <- subset(ovary, species=="BROL")
ovary_width_brol <- subset(ovary_width_brol, measurement=="ovary _ width")
boxplot(ovary_width_brol$um)
summary(ovary_width_brol$um)
ovary_width_brol <- ovary_width_brol[complete.cases(ovary_width_brol),]
#BRRA
ovary_width_brra <- subset(ovary, species=="BRRA")
ovary_width_brra <- subset(ovary_width_brra, measurement=="ovary _ width")
boxplot(ovary_width_brra$um)
summary(ovary_width_brra$um)
ovary_width_brra <- ovary_width_brra[complete.cases(ovary_width_brra),]
#SIAL
ovary_width_sial <- subset(ovary, species=="SIAL")
ovary_width_sial <- subset(ovary_width_sial, measurement=="ovary _ width")
boxplot(ovary_width_sial$um)
summary(ovary_width_sial$um)
ovary_width_sial <- ovary_width_sial[complete.cases(ovary_width_sial),]
#ERSA
ovary_width_ersa <- subset(ovary, species=="ERSA")
ovary_width_ersa <- subset(ovary_width_ersa, measurement=="ovary _ width")
boxplot(ovary_width_ersa$um)
summary(ovary_width_ersa$um)
ovary_width_ersa <- ovary_width_ersa[complete.cases(ovary_width_ersa),]
#IPAQ
ovary_width_ipaq <- subset(ovary, species=="IPAQ")
ovary_width_ipaq <- subset(ovary_width_ipaq, measurement=="ovary _ width")
boxplot(ovary_width_ipaq$um)
summary(ovary_width_ipaq$um)
ovary_width_ipaq <- ovary_width_ipaq[complete.cases(ovary_width_ipaq),]
#IPPU
ovary_width_ippu <- subset(ovary, species=="IPPU")
ovary_width_ippu <- subset(ovary_width_ippu, measurement=="ovary _ width")
boxplot(ovary_width_ippu$um)
summary(ovary_width_ippu$um)
ovary_width_ippu <- ovary_width_ippu[complete.cases(ovary_width_ippu),]

ovary_width_all <- rbind(ovary_width_brol,ovary_width_brra,ovary_width_caan,ovary_width_ersa,ovary_width_ipaq,
                         ovary_width_ippu,ovary_width_pein,ovary_width_sial,ovary_width_soly,ovary_width_some)
ovary_width_all<- ovary_width_all[complete.cases(ovary_width_all), ]

boxplot(ovary_width_all$um~ovary_width_all$species)


#OVARY WIDTH

#SOLY
ovary_length_soly <- subset(ovary, species=="SOLY")
ovary_length_soly <- subset(ovary_length_soly, measurement=="ovary _ legth")
boxplot(ovary_length_soly$um)
summary(ovary_length_soly$um)
ovary_length_soly <- ovary_length_soly[complete.cases(ovary_length_soly),]
#SOME
ovary_length_some <- subset(ovary, species=="SOME")
str(ovary_length_some)
ovary_length_some <- subset(ovary_length_some, measurement=="ovary _ legth")
boxplot(ovary_length_some$um)
summary(ovary_length_some$um)
ovary_length_some <- ovary_length_some[complete.cases(ovary_length_some),]
#PEIN
ovary_length_pein <- subset(ovary, species=="PEIN")
ovary_length_pein <- subset(ovary_length_pein, measurement=="ovary _ legth")
boxplot(ovary_length_pein$um)
summary(ovary_length_pein$um)
ovary_length_pein <- ovary_length_pein[complete.cases(ovary_length_pein),]
#CAAN
ovary_length_caan <- subset(ovary, species=="CAAN")
ovary_length_caan <- subset(ovary_length_caan, measurement=="ovary _ legth")
boxplot(ovary_length_caan$um)
summary(ovary_length_caan$um)
ovary_length_caan <- ovary_length_caan[complete.cases(ovary_length_caan),]
#BROL
ovary_length_brol <- subset(ovary, species=="BROL")
ovary_length_brol <- subset(ovary_length_brol, measurement=="ovary _ legth")
boxplot(ovary_length_brol$um)
summary(ovary_length_brol$um)
ovary_length_brol <- ovary_length_brol[complete.cases(ovary_length_brol),]
ovary_length_brol[ovary_length_brol$um<1000,]<- NA
#BRRA
ovary_length_brra <- subset(ovary, species=="BRRA")
ovary_length_brra <- subset(ovary_length_brra, measurement=="ovary _ legth")
boxplot(ovary_length_brra$um)
summary(ovary_length_brra$um)
ovary_length_brra <- ovary_length_brra[complete.cases(ovary_length_brra),]
#SIAL
ovary_length_sial <- subset(ovary, species=="SIAL")
ovary_length_sial <- subset(ovary_length_sial, measurement=="ovary _ legth")
boxplot(ovary_length_sial$um)
summary(ovary_length_sial$um)
ovary_length_sial <- ovary_length_sial[complete.cases(ovary_length_sial),]
#ERSA
ovary_length_ersa <- subset(ovary, species=="ERSA")
ovary_length_ersa <- subset(ovary_length_ersa, measurement=="ovary _ legth")
boxplot(ovary_length_ersa$um)
summary(ovary_length_ersa$um)
ovary_length_ersa <- ovary_length_ersa[complete.cases(ovary_length_ersa),]
#IPAQ
ovary_length_ipaq <- subset(ovary, species=="IPAQ")
ovary_length_ipaq <- subset(ovary_length_ipaq, measurement=="ovary _ legth")
boxplot(ovary_length_ipaq$um)
summary(ovary_length_ipaq$um)
ovary_length_ipaq <- ovary_length_ipaq[complete.cases(ovary_length_ipaq),]
#IPPU
ovary_length_ippu <- subset(ovary, species=="IPPU")
ovary_length_ippu <- subset(ovary_length_ippu, measurement=="ovary _ legth")
boxplot(ovary_length_ippu$um)
summary(ovary_length_ippu$um)
ovary_length_ippu <- ovary_length_ippu[complete.cases(ovary_length_ippu),]

ovary_length_all <- rbind(ovary_length_brol,ovary_length_brra,ovary_length_caan,ovary_length_ersa,ovary_length_ipaq,
                         ovary_length_ippu,ovary_length_pein,ovary_length_sial,ovary_length_soly,ovary_length_some)
ovary_length_all<- ovary_length_all[complete.cases(ovary_length_all), ]

boxplot(ovary_length_all$um~ovary_length_all$species)

















#Ovary width
ovary_width <-ovary[grep("width", ovary$measurement),] 
ovary_width[ovary_width$um>2000,]<- NA
boxplot(ovary_width$um)
ovary_width[ovary_width$um>2000,]<- NA
#Ovary width
ovary_length <-ovary[grep("legth", ovary$measurement),] 
ovary_length$measurement <- gsub('ovary _ legth', 'ovary _ length', ovary_length$measurement)
boxplot(ovary_length$um)
#Ok now all the traits are ready!! Lets add it to traits_all.
#dir.create("Data/Morpho_RData")

traits_all$stigma_area <- stigma_area$um
traits_all$stigma_length <- stigma_length$um
traits_all$stigma_surface <- stigma_surface$um
traits_all$stigma_width <- stigma_width$um
traits_all$style_length <- stigma_length$um
traits_all$style_width <- style_width$um
traits_all$ovary_width <- ovary_width$um
traits_all$ovary_length <- ovary_length$um


#save.image("Data/Morpho_RData/traits.RData")

#dir.create("Manuscript_draft/Data")

#save.image(file='Manuscript_draft/Data/trait_corr.RData')


cor.test(stigma_area$um, stigma_length$um)
cor.test(stigma_area$um, stigma_surface$um)
cor.test(stigma_area$um, stigma_width$um)
cor.test(stigma_area$um, style_length$um)
cor.test(stigma_area$um, style_width$um)
cor.test(stigma_area$um, ovary_width$um)
cor.test(stigma_area$um, ovary_length$um)

cor.test(stigma_length$um, stigma_surface$um)
cor.test(stigma_length$um, stigma_width$um)
cor.test(stigma_length$um, style_length$um)
cor.test(stigma_length$um, style_width$um)
cor.test(stigma_length$um, ovary_width$um)
cor.test(stigma_length$um, ovary_length$um)

cor.test(stigma_surface$um, stigma_width$um)
cor.test(stigma_surface$um, style_length$um)
cor.test(stigma_surface$um, style_width$um)
cor.test(stigma_surface$um, ovary_width$um)
cor.test(stigma_surface$um, ovary_length$um)

cor.test(stigma_width$um, style_length$um)
cor.test(stigma_width$um, style_width$um)
cor.test(stigma_width$um, ovary_width$um)
cor.test(stigma_width$um, ovary_length$um)

cor.test(style_length$um, style_width$um)
cor.test(style_length$um, ovary_width$um)
cor.test(style_length$um, ovary_length$um)

cor.test(style_width$um, ovary_width$um)
cor.test(style_width$um, ovary_length$um)

cor.test(ovary_width$um, ovary_length$um)


#Checking how to call the variables
a_1 <- cor.test(ovary_width$um, ovary_length$um)
library(kableExtra)
a <- c(a_1[3], a_1[4])
b <- c(a_1[3], a_1[4])
c <- rbind(a,b)
table<- data.frame(c)
kable(table)
#write.csv(traits_all, "Data/traits_all.csv")

#Checking for correlations

library(dplyr)

length <- filter(morphometry, measurement=="stigma _ length" & species=="SOME")
width <- filter(morphometry, measurement=="stigma _ width" & species=="SOME")
area <- filter(morphometry, measurement=="stigma _ area" & species=="SOME")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="SOME")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="SOME")
s_length <- filter(morphometry, measurement=="style _ length" & species=="SOME")
s_width <- filter(morphometry, measurement=="style _ width" & species=="SOME")

s_width<- s_width[1:15,]

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)



length <- filter(morphometry, measurement=="stigma _ length" & species=="CAAN")
width <- filter(morphometry, measurement=="stigma _ width" & species=="CAAN")
area <- filter(morphometry, measurement=="stigma _ area" & species=="CAAN")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="CAAN")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="CAAN")
s_length <- filter(morphometry, measurement=="style _ length" & species=="CAAN")
s_width <- filter(morphometry, measurement=="style _ width" & species=="CAAN")
s_width<- s_width[1:15,]

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)



length <- filter(morphometry, measurement=="stigma _ length" & species=="SOLY")
width <- filter(morphometry, measurement=="stigma _ width" & species=="SOLY")
area <- filter(morphometry, measurement=="stigma _ area" & species=="SOLY")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="SOLY")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="SOLY")
s_length <- filter(morphometry, measurement=="style _ length" & species=="SOLY")
s_width <- filter(morphometry, measurement=="style _ width" & species=="SOLY")
s_width<- s_width[1:15,]
s_length[15,]<-s_length[14,]

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)

length <- filter(morphometry, measurement=="stigma _ length" & species=="PEIN")
width <- filter(morphometry, measurement=="stigma _ width" & species=="PEIN")
area <- filter(morphometry, measurement=="stigma _ area" & species=="PEIN")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="PEIN")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="PEIN")
s_length <- filter(morphometry, measurement=="style _ length" & species=="PEIN")
s_width <- filter(morphometry, measurement=="style _ width" & species=="PEIN")
s_width<- s_width[1:15,]



cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)


length <- filter(morphometry, measurement=="stigma _ length" & species=="BRRA")
width <- filter(morphometry, measurement=="stigma _ width" & species=="BRRA")
area <- filter(morphometry, measurement=="stigma _ area" & species=="BRRA")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="BRRA")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="BRRA")
s_length <- filter(morphometry, measurement=="style _ length" & species=="BRRA")
s_width <- filter(morphometry, measurement=="style _ width" & species=="BRRA")
s_width<- s_width[1:15,]

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)

length <- filter(morphometry, measurement=="stigma _ length" & species=="BROL")
width <- filter(morphometry, measurement=="stigma _ width" & species=="BROL")
area <- filter(morphometry, measurement=="stigma _ area" & species=="BROL")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="BROL")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="BROL")
s_length <- filter(morphometry, measurement=="style _ length" & species=="BROL")
s_width <- filter(morphometry, measurement=="style _ width" & species=="BROL")
s_width<- s_width[1:15,]
s_length[15,]<-s_length[14,]

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)

length <- filter(morphometry, measurement=="stigma _ length" & species=="SIAL")
width <- filter(morphometry, measurement=="stigma _ width" & species=="SIAL")
area <- filter(morphometry, measurement=="stigma _ area" & species=="SIAL")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="SIAL")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="SIAL")
s_length <- filter(morphometry, measurement=="style _ length" & species=="SIAL")
s_width <- filter(morphometry, measurement=="style _ width" & species=="SIAL")
s_width<- s_width[1:15,]
s_length[15,]<-s_length[14,]



cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)


length <- filter(morphometry, measurement=="stigma _ length" & species=="ERSA")
width <- filter(morphometry, measurement=="stigma _ width" & species=="ERSA")
area <- filter(morphometry, measurement=="stigma _ area" & species=="ERSA")
o_length <- filter(morphometry, measurement=="ovary _ legth" & species=="ERSA")
o_width <- filter(morphometry, measurement=="ovary _ width" & species=="ERSA")
s_length <- filter(morphometry, measurement=="style _ length" & species=="ERSA")
s_width <- filter(morphometry, measurement=="style _ width" & species=="ERSA")
s_width<- s_width[1:15,]
s_length[15,]<-s_length[14,]



cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)
cor.test(o_width$um, o_length$um)
cor.test(length$um, o_length$um)
cor.test(s_length$um, s_width$um)
cor.test(s_length$um, length$um)
cor.test(s_length$um, o_length$um)


length <- filter(morphometry, measurement=="stigma _ length" & species=="IPAQ")
width <- filter(morphometry, measurement=="stigma _ width" & species=="IPAQ")
area <- filter(morphometry, measurement=="stigma _ area" & species=="IPAQ")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)

length <- filter(morphometry, measurement=="stigma _ length" & species=="IPPU")
width <- filter(morphometry, measurement=="stigma _ width" & species=="IPPU")
area <- filter(morphometry, measurement=="stigma _ area" & species=="IPPU")

cor.test(length$um, width$um)
cor.test(length$um, area$um)
cor.test(width$um, area$um)


ovary_width <- filter(morphometry, measurement=="ovary _ width")
ovary_length <- filter(morphometry, measurement=="ovary _ legth")#I maintain the spelling mistake...

cor.test(ovary_length$um, ovary_width$um)




####
####
####
#### GYNOECIUM LENGTH
####
####

#Here I prepare the a dataframe with the total gynoecium length
morphometry <- read.csv("Data/species_traits.csv")
morphometry <- morphometry[, -c(4:15)]
#I have to do it for each species
#Start with tomato
soly_traits <- filter(morphometry, species=="SOLY")
soly_style_length <- soly_traits[grep("style_leng", soly_traits$measurement),] 
#Fix missing value
soly_style_length[15,]<-soly_style_length[14,]
soly_style_length[15,2] <- "style_length_15"
#For now I leave it like this
soly_stigma_length <- soly_traits[grep("stigma_leng", soly_traits$measurement),]
soly_stigma_length[10,3] <- soly_stigma_length[9,3] 

soly_ovary_length <- soly_traits[grep("ovary_le", soly_traits$measurement),]
soly_gynoecium_length <- cbind(soly_style_length, soly_stigma_length, soly_ovary_length)
soly_gynoecium_length$gynoecium_length <- rowSums(soly_gynoecium_length[,c(3,6,9)])
soly_gynoecium_length <- soly_gynoecium_length[, c(1,10)]
