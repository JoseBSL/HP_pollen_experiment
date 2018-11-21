
#Step1
library(vegan)
library(qpcR)
data(iris)

#Step2
dst <- dist(iris[,1:4])
iris.bd <- betadisper(dst, iris$Species)
iris.bd

#step3
anova(iris.bd)


#Step4
permutest(iris.bd)


labs <- paste("Dimension", 1:4, "(", 
              round(100*iris.bd$eig / sum(iris.bd$eig), 2), "%)")

plot(iris.bd, cex=1, pch=15:17,
     main="Iris data: MDS coordinates", cex.lab=1.25,
     xlab=labs[1], ylab=labs[2],
     hull=FALSE, ellipse=TRUE, conf=0.68, lwd=2)


#Now I try to apply it to my data

traits_all <- read.csv("Data/traits_all.csv", sep=",")

d <-  traits_all[, -c(1,2)]
d <- scale(d)
d<- d[,c(2:6)]
dst_spp <- dist(d)

spp_dist <- betadisper(dst_spp, traits_all$species)
spp_dist
anova(spp_dist)


#Prepare dataset as the example above. Just with Gynoecium traits
morphometry <- read.csv("Data/species_traits.csv")
morphometry <- morphometry[, -c(4:15)]
#Now I have to remove the numbers after "_" 
measurement  <- str_split_fixed(as.character(morphometry$measurement), "_", 3)
measurement <- as.data.frame(measurement)
measurement$new <- paste(measurement$V1,"_", measurement$V2 )
morphometry[,2] <- measurement[,4] 

#Stigma
stigma <-morphometry[grep("stigma", morphometry$measurement),] 

#Stigma surface
stigma_surface <-stigma[grep("surface", stigma$measurement),] 
colnames(stigma_surface)[3] <- "stigma_surface"

#Style
style <-morphometry[grep("style", morphometry$measurement),] 
#Style length
style_length <-style[grep("length", style$measurement),] 
colnames(style_length)[3] <- "style_length"
#Style width
style_width <-style[grep("width", style$measurement),] 
colnames(style_width)[3] <- "style_width"
str(style_width)
#Ovary
ovary <-morphometry[grep("ovary", morphometry$measurement),] 

#Ovary width
ovary_width <-ovary[grep("width", ovary$measurement),] 
colnames(ovary_width)[3]<- "ovary_width"
#Ovary width
ovary_length <-ovary[grep("legth", ovary$measurement),] 
ovary_length$measurement <- gsub('ovary _ legth', 'ovary _ length', ovary_length$measurement)
colnames(ovary_width)[3]<- "ovary_length"


stigma_surface <- stigma_surface[,-2]
style_length <- style_length[,-2]
all <- cbind(stigma_surface, style_length)

dta <- qpcR:::cbind.na(stigma_surface, style_length)
