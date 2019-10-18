library(ape)
library(phytools)
pollen_tree=read.tree("Data/pollen_tree.nwk")
#pollen_tree=read.tree("Data/pollen_tree_no_outgroup.nwk")
#pollen_tree=read.tree("Data/tree_neighbour.nwk")
png("phylo.png", units="px", width=8000, height=10000, res=2000)
plot(pollen_tree, font= 3)
add.scale.bar(x=0, y=1, length = 0.01)


pollen_tree=read.tree("Data/phylo_all_correct_names.nwk")

png("Figure1.png", units="px", width=12000, height=12000, res=2500)
plot(pollen_tree, font= 3)
add.scale.bar(x=0, y=1, length = 0.01)

dev.off()

plot(pollen_tree)
add.scale.bar(x=0, y=1, length = 0.01)
#plot(tree_10)

tree_10 <- read.newick("Data/pollen_tree_no_outgroup.nwk")
tree_10=as.phylo(tree_10)
plot.phylo(tree_10)
add.scale.bar(x=0, y=9)

#For row names of traits all
readRDS("Manuscript_draft/Data/matrix_scale_effect.Rda")

#From here I start working with the traits
traits_all <- read.csv("Data/traits_all.csv", sep=",")
si_index <- readRDS("Data/si_index.RData")
matrix_scale_effect <- readRDS("Data/matrix_scale_effect.RDa")
traits_all$si_index <- si_index
rownames(traits_all) <- rownames(matrix_scale_effect)
traits_all <- traits_all[,-c(1:3,9,13)]
traits_all_scaled <- scale(traits_all)


#Phylo signal for selfing rate
selfing <-as.data.frame(traits_all[,c("Selfing_rate")])
rownames(selfing) <- tree_10[[3]]
selfing <- as.matrix((selfing))[,1]
selfing <- phylosig(tree=tree_10,x=selfing,method="lambda",test=TRUE)
#$`lambda`[1] 0.9467709
#Checking position of results to call them in Markdown
#a <- phylosig(tree=tree_10,x=selfing,method="lambda",test=TRUE)
#a[1]
#a[4]


#Phylo signal for pollen size
pollen_size <-as.data.frame(traits_all[,c("pollen_size")])
rownames(pollen_size) <- tree_10[[3]]
pollen_size <- as.matrix((pollen_size))[,1]
pollen_size <- phylosig(tree=tree_10,x=pollen_size,method="lambda",test=TRUE)
#$`lambda`[1] 0.9971989

#Phylo signal for mean pollen per anther
mean_pollen_anther <-as.data.frame(traits_all[,c("mean_pollen_anther")])
rownames(mean_pollen_anther) <- tree_10[[3]]
mean_pollen_anther <- as.matrix((mean_pollen_anther))[,1]
mean_pollen_anther <- phylosig(tree=tree_10,x=mean_pollen_anther,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal for mean ovules
mean_ovules <-as.data.frame(traits_all[,c("mean_ovules")])
rownames(mean_ovules) <- tree_10[[3]]
mean_ovules <- as.matrix((mean_ovules))[,1]
mean_ovules <- phylosig(tree=tree_10,x=mean_ovules,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal for pollen_ovule_ratio
pollen_ovule_ratio <-as.data.frame(traits_all[,c("pollen_ovule_ratio")])
rownames(pollen_ovule_ratio) <- tree_10[[3]]
pollen_ovule_ratio <- as.matrix((pollen_ovule_ratio))[,1]
pollen_ovule_ratio <- phylosig(tree=tree_10,x=pollen_ovule_ratio,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Anthers
anthers <-as.data.frame(traits_all[,c("anthers")])
rownames(anthers) <- tree_10[[3]]
anthers <- as.matrix((anthers))[,1]
anthers <- phylosig(tree=tree_10,x=anthers,method="lambda",test=TRUE)

#Stigma area
stigma_area <-as.data.frame(traits_all[,c("stigma_area")])
rownames(stigma_area) <- tree_10[[3]]
stigma_area <- as.matrix((stigma_area))[,1]
stigma_area <- phylosig(tree=tree_10,x=stigma_area,method="lambda",test=TRUE)
#$`lambda`[1] 0.8597021

#Stigma length
stigma_length <-as.data.frame(traits_all[,c("stigma_length")])
rownames(stigma_length) <- tree_10[[3]]
stigma_length <- as.matrix((stigma_length))[,1]
stigma_length <- phylosig(tree=tree_10,x=stigma_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.6961634

#Stigma_width
stigma_width <-as.data.frame(traits_all[,c("stigma_width")])
rownames(stigma_width) <- tree_10[[3]]
stigma_width <- as.matrix((stigma_width))[,1]
stigma_width <- phylosig(tree=tree_10,x=stigma_width,method="lambda",test=TRUE)
#$`lambda`[1] 0.7292784

#Stigma_surface
stigma_surface <-as.data.frame(traits_all[,c("stigma_surface")])
rownames(stigma_surface) <- tree_10[[3]]
stigma_surface <- as.matrix((stigma_surface))[,1]
stigma_surface <- phylosig(tree=tree_10,x=stigma_surface,method="lambda",test=TRUE)
#$`lambda`[1] 0.7292784

#Phylo signal style length
style_length <-as.data.frame(traits_all[,c("style_length")])
rownames(style_length) <- tree_10[[3]]
style_length <- as.matrix((style_length))[,1]
style_length <- phylosig(tree=tree_10,x=style_length,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal style width
style_width <-as.data.frame(traits_all[,c("style_width")])
rownames(style_width) <- tree_10[[3]]
style_width <- as.matrix((style_width))[,1]
style_width <- phylosig(tree=tree_10,x=style_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal ovary width
ovary_width <-as.data.frame(traits_all[,c("ovary_width")])
rownames(ovary_width) <- tree_10[[3]]
ovary_width <- as.matrix((ovary_width))[,1]
ovary_width <- phylosig(tree=tree_10,x=ovary_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal ovary length
ovary_length <-as.data.frame(traits_all[,c("ovary_length")])
rownames(ovary_length) <- tree_10[[3]]
ovary_length <- as.matrix((ovary_length))[,1]
ovary_length <- phylosig(tree=tree_10,x=ovary_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.4881212

#si_index
si_index <-as.data.frame(traits_all[,c("si_index")])
rownames(si_index) <- tree_10[[3]]
si_index <- as.matrix((si_index))[,1]
si_index <- phylosig(tree=tree_10,x=si_index,method="lambda",test=TRUE)
#$`lambda`[1] 0.4917088
#
##
#Without Convolvulaceae
##
#
tree_10 <- read.newick("Data/no_conv.nwk")

traits_all_no_conv=traits_all[-c(5,6),]
#Phylo signal for selfing rate
selfing <-as.data.frame(traits_all_no_conv[,c("Selfing_rate")])
rownames(selfing) <- tree_10[[3]]
selfing <- as.matrix((selfing))[,1]
selfing <- phylosig(tree=tree_10,x=selfing,method="lambda",test=TRUE)
#$`lambda`[1] 0.9467709
#Checking position of results to call them in Markdown
#a <- phylosig(tree=tree_10,x=selfing,method="lambda",test=TRUE)
#a[1]
#a[4]


#Phylo signal for pollen size
pollen_size <-as.data.frame(traits_all_no_conv[,c("pollen_size")])
rownames(pollen_size) <- tree_10[[3]]
pollen_size <- as.matrix((pollen_size))[,1]
pollen_size <- phylosig(tree=tree_10,x=pollen_size,method="lambda",test=TRUE)
#$`lambda`[1] 0.9971989

#Phylo signal for mean pollen per anther
mean_pollen_anther <-as.data.frame(traits_all_no_conv[,c("mean_pollen_anther")])
rownames(mean_pollen_anther) <- tree_10[[3]]
mean_pollen_anther <- as.matrix((mean_pollen_anther))[,1]
mean_pollen_anther <- phylosig(tree=tree_10,x=mean_pollen_anther,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal for mean ovules
mean_ovules <-as.data.frame(traits_all_no_conv[,c("mean_ovules")])
rownames(mean_ovules) <- tree_10[[3]]
mean_ovules <- as.matrix((mean_ovules))[,1]
mean_ovules <- phylosig(tree=tree_10,x=mean_ovules,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal for pollen_ovule_ratio
pollen_ovule_ratio <-as.data.frame(traits_all_no_conv[,c("pollen_ovule_ratio")])
rownames(pollen_ovule_ratio) <- tree_10[[3]]
pollen_ovule_ratio <- as.matrix((pollen_ovule_ratio))[,1]
pollen_ovule_ratio <- phylosig(tree=tree_10,x=pollen_ovule_ratio,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Anthers
anthers <-as.data.frame(traits_all_no_conv[,c("anthers")])
rownames(anthers) <- tree_10[[3]]
anthers <- as.matrix((anthers))[,1]
anthers <- phylosig(tree=tree_10,x=anthers,method="lambda",test=TRUE)

#Stigma area
stigma_area <-as.data.frame(traits_all_no_conv[,c("stigma_area")])
rownames(stigma_area) <- tree_10[[3]]
stigma_area <- as.matrix((stigma_area))[,1]
stigma_area <- phylosig(tree=tree_10,x=stigma_area,method="lambda",test=TRUE)
#$`lambda`[1] 0.8597021

#Stigma length
stigma_length <-as.data.frame(traits_all_no_conv[,c("stigma_length")])
rownames(stigma_length) <- tree_10[[3]]
stigma_length <- as.matrix((stigma_length))[,1]
stigma_length <- phylosig(tree=tree_10,x=stigma_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.6961634

#Stigma_width
stigma_width <-as.data.frame(traits_all_no_conv[,c("stigma_width")])
rownames(stigma_width) <- tree_10[[3]]
stigma_width <- as.matrix((stigma_width))[,1]
stigma_width <- phylosig(tree=tree_10,x=stigma_width,method="lambda",test=TRUE)
#$`lambda`[1] 0.7292784

#Stigma_surface
stigma_surface <-as.data.frame(traits_all_no_conv[,c("stigma_surface")])
rownames(stigma_surface) <- tree_10[[3]]
stigma_surface <- as.matrix((stigma_surface))[,1]
stigma_surface <- phylosig(tree=tree_10,x=stigma_surface,method="lambda",test=TRUE)
#$`lambda`[1] 0.7292784

#Phylo signal style length
style_length <-as.data.frame(traits_all_no_conv[,c("style_length")])
rownames(style_length) <- tree_10[[3]]
style_length <- as.matrix((style_length))[,1]
style_length <- phylosig(tree=tree_10,x=style_length,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal style width
style_width <-as.data.frame(traits_all_no_conv[,c("style_width")])
rownames(style_width) <- tree_10[[3]]
style_width <- as.matrix((style_width))[,1]
style_width <- phylosig(tree=tree_10,x=style_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal ovary width
ovary_width <-as.data.frame(traits_all_no_conv[,c("ovary_width")])
rownames(ovary_width) <- tree_10[[3]]
ovary_width <- as.matrix((ovary_width))[,1]
ovary_width <- phylosig(tree=tree_10,x=ovary_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal ovary length
ovary_length <-as.data.frame(traits_all_no_conv[,c("ovary_length")])
rownames(ovary_length) <- tree_10[[3]]
ovary_length <- as.matrix((ovary_length))[,1]
ovary_length <- phylosig(tree=tree_10,x=ovary_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.4881212

#si_index
si_index <-as.data.frame(traits_all_no_conv[,c("si_index")])
rownames(si_index) <- tree_10[[3]]
si_index <- as.matrix((si_index))[,1]
si_index <- phylosig(tree=tree_10,x=si_index,method="lambda",test=TRUE)
#$`lambda`[1] 0.4917088



#save.image("Manuscript_draft/Data/img_phylo_no_conv.RData")
effect_size_all <- readRDS( "Data/effect_size_all.RData")
effect_size_all$species <- c("SOME","SOLY","PEIN", "CAAN",
                               "IPPU", "IPAQ", "SIAL", "ERSA",
                               "BRRA", "BROL")
effect_size_all=effect_size_all[,-c(1,3,4)]
traits_all_no_conv$species <- rownames(traits_all_no_conv)
traits_all_no_conv <- merge(effect_size_all, traits_all_no_conv, by="species" )
Cohen_d <-as.data.frame(traits_all_no_conv[,c("Cohen_d")])
rownames(Cohen_d) <- tree_10[[3]]
Cohen_d <- as.matrix((Cohen_d))[,1]
Cohen_d <- phylosig(tree=tree_10,x=Cohen_d,method="lambda",test=TRUE)


##
##
###
####Now no Solanaceae
##
#

tree_no_sol <- read.newick("Data/no_sol.nwk")
traits_all_no_sol <- traits_all[,]
traits_all_no_sol <- traits_all_no_sol[-c(3,7,9,10),-c(1:3,9,13)]

#Phylo signal for selfing rate
selfing <-as.data.frame(traits_all_no_sol[,c("Selfing_rate")])
rownames(selfing) <- tree_no_sol[[3]]
selfing <- as.matrix((selfing))[,1]
selfing <- phylosig(tree=tree_no_sol,x=selfing,method="lambda",test=TRUE)

#Phylo signal for pollen size
pollen_size <-as.data.frame(traits_all_no_sol[,c("pollen_size")])
rownames(pollen_size) <- tree_no_sol[[3]]
pollen_size <- as.matrix((pollen_size))[,1]
pollen_size <- phylosig(tree=tree_no_sol,x=pollen_size,method="lambda",test=TRUE)
#$`lambda`[1] 0.9971989

#Phylo signal for mean pollen per anther
mean_pollen_anther <-as.data.frame(traits_all_no_sol[,c("mean_pollen_anther")])
rownames(mean_pollen_anther) <- tree_no_sol[[3]]
mean_pollen_anther <- as.matrix((mean_pollen_anther))[,1]
mean_pollen_anther <- phylosig(tree=tree_no_sol,x=mean_pollen_anther,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal for mean ovules
mean_ovules <-as.data.frame(traits_all_no_sol[,c("mean_ovules")])
rownames(mean_ovules) <- tree_no_sol[[3]]
mean_ovules <- as.matrix((mean_ovules))[,1]
mean_ovules <- phylosig(tree=tree_no_sol,x=mean_ovules,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal for pollen_ovule_ratio
pollen_ovule_ratio <-as.data.frame(traits_all_no_sol[,c("pollen_ovule_ratio")])
rownames(pollen_ovule_ratio) <- tree_no_sol[[3]]
pollen_ovule_ratio <- as.matrix((pollen_ovule_ratio))[,1]
pollen_ovule_ratio <- phylosig(tree=tree_no_sol,x=pollen_ovule_ratio,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Stigma area
stigma_area <-as.data.frame(traits_all_no_sol[,c("stigma_area")])
rownames(stigma_area) <- tree_no_sol[[3]]
stigma_area <- as.matrix((stigma_area))[,1]
stigma_area <- phylosig(tree=tree_no_sol,x=stigma_area,method="lambda",test=TRUE)
#$`lambda`[1] 0.8597021

#Stigma length
stigma_length <-as.data.frame(traits_all_no_sol[,c("stigma_length")])
rownames(stigma_length) <- tree_no_sol[[3]]
stigma_length <- as.matrix((stigma_length))[,1]
stigma_length <- phylosig(tree=tree_no_sol,x=stigma_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.6961634

#Stigma_width
stigma_width <-as.data.frame(traits_all_no_sol[,c("stigma_width")])
rownames(stigma_width) <- tree_no_sol[[3]]
stigma_width <- as.matrix((stigma_width))[,1]
stigma_width <- phylosig(tree=tree_no_sol,x=stigma_width,method="lambda",test=TRUE)
#$`lambda`[1] 0.7292784

#Phylo signal style length
style_length <-as.data.frame(traits_all_no_sol[,c("style_length")])
rownames(style_length) <- tree_no_sol[[3]]
style_length <- as.matrix((style_length))[,1]
style_length <- phylosig(tree=tree_no_sol,x=style_length,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal style width
style_width <-as.data.frame(traits_all_no_sol[,c("style_width")])
rownames(style_width) <- tree_no_sol[[3]]
style_width <- as.matrix((style_width))[,1]
style_width <- phylosig(tree=tree_no_sol,x=style_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal ovary width
ovary_width <-as.data.frame(traits_all_no_sol[,c("ovary_width")])
rownames(ovary_width) <- tree_no_sol[[3]]
ovary_width <- as.matrix((ovary_width))[,1]
ovary_width <- phylosig(tree=tree_no_sol,x=ovary_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal ovary length
ovary_length <-as.data.frame(traits_all_no_sol[,c("ovary_length")])
rownames(ovary_length) <- tree_no_sol[[3]]
ovary_length <- as.matrix((ovary_length))[,1]
ovary_length <- phylosig(tree=tree_no_sol,x=ovary_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.4881212

#si_index
si_index <-as.data.frame(traits_all_no_sol[,c("si_index")])
rownames(si_index) <- tree_no_sol[[3]]
si_index <- as.matrix((si_index))[,1]
si_index <- phylosig(tree=tree_no_sol,x=si_index,method="lambda",test=TRUE)
#$`lambda`[1] 0.4917088

#save.image("Manuscript_draft/Data/img_phylo_no_sol.RData")



#Now no Brassicaceae

tree_no_bra <- read.newick("Data/no_bra.nwk")
traits_all_no_bra <- traits_all[-c(1:2,4,8),-c(1:3,9,13)]
traits_all_no_bra <- scale(traits_all_no_bra)


#Phylo signal for selfing rate
selfing <-as.data.frame(traits_all_no_bra[,c("Selfing_rate")])
rownames(selfing) <- tree_no_bra[[3]]
selfing <- as.matrix((selfing))[,1]
selfing <- phylosig(tree=tree_no_bra,x=selfing,method="lambda",test=TRUE)

#Phylo signal for pollen size
pollen_size <-as.data.frame(traits_all_no_bra[,c("pollen_size")])
rownames(pollen_size) <- tree_no_bra[[3]]
pollen_size <- as.matrix((pollen_size))[,1]
pollen_size <- phylosig(tree=tree_no_bra,x=pollen_size,method="lambda",test=TRUE)
#$`lambda`[1] 0.9971989

#Phylo signal for mean pollen per anther
mean_pollen_anther <-as.data.frame(traits_all_no_bra[,c("mean_pollen_anther")])
rownames(mean_pollen_anther) <- tree_no_bra[[3]]
mean_pollen_anther <- as.matrix((mean_pollen_anther))[,1]
mean_pollen_anther <- phylosig(tree=tree_no_bra,x=mean_pollen_anther,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal for mean ovules
mean_ovules <-as.data.frame(traits_all_no_bra[,c("mean_ovules")])
rownames(mean_ovules) <- tree_no_bra[[3]]
mean_ovules <- as.matrix((mean_ovules))[,1]
mean_ovules <- phylosig(tree=tree_no_bra,x=mean_ovules,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal for pollen_ovule_ratio
pollen_ovule_ratio <-as.data.frame(traits_all_no_bra[,c("pollen_ovule_ratio")])
rownames(pollen_ovule_ratio) <- tree_no_bra[[3]]
pollen_ovule_ratio <- as.matrix((pollen_ovule_ratio))[,1]
pollen_ovule_ratio <- phylosig(tree=tree_no_bra,x=pollen_ovule_ratio,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Stigma area
stigma_area <-as.data.frame(traits_all_no_bra[,c("stigma_area")])
rownames(stigma_area) <- tree_no_bra[[3]]
stigma_area <- as.matrix((stigma_area))[,1]
stigma_area <- phylosig(tree=tree_no_bra,x=stigma_area,method="lambda",test=TRUE)
#$`lambda`[1] 0.8597021

#Stigma length
stigma_length <-as.data.frame(traits_all_no_bra[,c("stigma_length")])
rownames(stigma_length) <- tree_no_bra[[3]]
stigma_length <- as.matrix((stigma_length))[,1]
stigma_length <- phylosig(tree=tree_no_bra,x=stigma_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.6961634

#Stigma_width
stigma_width <-as.data.frame(traits_all_no_bra[,c("stigma_width")])
rownames(stigma_width) <- tree_no_bra[[3]]
stigma_width <- as.matrix((stigma_width))[,1]
stigma_width <- phylosig(tree=tree_no_bra,x=stigma_width,method="lambda",test=TRUE)
#$`lambda`[1] 0.7292784

#Phylo signal style length
style_length <-as.data.frame(traits_all_no_bra[,c("style_length")])
rownames(style_length) <- tree_no_bra[[3]]
style_length <- as.matrix((style_length))[,1]
style_length <- phylosig(tree=tree_no_bra,x=style_length,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal style width
style_width <-as.data.frame(traits_all_no_bra[,c("style_width")])
rownames(style_width) <- tree_no_bra[[3]]
style_width <- as.matrix((style_width))[,1]
style_width <- phylosig(tree=tree_no_bra,x=style_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal ovary width
ovary_width <-as.data.frame(traits_all_no_bra[,c("ovary_width")])
rownames(ovary_width) <- tree_no_bra[[3]]
ovary_width <- as.matrix((ovary_width))[,1]
ovary_width <- phylosig(tree=tree_no_bra,x=ovary_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal ovary length
ovary_length <-as.data.frame(traits_all_no_bra[,c("ovary_length")])
rownames(ovary_length) <- tree_no_bra[[3]]
ovary_length <- as.matrix((ovary_length))[,1]
ovary_length <- phylosig(tree=tree_no_bra,x=ovary_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.4881212

#si_index
si_index <-as.data.frame(traits_all_no_bra[,c("si_index")])
rownames(si_index) <- tree_no_bra[[3]]
si_index <- as.matrix((si_index))[,1]
si_index <- phylosig(tree=tree_no_bra,x=si_index,method="lambda",test=TRUE)
#$`lambda`[1] 0.4917088

save.image("Manuscript_draft/Data/img_phylo_no_bra.RData")