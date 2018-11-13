library(ape)
library(phytools)
pollen_tree=read.tree("Data/pollen_tree.nwk")
pollen_tree=read.tree("Data/pollen_tree_no_outgroup.nwk")
pollen_tree=read.tree("Data/tree_neighbour.nwk")

plot(pollen_tree)
add.scale.bar(x=0, y=1, length = 0.01)

plot(tree_10)


tree_10 <- read.newick("Data/pollen_tree_no_outgroup.nwk")
tree_10=as.phylo(tree_10)
plot.phylo(tree_10)
add.scale.bar(x=0, y=9)

locator(1)
#Phylo signal for selfing rate
selfing <-as.data.frame(traits_all[,c("Selfing_rate")])
rownames(selfing) <- tree_10[[3]]
selfing <- as.matrix((selfing))[,1]
phylosig(tree=tree_10,x=selfing,method="lambda",test=TRUE)
#$`lambda`[1] 0.9467709

#Phylo signal for pollen size
pollen_size <-as.data.frame(traits_all[,c("pollen_size")])
rownames(pollen_size) <- tree_10[[3]]
pollen_size <- as.matrix((pollen_size))[,1]
phylosig(tree=tree_10,x=pollen_size,method="lambda",test=TRUE)
#$`lambda`[1] 0.9971989

#Phylo signal for mean pollen per anther
mean_pollen_anther <-as.data.frame(traits_all[,c("mean_pollen_anther")])
rownames(mean_pollen_anther) <- tree_10[[3]]
mean_pollen_anther <- as.matrix((mean_pollen_anther))[,1]
phylosig(tree=tree_10,x=mean_pollen_anther,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal for mean ovules
mean_ovules <-as.data.frame(traits_all[,c("mean_ovules")])
rownames(mean_ovules) <- tree_10[[3]]
mean_ovules <- as.matrix((mean_ovules))[,1]
phylosig(tree=tree_10,x=mean_ovules,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal for pollen_ovule_ratio
pollen_ovule_ratio <-as.data.frame(traits_all[,c("pollen_ovule_ratio")])
rownames(pollen_ovule_ratio) <- tree_10[[3]]
pollen_ovule_ratio <- as.matrix((pollen_ovule_ratio))[,1]
phylosig(tree=tree_10,x=pollen_ovule_ratio,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal stigma area
stigma_area <-as.data.frame(traits_all[,c("stigma_area")])
rownames(stigma_area) <- tree_10[[3]]
stigma_area <- as.matrix((stigma_area))[,1]
phylosig(tree=tree_10,x=stigma_area,method="lambda",test=TRUE)
#$`lambda`[1] 0.8597021

#Phylo signal stigma length
stigma_length <-as.data.frame(traits_all[,c("stigma_length")])
rownames(stigma_length) <- tree_10[[3]]
stigma_length <- as.matrix((stigma_length))[,1]
phylosig(tree=tree_10,x=stigma_length,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal stigma surface
stigma_surface <-as.data.frame(traits_all[,c("stigma_surface")])
rownames(stigma_surface) <- tree_10[[3]]
stigma_surface <- as.matrix((stigma_surface))[,1]
phylosig(tree=tree_10,x=stigma_surface,method="lambda",test=TRUE)
#$`lambda`[1] 0.7292784

#Phylo signal stigma width
stigma_width <-as.data.frame(traits_all[,c("stigma_width")])
rownames(stigma_width) <- tree_10[[3]]
stigma_width <- as.matrix((stigma_width))[,1]
phylosig(tree=tree_10,x=stigma_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal ovary width
ovary_width <-as.data.frame(traits_all[,c("ovary_width")])
rownames(ovary_width) <- tree_10[[3]]
ovary_width <- as.matrix((ovary_width))[,1]
phylosig(tree=tree_10,x=ovary_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal ovary length
ovary_length <-as.data.frame(traits_all[,c("ovary_length")])
rownames(ovary_length) <- tree_10[[3]]
ovary_length <- as.matrix((ovary_length))[,1]
phylosig(tree=tree_10,x=ovary_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.4881212

