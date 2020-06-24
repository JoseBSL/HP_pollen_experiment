#################################################################################
#################################################################################
#################################################################################
##################### UNIPARTITE BIDIRECTIONAL NETWORK ############################
#################################################################################
#################################################################################
#################################################################################

####################################
############ load libraries ########
####################################

library(statnet)
library(reshape2)
library(sna)
library(igraph)

#######################################
############ Data preparataion ########
#######################################

#In this script I'm going to plot the transitivity of the effect oh HP pollen with effect sizes
effect <- readRDS("Data/RData/matrix_effect_size.RData")
#Now I convert the few values over 0 to maximum 0 (control)
effect[effect>0]<-0
#absolute values so less confusing without negative values
#So now greater values involve greater effect
#(opposite way in comparison with negative values as before)
effect=abs(effect)
#find out winners and losers
#int.to.dom=function(x){ ((x<t(x)) & (x+t(x)>0))+0}
#old way
#Now we conduct on this way
#0.2 as critical value based on cohens classification of effect sizes
int.to.dom=function(x){((x<t((x)+0.2) & (x<(t(x)-0.2))))+0}

d <- int.to.dom(effect)
#Plot nicely already
net=graph.adjacency(d,mode="directed",weighted=TRUE,diag=FALSE) 
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/2, edge.arrow.size=0.5)
#Now just add the differeces of effect sizes for intensity of the directional arrow
#do it manually no time to think for a better way

#OLD
#1st column
d[2,1] <- effect[1,2]-effect[2,1]
d[4,1] <- effect[1,4]-effect[4,1]
d[5,1] <- effect[1,5]-effect[5,1]
d[6,1] <- effect[1,6]-effect[6,1]
d[7,1] <- effect[1,7]-effect[7,1]
d[8,1] <- effect[1,8]-effect[8,1]
d[9,1] <- effect[1,9]-effect[9,1]
d[10,1] <- effect[1,10]-effect[10,1]
#2nd column
d[4,2] <- effect[2,4]-effect[4,2]
d[5,2] <- effect[2,5]-effect[5,2]
d[6,2] <- effect[2,6]-effect[6,2]
d[7,2] <- effect[2,7]-effect[7,2]
d[8,2] <- effect[2,8]-effect[8,2]
#3rd column 
d[1,3] <- effect[3,1]-effect[1,3]
d[2,3] <- effect[3,2]-effect[2,3]
d[4,3] <- effect[3,4]-effect[4,3]
d[5,3] <- effect[3,5]-effect[5,3]
d[6,3] <- effect[3,6]-effect[6,3]
d[7,3] <- effect[3,7]-effect[7,3]
d[8,3] <- effect[3,8]-effect[8,3]
d[10,3] <- effect[3,10]-effect[10,3]
#4th column 
#all 0
#5th column
d[4,5] <- effect[5,4]-effect[4,5]
d[6,5] <- effect[5,6]-effect[6,5]
d[8,5] <- effect[5,8]-effect[8,5]
#6th column
d[4,6] <- effect[6,4]-effect[4,6]
#7th column
d[4,7] <- effect[7,4]-effect[4,7]
d[5,7] <- effect[7,5]-effect[5,7]
d[6,7] <- effect[7,6]-effect[6,7]
d[8,7] <- effect[7,8]-effect[8,7]
d[10,7] <- effect[7,10]-effect[10,7]
#8th column
d[4,8] <- effect[8,4]-effect[4,8]
d[6,8] <- effect[8,6]-effect[6,8]
#9th column
d[2,9] <- effect[9,2]-effect[2,9]
d[3,9] <- effect[9,3]-effect[3,9]
d[4,9] <- effect[9,4]-effect[4,9]
d[5,9] <- effect[9,5]-effect[5,9]
d[6,9] <- effect[9,6]-effect[6,9]
d[7,9] <- effect[9,7]-effect[7,9]
d[8,9] <- effect[9,8]-effect[8,9]
d[10,9] <- effect[9,10]-effect[10,9]
#10th column
d[2,10] <- effect[10,2]-effect[2,10]
d[4,10] <- effect[10,4]-effect[4,10]
d[6,10] <- effect[10,6]-effect[6,10]
d[8,10] <- effect[10,8]-effect[8,10]
#load modified matrix and raw matrix to compare if I have done it well
d
z
a=d*0.45
#checked, seems correct

#######################################
############ PLOT NETWORK #############
#######################################

net=graph.adjacency(a,mode="directed",weighted=TRUE,diag=FALSE) 
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/2, edge.arrow.size=0.3)
b=d*1
net=graph.adjacency(b,mode="directed",weighted=TRUE,diag=FALSE) 
plot.igraph(net,vertex.label=V(net)$name,vertex.label.cex=.6,layout=layout.fruchterman.reingold, vertex.color=c(rep("pink",2),"grey","pink", rep("skyblue",2),"grey","pink", rep("grey",2)), vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/2, edge.arrow.size=0.3)

set.seed(32)
#png("Figure5.png", width = 9000, height = 9000, res = 1500)
#pdf("Figure5.pdf")

plot.igraph(net,vertex.label=V(net)$name,vertex.label.cex=.55,
layout=layout.fruchterman.reingold, vertex.color=c(rep("#0072B2",2),"#D55E00","#0072B2", rep("#009E73",2),"#D55E00","#0072B2", rep("#D55E00",2)), 
vertex.label.color="black",edge.color="grey4",edge.width=E(net)$weight/1.8, edge.arrow.size=0.65)
#dev.off()


(d+t(d)>0)
#Example
int.to.dom=function(x){ ((x<=(t(x)+0.2)) & ((x<=t(x)-0.2)) & (x+t(x)>0))+0}
d <- int.to.dom(effect)
z=d



#UNIPARTITE DOUBLE ASSYMETRICAL EFFECT
library("igraph")
G <- graph.adjacency(t(effect),weighted=TRUE,diag=FALSE)
plot(G)
E <- t(apply(get.edgelist(G),1,sort))
E(G)$curved <- 0
E(G)[duplicated(E) | duplicated(E,fromLast =TRUE)]$curved <- 0.2
plot(G, edge.width=E(G)$weight/0.8,  vertex.color=c(rep("#0072B2",2),"#D55E00","#0072B2", rep("#009E73",2),
              "#D55E00","#0072B2", rep("#D55E00",2)),edge.arrow.size=0.5,edge.color=adjustcolor(c(rep("#0072B2",8),rep("#0072B2",8), rep("#D55E00",7), 
              rep("#0072B2",9),rep("#009E73",7),rep("#009E73",9),rep("#D55E00",7),rep("#0072B2",8), rep("#D55E00",8), rep("#D55E00",7)), alpha.f = .5))






