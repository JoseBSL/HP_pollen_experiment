#################################################################################
#################################################################################
#################################################################################
########################### TRANSITIVITY METRICS ################################
#################################################################################
#################################################################################
#################################################################################


####################################
############ load libraries ########
####################################

#Remember statnet and igraph doesnt like each other so dettach 
#library(statnet)
library(reshape2)
library(igraph)
library(sna)



#######################################
############ Data preparataion ########
#######################################

#Effect sizes  matrix
effect <- readRDS("Data/RData/matrix_effect_size.RData")
#Now I convert the few values over 0 to maximum 0 (control)
effect[effect>0]<-0
effect=abs(effect)
#convert into dominance matrix
#int.to.dom=function(x){ ((x<t(x)) & (x+t(x)>0))+0}
#This is now an old step
#after reviewers comments we have decided to allow more cyclic triads
#therefore the new function is as follows
int.to.dom=function(x){((x<t((x)+0.2) & (x<(t(x)-0.2))))+0}
#We have selected 0.2 as critical value based on cohen classification of low effect size
d <- int.to.dom(effect)
#Making equal col and rownames
colnames(d)<-rownames(d)
d

#For plotting purposes spp that are losers have 1 and winners 0
#Doesn't change the analysis with gtrans (checked)

#GTRANS from library sna
#To understand how it works
#First we calculate random the transitivity of random dominance matrices
g<-rgraph(3,3)
g
#The output will vary each time we run it
#Values between 0 and 1 where 0 means intransitive and 1 transitive
gtrans(g)
#Now we calculate it for our data
gtrans(m)
#[1] 0.9285714
gtrans(matrix)

#######################################
########### TRANSITIVITY METRIC #######
#######################################

#ANALYSIS of Shikuza and Mcdonald 2012
#Detach in order to run statnet
detach("package:igraph") 
library(statnet) 


g=network(d,directed=TRUE)
# We calculate P.t and t.tri for this empirical network.
tri=triad.census(g) #The full triad census as an 16-element vector 
w=as.vector(c(0,0,0,0,0,0,0,0,1,0,0,1,1,0.5,0.75,0.75)) # The weighting vector for transitivity 
N.triangle=sum(tri*as.vector(c(0,1,0,1,1,1,0,0,1,1,0,0,0,0,0,0))) #Count and sum the number of triangles
#N.triangle=sum(tri*as.vector(c(0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0))) #Count and sum the number of triangles

Pt=sum(tri*w)/N.triangle 
t.tri=4*(Pt-0.75) 
Pt 
t.tri

## We now conduct 1,000 simulations of random networks with the same number of mutual,
#asymmetric and null dyads as the empirical dominance matrix. 
#We calculate Pt from each of these simulated networks (r.P.t). I use a while() loop because randomizations of some sparse matrices will produce networks with no triangles. For simplicity, we just throw these out. 

dyads=dyad.census(g)
r.p.t=vector(length=1000)
j=1

while(j<1001){
  r=rguman(1, nv=nrow(d), mut=dyads[1], asym=dyads[2], null=dyads[3])
  r.triad=triad.census(r)
  r.p.t[j]=r.triad[9]/(r.triad[10]+r.triad[9])
  if(is.na(r.p.t[j])) next else j=j+1
}
p=length(r.p.t[r.p.t>=Pt])/1000
p



