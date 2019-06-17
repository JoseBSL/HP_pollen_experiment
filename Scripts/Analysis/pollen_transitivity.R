#SCRIPT FOR:

#Measurements of Transitivity of heterospecific pollen competition network
#Plotting its structure


#LOAD LIBRARIES
#Remember statnet and igraph doesnt like each other so dettach 
#library(statnet)
library(reshape2)
library(igraph)
library(sna)

#LOAD MATRIX
#Effect sizes matrix
matrix <- as.matrix(read.csv("Data/effect.csv", header = T, row.names = 1))
matrix<- as.matrix(matrix)
str(matrix)
d<- matrix
#convert into dominance matrix
int.to.dom=function(x){ ((x<t(x)) & (x+t(x)>0))+0}
m <- int.to.dom(d)
#Making equal col and rownames
colnames(m)<-rownames(m)
m
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



#Numerical calculation, don't trust myself here too much (Jose)
#I just modified the values in vector from the output of triad.census (Jose)

m<-as.matrix(m)
m<-rgraph(5,10)

m=network(m,directed=TRUE)

str(m)
# We calculate P.t and t.tri for this empirical network.
tri=triad.census(g) #The full triad census as an 16-element vector
w=as.vector(c(0,0,0,0,0,0,0,0,117,3,0,0,0,0,0,0)) # The weighting
#vector for transitivity
#Here I think that 1 goes where we have values different to zero (Jose)
N.triangle=sum(tri*as.vector(c(0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0))) 
#Countand sum the number of triangles
Pt=sum(tri*w)/N.triangle
t.tri=4*(Pt-0.75)
Pt
t.tri
## We now conduct 1,000 simulations of random networks with the same
#number of mutual, asymmetric and null dyads as the empirical
#dominance matrix. We calculate Pt from each of these simulated
#networks (r.P.t).
dyads=dyad.census(g)
r.p.t=vector(length=1000)
j=1
while(j<1001){
  r=rguman(1, nv=nrow(m), mut=dyads[1], asym=dyads[2], null=dyads[3])
  r.triad=triad.census(r)
  r.p.t[j]=r.triad[9]/(r.triad[10]+r.triad[9])
  if(is.na(r.p.t[j])) next else j=j+1
}
p=length(r.p.t[r.p.t>=Pt])/1000
p
#So we have significant levels of transitivity? (Jose)


#other possible ways to plot the tyransitivity network

#install.packages("sna")
library(sna)






#other package
install.packages(graph)
g <- graph( c(0,1, 1,2, 2,3, 3,4), n=6, directed=TRUE )


 
library(igraph)

d
m


net=graph.adjacency(m,mode="directed",weighted=TRUE,diag=FALSE) 
#the only difference between this and the weighted network code is that mode="directed"
 
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/2, edge.arrow.size=0.5)
m=m*2
net=graph.adjacency(m,mode="directed",weighted=TRUE,diag=FALSE) 
#the only difference between this and the weighted network code is that mode="directed"
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/2, edge.arrow.size=0.5)

library(sna)
g<-rgraph(10,10)
gtrans(g)
#Find transitivity scores
b<- rgraph(m)
gtrans(m)
gtrans(m, mode="digraph", measure="weak")

a=m*0
gtrans(x)

triad.classify(m)
triad.census(a)

A<- matrix(1:9, nrow = 3, ncol = 3)
gtrans(A)
diag(A)<-0
A[1,2:3]<- 1
A[2:3,1]<-0



#R code

effect <- readRDS("Data/matrix_effect_size.RData")
write.csv(effect, "Data/effect.csv")
#Now I convert the few values over 0 to maximum 0 (control)
effect[effect>0]<-0
#absolute values so less confusing without negative values
#So now greater values involve greater effect
#(opposite way in comparison with negative values as before)
effect=abs(effect)
#find out winners and losers
int.to.dom=function(x){ ((x<t(x)) & (x+t(x)>0))+0}
d <- int.to.dom(effect)
m=d
detach("package:igraph") 

library(statnet) 

effect <- readRDS("Data/matrix_effect_size.RData")
#Now I convert the few values over 0 to maximum 0 (control)
effect[effect>0]<-0
#absolute values so less confusing without negative values
#So now greater values involve greater effect
#(opposite way in comparison with negative values as before)
effect=abs(effect)
#find out winners and losers
int.to.dom=function(x){ ((x<t(x)) & (x+t(x)>0))+0}
d <- int.to.dom(effect)
g=network(d,directed=TRUE)
gtrans(d)
detach("package:igraph") 

# We calculate P.t and t.tri for this empirical network.

tri=triad.census(g) #The full triad census as an 16-element vector 

w=as.vector(c(0,0,0,0,0,0,0,0,1,0,0,1,1,0.5,0.75,0.75)) # The weighting vector for transitivity 

N.triangle=sum(tri*as.vector(c(0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0))) #Count and sum the number of triangles

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
  r=rguman(1, nv=nrow(m), mut=dyads[1], asym=dyads[2], null=dyads[3])
  r.triad=triad.census(r)
  r.p.t[j]=r.triad[9]/(r.triad[10]+r.triad[9])
  if(is.na(r.p.t[j])) next else j=j+1
}
p=length(r.p.t[r.p.t>=Pt])/1000
p

triad.census(rgraph(15,5,tprob=c(0.1,0.25,0.5,0.75,0.9)))





#Plot the pollen network effect
net=network(m,matrix.type="adjacency",directed=T)
quartz()
a <- rownames(m)
gplot(net, label = a)

net=graph.adjacency(a,mode="directed",weighted=TRUE,diag=FALSE) 
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/2, edge.arrow.size=0.3)
b=d*1.5
net=graph.adjacency(m,mode="directed",weighted=TRUE,diag=FALSE) 
plot.igraph(net,vertex.label=V(net)$name,vertex.label.cex=.6,layout=layout.fruchterman.reingold, vertex.color=c(rep("pink",2),"grey","pink", rep("skyblue",2),"grey","pink", rep("grey",2)), vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/2, edge.arrow.size=0.3)
#try other colours? like the ones of effect sizes plots?
#"#0072B2", "#009E73", "#D55E00","#E69F00"
plot.igraph(net,vertex.label=V(net)$name,vertex.label.cex=.6,layout=layout.fruchterman.reingold, vertex.color=c(rep("#0072B2",2),"#D55E00","#0072B2", rep("#009E73",2),"#D55E00","#0072B2", rep("#D55E00",2)), vertex.label.color="black",edge.color="grey4",edge.width=E(net)$weight/1.8, edge.arrow.size=0.3)

