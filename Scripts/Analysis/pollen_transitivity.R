#Analysis of transitivity of pollen effect

library(statnet)
library(reshape2)
#load our matrix
matrix <- as.matrix(read.csv("Data/matrix_scale_effect.csv", header = T, row.names = 1))
matrix<- as.matrix(matrix)
str(matrix)
d<- matrix
#convert into dominance matrix
int.to.dom=function(x){ ((x>t(x)) & (x+t(x)>0))+0}
m <- int.to.dom(matrix)
x=m
#In order to see the reverse matrix ploted
# m[m==1]<-2
 #m[m==0]<-1
 #m[m==2]<-0
 #diag(m)<- 0

#Plot the pollen network effect
net=network(m,matrix.type="adjacency",directed=T)
quartz()
a <- rownames(m)
gplot(net, label = a)

#Numerical calculation, don't trust myself here too much (Jose)
#I just modified the values in vector from the output of triad.census (Jose)
g=network(m,directed=TRUE)
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
r=rguman(1000,nv=nrow(m),mut=dyads[1],asym=dyads[2],null=dyads[3])
r.triad=triad.census(r)
r.P.t=apply(r.triad,1,function(x) x[9]/(x[10]+x[9]))
# Finally, we conduct a one-tailed test of the null hypothesis that
#P.t of the empirical network is not significantly different from P.t
#of the simulated random networks (r.P.t). The P-value is the number
#of times r.P.t is greater or equal to the empirical P.t value. By
#convention, we reject the null hypothesis if P < 0.05
p=length(r.P.t[r.P.t>=Pt])/1000
p 
#So we have significant levels of transitivity? (Jose)


#other possible ways to plot the tyransitivity network

#install.packages("sna")
library(sna)

#Draw some random graphs
g<-rgraph(5,10)
g
#Find transitivity scores
gtrans(g)




#other package
install.packages(graph)
g <- graph( c(0,1, 1,2, 2,3, 3,4), n=6, directed=TRUE )


 
library(igraph)

d
m

m[3,1]<- d[3,1]
m[1,2]<- d[1,2]
m[3,2]<- d[3,2]
m[5,2]<- d[5,2]
m[9,3]<- d[9,3]
m[1,4]<- d[1,4]
m[2,4]<- d[2,4]
m[3,4]<- d[3,4]
m[5,4]<- d[5,4]
m[6,4]<- d[6,4]
m[7,4]<- d[7,4]
m[8,4]<- d[8,4]
m[9,4]<- d[9,4]
m[10,4]<- d[10,4]
m[1,5]<-d[1,5]
m[3,5]<- d[3,5]
m[9,5]<- d[9,5]
m[1,6]<-d[1,6]
m[2,6]<-d[2,6]
m[3,6]<-d[3,6]
m[5,6]<-d[5,6]
m[7,6]<-d[7,6]
m[8,6]<-d[8,6]
m[9,6]<-d[9,6]
m[10,6]<-d[10,6]
m[1,7]<-d[1,7]
m[2,7]<-d[2,7]
m[3,7]<-d[3,7]
m[5,7]<-d[5,7]
m[9,7]<-d[9,7]
m[1,8]<-d[1,8]
m[2,8]<-d[2,8]
m[3,8]<-d[3,8]
m[5,8]<-d[5,8]
m[7,8]<-d[7,8]
m[9,8]<-d[9,8]
m[10,8]<-d[10,8]
m[1,9]<-d[1,9]
m[2,9]<-d[2,9]
m[1,10]<-d[1,10]
m[2,10]<-d[2,10]
m[3,10]<-d[3,10]
m[5,10]<-d[5,10]
m[7,10]<-d[7,10]
m[9,10]<-d[9,10]

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
