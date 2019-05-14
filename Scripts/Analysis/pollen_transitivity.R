#Analysis of transitivity of pollen effect

library(statnet)
library(reshape2)
#load our matrix
matrix <- as.matrix(read.csv("Data/matrix_scale_effect.csv", header = T, row.names = 1))
matrix<- as.matrix(matrix)
str(matrix)
#convert into dominance matrix
int.to.dom=function(x){ ((x>t(x)) & (x+t(x)>0))+0}
m <- int.to.dom(matrix)
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
#import the sample_dw_adj.csv file:
dat=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE) # read .csv file
m=as.matrix(matrix)

m[1,2]<-5

net=graph.adjacency(m,mode="directed",weighted=TRUE,diag=FALSE) 
#the only difference between this and the weighted network code is that mode="directed"
 
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/2, edge.arrow.size=0.5)
