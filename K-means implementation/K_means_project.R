#YOU HAVE TO CTRL+ENTER LINEBYLINE FOR COMPILE CODE
#ALSO https://rstudio.cloud/spaces/35887/project/633762 LINK IS HIRE

install.packages('tidyverse')
library(tidyverse)



data <- read_csv("iris.csv")

x<-iris[,1:4]
standardize<-function(x){
  for (i in 1:dim(x)[2]){
    x[,i]=(x[,i]-mean(x[,i]))/sd(x[,i])
  }
    return(x) }
  X<-as.data.frame(standardize(x))
  boxplot(X)
  
  
model<-kmeans(X,3) ;model$cluster
as.numeric(iris[,5])
table(model$cluster,as.numeric(iris[,5]))


centroid<-X[sample(150,3),]
dist_to_centroid<-matrix(NA,ncol=3,nrow=dim(X)[1])


centroid_new<-matrix(0,nrow=3,ncol=4)  
while(all(centroid !=centroid_new)){
  centroid_new<-centroid
  for (i in 1:150) {
    for(j in 1:3){
      dist_to_centroid[i,j]<-sqrt(sum((X[i,]-centroid[j,])^2))
    }
    
  }
  category<-rep(NA,150)
  for(i in 1:150){
    category[i]<-which.min(dist_to_centroid[i,])
    
  }
  for(i in 1:3){
    centroid[i,]<-colMeans(X[which(category==i),])
  }
}


nodes<-dim(X)[1]
S<-matrix(NA,nrow=nodes,ncol = nodes)
sigma<-1.5 #similarity Gaussion matrix
for(i in 1:nodes){
  for(j in 1:nodes){
    S[i,j]<-exp(-sum((X[i,]-X[j,])^2/2*sigma^2))
  }
}


S_degree<-rowSums(S)
Laplacian<-diag(S_degree)-S
normalized_Laplacian<-diag(S_degree^(-1/2))%*% S %*% diag(S_degree^(-1/2))


#error=sum(S[i,j]*(fi-fi)^2)
ev<-eigen(Laplacian)
e<-kmeans(ev$vectors[,which.min(ev$values)],3)$cluster
as.numeric(iris[,5])
e


ev<-eigen(normalized_Laplacian)
ev_vector<-ev$vectors[,1:3]
for(i in 1:nodes){
  ev_vector[i,]<-ev_vector[i,]/sqrt(sum(ev_vector[i,]^2))
}
e<-kmeans(ev_vector,3)$cluster
as.numeric(iris[,5])
e
table(e,as.numeric(iris[,5]))


Sx=var(X)
EP=eigen(Sx)
V=EP$vectors
PC=as.matrix(X)%*%as.matrix(V)
cumsum(EP$values)/sum(EP$values)

plot(PC[,1],PC[,2],col=c("red","blue","green")[iris[,5]])







