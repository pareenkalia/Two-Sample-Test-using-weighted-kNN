
#install.packages('corpcor')
library('corpcor')
library('ggplot2')
library('gplots')

final_unif_list<- list()
vector1_unif<-list()

# iterating code 20 times to get 20 plots
iter<-20

par(mfrow=c(5,4))

for (f in 1:iter){


n1 <- 30 #sample 1 size
n2 <- 30 #sample 2 size
n<- n1+n2
k <- 3 # number of nearest neighbors

#Generate random points in 2D from Uniform Distribution

x1 <- runif(n1) # SAMPLE 1 x coordinate from uniform
y1 <- runif(n1) # SAMPLE 1 y coordinate from uniform
x2 <- runif(n2) # SAMPLE 2 x coordinate from uniform
y2 <- runif(n2) # SAMPLE 2 y coordinate from uniform

# Putting data points from sample 1 in the dataframe df1_unif, and data points from sample 2 in df2_unif

df1_unif <- data.frame(x1,y1)
rownames(df1_unif)<-c(1:n1) # labeling the points from 1 through n1
df2_unif <- data.frame(x2,y2)
rownames(df2_unif)<-c(n1+1:n2) # labeling the points from n1+1 through n1+n2

# Scatterplot of points

scatterplot_unif <- ggplot() + geom_point(data=df1_unif, aes(x=df1_unif[,1],y=df1_unif[,2]),shape=1, fill="red",color="red")+
  geom_point(data=df2_unif, aes(x=df2_unif[,1],y=df2_unif[,2]), shape=1, color="blue") +xlab("x")+ylab("y")
  
scatterplot_unif

#combining two samples (appending df2_unif under df1_unif)

df_unif<-rbind(as.matrix(df1_unif), as.matrix(df2_unif))

#Computing distance of each data point in combined sample with every other data point in the sample

distance_mat_unif <- matrix(NA,nrow=n, ncol=n)
rownames(distance_mat_unif)<-c(1:n)

for (i in 1:n)
{
  for ( j in 1:n)
  {
    distance_mat_unif[i,j] <- sqrt(min(abs(df_unif[i,1]-df_unif[j,1]),(1-abs(df_unif[i,1]-df_unif[j,1])))^2+min(abs(df_unif[i,2]-df_unif[j,2]),(1-abs(df_unif[i,2]-df_unif[j,2])))^2)
  }
}

# distance_mat_unif is the dataframe with distances of each point to every other point

# Matrix of k nearest neighbors of each point

mat_unif<- matrix(NA, nrow=n, ncol=k) 

for (i in 1:n)
{ 
  for (j in 1:k)
  {
    mat_unif[i,j]<- which(distance_mat_unif[i,] == sort(unique(distance_mat_unif[i,]),partial=j+1)[j+1])   
  }
}
rownames(mat_unif)<-c(1:n)

# mat_unif is the list of points and their k (in this case 3) nearest neighbors 

# Boolean Matrix indicating if neighbor is from the same sample as corresponding data point

bool_mat_unif<- matrix(NA, nrow=n, ncol=k) #boolean matrix indicating if NN's are from same sample as data point

for (i in 1:n)
{
  for (j in 1: k)
  {
    if (i<=n1 & mat_unif[i,j] <= n1 )
    {
      bool_mat_unif[i,j]=1
    }
    else if (i > n1 & mat_unif[i,j] > n1)
    {
      bool_mat_unif[i,j]=1
    }
    else 
    {
      bool_mat_unif[i,j]=0
    }
  }
}
rownames(bool_mat_unif)<-c(1:n)

# bool_mat_unif is a boolean matrix, 1 indicates the neighbor is from the same sample as the reference point, 0 indicates it's not

#counting number of nearest neighbors from same sample of each data point 

counter_mat_unif <- matrix(NA, nrow=n, ncol=1)

for(i in 1:n)
{
  counter_mat_unif[i,1]<-sum(bool_mat_unif[i,])
}

rownames(counter_mat_unif)<-c(1:n)

# counter_mat_unif is just the number of nearest neighbors of a reference point from the same sample as the reference point

#-------------- Approximating P(....) by computing number of pairs of points (i,i') with with number of nearest neighbors h and h' respectively
#Computing number of pairs of NN for different combinations of h and h'

newmat_unif<- matrix(NA,nrow=n1,ncol=1)
for(i in 1:n1)
{
  newmat_unif[i,1]<-counter_mat_unif[i,1]
}
rownames(newmat_unif)<-c(1:n1)
colnames(newmat_unif)<-c("h")

ctr<-0
ctr_mat<-1
number_of_pairs_mat_unif<-matrix(NA, nrow=(k+1)^2, ncol=3)

for (h in 0:k)
{
  for (k in 0:k)
  {
    for (i in 1:n1)
    {
      if (newmat_unif[i,]==h)
      {
        for (j in 1:n1)
        { 
          if(newmat_unif[j,]==k & j!=i)
          {         
            ctr=ctr+1
          }
        }
      }
    }
    if (h==k)
    {
      ctr=ctr/2
    }
    
    number_of_pairs_mat_unif[ctr_mat,]<- c(h,k,ctr)
    ctr_mat<-ctr_mat+1
    ctr<-0
    
  }
}

# writing the combination of pairs as matrix

nn_unif<- matrix(NA,nrow=k+1,ncol=k+1)
for (i in 0:k)
{
  for (j in 0:k)
  {
    nn_unif[i+1,j+1]<-number_of_pairs_mat_unif[j+1+(k+1)*(i),3]
  }
}


rownames(nn_unif)<-c(0:k)
colnames(nn_unif)<-c(0:k)

final_unif_list[[f]]<-nn_unif
vector1_unif[[f]]<-sm2vec(final_unif_list[[f]], diag=TRUE)
#Estimate of P(NN(i)=h,NN(i')=h')

p_est<-matrix(NA,nrow=k+1,ncol=k+1)
for(h in 0:k)
{
  for(hp in 0:k)
  {
    if (h!=hp)
    {
      p_est[h+1,hp+1] <- ((nn_unif[h+1,hp+1]+nn_unif[hp+1,h+1]))/(n1*(n1-1))
    }
    else 
    {
      p_est[h+1,hp+1]<-(2*nn_unif[h+1,hp+1]/(n1*(n1-1))) ###numerator was multiplied by 2- check
    }
  }
}
rownames(p_est)<-c(0:k)
colnames(p_est)<-c(0:k) 



#plot(x=df1_unif[,1],y=df1_unif[,2],xlab="X",ylab="Y", col="blue",main="Sample 1")

# plot_mat<-matrix()
# 

# 
# print("Data set")
# df
# 
# print("Distance between data points")
# distance_mat_unif
# 
# print("Matrix of nearest neighbors")
# mat_unif
# 
# print("matrix indicating if k-nearest neighbor is from same sample as data point")
# bool_mat
# 
# print("count of number of nearest neighbors from same sample")
# newmat
# 
# print("number of pairs matrix")
# number_of_pairs_mat
# 
# print("number of pairs of combinations of nearest neighbors")
# nn_unif
# 
# print("Estimate of P")
# p_est

#print("decreasing order of number of pairs")
#ndx <- order(number_of_pairs_mat, decreasing = T)[1:36]
#number_of_pairs_mat[ndx]




#g<-nng(x = NULL, dx = distance_mat_unif, k = 3, mutual = FALSE, method = NULL)

#V(g)[V(g)<n1+1]$color <- "red"
#V(g)[V(g)>n1]$color <- "blue"
#plot.new()
#plot(g,layout=df,axes=TRUE, vertex.color=V(g)$color,edge.arrow.size=0.3,rescale=T, vertex.size=3.5,vertex.label.color="black",vertex.label.dist=0.5, vertex.label.cex=0.5, main="all neighbors- uniform distribution")



#f<-nng(x = NULL, dx = distance_mat_unif, k = 3, mutual = TRUE, method = NULL)

#V(f)[V(f)<n1+1]$color <- "red"
#V(f)[V(f)>n1]$color <- "blue"
#plot.new()
#plot(f,layout=df,vertex.color=V(f)$color,edge.arrow.size=0.3,rescale=T,axes=TRUE, vertex.size=3.5,vertex.label.color="black",vertex.label.dist=0.5, vertex.label.cex=0.5, main="mutual neighbors- uniform distribution")



 col_palette<-c("lightcyan","lightblue1","skyblue1","steelblue1","royalblue1","mediumblue","navy")
# 
 col_breaks <- c(0,10,20,40,60,100,140,200) #red,orange,yellow,green
# 
jpeg(file='heatmap.jpeg')
 heatmap.2(nn_unif, symm=TRUE, cellnote=nn_unif, margins=c(5,5),notecol="grey",dendrogram="none",density.info="none",trace="none", col=col_palette, breaks=col_breaks, Rowv=FALSE, Colv=NA)
# 
dev.off()



#print(nn_unif)

}

