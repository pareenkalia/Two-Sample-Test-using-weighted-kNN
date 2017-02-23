 
n1 <- 20
n2 <- 20
n<- n1+n2

p<-3

x1 <- runif(n1) 
x2 <- runif(n1) 
x3 <- runif(n1) 
y1 <- runif(n2) 
y2 <- runif(n2)
y3 <- runif(n2)

x <- cbind(x1,x2,x3)
y <- cbind(y1,y2,y3)
xy <- rbind(x,y)
#-- finding distance between all pairs of points
d <- matrix(NA,nrow=n, ncol=n)
for (i in 1:n)
{
  for (j in 1:n)
  {
    z <- 0
    for (k in 1:p)
    {
    z <- z+ min(abs(xy[i,k]-xy[j,k]),1-abs(xy[i,k]-xy[j,k]))^2
    d[i,j] = z
    }
  }
}
#-- finding k nearest neighbors of each point
knn<- matrix(NA, nrow=n, ncol=k) 
for (i in 1:n)
{ 
  for (j in 1:k)
  {
    knn[i,j]<- which(d[i,] == sort(unique(d[i,]),partial=j+1)[j+1])   
  }
}
rownames(knn)<-c(1:n)
#-- indicating if p nearest neighbors are from same sample

sample_indicator<- matrix(NA, nrow=n, ncol=k) #boolean matrix indicating if NN's are from same sample as data point

for (i in 1:n)
{
  for (j in 1: k)
  {
    if (i<=n1 & knn[i,j] <= n1 )
    {
      sample_indicator[i,j]=1
    }
    else if (i > n1 & knn[i,j] > n1)
    {
      sample_indicator[i,j]=1
    }
    else 
    {
      sample_indicator[i,j]=0
    }
  }
}
rownames(sample_indicator)<-c(1:n)

#--counting number of nearest neighbors from same sample of each data point 

nn_count <- matrix(NA, nrow=n, ncol=1)

for(i in 1:n)
{
  nn_count[i,1]<-sum(sample_indicator[i,])
}


#-- computing the counts of points with h nearest neighbors
c1 <- c()
for(h in 0:k)
{
add <- 0  
for (i in 1:n1)
{
  if (nn_count[i,1]==h)
  {
    add <- add + 1
  }
}
c1[h+1] <- add
}

#--
c2<-c()
for(h in 0:k)
{
  add <- 0  
  for (i in n1+1:n2)
  {
    if (nn_count[i,1]==h)
    {
      add <- add + 1
    }
  }
  c2[h+1] <- add
}

#-- computing the weights w1 and w2
w1<-c()

for (h in 0:k)
{
  w1[h+1] <- choose(n1+n2-1, k) / (choose(n1-1,h)*choose(n2,k-h))
}

w2<-c()

for (h in 0:k)
{
  w2[h+1] <- choose(n1+n2-1, k) / (choose(n2-1,h)*choose(n1,k-h))
}

#-- Computing Test Statistic
wc1<-0
for(h in 1:k)
{
  t1 <- wc1 + w1[h]*c1[h] 
}  
wc2<-0
for(h in 1:k)
{
  t2 <- wc2 + w2[h]*c2[h]  
}

t <- (t1 + t2)/(n*k)
t