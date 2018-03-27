tags <- read.csv('tags.csv')
links <- read.csv('links.csv')
movies <- read.csv('movies.csv')
ratings <- read.csv('ratings.csv')

library(corrplot)
library(cluster)

## find the mean rating a user gives for a genre
  
  
s <- strsplit(as.character(movies$genres), split = "|",fixed=T)
newmovies <- data.frame(movieId=rep(movies$movieId, sapply(s,length)),title = rep(movies$title,sapply(s, length)), genre = unlist(s))
  
allgenres <- unique(newmovies$genre)

m <- matrix(0, ncol = length(allgenres), nrow = length(movies$movieId))
m<-data.frame(m)
colnames(m)<-allgenres

movieswide<-cbind(movies$movieId,movies$title, m)
colnames(movieswide)[1] <- colnames(movies)[1]
colnames(movieswide)[2] <- colnames(movies)[2]
for(j in 3:length(allgenres)){
  for(i in 1:length(newmovies$genre)){
      if(newmovies$genre[i]==colnames(movieswide)[j]){
        id <- newmovies$movieId[i]
        
        
        movieswide[(movieswide$movieId==id),j] <-1
      }
          
      
      
  }

}

#train.data <- movieswide[train,]
#test.data <- movieswide[test,]
#responseY <- train.data[3:length(train.data)]
#row.names(responseY)<-movieswide[train,]$title

responseY <- movieswide[3:length(movieswide)]



pr.out=prcomp(responseY)

#inspect output
names(pr.out)
pr.out$center
pr.out$scale

#The rotation component provides the principal component loadings
pr.out$rotation
dim(pr.out$x)
#The scale=0 is to make sure that arrows are scaled to represent the loadings
biplot(pr.out, scale=0)

#We make a few changes in order to mirror the figure 
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

#Get standard deviations
pr.out$sdev

#Variance explained by each PC
pr.var=pr.out$sdev^2
pr.var
#Variance explained by all PC
pve=pr.var/sum(pr.var)
pve

X <- cbind(pr.out$x[,1:4])
cl <- kmeans(X,3)
plotcluster(X,cl$cluster)
legend("topleft")

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 20
data <- X
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
w

# According to this plot we choose to have k=7 although there is not a clear elbow in the plot. 

X <- cbind(pr.out$x[,1:4])
cl <- kmeans(X,7)

plotdata <- as.data.frame(cbind(X[,1:2],cl$cluster))
row.names(plotdata)<-movieswide$movieId
colnames(plotdata)<- c("PC1","PC2","Cluster")
plotdata$Cluster <- factor(plotdata$Cluster)

ggplot(plotdata, aes(x= PC1, y= PC2, label=rownames(plotdata))) +
  geom_point() + geom_text(aes(label=rownames(plotdata),colour=Cluster),hjust=0.5, vjust=0)

ggplot(plotdata, aes(x= PC1, y= PC2, label=rownames(plotdata))) +
  geom_point(aes(colour=Cluster))


## Now we have all similar movies according to their genres using PCA
movieswide[movieswide$movieId ==680,]
movieswide[movieswide$movieId ==27317,]


# Clustering and ratings

df <- data.frame(movieswide$movieId,cl$cluster)

cluster1 <- df[df$cl.cluster==1,1:2]
cluster2 <- df[df$cl.cluster==2,1:2]
cluster3 <- df[df$cl.cluster==3,1:2]
cluster4 <- df[df$cl.cluster==4,1:2]
cluster5 <- df[df$cl.cluster==5,1:2]
cluster6 <- df[df$cl.cluster==6,1:2]
cluster7 <- df[df$cl.cluster==7,1:2]

uniqueUsers <- unique(ratings$userId)

fRatings <- function(uniqueUsers,cluster1,threshold){
  meanRatings<-NULL
  meanRatings <- matrix(ncol=3, nrow=length(uniqueUsers))
  instances<-length(uniqueUsers)
  
  for(i in 1:instances){
  
    am<- ratings[ratings$userId==uniqueUsers[i],1:3] #allMoviesFromUser
    amc <- am[am$movieId %in% cluster1$movieswide.movieId,1:3] #allMoviesFromUserInCluster
    x<- round(mean(amc$rating),2)
  
    meanRatings[i,]<- c( uniqueUsers[i],cluster=cluster1$cl.cluster[1],rating=x)
  
  }

  output1 <- data.frame(userId =meanRatings[,1], cluster =meanRatings[,2],rating = meanRatings[,3])
  output1 <- output1[output1$rating>threshold,1:3]
  output1 <- na.omit(output1)

  return(output1)
}


output1 <- fRatings(uniqueUsers,cluster1,4.5)
output2 <- fRatings(uniqueUsers,cluster2,4.5)
output3 <- fRatings(uniqueUsers,cluster3,4.5)
output4 <- fRatings(uniqueUsers,cluster4,4.5)
output5 <- fRatings(uniqueUsers,cluster5,4.5)
output6 <- fRatings(uniqueUsers,cluster6,4.5)
output7 <- fRatings(uniqueUsers,cluster7,4.5)



