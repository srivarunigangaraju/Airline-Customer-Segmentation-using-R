


setwd('C:/Users/sxg210122/Desktop/BA with R/DataSets')
df <- read.csv('EastWestAirlinesSpring2022.csv') #Load the dataset
View(df)
df <- df[,-c(1)] #remove the id column
View(df)
df1 <- df[1:10,] #get the first 10 rows to illustrate calculations
df1
ds <- dist(df1,method = 'euclidean') #find the euclidean distance using all the 11 variables.
ds
#set k = 4 to create 4 clusters.  The nstart parameter specifies the number of times to
#run the cluster analysis with
#different starting points (seeds)
kmu <- kmeans(df,4,nstart = 100)
kmu
View(kmu$centers) #show centroids
View(kmu$cluster) #show cluster membership
write.csv(kmu$cluster,'kmcluster.csv')
write.csv(kmu$centers,'kmcenters.csv')
# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = c(min(kmu$centers), max(kmu$centers)),xlim=c(0,11))
     
# label x-axes
axis(1, at = c(1:11), labels = names(df))

# plot centroids for n = 4
for (i in c(1:4))
  lines(kmu$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                        "black", "dark grey"))

# name clusters
text(x =0.5,kmu$centers[, 1], labels = paste("Cluster", c(1:4)))
library(factoextra)
fviz_cluster(kmu, data = df,xlab=FALSE,ylab = FALSE)

#transforming data to standard normal distribution
df.norm <- scale(df,center = TRUE, scale = TRUE)

View(df.norm)
kmu1 <- kmeans(df.norm,4,nstart=100)
kmu1
View(kmu1$cluster)
View(kmu1$centers)
write.csv(kmu1$cluster,'kmu1cluster.csv')
write.csv(kmu1$centers,'kmu1centers.csv')
# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = c(min(kmu1$centers), max(kmu1$centers)),xlim=c(0,11))

# label x-axes
axis(1, at = c(1:11), labels = names(df))

# plot centroids for n = 4
for (i in c(1:4))
  lines(kmu1$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                        "black", "dark grey"))

# name clusters
text(x =0.5,kmu1$centers[, 1], labels = paste("Cluster", c(1:4)))

fviz_cluster(kmu1,data=df.norm,xlab=FALSE,ylab=FALSE)

#hierarchical clustering

d.norm <- dist(df.norm,method = 'euclidean')
d.norm
hc <- hclust(d.norm, method ='ward.D')
hc1 <- hclust(d.norm, method='single')
hc2<- hclust(d.norm,method = 'complete')
hc3<- hclust(d.norm,method = 'centroid')
hc4<- hclust(d.norm,method = 'average')
plot(hc,hang=-1,ann = FALSE)
plot(hc1,hang=-1,ann=FALSE)
plot(hc2,hang=-1,ann=FALSE)
plot(hc3,hang=-1,ann=FALSE)

#factoextra::fviz_nbclust(d.norm) + theme_minimal() + ggtitle("NbClust's optimal number of clusters") 
memb <- cutree(hc,k=4)
memb
memb1 <- cutree(hc,k=3)
memb1
write.csv(memb,'hcclusters1.csv')
library(factoextra)
write.csv(kmu1$centers,'kmeanscenters.csv')


# label clusters and add id
row.names(df.norm) <- paste(memb1, ": ", row.names(df.norm), sep = "")
View(df.norm)

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))


#calculate cluster centers
library(dplyr)
cluster_v <- substr(row.names(df.norm), 1, 1)
dfh <- data.frame(cluster_v, df.norm)
cluster_centers <- dfh %>% group_by(cluster_v) %>% summarise(balanceMean = mean(Balance),
                                                            Qual_milesmean = mean(Qual_miles),
                                                            cc1_milesmean = mean(cc1_miles),
                                                            cc2milesMean = mean(cc2_miles),
                                                            cc3milesMean = mean(cc3_miles),
                                                            bonusmilesMean = mean(Bonus_miles),
                                                            bonustransMean = mean(Bonus_trans),
                                                            flightmiles_12m0Mean = mean(Flight_miles_12mo),
                                                            flightmiles_transmean = mean(Flight_trans_12),
                                                            daysenrollmean = mean(Days_since_enroll),
                                                            awardmean = mean(Award.)
                                    
)

View(cluster_centers)
write.csv(cluster_centers,'hcclustercenters.csv')
library(factoextra)
fviz_nbclust(df.norm, hcut , method = "wss") + theme_minimal() + ggtitle("Elbow Chart")


newdata <- sample(nrow(df),nrow(df)*0.95)
newdata
newdf <- df[newdata,]
newdf
newdf.norm <- scale(newdf, center = TRUE, scale = TRUE)
newdf.norm
kmnew <- kmeans(newdf.norm, 4, nstart = 100)
kmnew
newdist <- dist(newdf.norm, method = 'euclidean')
hcnew <- hclust(newdist,method = 'ward.D')
plot(hcnew)
mem1 <- cutree(hcnew,k=4)
row.names(newdf.norm) <- paste(mem1, ": ", row.names(newdf.norm), sep = "")
library(dplyr)
cluster_n <- substr(row.names(newdf.norm), 1, 1)
dfnew <- data.frame(cluster_n, newdf.norm)
clustercenters <- dfnew %>% group_by(cluster_n) %>% summarise(balanceMean = mean(Balance),
                                                             Qual_milesmean = mean(Qual_miles),
                                                             cc1_milesmean = mean(cc1_miles),
                                                             cc2milesMean = mean(cc2_miles),
                                                             cc3milesMean = mean(cc3_miles),
                                                             bonusmilesMean = mean(Bonus_miles),
                                                             bonustransMean = mean(Bonus_trans),
                                                             flightmiles_12m0Mean = mean(Flight_miles_12mo),
                                                             flightmiles_transmean = mean(Flight_trans_12),
                                                             daysenrollmean = mean(Days_since_enroll),
                                                             awardmean = mean(Award.)
                                                             
)
write.csv(clustercenters,'hcnewdata.csv')
