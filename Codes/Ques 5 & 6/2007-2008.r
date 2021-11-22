#Clear Memory
rm(list=ls())

#Loading Data
df_2007_daily_avg <- read.csv("2007_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")
df_2008_daily_avg <- read.csv("2008_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")

#### Clustering with Seed : 120 ###
df_temp <- df_2007_daily_avg
df_temp$STN_WBAN_DAYOFMONTH <- paste(df_temp$STN, "_", df_temp$WBAN, "_", df_temp$DAYOFMONTH, sep = "")
df_temp <- df_temp[-c(1,2,3)]
df_temp <- df_temp[c(5,1,2,3,4)]

rownames(df_temp) <- df_temp$STN_WBAN_DAYOFMONTH
df_temp <- df_temp[-c(1)]

#### Clustering with Seed : 120 ###
df_temp1 <- df_2008_daily_avg
df_temp1$STN_WBAN_DAYOFMONTH <- paste(df_temp1$STN, "_", df_temp1$WBAN, "_", df_temp1$DAYOFMONTH, sep = "")
df_temp1 <- df_temp1[-c(1,2,3)]
df_temp1 <- df_temp1[c(5,1,2,3,4)]

rownames(df_temp1) <- df_temp1$STN_WBAN_DAYOFMONTH
df_temp1 <- df_temp1[-c(1)]

row2 <- c()
c=1
list2 <- as.list(row.names(df_temp1))
for (row1 in as.list(row.names(df_temp))) {
  if (row1 %in% list2) {
  }
  else{
    row2 <- append(row2,row1)
  }
}
df_sim <- data.frame(STN=row2,TEMP=0,DEWP=0,STP=0,WDSP=0)
rownames(df_sim) <- df_sim$STN
df_sim <- df_sim[-c(1)]

df_temp2 <- rbind(df_temp1,df_sim)

row2 <- c()
c=1
list2 <- as.list(row.names(df_temp))
for (row1 in as.list(row.names(df_temp1))) {
  if (row1 %in% list2) {
  }
  else{
    row2 <- append(row2,row1)
  }
}
df_sim <- data.frame(STN=row2,TEMP=0,DEWP=0,STP=0,WDSP=0)
rownames(df_sim) <- df_sim$STN
df_sim <- df_sim[-c(1)]

df_temp3 <- rbind(df_temp,df_sim)

## For Euclidean

library(amap)

ky1 <- Kmeans(df_temp2, centers=2, method="euclidean", iter.max = 100)$cluster
ky2 <- Kmeans(df_temp3, centers=2, method="euclidean", iter.max = 100)$cluster

## Compare Clusters from same year using Jaccard Coefficient
library(clusteval)
jacc.clust <- cluster_similarity(ky2, ky1,
                                 similarity = c("jaccard"))

print(jacc.clust)
print(comembership_table(ky1, ky2))

## For Pearson

library(amap)

ky3 <- Kmeans(df_temp2, centers=2, method="pearson", iter.max = 100)$cluster
ky4 <- Kmeans(df_temp3, centers=2, method="pearson", iter.max = 100)$cluster

## Compare Clusters from same year using Jaccard Coefficient
library(clusteval)
jacc.clust1 <- cluster_similarity(ky4, ky3,
                                 similarity = c("jaccard"))

print(jacc.clust1)
print(comembership_table(ky3, ky4))
