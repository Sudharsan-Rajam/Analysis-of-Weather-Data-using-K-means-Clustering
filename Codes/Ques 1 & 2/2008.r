#Clear Memory
rm(list=ls())

#Loading 2007 Data
df_2008_daily_avg <- read.csv("2008_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")

#### Clustering with Seed : 120 ###
df_temp <- df_2008_daily_avg
df_temp$STN_WBAN_DAYOFMONTH <- paste(df_temp$STN, "_", df_temp$WBAN, "_", df_temp$DAYOFMONTH, sep = "")
df_temp <- df_temp[-c(1,2,3)]
df_temp <- df_temp[c(5,1,2,3,4)]

rownames(df_temp) <- df_temp$STN_WBAN_DAYOFMONTH
df_temp <- df_temp[-c(1)]

set.seed(120)
rows <- sample(nrow(df_temp))
df_temp <- df_temp[rows, ]

library(factoextra)
library(amap)
km <- Kmeans(df_temp, centers=2, method="euclidean", iter.max = 100)

df_temp$cluster <- km$cluster
fviz_cluster(km, data = df_temp)

kp <- Kmeans(df_temp, centers=2, method="pearson", iter.max = 100)

df_temp$cluster <- kp$cluster
fviz_cluster(kp, data = df_temp)


#### Clustering with Seed : 1905 ###
df_temp <- df_2008_daily_avg
df_temp$STN_WBAN_DAYOFMONTH <- paste(df_temp$STN, "_", df_temp$WBAN, "_", df_temp$DAYOFMONTH, sep = "")
df_temp <- df_temp[-c(1,2,3)]
df_temp <- df_temp[c(5,1,2,3,4)]

rownames(df_temp) <- df_temp$STN_WBAN_DAYOFMONTH
df_temp <- df_temp[-c(1)]

set.seed(1905)
rows <- sample(nrow(df_temp))
df_temp <- df_temp[rows, ]

library(factoextra)
library(amap)
km <- Kmeans(df_temp, centers=2, method="euclidean", iter.max = 100)

df_temp$cluster <- km$cluster
fviz_cluster(km, data = df_temp)

kp <- Kmeans(df_temp, centers=2, method="pearson", iter.max = 100)

df_temp$cluster <- kp$cluster
fviz_cluster(kp, data = df_temp)