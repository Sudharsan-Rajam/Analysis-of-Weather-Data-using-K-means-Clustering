#Clear Memory
rm(list=ls())

#Loading 2007 Data
df_2007_daily_avg <- read.csv("2007_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")

#### Euclidean Metric SSE with Seed : 120 ###
df_temp <- df_2007_daily_avg
df_temp$STN_WBAN_DAYOFMONTH <- paste(df_temp$STN, "_", df_temp$WBAN, "_", df_temp$DAYOFMONTH, sep = "")
df_temp <- df_temp[-c(1,2,3)]
df_temp <- df_temp[c(5,1,2,3,4)]

rownames(df_temp) <- df_temp$STN_WBAN_DAYOFMONTH
df_temp <- df_temp[-c(1)]

set.seed(120)
rows <- sample(nrow(df_temp))
df_temp <- df_temp[rows, ]

library(cluster)
library(factoextra)

k1 <- kmeans(df_temp, centers = 2, nstart = 25, iter.max = 15)

df_temp$cluster <- k1$cluster

df_cluster <- k1$centers[1,]
df_cluster$cluster <- 1
df_cluster <- data.frame(df_cluster)
row.names(df_cluster) <- "centroid1"

df_temp[length(df_temp$TEMP)+1,] <- df_cluster

df_cluster <- k1$centers[2,]
df_cluster$cluster <- 2
df_cluster <- data.frame(df_cluster)
row.names(df_cluster) <- "centroid2"

df_temp[length(df_temp$TEMP)+1,] <- df_cluster

df_temp_1 <- subset(df_temp, df_temp$cluster < 2)
df_temp_2 <- subset(df_temp, df_temp$cluster > 1)

# Euclidean distance
temp1.dist.eucl <-dist(df_temp_1,method ="euclidean")
temp2.dist.eucl <-dist(df_temp_2,method ="euclidean")

temp1.eucl <- data.frame(round(as.matrix(temp1.dist.eucl)[1:7231,7231],2))
temp2.eucl <- data.frame(round(as.matrix(temp2.dist.eucl)[1:800,800],2))
colnames(temp1.eucl) <- "error"
colnames(temp2.eucl) <- "error"

temp1.eucl$squared_error = temp1.eucl$error * temp1.eucl$error
temp2.eucl$squared_error = temp2.eucl$error * temp2.eucl$error

sse_1_120 <- sum(temp1.eucl$squared_error)
sse_2_120 <- sum(temp2.eucl$squared_error)

sse_total_120 = sse_1_120 + sse_2_120


#### Euclidean Metric SSE with Seed : Random i.e 1905 ###
df_temp <- df_2007_daily_avg
df_temp$STN_WBAN_DAYOFMONTH <- paste(df_temp$STN, "_", df_temp$WBAN, "_", df_temp$DAYOFMONTH, sep = "")
df_temp <- df_temp[-c(1,2,3)]
df_temp <- df_temp[c(5,1,2,3,4)]

rownames(df_temp) <- df_temp$STN_WBAN_DAYOFMONTH
df_temp <- df_temp[-c(1)]

set.seed(1905)
rows <- sample(nrow(df_temp))
df_temp <- df_temp[rows, ]

k1 <- kmeans(df_temp, centers = 2, nstart = 25, iter.max = 15)

df_temp$cluster <- k1$cluster

df_cluster <- k1$centers[1,]
df_cluster$cluster <- 1
df_cluster <- data.frame(df_cluster)
row.names(df_cluster) <- "centroid1"

df_temp[length(df_temp$TEMP)+1,] <- df_cluster

df_cluster <- k1$centers[2,]
df_cluster$cluster <- 2
df_cluster <- data.frame(df_cluster)
row.names(df_cluster) <- "centroid2"

df_temp[length(df_temp$TEMP)+1,] <- df_cluster

df_temp_1 <- subset(df_temp, df_temp$cluster < 2)
df_temp_2 <- subset(df_temp, df_temp$cluster > 1)

# Euclidean distance
temp1.dist.eucl <-dist(df_temp_1,method ="euclidean")
temp2.dist.eucl <-dist(df_temp_2,method ="euclidean")

temp1.eucl <- data.frame(round(as.matrix(temp1.dist.eucl)[1:7231,7231],2))
temp2.eucl <- data.frame(round(as.matrix(temp2.dist.eucl)[1:800,800],2))
colnames(temp1.eucl) <- "error"
colnames(temp2.eucl) <- "error"

temp1.eucl$squared_error = temp1.eucl$error * temp1.eucl$error
temp2.eucl$squared_error = temp2.eucl$error * temp2.eucl$error

sse_1_1905 <- sum(temp1.eucl$squared_error)
sse_2_1905 <- sum(temp2.eucl$squared_error)

sse_total_1905 = sse_1_1905 + sse_2_1905