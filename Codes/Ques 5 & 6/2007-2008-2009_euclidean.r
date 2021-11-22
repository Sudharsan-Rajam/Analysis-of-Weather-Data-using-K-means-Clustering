#Clear Memory
rm(list=ls())

#Loading Data
df_2007_daily_avg <- read.csv("2007_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")
df_2008_daily_avg <- read.csv("2008_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")
df_2009_daily_avg <- read.csv("2009_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")


# 2007-2008
df_temp <- df_2007_daily_avg
df_temp$STN_WBAN_DAYOFMONTH <- paste(df_temp$STN, "_", df_temp$WBAN, "_", df_temp$DAYOFMONTH, sep = "")
df_temp <- df_temp[-c(1,2,3)]
df_temp <- df_temp[c(5,1,2,3,4)]

rownames(df_temp) <- df_temp$STN_WBAN_DAYOFMONTH
df_temp <- df_temp[-c(1)]

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



## 2008-2009

#### Clustering with Seed : 120 ###
df_temp4 <- df_2008_daily_avg
df_temp4$STN_WBAN_DAYOFMONTH <- paste(df_temp4$STN, "_", df_temp4$WBAN, "_", df_temp4$DAYOFMONTH, sep = "")
df_temp4 <- df_temp4[-c(1,2,3)]
df_temp4 <- df_temp4[c(5,1,2,3,4)]

rownames(df_temp4) <- df_temp4$STN_WBAN_DAYOFMONTH
df_temp4 <- df_temp4[-c(1)]

#### Clustering with Seed : 120 ###
df_temp5 <- df_2009_daily_avg
df_temp5$STN_WBAN_DAYOFMONTH <- paste(df_temp5$STN, "_", df_temp5$WBAN, "_", df_temp5$DAYOFMONTH, sep = "")
df_temp5 <- df_temp5[-c(1,2,3)]
df_temp5 <- df_temp5[c(5,1,2,3,4)]

rownames(df_temp5) <- df_temp5$STN_WBAN_DAYOFMONTH
df_temp5 <- df_temp5[-c(1)]

row2 <- c()
c=1
list2 <- as.list(row.names(df_temp5))
for (row1 in as.list(row.names(df_temp4))) {
  if (row1 %in% list2) {
  }
  else{
    row2 <- append(row2,row1)
  }
}
df_sim <- data.frame(STN=row2,TEMP=0,DEWP=0,STP=0,WDSP=0)
rownames(df_sim) <- df_sim$STN
df_sim <- df_sim[-c(1)]

df_temp6 <- rbind(df_temp5,df_sim)

row2 <- c()
c=1
list2 <- as.list(row.names(df_temp4))
for (row1 in as.list(row.names(df_temp5))) {
  if (row1 %in% list2) {
  }
  else{
    row2 <- append(row2,row1)
  }
}
df_sim <- data.frame(STN=row2,TEMP=0,DEWP=0,STP=0,WDSP=0)
rownames(df_sim) <- df_sim$STN
df_sim <- df_sim[-c(1)]

df_temp7 <- rbind(df_temp4,df_sim)

## For Euclidean

library(amap)

ky3 <- Kmeans(df_temp6, centers=2, method="euclidean", iter.max = 100)$cluster
ky4 <- Kmeans(df_temp7, centers=2, method="euclidean", iter.max = 100)$cluster


## 2007-2009

#### Clustering with Seed : 120 ###
df_temp8 <- df_2007_daily_avg
df_temp8$STN_WBAN_DAYOFMONTH <- paste(df_temp8$STN, "_", df_temp8$WBAN, "_", df_temp8$DAYOFMONTH, sep = "")
df_temp8 <- df_temp8[-c(1,2,3)]
df_temp8 <- df_temp8[c(5,1,2,3,4)]

rownames(df_temp8) <- df_temp8$STN_WBAN_DAYOFMONTH
df_temp8 <- df_temp8[-c(1)]

#### Clustering with Seed : 120 ###
df_temp9 <- df_2009_daily_avg
df_temp9$STN_WBAN_DAYOFMONTH <- paste(df_temp9$STN, "_", df_temp9$WBAN, "_", df_temp9$DAYOFMONTH, sep = "")
df_temp9 <- df_temp9[-c(1,2,3)]
df_temp9 <- df_temp9[c(5,1,2,3,4)]

rownames(df_temp9) <- df_temp9$STN_WBAN_DAYOFMONTH
df_temp9 <- df_temp9[-c(1)]

row2 <- c()
c=1
list2 <- as.list(row.names(df_temp9))
for (row1 in as.list(row.names(df_temp8))) {
  if (row1 %in% list2) {
  }
  else{
    row2 <- append(row2,row1)
  }
}
df_sim <- data.frame(STN=row2,TEMP=0,DEWP=0,STP=0,WDSP=0)
rownames(df_sim) <- df_sim$STN
df_sim <- df_sim[-c(1)]

df_temp10 <- rbind(df_temp9,df_sim)

row2 <- c()
c=1
list2 <- as.list(row.names(df_temp8))
for (row1 in as.list(row.names(df_temp9))) {
  if (row1 %in% list2) {
  }
  else{
    row2 <- append(row2,row1)
  }
}
df_sim <- data.frame(STN=row2,TEMP=0,DEWP=0,STP=0,WDSP=0)
rownames(df_sim) <- df_sim$STN
df_sim <- df_sim[-c(1)]

df_temp11 <- rbind(df_temp8,df_sim)

## For Euclidean

library(amap)

ky5 <- Kmeans(df_temp10, centers=2, method="euclidean", iter.max = 100)$cluster
ky6 <- Kmeans(df_temp11, centers=2, method="euclidean", iter.max = 100)$cluster



# Calculating Jaccard Coefficient
library(clusteval)
j1 <- cluster_similarity(ky2, ky1,similarity = c("jaccard"))
j2 <- cluster_similarity(ky4, ky3,similarity = c("jaccard"))
j3 <- cluster_similarity(ky6, ky5,similarity = c("jaccard"))

j_total <- (j1 + j2 + j3)/3

print(j_total)