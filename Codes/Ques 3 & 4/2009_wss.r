#Clear Memory
rm(list=ls())

#Loading 2007 Data
df_2009_daily_avg <- read.csv("2009_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")

#### Clustering with Seed : 120 ###
df_temp <- df_2009_daily_avg
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
library(amap)

wss_eucl = kmeans(df_temp, centers=2)$tot.withinss
for (i in 1:8){
  wss_eucl[i] = kmeans(df_temp, centers=i)$tot.withinss
}

library(ggvis)
sse = data.frame(c(1:8), c(wss_eucl))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'
sse %>%
  ggvis(~Clusters, ~SSE) %>%
  layer_points(fill := 'blue') %>% 
  layer_lines() %>%
  set_options(height = 300, width = 400)

wss_pear = sum(Kmeans(df_temp, centers=i, method="pearson", iter.max = 100)$withinss)
for (i in 1:8){
  wss_pear[i] = sum(Kmeans(df_temp, centers=i, method="pearson", iter.max = 100)$withinss)
}

library(ggvis)
sse = data.frame(c(1:8), c(wss_pear))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'
sse %>%
  ggvis(~Clusters, ~SSE) %>%
  layer_points(fill := 'blue') %>% 
  layer_lines() %>%
  set_options(height = 300, width = 400)





#### Clustering with Seed : 1905 ###
df_temp1 <- df_2009_daily_avg
df_temp1$STN_WBAN_DAYOFMONTH <- paste(df_temp1$STN, "_", df_temp1$WBAN, "_", df_temp1$DAYOFMONTH, sep = "")
df_temp1 <- df_temp1[-c(1,2,3)]
df_temp1 <- df_temp1[c(5,1,2,3,4)]

rownames(df_temp1) <- df_temp1$STN_WBAN_DAYOFMONTH
df_temp1 <- df_temp1[-c(1)]

set.seed(1905)
rows <- sample(nrow(df_temp1))
df_temp1 <- df_temp1[rows, ]

wss_eucl = kmeans(df_temp1, centers=2)$tot.withinss
for (i in 1:8){
  wss_eucl[i] = kmeans(df_temp1, centers=i)$tot.withinss
}

library(ggvis)
sse = data.frame(c(1:8), c(wss_eucl))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'
sse %>%
  ggvis(~Clusters, ~SSE) %>%
  layer_points(fill := 'blue') %>% 
  layer_lines() %>%
  set_options(height = 300, width = 400)

wss_pear = sum(Kmeans(df_temp1, centers=i, method="pearson", iter.max = 100)$withinss)
for (i in 1:8){
  wss_pear[i] = sum(Kmeans(df_temp1, centers=i, method="pearson", iter.max = 100)$withinss)
}

library(ggvis)
sse = data.frame(c(1:8), c(wss_pear))
names(sse)[1] = 'Clusters'
names(sse)[2] = 'SSE'
sse %>%
  ggvis(~Clusters, ~SSE) %>%
  layer_points(fill := 'blue') %>% 
  layer_lines() %>%
  set_options(height = 300, width = 400)
