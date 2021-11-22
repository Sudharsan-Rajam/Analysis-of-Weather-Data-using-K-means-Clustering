#Clear Memory
rm(list=ls())

#Loading 2007 Data
df_2007_daily_avg <- read.csv("2007_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")
df_2008_daily_avg <- read.csv("2008_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")
df_2009_daily_avg <- read.csv("2009_Daily_Average.csv", header = TRUE, sep = ",", dec = ".")

#Finding different Curves
library(factoextra)
library(NbClust)

#2007
df_temp <- df_2007_daily_avg
df_temp$STN_WBAN_DAYOFMONTH <- paste(df_temp$STN, "_", df_temp$WBAN, "_", df_temp$DAYOFMONTH, sep = "")
df_temp <- df_temp[-c(1,2,3)]
df_temp <- df_temp[c(5,1,2,3,4)]

rownames(df_temp) <- df_temp$STN_WBAN_DAYOFMONTH
df_temp <- df_temp[-c(1)]


fviz_nbclust(df_temp, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(df_temp, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(df_temp, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#2008
df_temp <- df_2008_daily_avg
df_temp$STN_WBAN_DAYOFMONTH <- paste(df_temp$STN, "_", df_temp$WBAN, "_", df_temp$DAYOFMONTH, sep = "")
df_temp <- df_temp[-c(1,2,3)]
df_temp <- df_temp[c(5,1,2,3,4)]

rownames(df_temp) <- df_temp$STN_WBAN_DAYOFMONTH
df_temp <- df_temp[-c(1)]

fviz_nbclust(df_temp, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(df_temp, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(df_temp, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#2009
df_temp <- df_2009_daily_avg
df_temp$STN_WBAN_DAYOFMONTH <- paste(df_temp$STN, "_", df_temp$WBAN, "_", df_temp$DAYOFMONTH, sep = "")
df_temp <- df_temp[-c(1,2,3)]
df_temp <- df_temp[c(5,1,2,3,4)]

rownames(df_temp) <- df_temp$STN_WBAN_DAYOFMONTH
df_temp <- df_temp[-c(1)]

fviz_nbclust(df_temp, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(df_temp, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(df_temp, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")