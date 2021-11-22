#Clear Memory
rm(list=ls())

#Loading Data
df_2007_monthly_avg <- read.csv("2007_Monthly_Average.csv", header = TRUE, sep = ",", dec = ".")
df_2007_month <- df_2007_monthly_avg
df_2007_month_temp <- data.frame(STN=numeric(),AVG_TEMP=numeric(),AVG_DEWP=numeric(),AVG_STP=numeric(),AVG_WDSP=numeric())

stn_list <- unique(df_2007_month$STN)
count <- length(stn_list)
print(count)
m <- 1
for(stn in stn_list) {
  df_temp <- subset(df_2007_month, grepl(glob2rx(stn), STN))
  wban_list <- unique(df_temp$WBAN)
  print(stn)
  df_2007_month_temp[m, ] <- c(stn, mean(df_temp$AVG_TEMP), mean(df_temp$AVG_DEWP), mean(df_temp$AVG_STP), mean(df_temp$AVG_WDSP))
  m <- m + 1
  print("Done!")
  count <- count - 1
  print(count)
}
df_temp <- df_2007_month_temp
rownames(df_temp) <- df_temp$STN
df_temp <- df_temp[-c(1)]

library(amap)
library(factoextra)

ky1 <- Kmeans(df_temp, centers=2, method="euclidean", iter.max = 100)

df_temp$cluster <- ky1$cluster
df_station <- read.csv("stations.csv", header = TRUE, sep = ",", dec = ".")

df_station <- subset(df_station, select = c(StationNumber,Lon,Lat))

df_2007_station <- data.frame(STN=numeric(),Lon=numeric(),Lat=numeric())

stn_list <- unique(df_station$StationNumber)
count <- length(stn_list)
print(count)
m <- 1
for(stn in stn_list) {
  df_temp1 <- subset(df_station, grepl(glob2rx(stn), StationNumber))
  print(stn)
  df_2007_station[m, ] <- c(stn, mean(df_temp1$Lon), mean(df_temp1$Lat))
  m <- m + 1
  print("Done!")
  count <- count - 1
  print(count)
}

rownames(df_2007_station) <- df_2007_station$STN
df_2007_station <- df_2007_station[-c(1)]

df_2007 <- data.frame(STN=numeric(),cluster=numeric(),Lon=numeric(),Lat=numeric())

c <- 1
row2 <- c()
list1 <- as.list(row.names(df_2007_station))
for (row1 in as.list(row.names(df_temp))) {
  if (row1 %in% list1) {
    row2 <- append(row2,row1)
    df_2007[c,] <- cbind("",df_temp[row1,]$cluster,df_2007_station[row1,]$Lon,df_2007_station[row1,]$Lat)
    c <- c +1
  }
}
df_2007$STN <- row2

df_2007$STN <- as.numeric(df_2007$STN)
df_2007$cluster <- as.numeric(df_2007$cluster)
df_2007$Lon <- as.numeric(df_2007$Lon)
df_2007$Lat <- as.numeric(df_2007$Lat)

library(ggmap)

qmplot(Lon, Lat, data = df_2007, maptype = "toner-lite", color = as.factor(cluster))