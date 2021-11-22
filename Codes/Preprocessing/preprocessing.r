#Clear Memory
rm(list=ls())

#Loading 2007 Data
df_2007 <- read.csv("hourly_2007.g", header = TRUE, sep = ",", dec = ".")
df_2007 <- subset(df_2007, grepl(glob2rx("200712*"), YEARMODA))

#Loading 2008 Data
df_2008 <- read.csv("hourly_2008.g", header = TRUE, sep = ",", dec = ".")
df_2008 <- subset(df_2008, grepl(glob2rx("200812*"), YEARMODA))

#Loading 2009 Data
df_2009 <- read.csv("hourly_2009.g", header = TRUE, sep = ",", dec = ".")
df_2009 <- subset(df_2009, grepl(glob2rx("200912*"), YEARMODA))

#Selecting Desired Columns from the three dataframes
df_2007_cal <- subset(df_2007, select = c(STN,WBAN,YEARMODA,TEMP,DEWP,STP,WDSP))
df_2008_cal <- subset(df_2008, select = c(STN,WBAN,YEARMODA,TEMP,DEWP,STP,WDSP))
df_2009_cal <- subset(df_2009, select = c(STN,WBAN,YEARMODA,TEMP,DEWP,STP,WDSP))

days = list("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

### Replacing Outliers with Mean###
df_2007_cal$TEMP[df_2007_cal$TEMP > 120] = NA
df_2008_cal$TEMP[df_2008_cal$TEMP > 120] = NA
df_2009_cal$TEMP[df_2009_cal$TEMP > 120] = NA

for(i in 1:ncol(df_2007_cal)){
  df_2007_cal[is.na(df_2007_cal[,i]), i] <- mean(df_2007_cal[,i], na.rm = TRUE)
}

for(i in 1:ncol(df_2008_cal)){
  df_2008_cal[is.na(df_2008_cal[,i]), i] <- mean(df_2008_cal[,i], na.rm = TRUE)
}

for(i in 1:ncol(df_2009_cal)){
  df_2009_cal[is.na(df_2009_cal[,i]), i] <- mean(df_2009_cal[,i], na.rm = TRUE)
}

##### Daily Average #####

## 2007 ##
stn_list <- unique(df_2007_cal$STN)
df_2007_temp <- data.frame(STN=numeric(),WBAN=numeric(),DAYOFMONTH=numeric(),TEMP=numeric(),DEWP=numeric(),STP=numeric(),WDSP=numeric())
df_2007_daily_avg <- data.frame(STN=numeric(),WBAN=numeric(),DAYOFMONTH=numeric(),TEMP=numeric(),DEWP=numeric(),STP=numeric(),WDSP=numeric())

count <- length(stn_list)
print(count)
for(stn in stn_list) {
  df_temp <- subset(df_2007_cal, grepl(glob2rx(stn), STN))
  wban_list <- unique(df_temp$WBAN)
  print(stn)
  print(length(wban_list))
  for(wban in wban_list)  {
    m <- 1
    for(values in days)  {
      date_string <- paste("200712", values, "*", sep="")
      df_temp <- subset(df_2007_cal, grepl(glob2rx(stn), STN))
      df_temp <- subset(df_temp, grepl(glob2rx(wban), WBAN))
      df_temp <- subset(df_temp, grepl(glob2rx(date_string), YEARMODA))
      
      df_2007_temp[m, ] <- c(stn, wban, m, mean(df_temp$TEMP[df_temp$TEMP < 999.9]), mean(df_temp$DEWP[df_temp$DEWP < 9999.9]), mean(df_temp$STP[df_temp$STP < 9999.9]), mean(df_temp$WDSP[df_temp$WDSP < 999.9]))
      m <- m + 1
    }
    df_2007_daily_avg <- rbind(df_2007_daily_avg, df_2007_temp)
  }
  print("Done!")
  count <- count - 1
  print(count)
}

## 2008 ##
stn_list <- unique(df_2008_cal$STN)
df_2008_temp <- data.frame(STN=numeric(),WBAN=numeric(),DAYOFMONTH=numeric(),TEMP=numeric(),DEWP=numeric(),STP=numeric(),WDSP=numeric())
df_2008_daily_avg <- data.frame(STN=numeric(),WBAN=numeric(),DAYOFMONTH=numeric(),TEMP=numeric(),DEWP=numeric(),STP=numeric(),WDSP=numeric())

count <- length(stn_list)
print(count)
for(stn in stn_list) {
  df_temp <- subset(df_2008_cal, grepl(glob2rx(stn), STN))
  wban_list <- unique(df_temp$WBAN)
  print(stn)
  print(length(wban_list))
  for(wban in wban_list)  {
    m <- 1
    for(values in days)  {
      date_string <- paste("200812", values, "*", sep="")
      df_temp <- subset(df_2008_cal, grepl(glob2rx(stn), STN))
      df_temp <- subset(df_temp, grepl(glob2rx(wban), WBAN))
      df_temp <- subset(df_temp, grepl(glob2rx(date_string), YEARMODA))
      
      df_2008_temp[m, ] <- c(stn, wban, m, mean(df_temp$TEMP[df_temp$TEMP < 999.9]), mean(df_temp$DEWP[df_temp$DEWP < 9999.9]), mean(df_temp$STP[df_temp$STP < 9999.9]), mean(df_temp$WDSP[df_temp$WDSP < 999.9]))
      m <- m + 1
    }
    df_2008_daily_avg <- rbind(df_2008_daily_avg, df_2008_temp)
  }
  print("Done!")
  count <- count - 1
  print(count)
}

## 2009 ##
stn_list <- unique(df_2009_cal$STN)
df_2009_temp <- data.frame(STN=numeric(),WBAN=numeric(),DAYOFMONTH=numeric(),TEMP=numeric(),DEWP=numeric(),STP=numeric(),WDSP=numeric())
df_2009_daily_avg <- data.frame(STN=numeric(),WBAN=numeric(),DAYOFMONTH=numeric(),TEMP=numeric(),DEWP=numeric(),STP=numeric(),WDSP=numeric())

count <- length(stn_list)
print(count)
for(stn in stn_list) {
  df_temp <- subset(df_2009_cal, grepl(glob2rx(stn), STN))
  wban_list <- unique(df_temp$WBAN)
  print(stn)
  print(length(wban_list))
  for(wban in wban_list)  {
    m <- 1
    for(values in days)  {
      date_string <- paste("200912", values, "*", sep="")
      df_temp <- subset(df_2009_cal, grepl(glob2rx(stn), STN))
      df_temp <- subset(df_temp, grepl(glob2rx(wban), WBAN))
      df_temp <- subset(df_temp, grepl(glob2rx(date_string), YEARMODA))
      
      df_2009_temp[m, ] <- c(stn, wban, m, mean(df_temp$TEMP[df_temp$TEMP < 999.9]), mean(df_temp$DEWP[df_temp$DEWP < 9999.9]), mean(df_temp$STP[df_temp$STP < 9999.9]), mean(df_temp$WDSP[df_temp$WDSP < 999.9]))
      m <- m + 1
    }
    df_2009_daily_avg <- rbind(df_2009_daily_avg, df_2009_temp)
  }
  print("Done!")
  count <- count - 1
  print(count)
}

### Writing Files to CSV ###

df_temp <- df_2007_daily_avg
for(i in 1:ncol(df_temp)){
  df_temp[is.na(df_temp[,i]), i] <- mean(df_temp[,i], na.rm = TRUE)
}

write.csv(df_temp,"2007_Daily_Average.csv", row.names = FALSE)

df_temp <- df_2008_daily_avg
for(i in 1:ncol(df_temp)){
  df_temp[is.na(df_temp[,i]), i] <- mean(df_temp[,i], na.rm = TRUE)
}

write.csv(df_temp,"2008_Daily_Average.csv", row.names = FALSE)

df_temp <- df_2009_daily_avg
for(i in 1:ncol(df_temp)){
  df_temp[is.na(df_temp[,i]), i] <- mean(df_temp[,i], na.rm = TRUE)
}

write.csv(df_temp,"2009_Daily_Average.csv", row.names = FALSE)


######  Monthly  Average #####

df_2007_month <- df_2007_daily_avg
df_2008_month <- df_2008_daily_avg
df_2009_month <- df_2009_daily_avg


## 2007 ##
df_2007_month_temp <- data.frame(STN=numeric(),WBAN=numeric(),AVG_TEMP=numeric(),AVG_DEWP=numeric(),AVG_STP=numeric(),AVG_WDSP=numeric())
df_2007_monthly_avg <- data.frame(STN=numeric(),WBAN=numeric(),AVG_TEMP=numeric(),AVG_DEWP=numeric(),AVG_STP=numeric(),AVG_WDSP=numeric())

for(i in 1:ncol(df_2007_month)){
  df_2007_month[is.na(df_2007_month[,i]), i] <- mean(df_2007_month[,i], na.rm = TRUE)
}

stn_list <- unique(df_2007_month$STN)
count <- length(stn_list)
print(count)
for(stn in stn_list) {
  df_temp <- subset(df_2007_month, grepl(glob2rx(stn), STN))
  wban_list <- unique(df_temp$WBAN)
  print(stn)
  print(length(wban_list))
  m <- 1
  for(wban in wban_list)  {
    df_temp <- subset(df_2007_month, grepl(glob2rx(stn), STN))
    df_temp <- subset(df_temp, grepl(glob2rx(wban), WBAN))
    
    df_2007_month_temp[m, ] <- c(stn, wban, mean(df_temp$TEMP[df_temp$TEMP < 9999.9]), mean(df_temp$DEWP[df_temp$DEWP < 9999.9]), mean(df_temp$STP[df_temp$STP < 9999.9]), mean(df_temp$WDSP[df_temp$WDSP < 999.9]))
    m <- m + 1
  }
  df_2007_monthly_avg <- rbind(df_2007_monthly_avg, df_2007_month_temp)
  print("Done!")
  count <- count - 1
  print(count)
}

df_temp <- df_2007_monthly_avg
write.csv(df_temp,"2007_Monthly_Average.csv", row.names = FALSE)


## 2008 ##
df_2008_month_temp <- data.frame(STN=numeric(),WBAN=numeric(),AVG_TEMP=numeric(),AVG_DEWP=numeric(),AVG_STP=numeric(),AVG_WDSP=numeric())
df_2008_monthly_avg <- data.frame(STN=numeric(),WBAN=numeric(),AVG_TEMP=numeric(),AVG_DEWP=numeric(),AVG_STP=numeric(),AVG_WDSP=numeric())

for(i in 1:ncol(df_2008_month)){
  df_2008_month[is.na(df_2008_month[,i]), i] <- mean(df_2008_month[,i], na.rm = TRUE)
}

stn_list <- unique(df_2008_month$STN)
count <- length(stn_list)
print(count)
for(stn in stn_list) {
  df_temp <- subset(df_2008_month, grepl(glob2rx(stn), STN))
  wban_list <- unique(df_temp$WBAN)
  print(stn)
  print(length(wban_list))
  m <- 1
  for(wban in wban_list)  {
    df_temp <- subset(df_2008_month, grepl(glob2rx(stn), STN))
    df_temp <- subset(df_temp, grepl(glob2rx(wban), WBAN))
    
    df_2008_month_temp[m, ] <- c(stn, wban, mean(df_temp$TEMP[df_temp$TEMP < 9999.9]), mean(df_temp$DEWP[df_temp$DEWP < 9999.9]), mean(df_temp$STP[df_temp$STP < 9999.9]), mean(df_temp$WDSP[df_temp$WDSP < 999.9]))
    m <- m + 1
  }
  df_2008_monthly_avg <- rbind(df_2008_monthly_avg, df_2008_month_temp)
  print("Done!")
  count <- count - 1
  print(count)
}

df_temp <- df_2008_monthly_avg
write.csv(df_temp,"2008_Monthly_Average.csv", row.names = FALSE)


## 2009 ##
df_2009_month_temp <- data.frame(STN=numeric(),WBAN=numeric(),AVG_TEMP=numeric(),AVG_DEWP=numeric(),AVG_STP=numeric(),AVG_WDSP=numeric())
df_2009_monthly_avg <- data.frame(STN=numeric(),WBAN=numeric(),AVG_TEMP=numeric(),AVG_DEWP=numeric(),AVG_STP=numeric(),AVG_WDSP=numeric())

for(i in 1:ncol(df_2009_month)){
  df_2009_month[is.na(df_2009_month[,i]), i] <- mean(df_2009_month[,i], na.rm = TRUE)
}

stn_list <- unique(df_2009_month$STN)
count <- length(stn_list)
print(count)
for(stn in stn_list) {
  df_temp <- subset(df_2009_month, grepl(glob2rx(stn), STN))
  wban_list <- unique(df_temp$WBAN)
  print(stn)
  print(length(wban_list))
  m <- 1
  for(wban in wban_list)  {
    df_temp <- subset(df_2009_month, grepl(glob2rx(stn), STN))
    df_temp <- subset(df_temp, grepl(glob2rx(wban), WBAN))
    
    df_2009_month_temp[m, ] <- c(stn, wban, mean(df_temp$TEMP[df_temp$TEMP < 9999.9]), mean(df_temp$DEWP[df_temp$DEWP < 9999.9]), mean(df_temp$STP[df_temp$STP < 9999.9]), mean(df_temp$WDSP[df_temp$WDSP < 999.9]))
    m <- m + 1
  }
  df_2009_monthly_avg <- rbind(df_2009_monthly_avg, df_2009_month_temp)
  print("Done!")
  count <- count - 1
  print(count)
}

df_temp <- df_2009_monthly_avg
write.csv(df_temp,"2009_Monthly_Average.csv", row.names = FALSE)


##### End of Phase-1 : Preprocessing #####