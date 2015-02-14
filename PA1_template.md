---
title: "Peer Assessment 1"
output: html_document
---

Data
---------------------


In this paper we used monitoring activities data set provided by the instructor. Data and description of them are available in https://github.com/rdpeng/RepData_PeerAssessment1.

The first step is to load the data into r and verify its structure.


```r
setwd("//192.168.1.27/Alberto/Cursos/Reproducible Research/Proyect 1")  #cambio de wd

base <- read.csv("activity.csv", na.strings = "NA")  #cargar la base
str(base)  #Check estructura de la base
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


Changes to the structure of the base was made, a variable is added and a backup of it was as shown below:


```r
base$date1 <- strptime(base$date, format = "%Y-%m-%d")  ### Cambio a formato fecha
base$steps <- as.numeric(base$steps)  ### Cambio a formato fecha
base1 <- base  # respaldo de la base
```


What is mean total number of steps taken per day?
---------------------

For the first part of the project missing values were removed. The histogram, the mean and median are presented below.


```r
base <- na.omit(base)  # Quitando valores perdidos
numstepsday <- tapply(base$steps, base$date, sum)  # Calculando numero de pasos por dia
png(filename = "hist1.png")
hist(numstepsday, col = "cornflowerblue", main = "Numero de pasos por dia", 
    xlab = "Cantidad de pasos")  # Histograma
dev.off()
```

```
## pdf 
##   2
```

```r
mean(numstepsday, na.rm = T)  #Calculo de la media
```

```
## [1] 10766
```

```r
median(numstepsday, na.rm = T)  #Calculo de la mediana
```

```
## [1] 10765
```

```r
summary(numstepsday)[3:4]  #Calculo de la media y la mediana
```

```
## Median   Mean 
##  10800  10800
```


Note that the calculation of the mean and median changes when using the functions mean and median regarding the summary function because the latter includes missing values, and the first bypasses.

What is the average daily activity pattern?
---------------------

Ranges between 50 and 250 is increased when activity is reaching its highest point around the interval 835 (corresponding to line 104) with an average of about 206.2 steps. In this part of the task is done using code.


```r
numstepsint <- tapply(base$steps, base$interval, mean)  # Calculando promedio de pasos en cada intervalo de tiempo por dia
png(filename = "plot1.png")
plot(numstepsint, type = "l", col = "dodgerblue2", xlab = "Intervalo", ylab = "Pasos", 
    main = "Numero de pasos promedio\n durante el dia")
dev.off()
```

```
## pdf 
##   2
```

```r
names(which.max(numstepsint))
```

```
## [1] "835"
```

```r
max(numstepsint)
```

```
## [1] 206.2
```


Imputing missing values
---------------------

The number of missing values in the data set is 2304. The imputation of missing values was performed by imputation by the mean of each interval. The database structure was modified in the interval variable which was copy of the database backup.


```r
base2 <- base1  # Base para imputar
summary(base2$steps)[7]  #Numero de NA's
```

```
## NA's 
## 2304
```

```r
system.time(for (i in 1:nrow(base2)) {
    for (j in 1:nrow(numstepsint)) {
        if (base2[i, 3] == rownames(numstepsint)[j] & is.na(base2[i, 1])) {
            base2[i, 1] <- numstepsint[j]
        }
    }
})
```

```
##    user  system elapsed 
##  386.15    0.03  387.17
```

```r
base2$interval <- base1$interval
```


With this new basis the number of steps per day was calculated and the histogram as the first part of the task graph. The mean and median were calculated again. No changes were observed except in the middle but is only one unit. Thus concludes that the imputation method selected there is no difference in average daily steps.



```r
numstepsdayimp <- tapply(base2$steps, base2$date, sum)  # Calculando numero de pasos por dia
png(filename = "hist2.png")
hist(numstepsdayimp, col = "cornflowerblue", main = "Numero de pasos por dia", 
    xlab = "Cantidad de pasos")  # Histograma
dev.off()
```

```
## pdf 
##   2
```

```r
mean(numstepsdayimp)  #Calculo de la media
```

```
## [1] 10766
```

```r
median(numstepsdayimp)  #Calculo de la mediana
```

```
## [1] 10766
```

```r
summary(numstepsdayimp)[3:4]  #Calculo de la media y la mediana
```

```
## Median   Mean 
##  10800  10800
```



Are there differences in activity patterns between weekdays and weekends?
---------------------

To answer this question, new variables were created: day and weekday, the first has the corresponding day of the week and the second weekday or weekend if. Also, the weekday variable became a factor. If there is a different pattern on a weekend that weekday. In the weekend greater activity is observed throughout the day. Two periods of maxima near 100 are observed period.



```r
base2$day <- weekdays(base2$date1, abbreviate = T)
system.time(for (i in 1:nrow(base2)) {
    if (base2[i, 5] == "dom" | base2[i, 5] == "sáb") {
        base2[i, 6] <- "Weekend"
    } else {
        base2[i, 6] <- "Weekday"
    }
})
```

```
##    user  system elapsed 
##  123.66    0.01  123.95
```

```r
names(base2)[6] <- "weekday"
base2[, 6] <- factor(base2[, 6])
numstepsweek <- tapply(base2$steps, list(base2$interval, base2$weekday), mean)  # Calculando numero de pasos por dia
png(filename = "plot2.png")
par(mfrow = c(2, 1))
plot(numstepsweek[, 1], type = "l", col = "dodgerblue2", xlab = "Intervalo", 
    ylab = "Pasos", main = "Numero de pasos promedio durante el dia\n\n         Weekday")
plot(numstepsweek[, 2], type = "l", col = "dodgerblue2", xlab = "Intervalo", 
    ylab = "Pasos", main = "Numero de pasos promedio durante el dia\n\n         Weekend")
dev.off()
```

```
## pdf 
##   2
```


