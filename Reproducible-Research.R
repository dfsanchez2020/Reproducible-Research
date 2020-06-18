---
  title: "Reproducible-Research"
author: "Diego Sanchez"
date: "18/6/2020"
output: html_document
---
  
  ## Cargando y pre procesando los datos
  if (!file.exists('activity.csv')) {
    unzip(zipfile = "activity.zip")
  }
activityData <- read.csv(file="activity.csv", header=TRUE)

## ¿Cuál es el número total medio de pasos dados por día?
# Calcule los pasos totales tomados por día
totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)

# Haga un histograma del número total de pasos realizados por día.
hist(totalSteps$steps,
     main = "Total Steps per Day",
     xlab = "Number of Steps")|
  
  # Calcule e informe la media y la mediana de los pasos totales realizados por día
  meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
medSteps <- median(totalSteps$steps, na.rm = TRUE)

Mean Number of Steps Taken per Day = 1.076618910^{4}
Median Number of Steps Taken per Day = 10765

## ¿Cuál es el patrón de actividad diaria promedio?
# Haga un diagrama de series de tiempo del intervalo de 5 minutos y el número promedio de
# pasos tomados, promedios durante todos los días.
library(ggplot2)
meanStepsByInt <- aggregate(steps ~ interval, activityData, mean)
ggplot(data = meanStepsByInt, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))

# ¿Qué intervalo de 5 minutos en todos los días contiene el número máximo de pasos?
maxInt <- meanStepsByInt[which.max(meanStepsByInt$steps),]

## Imputando valores perdidos
# Calcular e informar el número total de valores faltantes en el conjunto de datos
missingVals <- is.na(activityData$steps)

# Diseñe una estrategia para completar todos los valores faltantes
Hay 17568 valores faltantes. Reemplazaré estos valores faltantes con el promedio de 5 días de ese intervalo respectivo.

# Cree un nuevo conjunto de datos que sea igual al conjunto de datos original pero con
# Completando datos faltantes
imp_activityData <- transform(activityData,
                              steps = ifelse(is.na(activityData$steps),
                                             meanStepsByInt$steps[match(activityData$interval, 
                                                                        meanStepsByInt$interval)],
                                             activityData$steps))

# Haga un histograma del número total de pasos tomados cada día e informe la media y la mediana.
impStepsByInt <- aggregate(steps ~ date, imp_activityData, FUN=sum)
hist(impStepsByInt$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")

impMeanSteps <- mean(impStepsByInt$steps, na.rm = TRUE)
impMedSteps <- median(impStepsByInt$steps, na.rm = TRUE)
diffMean = impMeanSteps - meanSteps
diffMed = impMedSteps - medSteps
diffTotal = sum(impStepsByInt$steps) - sum(totalSteps$steps)

There is a difference of 0 in the mean steps of the two dataset. There is a difference of -1.076381110^{4} in the median steps of the two dataset. There is a difference of 8.612950910^{4} in the total steps of the two dataset.

## ¿Hay diferencias en los patrones de actividad entre semana y fines de semana?
# Cree una nueva variable de factor en el conjunto de datos con dos niveles: "fin de semana" y "día de la semana"
DayType <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
    return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
    return ("weekend")
  else
    stop ("Invalid Date Format.")
}
imp_activityData$date <- as.Date(imp_activityData$date)
imp_activityData$day <- sapply(imp_activityData$date, FUN = DayType)

# Hacer un diagrama de panel que contenga un diagrama de serie temporal del intervalo de 5 minutos
# y el número promedio de pasos dados durante todos los días de la semana o fines de semana
meanStepsByDay <- aggregate(steps ~ interval + day, imp_activityData, mean)
ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + 
  geom_line() +
  facet_grid(day ~ .) +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))