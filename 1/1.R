# Чтение данных
airportData <- read.table("airportdat1.txt", header = FALSE, sep = "", fill = TRUE)

# Переименование переменных
colnames(airportData) <- c("airport",
                           "city",
                           "scheduledDepartures",
                           "performedDepartures", 
                           "enplanedPassengers",
                           "enplanedFreightTons",
                           "enplanedMailTons")

# Оставляем только числовые переменные
airportNumeric <- airportData[, c("scheduledDepartures",
                                  "performedDepartures", 
                                  "enplanedPassengers",
                                  "enplanedFreightTons",
                                  "enplanedMailTons")]

# Гистограммы
par(mfrow = c(2, 3))
for (col in colnames(airportNumeric)) {
  hist(airportNumeric[[col]], 
       main = paste(col),
       xlab = col, 
       col = "lightblue", 
       border = "black")
}

# Ящики с усами
par(mfrow = c(2, 3))
for (col in colnames(airportNumeric)) {
  boxplot(airportNumeric[[col]], 
          main = paste(col), 
          col = "lightgreen")
}

# Описательная статистика
summaryStats <- data.frame(
  mean = sapply(airportNumeric, mean, na.rm = TRUE),
  variance = sapply(airportNumeric, var, na.rm = TRUE),
  sd = sapply(airportNumeric, sd, na.rm = TRUE),
  median = sapply(airportNumeric, median, na.rm = TRUE),
  q1 = sapply(airportNumeric, function(x) quantile(x, 0.25, na.rm = TRUE)),
  q3 = sapply(airportNumeric, function(x) quantile(x, 0.75, na.rm = TRUE))
)
print(summaryStats)

# Корреляции
corMatrix <- cor(airportNumeric)
print(corMatrix)
