# Чтение данных
babyboomData <- read.table("babyboom.dat.txt", header = FALSE, sep = "", fill = TRUE)

# Переименование переменных
colnames(babyboomData) <- c("birthWeight", "minutesAfterMidnight")

# Оставляем только числовые переменные
babyboomNumeric <- babyboomData[, c("birthWeight", "minutesAfterMidnight")]

# Гистограммы и ящики с усами
numVars <- colnames(babyboomNumeric)
par(mfrow = c(length(numVars), 2), mar = c(4, 4, 3, 1))

for (col in numVars) {
  # Гистограмма
  hist(babyboomNumeric[[col]], 
       main = paste(col, "- Histogram"),
       xlab = col, 
       col = "lightblue", 
       border = "black")
  
  # Ящик с усами
  boxplot(babyboomNumeric[[col]], 
          main = paste(col, "- Boxplot"), 
          col = "lightgreen")
}

# Описательная статистика
summaryStats <- data.frame(
  mean = sapply(babyboomNumeric, mean, na.rm = TRUE),
  variance = sapply(babyboomNumeric, var, na.rm = TRUE),
  sd = sapply(babyboomNumeric, sd, na.rm = TRUE),
  median = sapply(babyboomNumeric, median, na.rm = TRUE),
  q1 = sapply(babyboomNumeric, function(x) quantile(x, 0.25, na.rm = TRUE)),
  q3 = sapply(babyboomNumeric, function(x) quantile(x, 0.75, na.rm = TRUE))
)
print(summaryStats)

# Попарные коэффициенты корреляции
corMatrix <- cor(babyboomNumeric)
print(corMatrix)
