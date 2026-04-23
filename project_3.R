# install.packages("readxl")
# install.packages("TTR")
# install.packages("tseries")
# install.packages("forecast")

library(readxl)
library(TTR)
library(tseries)
library(forecast)



# Раздел 1: Загрузка и подготовка данных  ----


raw <- read_excel("singlefamily-housing-starts-and-_p-2.xls",
                  col_names = FALSE,
                  skip = 15)

# Переименовываем столбцы
colnames(raw) <- c("дата", "значение")

# Убираем возможные NA в конце
raw <- raw[!is.na(raw$значение), ]

# Создаём объект временного ряда: ежемесячные данные, начало — январь 1976
ts_data <- ts(as.numeric(raw$значение),
              start = c(1976, 1),
              frequency = 12)

# Вывод описательной статистики
cat("=== Описание данных ===\n")
cat("Число наблюдений:", length(ts_data), "\n")
cat("Период: с", format(as.Date(paste(start(ts_data), collapse = "-"), "%Y-%m"), "%B %Y"),
    "по", format(as.Date(paste(end(ts_data), collapse = "-"), "%Y-%m"), "%B %Y"), "\n")
cat("Структура ряда:\n")
print(str(ts_data))
cat("Мин / Макс / Среднее:", round(min(ts_data), 2),
    "/", round(max(ts_data), 2),
    "/", round(mean(ts_data), 2), "\n")



# Раздел 2: График с 12-периодным скользящим средним  ----


# Вычисляем 12-периодное скользящее среднее
ma12 <- SMA(ts_data, n = 12)

# Строим график
plot(ts_data,
     main = "Ввод индивидуального жилья в США\n(с 12-периодным скользящим средним)",
     xlab = "Год",
     ylab = "Тыс. единиц",
     col  = "steelblue",
     lwd  = 1.5,
     ylim = c(min(ts_data, na.rm = TRUE) * 0.9,
              max(ts_data, na.rm = TRUE) * 1.05))

lines(ma12,
      col = "firebrick",
      lwd = 2.5)

legend("topright",
       legend = c("Исходный ряд", "Скользящее среднее (12)"),
       col    = c("steelblue", "firebrick"),
       lwd    = c(1.5, 2.5),
       bty    = "n")

grid(col = "grey85", lty = "dotted")



# Раздел 3: Декомпозиция временного ряда  ----


# Аддитивная декомпозиция: тренд + сезонность + остатки
decomp <- decompose(ts_data, type = "additive")

plot(decomp,
     xlab = "Год")

# Подпись общего заголовка поверх стандартного вывода



# Раздел 4: Тест Дики-Фуллера на стационарность  ----


cat("\n=== Тест Дики-Фуллера (ADF) ===\n")

adf_result <- adf.test(ts_data)

print(adf_result)

cat("\nP-value:", round(adf_result$p.value, 4), "\n")

if (adf_result$p.value < 0.05) {
  cat("Вывод: ряд СТАЦИОНАРНЫЙ (p < 0.05, отвергаем H0 о единичном корне)\n")
} else {
  cat("Вывод: ряд НЕСТАЦИОНАРНЫЙ (p >= 0.05, не отвергаем H0 о единичном корне)\n")
}



# Раздел 5: Коррелограмма (ACF)  ----


acf(ts_data,
    lag.max = 30,
    main    = "Коррелограмма (ACF): ввод индивидуального жилья",
    xlab    = "Лаг (месяцы)",
    ylab    = "Автокорреляция",
    col     = "steelblue",
    lwd     = 2)

grid(col = "grey85", lty = "dotted")



# Раздел 6: Коэффициент автокорреляции первого порядка  ----


# Вычисляем вручную через cor()
y     <- as.numeric(ts_data)
n     <- length(y)
y_t   <- y[2:n]          # y_t  (t = 2 ... n)
y_lag <- y[1:(n - 1)]    # y_{t-1}

r1 <- cor(y_t, y_lag)
cat("\n=== Автокорреляция первого порядка ===\n")
cat("r1 =", round(r1, 4), "\n")

# Scatter plot: y_t ~ y_{t-1}
plot(y_lag, y_t,
     main = paste0("Диаграмма рассеяния: y_t ~ y_{t-1}\n(r1 = ", round(r1, 3), ")"),
     xlab = "Значение в момент t–1",
     ylab = "Значение в момент t",
     pch  = 19,
     col  = adjustcolor("steelblue", alpha.f = 0.6),
     cex  = 0.8)

# Линия регрессии
abline(lm(y_t ~ y_lag), col = "firebrick", lwd = 2)

grid(col = "grey85", lty = "dotted")

legend("topleft",
       legend = c(paste0("r1 = ", round(r1, 3)), "Линия регрессии"),
       col    = c("steelblue", "firebrick"),
       pch    = c(19, NA),
       lwd    = c(NA, 2),
       bty    = "n")



# Раздел 7: Тройное экспоненциальное сглаживание  ----


model_auto <- ets(ts_data)

model_AAA  <- ets(ts_data, model = "AAA")

model_MAM  <- ets(ts_data, model = "MAM")

cat("\n=== Сравнение моделей ETS по AIC ===\n")

aic_table <- data.frame(
  Модель = c(
    paste0("Авто (", model_auto$method, ")"),
    "AAA (Holt-Winters аддитивная)",
    "MAM (мультипл. сезонность)"
  ),
  AIC  = round(c(AIC(model_auto), AIC(model_AAA), AIC(model_MAM)), 2),
  AICc = round(c(model_auto$aicc, model_AAA$aicc, model_MAM$aicc), 2),
  BIC  = round(c(BIC(model_auto), BIC(model_AAA), BIC(model_MAM)), 2)
)

print(aic_table)

# Выбираем лучшую модель по AIC
best_aic   <- min(aic_table$AIC)
best_index <- which.min(aic_table$AIC)
best_model <- list(model_auto, model_AAA, model_MAM)[[best_index]]

cat("\nЛучшая модель:", aic_table$Модель[best_index],
    "  AIC =", best_aic, "\n\n")

cat("=== Summary лучшей модели ===\n")
print(summary(best_model))



# Раздел 8: Прогноз на 3 лага вперёд ----


# Прогноз на 3 месяца
fc <- forecast(best_model, h = 3)

cat("\n=== Прогноз на 3 месяца вперёд ===\n")

fc_table <- data.frame(
  Период        = as.character(time(fc$mean)),
  Прогноз       = round(as.numeric(fc$mean),    2),
  Нижний_80     = round(as.numeric(fc$lower[, 1]), 2),
  Верхний_80    = round(as.numeric(fc$upper[, 1]), 2),
  Нижний_95     = round(as.numeric(fc$lower[, 2]), 2),
  Верхний_95    = round(as.numeric(fc$upper[, 2]), 2)
)

print(fc_table)

# График ----
y_min <- min(ts_data, fc$lower, na.rm = TRUE) * 0.90
y_max <- max(ts_data, fc$upper, na.rm = TRUE) * 1.05

plot(ts_data,
     main  = paste0("Прогноз ввода жилья на 3 месяца:"),
     xlab  = "Год",
     ylab  = "Тыс. единиц",
     col   = "grey40",
     lwd   = 1.2,
     ylim  = c(y_min, y_max))

lines(fitted(best_model),
      col = "steelblue",
      lwd = 1.5,
      lty = 2)

# Доверительный интервал 95%
fc_time <- as.numeric(time(fc$mean))
ts_time <- c(as.numeric(time(ts_data)[length(ts_data)]),
             fc_time)                              # начинаем с последней точки ряда

lower95 <- c(as.numeric(ts_data)[length(ts_data)], as.numeric(fc$lower[, 2]))
upper95 <- c(as.numeric(ts_data)[length(ts_data)], as.numeric(fc$upper[, 2]))
lower80 <- c(as.numeric(ts_data)[length(ts_data)], as.numeric(fc$lower[, 1]))
upper80 <- c(as.numeric(ts_data)[length(ts_data)], as.numeric(fc$upper[, 1]))

polygon(c(ts_time, rev(ts_time)),
        c(lower95, rev(upper95)),
        col    = adjustcolor("skyblue", alpha.f = 0.25),
        border = NA)

polygon(c(ts_time, rev(ts_time)),
        c(lower80, rev(upper80)),
        col    = adjustcolor("skyblue", alpha.f = 0.45),
        border = NA)

lines(c(as.numeric(time(ts_data))[length(ts_data)],
        fc_time),
      c(as.numeric(ts_data)[length(ts_data)],
        as.numeric(fc$mean)),
      col = "firebrick",
      lwd = 2.5)

points(fc_time,
       as.numeric(fc$mean),
       pch = 19,
       col = "firebrick",
       cex = 1.4)

legend("topright",
       legend = c("Исходный ряд",
                  "Сглаженные значения",
                  "Прогноз (3 мес.)",
                  "Довер. интервал 80%",
                  "Довер. интервал 95%"),
       col    = c("grey40", "steelblue", "firebrick",
                  adjustcolor("skyblue", 0.55),
                  adjustcolor("skyblue", 0.30)),
       lwd    = c(1.2, 1.5, 2.5, 8, 8),
       lty    = c(1, 2, 1, 1, 1),
       pch    = c(NA, NA, 19, NA, NA),
       bty    = "n",
       cex    = 0.85)

grid(col = "grey85", lty = "dotted")