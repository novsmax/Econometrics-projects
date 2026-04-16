# Пакеты ----
if (!require("readxl")) install.packages("readxl") # if сделал чтобы не скачивать каждый раз данные
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("forecast")) install.packages("forecast")
if (!require("plm")) install.packages("plm")
if (!require("modelsummary")) install.packages("modelsummary")

library(readxl)
library(dplyr)
library(tidyr)
library(forecast)
library(plm)
library(modelsummary)
library(lmtest)
library(sandwich)

# Загрузка данных ----
invest_data_df <- read_excel("Инвестиции в жилища.xlsx", sheet = 1, skip = 1)
salary_data_df <- read_excel("Среднедушевые денежные доходы.xlsx", sheet = 1, skip = 1)

invest_data_df <- invest_data_df[-1, ]
salary_data_df <- salary_data_df[-1, ]

colnames(invest_data_df)[1] <- "Год"
colnames(salary_data_df)[1] <- "Год"

# Общиее колонки и фильтрация ----
common_cols <- intersect(colnames(salary_data_df), colnames(invest_data_df))
salary_data_df <- salary_data_df[, colnames(salary_data_df) %in% common_cols]
invest_data_df <- invest_data_df[, colnames(invest_data_df) %in% common_cols]

# Длинный формат ----
# без него нельзя было бы склеить доходы и инвестиции + потом проще с фильтрацией + для регрессии и графиков
invest_long <- pivot_longer(invest_data_df, cols = -Год, names_to = "Регион", values_to = "invest", values_transform = list(invest = as.numeric))
salary_long <- pivot_longer(salary_data_df, cols = -Год, names_to = "Регион", values_to = "salary")

# Итоговые данные ----
joined_df <- left_join(salary_long, invest_long, by = c("Год", "Регион"))
final_df  <- filter(joined_df, salary > 0 & invest > 0)

aggregates <- c("Российская Федерация",
                "Центральный федер",
                "Северо-Западный ф",
                "Южный федеральный",
                "Приволжский федер",
                "Уральский федерал",
                "Сибирский федерал",
                "Дальневосточный ф",
                "Северо-Кавказский")

final_df <- filter(final_df, !grepl(paste(aggregates, collapse = "|"), Регион))

write.csv(final_df, "final_data.csv", row.names = FALSE, fileEncoding = "UTF-8")

print(paste("Число наблюдений:", nrow(final_df)))
print(paste("Уникальные регионы:", n_distinct(final_df$Регион)))
print(paste("Уникальные года:", n_distinct(final_df$Год)))

# Проверка линейной связи ----
summary(final_df[, c("salary", "invest")])
round(cor(final_df$salary, final_df$invest), 4)
cor.test(final_df$salary, final_df$invest)

# Корреляционное поле в линейных координатах ----
plot(final_df$salary, final_df$invest,
     xlab = "Среднедушевые доходы (руб.)",
     ylab = "Инвестиции в жилища (млн руб.)",
     main = "Кореляционное поле",
     pch = 16, col = "steelblue")

# Корреляционное поле в log координатах ----
plot(log(final_df$salary), log(final_df$invest),
     xlab = "ln(Среднедушевые доходы)",
     ylab = "ln(Инвестиции в жилища)",
     main = "Корреляционное поле (логарифмы)",
     pch = 16, col = "steelblue")

final_df <- filter(final_df, log(final_df$invest) >= 0)

final_df$log_salary <- log(final_df$salary)
final_df$log_invest <- log(final_df$invest)

# 4 комбинации линейной и логарифмической зависимости ----
m_lin_lin <- lm(invest ~ salary,                         data = final_df)
m_log_log <- lm(log(final_df$invest) ~ log(final_df$salary), data = final_df)
m_log_lin <- lm(log(final_df$invest) ~ salary,           data = final_df)
m_lin_log <- lm(invest ~ log(final_df$salary),           data = final_df)

# Сравнение моделей ----
comparison <- data.frame(
  model  = c("lin_lin", "log_log", "log_lin", "lin_log"),
  R2     = c(summary(m_lin_lin)$r.squared,
             summary(m_log_log)$r.squared,
             summary(m_log_lin)$r.squared,
             summary(m_lin_log)$r.squared),
  Adj_R2 = c(summary(m_lin_lin)$adj.r.squared,
             summary(m_log_log)$adj.r.squared,
             summary(m_log_lin)$adj.r.squared,
             summary(m_lin_log)$adj.r.squared),
  AIC    = c(AIC(m_lin_lin), AIC(m_log_log),
             AIC(m_log_lin), AIC(m_lin_log))
)
print(comparison)

# Коэффициенты всех моделей ----
summary(m_lin_lin)
summary(m_log_lin)
summary(m_lin_log)

# Анализ log_log модели ----
summary(m_log_log)
coef(m_log_log)

# График модели ----
plot(log(final_df$salary), log(final_df$invest),
     xlab = "ln(Среднедушевые доходы)",
     ylab = "ln(Инвестиции в жилища)",
     main = "Степенная модель",
     pch = 16, col = "steelblue")
abline(m_log_log, col = "red", lwd = 2)

# График остатков ----
plot(fitted(m_log_log), residuals(m_log_log),
     xlab = "Предсказанные значения ln(invest)",
     ylab = "Остатки",
     main = "Остатки степенной модели",
     pch = 16, col = "steelblue")
abline(h = 0, col = "red", lwd = 2)

# MAPE в логарифмах ----
mape <- mean(abs((log(final_df$invest) - fitted(m_log_log)) / log(final_df$invest))) * 100
round(mape, 2)

# Анализ по одному году — 2012 ----
df_2012 <- filter(final_df, Год == 2012)

# Корреляция
round(cor(df_2012$salary, df_2012$invest), 4)
cor.test(df_2012$salary, df_2012$invest)

# Корреляционное поле
plot(df_2012$salary, df_2012$invest,
     xlab = "Среднедушевые доходы (руб.)",
     ylab = "Инвестиции в жилища (млн руб.)",
     main = "Корреляционное поле — 2012 год",
     pch = 16, col = "steelblue")

# Логарифмический срез 2012 ----
round(cor(log(df_2012$salary), log(df_2012$invest)), 4)

plot(log(df_2012$salary), log(df_2012$invest),
     xlab = "ln(Среднедушевые доходы)",
     ylab = "ln(Инвестиции в жилища)",
     main = "Корреляционное поле (логарифмы) — 2012 год",
     pch = 16, col = "steelblue")


# Несколько регионов на одном графике ----
regions_sample <- c("Республика Карелия", 
                    "Московская область",
                    "Краснодарский край",
                    "Иркутская область",
                    "Тверская область",
                    "Свердловская область",
                    "Новосибирская обл...")

df_sample <- filter(final_df, Регион %in% regions_sample)

colors <- c("steelblue", "tomato", "green3", "purple", "orange", "brown", "pink2")

plot(df_sample$salary, df_sample$invest,
     col = colors[as.factor(df_sample$Регион)],
     pch = 16, type = "n",
     xlab = "Среднедушевые доходы (руб.)",
     ylab = "Инвестиции в жилища (млн руб.)",
     main = "Зависимость по отдельным регионам")

for (i in seq_along(regions_sample)) {
  df_reg <- filter(df_sample, Регион == regions_sample[i])
  df_reg <- arrange(df_reg, salary)
  lines(df_reg$salary, df_reg$invest, col = colors[i], lwd = 2)
  points(df_reg$salary, df_reg$invest, col = colors[i], pch = 16)
}

legend("topleft",
       legend = regions_sample,
       col = colors,
       pch = 16, lty = 1, lwd = 2)

# Сравнение 4 моделей по каждому региону ----
results_all <- data.frame()

for (reg in regions_sample) {
  df_reg <- filter(final_df, Регион == reg)
  
  m_lin_lin <- lm(invest ~ salary,           data = df_reg)
  m_log_log <- lm(log(invest) ~ log(salary), data = df_reg)
  m_log_lin <- lm(log(invest) ~ salary,      data = df_reg)
  m_lin_log <- lm(invest ~ log(salary),      data = df_reg)
  
  results_all <- rbind(results_all, data.frame(
    Регион        = reg,
    R2_линейная   = round(summary(m_lin_lin)$r.squared, 3),
    R2_степенная  = round(summary(m_log_log)$r.squared, 3),
    R2_показат    = round(summary(m_log_lin)$r.squared, 3),
    R2_логарифм   = round(summary(m_lin_log)$r.squared, 3)
  ))
}

print(results_all)
# Данные по Карелии ----
karelia <- filter(final_df, grepl("Карел", Регион))
print(karelia)

# Корреляционное поле Карелия ----
plot(karelia$salary, karelia$invest,
     xlab = "Среднедушевые доходы (руб.)",
     ylab = "Инвестиции в жилища (млн руб.)",
     main = "Корреляционное поле — Республика Карелия",
     pch = 16, col = "steelblue")

# Динамика по годам — Карелия ----
par(mfrow = c(1, 2))
plot(karelia$Год, karelia$salary,
     type = "b", pch = 16, col = "steelblue",
     xlab = "Год", ylab = "Доходы (руб.)",
     main = "Динамика доходов — Карелия")
plot(karelia$Год, karelia$invest,
     type = "b", pch = 16, col = "tomato",
     xlab = "Год", ylab = "Инвестиции (млн руб.)",
     main = "Динамика инвестиций — Карелия")
par(mfrow = c(1, 1))

# Данные без 2010 года ----
karelia_train <- filter(karelia, Год != 2010)
karelia_test  <- filter(karelia, Год == 2010)

# Сравнение моделей для Карелии ----
m_kar_linlin <- lm(invest ~ salary,             data = karelia_train)
m_kar_loglog <- lm(log(invest) ~ log(salary),   data = karelia_train)
m_kar_loglin <- lm(log(invest) ~ salary,        data = karelia_train)
m_kar_linlog <- lm(invest ~ log(salary),        data = karelia_train)

comparison_kar <- data.frame(
  Модель = c("Линейная", "Степенная", "Показательная", "Логарифмическая"),
  R2     = c(summary(m_kar_linlin)$r.squared,
             summary(m_kar_loglog)$r.squared,
             summary(m_kar_loglin)$r.squared,
             summary(m_kar_linlog)$r.squared),
  AIC    = c(AIC(m_kar_linlin), AIC(m_kar_loglog),
             AIC(m_kar_loglin), AIC(m_kar_linlog)),
  MAPE   = c(mape_linlin, mape_loglog, mape_loglin, mape_linlog)
)

print(comparison_kar)
# MAPE для каждой модели Карелии ----
mape_linlin <- mean(abs((karelia_train$invest - fitted(m_kar_linlin)) / karelia_train$invest)) * 100
mape_loglog <- mean(abs((karelia_train$invest - exp(fitted(m_kar_loglog))) / karelia_train$invest)) * 100
mape_loglin <- mean(abs((karelia_train$invest - exp(fitted(m_kar_loglin))) / karelia_train$invest)) * 100
mape_linlog <- mean(abs((karelia_train$invest - fitted(m_kar_linlog)) / karelia_train$invest)) * 100

cat("Линейная MAPE:", round(mape_linlin, 2), "%\n")
cat("Степенная MAPE:", round(mape_loglog, 2), "%\n")
cat("Показательная MAPE:", round(mape_loglin, 2), "%\n")
cat("Логарифмическая MAPE:", round(mape_linlog, 2), "%\n")

# Модель по Карелии ----
karelia_model <- lm(log(invest) ~ log(salary), data = karelia_train)
summary(karelia_model)

# График модели по Карелии ----
plot(log(karelia_train$salary), log(karelia_train$invest),
     xlab = "ln(Среднедушевые доходы)",
     ylab = "ln(Инвестиции в жилища)",
     main = "Степенная модель — Республика Карелия",
     pch = 16, col = "steelblue")
abline(karelia_model, col = "red", lwd = 2)

# Прогноз на 2010 год ----
pred_log    <- predict(karelia_model, newdata = karelia_test, interval = "prediction")
pred_invest <- exp(pred_log)

cat("Фактические инвестиции:", karelia_test$invest, "\n")
cat("Прогноз:", round(pred_invest[1], 1), "\n")
cat("Доверительный интервал: [", round(pred_invest[2], 1), ";", round(pred_invest[3], 1), "]\n")
cat("Относительная ошибка:", round(abs(pred_invest[1] - karelia_test$invest) / karelia_test$invest * 100, 2), "%\n")

# График прогноза vs факт — Карелия----
pred_all <- exp(predict(karelia_model, newdata = karelia))

plot(karelia$Год, karelia$invest,
     type = "b", pch = 16, col = "steelblue",
     xlab = "Год", ylab = "Инвестиции (млн руб.)",
     main = "Прогноз vs Факт — Республика Карелия")
lines(karelia$Год, pred_all, type = "b", pch = 17, col = "red", lty = 2)
legend("topleft",
       legend = c("Факт", "Прогноз модели"),
       col    = c("steelblue", "red"),
       pch    = c(16, 17),
       lty    = c(1, 2))