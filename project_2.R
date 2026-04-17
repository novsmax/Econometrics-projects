

# Пакеты ----
if (!require("readxl"))  install.packages("readxl")
if (!require("car"))     install.packages("car")

library(readxl)
library(car)

# Загрузка данных ----
data <- read.csv("final_data2.csv", encoding = "UTF-8")

data$salary      <- as.numeric(data$salary)
data$invest      <- as.numeric(data$invest)
data$latitude    <- as.numeric(data$latitude)
data$dist_moscow <- as.numeric(data$dist_moscow)
data$population  <- as.numeric(data$population)

cat("Наблюдений:", nrow(data), "\n")
cat("Регионов:", length(unique(data$Регион)), "\n")
str(data)



#  1: Корреляционная матрица ----


num_vars <- data[, c("invest", "salary", "population", "dist_moscow", "latitude")]
log_vars <- data.frame(
  ln_invest      = log(data$invest),
  ln_salary      = log(data$salary),
  ln_population  = log(data$population),
  dist_moscow    = data$dist_moscow,
  latitude       = data$latitude
)

cor_matrix <- round(cor(log_vars, use = "complete.obs"), 3)
print(cor_matrix)



#  2: Проверка гипотезы об отсутствии корреляции ----


n <- nrow(log_vars)

for (var in c("ln_salary", "ln_population", "dist_moscow", "latitude")) {
  r   <- cor(log_vars$ln_invest, log_vars[[var]], use = "complete.obs")
  df  <- n - 2
  t_stat  <- r * sqrt(df) / sqrt(1 - r^2)
  t_crit  <- qt(0.975, df)
  p_value <- 2 * pt(-abs(t_stat), df)
  cat(sprintf("%-20s r=%6.3f  t=%7.3f  t_кр=%5.3f  p=%s\n",
              var, r, t_stat, t_crit,
              ifelse(p_value < 0.001, "<0.001", round(p_value, 4))))
}



#  3: Линейная регрессия (log-log + нелогарифм. факторы) ----


model <- lm(log(invest) ~ log(salary) + log(population) + latitude + dist_moscow,
            data = data)
summary(model)



#  4: Стандартизованная регрессия ----

# Стандартизованный коэффициент = b_i * (sd_xi / sd_y)
# Это и есть бета-коэффициент — показывает вклад фактора
# в единицах стандартных отклонений

b     <- coef(model)[-1]  # без intercept

sd_y  <- sd(log(data$invest))
sd_x1 <- sd(log(data$salary))
sd_x2 <- sd(log(data$population))
sd_x3 <- sd(data$latitude)
sd_x4 <- sd(data$dist_moscow)

# Стандартизованный коэффициент = b_i * (sd_xi / sd_y)
beta_salary  <- b["log(salary)"]     * sd_x1 / sd_y
beta_pop     <- b["log(population)"] * sd_x2 / sd_y
beta_lat     <- b["latitude"]        * sd_x3 / sd_y
beta_dist    <- b["dist_moscow"]     * sd_x4 / sd_y

cat(sprintf(
  "Вывод бета-коэффициентов через формулу β_i = b_i * (sd_xi / sd_y):\n\n
β_salary      = %.3f * (%.4f / %.4f) = %.3f\n
β_population  = %.3f * (%.4f / %.4f) = %.3f\n
β_latitude    = %.5f * (%.4f / %.4f) = %.3f\n
β_dist_moscow = %.6f * (%.4f / %.4f) = %.3f\n",
  b["log(salary)"],     sd_x1, sd_y, beta_salary,
  b["log(population)"], sd_x2, sd_y, beta_pop,
  b["latitude"],        sd_x3, sd_y, beta_lat,
  b["dist_moscow"],     sd_x4, sd_y, beta_dist
))

# Проверка: должно совпасть с summary(model_std)
cat("\nПроверка через model_std:\n")
print(round(coef(model_std)[-1], 3))



#  5: Степень влияния каждого фактора ----


# Частные коэффициенты корреляции через cor2pcor не нужны —
# используем долю объяснённой дисперсии через аномальную часть SS
anova_res <- anova(model)
ss_total  <- sum((log(data$invest) - mean(log(data$invest)))^2)
ss_factors <- anova_res$`Sum Sq`[1:4]
names(ss_factors) <- c("ln_salary", "ln_population", "latitude", "dist_moscow")

share <- round(ss_factors / ss_total * 100, 1)
print(share)
cat("Сумма долей:", sum(share), "%\n")
cat("R² модели:", round(summary(model)$r.squared * 100, 1), "%\n")



#  6: Проверка значимости регрессии (F-тест) ----


f_stat  <- summary(model)$fstatistic
f_value <- f_stat[1]
df1     <- f_stat[2]
df2     <- f_stat[3]
f_crit  <- qf(0.95, df1, df2)
p_model <- pf(f_value, df1, df2, lower.tail = FALSE)

cat(sprintf("F = %.2f\n", f_value))
cat(sprintf("F_кр (df1=%d, df2=%d, α=0.05) = %.2f\n", df1, df2, f_crit))
cat(sprintf("p-value = %s\n", ifelse(p_model < 0.001, "<0.001", round(p_model, 4))))
cat(ifelse(f_value > f_crit, "→ Модель значима\n", "→ Модель НЕ значима\n"))



#  7: Значимость каждого коэффициента (t-тест) ----


coef_table <- summary(model)$coefficients
t_crit7    <- qt(0.975, df = nrow(data) - 5)

cat(sprintf("t_кр = %.3f (α=0.05, df=%d)\n\n", t_crit7, nrow(data) - 5))
print(round(coef_table, 5))



#  8: Частные уравнения регрессии ----

b0 <- coef(model)["(Intercept)"]
b1 <- coef(model)["log(salary)"]
b2 <- coef(model)["log(population)"]
b3 <- coef(model)["latitude"]
b4 <- coef(model)["dist_moscow"]

# Средние значения факторов
m1 <- mean(log(data$salary))
m2 <- mean(log(data$population))
m3 <- mean(data$latitude)
m4 <- mean(data$dist_moscow)

cat(sprintf(
  "Фиксируем все факторы кроме одного на среднем значении:\n
ln(Доходы):
  ln(Инвест.) = (%.3f + %.3f*%.3f + %.5f*%.2f + %.6f*%.1f) + %.3f*ln(Доходы)
             = %.3f + %.3f * ln(Доходы)\n
ln(Население):
  ln(Инвест.) = (%.3f + %.3f*%.3f + %.5f*%.2f + %.6f*%.1f) + %.3f*ln(Население)
             = %.3f + %.3f * ln(Население)\n
Широта:
  ln(Инвест.) = (%.3f + %.3f*%.3f + %.3f*%.3f + %.6f*%.1f) + %.5f*Широта
             = %.3f + %.5f * Широта\n
Расст. до Москвы:
  ln(Инвест.) = (%.3f + %.3f*%.3f + %.3f*%.3f + %.5f*%.2f) + %.6f*Расст.
             = %.3f + %.6f * Расст. до Москвы\n",
  # ln(salary)
  b0, b2, m2, b3, m3, b4, m4, b1,
  c1, b1,
  # ln(population)
  b0, b1, m1, b3, m3, b4, m4, b2,
  c2, b2,
  # latitude
  b0, b1, m1, b2, m2, b4, m4, b3,
  c3, b3,
  # dist_moscow
  b0, b1, m1, b2, m2, b3, m3, b4,
  c4, b4
))



#  9: Коэффициенты эластичности ----


# Для логарифмированных X: эластичность = коэффициент напрямую
# Для нелогарифмированных X: эластичность = β * mean(X)
mean_lat  <- mean(data$latitude,    na.rm = TRUE)
mean_dist <- mean(data$dist_moscow, na.rm = TRUE)

e_salary  <- b[2]                   # ln(salary)  → напрямую
e_pop     <- b[3]                   # ln(pop)     → напрямую
e_lat     <- b[4] * mean_lat        # latitude    → β * mean(X)
e_dist    <- b[5] * mean_dist       # dist_moscow → β * mean(X)

cat(sprintf("Эластичность по доходам:          %.3f\n", e_salary))
cat(sprintf("Эластичность по населению:        %.3f\n", e_pop))
cat(sprintf("Эластичность по широте:           %.3f\n", e_lat))
cat(sprintf("Эластичность по расст. до Москвы: %.3f\n", e_dist))



#  10: Мультиколлинеарность (VIF) ----


vif_values <- vif(model)
print(round(vif_values, 3))
cat(ifelse(all(vif_values < 10),
           "→ Все VIF < 10, мультиколлинеарность отсутствует\n",
           "→ ВНИМАНИЕ: обнаружена мультиколлинеарность\n"))



#  11: Неоднородность данных (фиктивные переменные по округам) ----

data$district <- relevel(factor(data$district), ref = "Центральный федеральный округ")
model_with_district <- lm(log(invest) ~ log(salary) + log(population) +
                            latitude + dist_moscow + district,
                          data = data)
summary(model_with_district)



#  11: Неоднородность данных (модели по федеральным округам) ----

districts <- unique(data$district)
results_by_district <- data.frame()

for (d in districts) {
  sub <- data[data$district == d, ]
  
  m <- lm(log(invest) ~ log(salary) + log(population) + latitude + dist_moscow,
          data = sub)
  s <- summary(m)
  
  results_by_district <- rbind(results_by_district, data.frame(
    Округ         = d,
    n             = nrow(sub),
    R2            = round(s$r.squared, 3),
    b_salary      = round(coef(m)["log(salary)"], 3),
    b_population  = round(coef(m)["log(population)"], 3),
    b_latitude    = round(coef(m)["latitude"], 4),
    b_dist        = round(coef(m)["dist_moscow"], 6),
    F_stat        = round(s$fstatistic[1], 1)
  ))
}

print(results_by_district)

# Отдельно — модель для СЗФО (Карелия)
data_szfo <- data[data$district == "Северо-Западный федеральный округ", ]
model_szfo <- lm(log(invest) ~ log(salary) + log(population) + latitude + dist_moscow,
                 data = data_szfo)
summary(model_szfo)
#  12: Точечный прогноз + график ----

pred_log     <- predict(model_szfo, newdata = karelia)
karelia_pred <- exp(pred_log)

result12 <- data.frame(
  Год        = karelia$Год,
  Факт       = round(karelia$invest, 1),
  Прогноз    = round(karelia_pred, 1),
  Доля_ошибки = round(abs(karelia_pred - karelia$invest) / karelia$invest * 100, 1)
)
print(result12)

plot(karelia$Год, karelia$invest,
     type = "b", pch = 16, col = "steelblue", lwd = 2,
     xlab = "Год", ylab = "Инвестиции (млн руб.)",
     main = "Карелия: факт vs точечный прогноз",
     ylim = range(c(karelia$invest, karelia_pred)))
lines(karelia$Год, karelia_pred,
      type = "b", pch = 17, col = "firebrick", lwd = 2, lty = 2)
legend("topleft",
       legend = c("Факт", "Прогноз"),
       col    = c("steelblue", "firebrick"),
       pch    = c(16, 17), lty = c(1, 2), lwd = 2)


#  13: Интервальный прогноз + график ----

pred_interval <- predict(model_szfo,
                         newdata  = karelia,
                         interval = "confidence",
                         level    = 0.95)

lwr <- exp(pred_interval[, "lwr"])
fit <- exp(pred_interval[, "fit"])
upr <- exp(pred_interval[, "upr"])

result13 <- data.frame(
  Год         = karelia$Год,
  Факт        = round(karelia$invest, 1),
  Прогноз     = round(fit, 1),
  Нижняя      = round(lwr, 1),
  Верхняя     = round(upr, 1),
  В_интервале = ifelse(karelia$invest >= lwr & karelia$invest <= upr, "✓", "✗")
)
print(result13)

in_interval <- sum(result13$В_интервале == "✓")
cat(sprintf("\nПопадает в интервал: %d из %d (%.0f%%)\n",
            in_interval, nrow(result13),
            in_interval / nrow(result13) * 100))

plot(karelia$Год, karelia$invest,
     type = "n",
     xlab = "Год", ylab = "Инвестиции (млн руб.)",
     main = "Карелия: прогноз",
     ylim = range(c(karelia$invest, lwr, upr)))

polygon(c(karelia$Год, rev(karelia$Год)),
        c(upr, rev(lwr)),
        col = rgb(1, 0, 0, 0.1), border = NA)

lines(karelia$Год, lwr, col = "firebrick", lwd = 1, lty = 3)
lines(karelia$Год, upr, col = "firebrick", lwd = 1, lty = 3)
lines(karelia$Год, fit,
      type = "b", pch = 17, col = "firebrick", lwd = 2, lty = 2)
lines(karelia$Год, karelia$invest,
      type = "b", pch = 16, col = "steelblue", lwd = 2)

legend("topleft",
       legend = c("Факт", "Прогноз", "95% ДИ"),
       col    = c("steelblue", "firebrick", rgb(1, 0, 0, 0.3)),
       pch    = c(16, 17, 15),
       lty    = c(1, 2, NA), lwd = 2)