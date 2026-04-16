# Пакеты ----
install.packages("car")

library(readxl)
library(car)

# Подготовка данных ----
name_map <- c(
  # Центральный ФО
  "г. Москва"                              = "г.Москва",
  
  # Северо-Западный ФО
  "Архангельская область"                  = "Архангельская обл...",
  "Калининградская область"                = "Калининградская о...",
  "Ленинградская область"                  = "Ленинградская обл...",
  "г. Санкт-Петербург"                     = "г.Санкт-Петербург",
  
  # Южный ФО
  "Волгоградская область"                  = "Волгоградская обл...",
  "Кабардино-Балкарская Республика"        = "Кабардино-Балкарс...",
  "Карачаево-Черкесская Республика"        = "Карачаево-Черкесс...",
  "Республика Северная Осетия-Алания"      = "Республика Северн...",
  "Республика Северная Осетия - Алания"    = "Республика Северн...",
  
  # Приволжский ФО
  "Республика Башкортостан"                = "Республика Башкор...",
  "Нижегородская область"                  = "Нижегородская обл...",
  "Удмуртская Республика"                  = "Удмуртская Респуб...",
  "Пермская область"                       = "Пермский край",
  "Коми-Пермяцкий автономный округ"        = "Коми-Пермяцкий ав...",
  
  # Уральский ФО
  "Ханты-Мансийский автономный округ"      = "Ханты-Мансийский ...",
  "Ханты-Мансийский авт.округ-Югра"        = "Ханты-Мансийский ...",
  "Ямало-Ненецкий автономный округ"        = "Ямало-Ненецкий ав...",
  "Ямало-Ненецкий авт.округ"               = "Ямало-Ненецкий ав...",
  
  # Сибирский ФО
  "Новосибирская область"                  = "Новосибирская обл...",
  "Читинская область"                      = "Забайкальский край",
  "Таймырский автономный округ"            = "Таймырский (Долга...",
  "Эвенкийский автономный округ"           = "Эвенкийский авт.о...",
  "Усть-Ордынский автономный округ"        = "Усть-Ордынский Бу...",
  "Агинский Бурятский автономный округ"    = "Агинский Бурятски...",
  
  # Дальневосточный ФО
  "Камчатская область"                     = "Камчатский край",
  "Корякский автономный округ"             = "Корякский авт.округ",
  "Ненецкий автономный округ"              = "Ненецкий авт.округ",
  "Чукотский автономный округ"             = "Чукотский авт.округ",
  "Республика Саха (Якутия)"               = "Республика Саха (...",
  "Еврейская автономная область"           = "Еврейская автоном...",
  "Приморский край."                       = "Приморский край",
  
  # Особые имена в файле населения
  "Город Москва столица Российской Федерации город федерального значения" = "г.Москва",
  "Город Санкт-Петербург город федерального значения"                    = "г.Санкт-Петербург",
  "Кемеровская область - Кузбасс"                                        = "Кемеровская область",
  "Республика Адыгея (Адыгея)"                                           = "Республика Адыгея",
  "Республика Татарстан (Татарстан)"                                     = "Республика Татарстан",
  "Чувашская Республика - Чувашия"                                       = "Чувашская Республика",
  "Ханты-Мансийский автономный округ - Югра (Тюменская область)"        = "Ханты-Мансийский ...",
  "Ямало-Ненецкий автономный округ (Тюменская область)"                 = "Ямало-Ненецкий ав...",
  
  # Упразднённые округа в файле населения
  "Ненецкий автономный округ (Архангельская область)"                   = "Ненецкий авт.округ",
  "Коми-Пермяцкий округ, входящий в состав Пермского края"             = "Коми-Пермяцкий ав...",
  "Корякский округ, входящий в состав Камчатского края"                = "Корякский авт.округ",
  "Агинский Бурятский округ (Забайкальский край)"                      = "Агинский Бурятски...",
  "Таймырский (Долгано-Ненецкий) автономный округ (Красноярский край)" = "Таймырский (Долга...",
  "Усть-Ордынский Бурятский округ"                                     = "Усть-Ордынский Бу...",
  "Эвенкийский автономный округ (Красноярский край)"                   = "Эвенкийский авт.о..."
)

fix_name <- function(x) {
  ifelse(x %in% names(name_map), name_map[x], x)
}

df <- read.csv("final_data.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

dist_raw <- read_excel("Регионы по федеральным округам.xlsx",
                       sheet = "Лист1", col_names = FALSE)
colnames(dist_raw) <- c("district", "num", "region_orig")
dist_raw$Регион <- fix_name(dist_raw$region_orig)
districts <- dist_raw[, c("Регион", "district")]
df <- merge(df, districts, by = "Регион", all.x = TRUE)

# широта и расстояние до Москвы
geo_raw <- read_excel("Широта и расстояние до Москвы.xlsx",
                      sheet = "Данные", col_names = TRUE)
colnames(geo_raw) <- c("region_orig", "capital", "latitude", "dist_moscow")
geo_raw$Регион <- fix_name(geo_raw$region_orig)
geo <- geo_raw[, c("Регион", "latitude", "dist_moscow")]
df <- merge(df, geo, by = "Регион", all.x = TRUE)

# для упразднённых округов
geo_manual <- data.frame(
  Регион      = c("Агинский Бурятски...", "Коми-Пермяцкий ав...",
                  "Корякский авт.округ",  "Ненецкий авт.округ",
                  "Таймырский (Долга...", "Усть-Ордынский Бу...",
                  "Эвенкийский авт.о..."),
  latitude    = c(51.10, 59.01, 59.10, 67.64, 69.40, 52.80, 64.22),
  dist_moscow = c(6200,  1410,  9000,  1900,  3600,  5100,  4400),
  stringsAsFactors = FALSE
)
for (i in 1:nrow(geo_manual)) {
  mask <- df$Регион == geo_manual$Регион[i] & is.na(df$latitude)
  df$latitude[mask]    <- geo_manual$latitude[i]
  df$dist_moscow[mask] <- geo_manual$dist_moscow[i]
}

# население 
pop_raw <- read_excel("Население регионов.xlsx",
                      sheet = "Отчет", col_names = FALSE)
region_headers     <- as.character(unlist(pop_raw[2, ]))
data_block         <- pop_raw[4:16, ]

clean_header <- function(h) trimws(sub("^[0-9]+ ", "", h))
region_names_clean  <- sapply(region_headers, clean_header)
region_names_mapped <- fix_name(region_names_clean)
keep_cols           <- which(region_names_mapped %in% unique(df$Регион))

pop_list <- list()
for (col_idx in keep_cols) {
  region_name <- region_names_mapped[col_idx]
  for (row_idx in 1:nrow(data_block)) {
    year_val <- as.integer(sub(" г\\.", "", as.character(data_block[row_idx, 1])))
    pop_val  <- as.numeric(data_block[row_idx, col_idx])
    pop_list[[length(pop_list) + 1]] <- data.frame(
      Регион     = region_name,
      Год        = year_val,
      population = pop_val,
      stringsAsFactors = FALSE
    )
  }
}
pop_df <- do.call(rbind, pop_list)
df <- merge(df, pop_df, by = c("Регион", "Год"), all.x = TRUE)


# Итоговый файл ----
df <- df[order(df$Регион, df$Год), ]
df <- df[!is.na(df$population), ]

write.csv(df, "final_data2.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Готово. Строк:", nrow(df), "| Регионов:", length(unique(df$Регион)),
    "| NA:", sum(is.na(df)), "\n")

# 1: Корреляционная матрица ----
data <- read.csv("final_data2.csv", sep = ",", stringsAsFactors = FALSE)

# Создаём логарифмы числовых переменных
data$ln_invest      <- log(data$invest)
data$ln_salary      <- log(data$salary)
data$ln_population  <- log(data$population)
data$ln_dist_moscow <- log(data$dist_moscow)

# Формируем матрицу нужных переменных
# Используем dist_moscow без лога (Москва = 0, лог неприменим)
cor_vars <- data[, c("ln_invest", "ln_salary", "ln_population",
                     "latitude", "dist_moscow")]

colnames(cor_vars) <- c("ln(Инвестиции)", "ln(Доходы)",
                        "ln(Население)", "Широта",
                        "Расст. до Москвы (км)")

cor_matrix <- cor(cor_vars, use = "complete.obs")
round(cor_matrix, 3)
# 2: Проверка гипотезы об отсутствии корреляции между факторами ----

cor.test(data$ln_invest, data$ln_salary)
cor.test(data$ln_invest, data$ln_population)
cor.test(data$ln_invest, data$latitude)
cor.test(data$ln_invest, data$dist_moscow)
# 3: Построение линейной регрессии ----

# lm() — оценивает линейную регрессию МНК
model <- lm(ln_invest ~ ln_salary + ln_population + latitude + dist_moscow,
            data = data)

summary(model)

# 4: Линейная регрессия в стандартизованном виде ----

# scale() — стандартизует переменную: (x - mean(x)) / sd(x)
data_scaled <- data.frame(
  ln_invest     = scale(data$ln_invest),
  ln_salary     = scale(data$ln_salary),
  ln_population = scale(data$ln_population),
  latitude      = scale(data$latitude),
  dist_moscow   = scale(data$dist_moscow)
)

model_scaled <- lm(ln_invest ~ ln_salary + ln_population + latitude + dist_moscow,
                   data = data_scaled)

summary(model_scaled)
# 5: Степень влияния каждого фактора ----

betas <- coef(model_scaled)[-1]

# Корреляции каждого фактора с ln(invest)
r_xy <- cor(data[, c("ln_salary", "ln_population", "latitude", "dist_moscow")],
            data$ln_invest,
            use = "complete.obs")

shares <- betas * r_xy
shares_pct <- shares / sum(shares) * 100

result <- data.frame(
  Бета       = round(betas, 4),
  r_с_Y      = round(r_xy, 4),
  Доля_R2    = round(shares, 4),
  Доля_проц  = round(shares_pct, 2)
)

print(result)
cat("Сумма долей R²:", round(sum(shares), 4), "\n")
cat("R² модели:     ", round(summary(model)$r.squared, 4), "\n")
# 6: Проверка значимости регрессии (F-тест) ----

f_stat <- summary(model)$fstatistic
f_value <- f_stat[1]
df1 <- f_stat[2]
df2 <- f_stat[3]
p_value <- pf(f_value, df1, df2, lower.tail = FALSE)

# pf() — функция распределения Фишера
f_crit <- qf(0.95, df1 = df1, df2 = df2)
# qf() — квантиль распределения Фишера

cat("F-статистика:      ", round(f_value, 2), "\n")
cat("df1 (факторы):     ", df1, "\n")
cat("df2 (остатки):     ", df2, "\n")
cat("F критическое:     ", round(f_crit, 3), "\n")
cat("p-значение:        ", format(p_value, scientific = TRUE), "\n")
# 7: Проверка значимости коэффициентов ----
# В summary(model) уже есть p-значения для каждого коэффициента

# 8: Частные уравнения регрессии ----

# Средние значения всех факторов
m_salary     <- mean(data$ln_salary)
m_population <- mean(data$ln_population)
m_latitude   <- mean(data$latitude)
m_dist       <- mean(data$dist_moscow)

b <- coef(model)

# Частное уравнение по ln(salary): остальные на среднем
const_salary <- b[1] + b[3]*m_population + b[4]*m_latitude + b[5]*m_dist
cat("Частное уравнение ln(Доходы):\n")
cat("ln(Инвестиции) =", round(const_salary, 3), "+", round(b[2], 3), "* ln(Доходы)\n\n")

# Частное уравнение по ln(population)
const_pop <- b[1] + b[2]*m_salary + b[4]*m_latitude + b[5]*m_dist
cat("Частное уравнение ln(Население):\n")
cat("ln(Инвестиции) =", round(const_pop, 3), "+", round(b[3], 3), "* ln(Население)\n\n")

# Частное уравнение по latitude
const_lat <- b[1] + b[2]*m_salary + b[3]*m_population + b[5]*m_dist
cat("Частное уравнение Широта:\n")
cat("ln(Инвестиции) =", round(const_lat, 3), "+", round(b[4], 4), "* Широта\n\n")

# Частное уравнение по dist_moscow
const_dist <- b[1] + b[2]*m_salary + b[3]*m_population + b[4]*m_latitude
cat("Частное уравнение Расст. до Москвы:\n")
cat("ln(Инвестиции) =", round(const_dist, 3), "+", round(b[5], 7), "* Расст. до Москвы\n")
# 9: Коэффициенты эластичности ----

b <- coef(model)

e_salary     <- b[2]
e_population <- b[3]

e_latitude <- b[4] * mean(data$latitude) / mean(data$ln_invest)
e_dist     <- b[5] * mean(data$dist_moscow) / mean(data$ln_invest)

elasticity <- data.frame(
  Фактор        = c("ln(Доходы)", "ln(Население)", "Широта", "Расст. до Москвы"),
  Коэф_b        = round(c(b[2], b[3], b[4], b[5]), 6),
  Среднее_X     = round(c(mean(data$ln_salary), mean(data$ln_population),
                          mean(data$latitude), mean(data$dist_moscow)), 3),
  Эластичность  = round(c(e_salary, e_population, e_latitude, e_dist), 4)
)

print(elasticity)
cat("Среднее ln(Инвестиции):", round(mean(data$ln_invest), 3), "\n")
# 10: Мультиколлинеарность ----
vif_values <- vif(model)
print(round(vif_values, 3))

cor_factors <- cor(data[, c("ln_salary", "ln_population", "latitude", "dist_moscow")],
                   use = "complete.obs")

colnames(cor_factors) <- c("ln(Доходы)", "ln(Население)", "Широта", "Расст. до Москвы")
rownames(cor_factors) <- colnames(cor_factors)
round(cor_factors, 3)
# 11: Неоднородность данных ----

# factor() — превращает строковую переменную в категориальную
data$district <- relevel(data$district, ref = "Центральный федеральный округ")

# Базовая категория (первая по алфавиту)
levels(data$district)

# Модель с дамми по округам
model_dummy <- lm(ln_invest ~ ln_salary + ln_population + latitude +
                    dist_moscow + district, data = data)

summary(model_dummy)

# anova() — сравнивает две вложенные модели
anova(model, model_dummy)

data$residuals <- residuals(model)

data$district_short <- factor(data$district,
                              levels = c("Центральный федеральный округ",
                                         "Дальневосточный федеральный округ",
                                         "Приволжский федеральный округ",
                                         "Северо-Западный федеральный округ",
                                         "Сибирский федеральный округ",
                                         "Уральский федеральный округ",
                                         "Южный федеральный округ"),
                              labels = c("ЦФО", "ДФО", "ПФО", "СЗФО", "СФО", "УФО", "ЮФО"))

par(mar = c(5, 4, 4, 2))
boxplot(residuals ~ district_short, data = data,
        main = "Остатки модели по федеральным округам",
        xlab = "Федеральный округ",
        ylab = "Остатки ln(Инвестиции)",
        las = 1,
        col = "lightblue")

abline(h = 0, col = "red", lty = 2)

# Найти регионы с наибольшими по модулю остатками
data[order(abs(data$residuals), decreasing = TRUE), 
     c("Регион", "district", "residuals")][1:10, ]
# 12: Точечный прогноз (Карелия) ----

# Данные по Карелии со всеми годами
karelia_pred <- data.frame(
  ln_salary     = log(karelia$salary),
  ln_population = log(karelia$population),
  latitude      = karelia$latitude,
  dist_moscow   = karelia$dist_moscow,
  district      = factor("Северо-Западный федеральный округ",
                         levels = levels(data$district))
)

# Прогноз для всех лет
karelia$pred_log <- predict(model_dummy, newdata = karelia_pred)
karelia$pred_rub <- exp(karelia$pred_log)
karelia$error_rub <- karelia$pred_rub - karelia$invest
karelia$error_pct <- round((karelia$pred_rub - karelia$invest) / karelia$invest * 100, 2)

# Итоговая таблица
result <- karelia[order(karelia$Год), c("Год", "invest", "pred_rub", "error_rub", "error_pct")]
result$pred_rub  <- round(result$pred_rub, 1)
result$error_rub <- round(result$error_rub, 1)
colnames(result) <- c("Год", "Факт (млн руб.)", "Прогноз (млн руб.)", 
                      "Ошибка (млн руб.)", "Ошибка (%)")
print(result)

# График факт vs прогноз
plot(karelia$Год, karelia$invest,
     type = "b", col = "black", pch = 16,
     main = "Карелия: факт и прогноз инвестиций",
     xlab = "Год", ylab = "Инвестиции, млн руб.",
     ylim = range(c(karelia$invest, karelia$pred_rub)))

lines(karelia$Год, karelia$pred_rub, 
      type = "b", col = "red", pch = 17, lty = 2)

legend("topleft", legend = c("Факт", "Прогноз"),
       col = c("black", "red"), pch = c(16, 17), lty = c(1, 2))

cat("Средняя ошибка (млн руб.):", round(mean(karelia$error_rub), 1), "\n")
cat("Средняя ошибка по модулю (млн руб.):", round(mean(abs(karelia$error_rub)), 1), "\n")
cat("Средняя ошибка по модулю (%):", round(mean(abs(karelia$error_pct)), 2), "\n")
# 13: Интервальный прогноз (Карелия) ----

conf_int <- predict(model_dummy, newdata = karelia_pred,
                    interval = "confidence", level = 0.95)

conf_rub <- exp(conf_int)

# Итоговая таблица
result_int <- data.frame(
  Год         = karelia[order(karelia$Год), "Год"],
  Факт        = karelia[order(karelia$Год), "invest"],
  Прогноз     = round(conf_rub[, "fit"], 1),
  ДИ_нижний  = round(conf_rub[, "lwr"], 1),
  ДИ_верхний = round(conf_rub[, "upr"], 1)
)

print(result_int)

# График с доверительным интервалом
karelia_ord <- karelia[order(karelia$Год), ]

plot(karelia_ord$Год, karelia_ord$invest,
     type = "b", col = "black", pch = 16,
     main = "Карелия: прогноз с доверительным интервалом",
     xlab = "Год", ylab = "Инвестиции, млн руб.",
     ylim = range(c(conf_rub[, "lwr"], conf_rub[, "upr"], karelia_ord$invest)))

polygon(c(karelia_ord$Год, rev(karelia_ord$Год)),
        c(conf_rub[, "lwr"], rev(conf_rub[, "upr"])),
        col = rgb(0, 0, 1, 0.2), border = NA)

lines(karelia_ord$Год, conf_rub[, "fit"],
      type = "b", col = "red", pch = 17, lty = 2)

legend("topleft",
       legend = c("Факт", "Прогноз", "Доверительный интервал 95%"),
       col = c("black", "red", rgb(0, 0, 1, 0.5)),
       pch = c(16, 17, NA),
       lty = c(1, 2, NA),
       fill = c(NA, NA, rgb(0, 0, 1, 0.2)),
       border = NA)