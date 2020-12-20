library('stats')      # для функции p.adjust()
library('Hmisc')          # для расчёта корреляционной матрицы
library('corrplot')       # визуализация корреляционных матриц: corrplot()
library('nortest')        # для теста Андерсона-Дарлинга ad.test()
library('knitr')

# 1. Импорт данных  ------------------------------------------------------------

# загрузка объектов из сохранённого рабочего пространства
load('Example.RData')
# просмотр списка объектов
ls()

dim(reg.df)
head(reg.df)
str(reg.df)
dim(reglog.df)
head(reglog.df)
str(reglog.df)

# 2. Оценка параметров моделей  ------------------------------------------------

# множественная регрессия для всех регионов ====================================
fit.1 <- lm(y ~ x1 + x2, 
            data = reg.df)
summary(fit.1)  #сводка.по.построенной.модели

fit.2 <- lm(y ~ x1, 
            data = reg.df)
summary(fit.2)  # оставляем самый значимый параметр
# модель с переменной структурой ===============================================
fit.X1.fo <- lm(y ~ x1 * FO , 
                data = reg.df)
summary(fit.X1.fo)
# создаём фрейм со всеми переменными-факторами (создаём фиктивные)
X.matrix <- model.matrix(y ~ x1 * FO, data = reg.df)
# результат
head(X.matrix)

# присоединяем независимую переменную
data.fit <- cbind(y = reg.df$y, 
                  data.frame(X.matrix)[, -1])
# результат
head(data.fit)
tail(data.fit)

# сохраняем для следующей лабораторной
data.fit.X1.fo <- data.fit
# доводим модели до состояния значимости всех коэффициентов ====================

# функция с последовательным исключением незначимых регрессоров
source('https://raw.githubusercontent.com/aksyuk/R-Practice-basics/master/user_functions/removeFactorsByPValue.R')

# применяем процедуру, сначала без поправок на p-значения
fit.X1.fo <- removeFactorsByPValue(data = data.fit, 
                                   y.var.name = 'y')
summary(fit.X1.fo)
# строим ПЛР на второй по силе корреляции фактор
fit.X2 <- lm(y ~ x2, 
             data = reg.df)
summary(fit.X2)

# модель с переменной структурой
fit.X2.fo <- lm(y ~ FO + x2, 
                data = reg.df)
summary(fit.X2.fo)

# доводим до состояния значимости
# создаём фрейм со всеми переменными-факторами (создаём фиктивные)
X.matrix <- model.matrix(y ~ x2 * FO, data = reg.df)
data.fit <- cbind(y = reg.df$y, 
                  data.frame(X.matrix)[, -1])
head(data.fit)
tail(data.fit)

# сохраняем для следующей лабораторной
data.fit.X2.fo <- data.fit

# доводим до значимости с помощью пользовательской функции
# без поправки
fit.X2.fo <- removeFactorsByPValue(data = data.fit, 
                                   y.var.name = 'y')
summary(fit.X2.fo)
# с поправкой
fit.X2.fo <- removeFactorsByPValue(data = data.fit, 
                                   y.var.name = 'y',
                                   p.adj.method = 'bonferroni')
summary(fit.X2.fo)

# с поправкой, и повышаем уровень значимости
fit.X2.fo <- removeFactorsByPValue(data = data.fit, 
                                   y.var.name = 'y',
                                   p.adj.method = 'bonferroni',
                                   alpha = 0.10)
summary(fit.X2.fo)



# 3. Сравнение моделей  --------------------------------------------------------

# модели с фактором 
anova(fit.2, fit.X1.fo)

# модели с фактором
anova(fit.2, fit.X2.fo)
table7 <- data.frame(anova(fit.2, fit.X1.fo))
kable(table7)
table8 <- data.frame(anova(fit.2, fit.X2.fo))
kable(table8)
# основные характеристики качества аппроксимации
str(summary(fit.2))
# скорректированный R-квадрат
summary(fit.2)$adj.r.squared
# F-расчётное
summary(fit.2)$fstatistic
summary(fit.2)$fstatistic[1]
# стандартная ошибка модели
summary(fit.2)$sigma

# список построенных моделей
models.list <- list(fit.2, fit.X1.fo, fit.X2, fit.X2.fo)
names(models.list) <- c('fit.2', 'fit.X1.fo', 'fit.X2', 'fit.X2.fo')

# фрейм с характеристиками четырёх моделей
df.goodness.of.fit <- data.frame(Модель = names(models.list), 
                                       R.2.скорр = 0,
                                       F.расч = 0,
                                       Станд.Ошибка = 0)
for (i in 1:length(models.list)) {
  # скорректированный R-квадрат
  df.goodness.of.fit[i, 'R.2.скорр'] <- 
    round(summary(models.list[[i]])$adj.r.squared, 3)
  # F расчётное
  df.goodness.of.fit[i, 'F.расч'] <- 
    round(summary(models.list[[i]])$fstatistic[1], 2)
  # стандартная ошибка
  df.goodness.of.fit[i, 'Станд.Ошибка'] <- 
    round(summary(models.list[[i]])$sigma, 1)
}
df.goodness.of.fit
table3 <- data.frame(df.goodness.of.fit)
kable(table3)

# 4. Оценка параметров моделей для логарифмов ------------------------------------------------

# множественная регрессия для всех регионов ====================================
fitlog.1 <- lm(y ~ x1 + x2 + x4, 
            data = reglog.df)
summary(fitlog.1)  #сводка.по.построенной.модели

fitlog.2 <- lm(y ~ x1 + x2, 
            data = reglog.df)
round(summary(fitlog.2)$coef,4)  # оставляем самый значимый параметр
fitlog.X1 <- lm(y ~ x1, 
             data = reglog.df)
summary(fitlog.X1)
FO<-reg.df[2]
reglog.df<-c(reglog.df,FO)
# модель с переменной структурой ===============================================
fitlog.X1.fo <- lm(y ~ x1 * FO , 
                data = reglog.df
                )
summary(fitlog.X1.fo)
# создаём фрейм со всеми переменными-факторами (создаём фиктивные)
X.matrixlog <- model.matrix(y ~ x1 * FO, data = reglog.df)
# результат
head(X.matrixlog)

# присоединяем независимую переменную
data.fitlog <- cbind(y = reglog.df$y, 
                  data.frame(X.matrixlog)[, -1])
# результат
head(data.fitlog)
tail(data.fitlog)

# сохраняем для следующей лабораторной
data.fitlog.X1.fo <- data.fitlog
# доводим модели до состояния значимости всех коэффициентов ====================

# функция с последовательным исключением незначимых регрессоров
source('https://raw.githubusercontent.com/aksyuk/R-Practice-basics/master/user_functions/removeFactorsByPValue.R')

# применяем процедуру, сначала без поправок на p-значения
fitlog.X1.fo <- removeFactorsByPValue(data = data.fitlog, 
                                   y.var.name = 'y')
summary(fitlog.X1.fo)
# строим ПЛР на второй по силе корреляции фактор
fitlog.X2 <- lm(y ~ x2, 
             data = reglog.df)
summary(fit.X2)

# модель с переменной структурой
fitlog.X2.fo <- lm(y ~ FO + x2, 
                data = reglog.df)
summary(fitlog.X2.fo)

# доводим до состояния значимости
# создаём фрейм со всеми переменными-факторами (создаём фиктивные)
X.matrixlog <- model.matrix(y ~ x2 * FO, data = reglog.df)
data.fitlog <- cbind(y = reglog.df$y, 
                  data.frame(X.matrixlog)[, -1])
head(data.fitlog)
tail(data.fitlog)

# сохраняем для следующей лабораторной
data.fitlog.X2.fo <- data.fitlog

# доводим до значимости с помощью пользовательской функции
# без поправки
fitlog.X2.fo <- removeFactorsByPValue(data = data.fitlog, 
                                   y.var.name = 'y')
summary(fitlog.X2.fo)

# с поправкой
fitlog.X2.fo <- removeFactorsByPValue(data = data.fitlog, 
                                   y.var.name = 'y',
                                   p.adj.method = 'bonferroni')
summary(fitlog.X2.fo)

# с поправкой, и повышаем уровень значимости
fitlog.X2.fo <- removeFactorsByPValue(data = data.fitlog, 
                                   y.var.name = 'y',
                                   p.adj.method = 'bonferroni',
                                   alpha = 0.10)
summary(fitlog.X2.fo)


# 5. Сравнение моделей  --------------------------------------------------------

# модели с фактором x1
anova(fitlog.X1, fitlog.X1.fo)

# модели с фактором x2
anova(fitlog.X1, fit.X2.fo)
table2 <- data.frame(anova(fitlog.X1, fitlog.X1.fo))
kable(table2)
table9 <- data.frame(anova(fitlog.X1, fit.X2.fo))
kable(table9)
# основные характеристики качества аппроксимации
str(summary(fitlog.X1))
# скорректированный R-квадрат
summary(fitlog.X1)$adj.r.squared
# F-расчётное
summary(fitlog.X1)$fstatistic
summary(fitlog.X1)$fstatistic[1]
# стандартная ошибка модели
summary(fitlog.X1)$sigma

# список построенных моделей
modelslog.list <- list(fitlog.X1, fitlog.X1.fo, fitlog.X2, fitlog.X2.fo)
names(modelslog.list) <- c('fitlog.X1', 'fitlog.X1.fo', 'fitlog.X2', 'fitlog.X2.fo')

# фрейм с характеристиками четырёх моделей
df.goodness.of.fitlog <- data.frame(Модель = names(modelslog.list), 
                                       R.2.скорр = 0,
                                       F.расч = 0,
                                       Станд.Ошибка = 0)
for (i in 1:length(modelslog.list)) {
  # скорректированный R-квадрат
  df.goodness.of.fitlog[i, 'R.2.скорр'] <- 
    round(summary(modelslog.list[[i]])$adj.r.squared, 3)
  # F расчётное
  df.goodness.of.fitlog[i, 'F.расч'] <- 
    round(summary(modelslog.list[[i]])$fstatistic[1], 2)
  # стандартная ошибка
  df.goodness.of.fitlog[i, 'Станд.Ошибка'] <- 
    round(summary(modelslog.list[[i]])$sigma, 1)
}
df.goodness.of.fitlog

table1 <- data.frame(df.goodness.of.fitlog)
kable(table1)
# 6. Сохранение нужных объектов рабочего пространства  -------------------------
save(list = c('data.fit.X1.fo', 'data.fit.X2.fo', 'DF', 'reg.df',
              'models.list', 'data.fitlog.X1.fo', 'data.fitlog.X2.fo', 'DF', 'reglog.df',
              'modelslog.list'), 
     file = 'Example_model.RData')
