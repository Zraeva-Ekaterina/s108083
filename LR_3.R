# Loading the libraries
library('lmtest')     # tests of residues: bptest(), dwtest()
library('broom')      # data transformations: augment()
library('car')        # test for multicollinearity: vif()
library('sandwich')   # model estimates adjusted for heteroscedasticity: vcovHC()
library('knitr')
library('stats')     
library('Hmisc')          
library('corrplot')       
library('nortest')       

# 1. Importing data  ------------------------------------------------------------

# loading objects from a saved workspace
load('Example_model.RData')
# viewing a list of objects
ls()
# model names in the list
names(models.list)
names(modelslog.list)

# 2. Balance charts  ---------------------------------------------------------

#  cycle through the models in the list models.list
for (i in 1:length(models.list)) {
  # open the output to a file
  png(paste('RPlot', i, '.png', sep = ''), height = 500, width = 500)
  
  # divide the canvas into four parts
  par(mfrow = c(2, 2))
  
  # draw 4 graphs for the same model
  plot(models.list[[i]], 1)
  plot(models.list[[i]], 2)
  plot(models.list[[i]], 3)
  plot(models.list[[i]], 4)
  
  # adding a General title with the model name
  mtext(paste('The remains of the model ', names(models.list)[i], sep = ''), 
        side = 3, line = -2, outer = TRUE, cex = 1.2)
  par(mfrow = c(1, 1))
  
  # closing the output to a file
  dev.off()
}

# The regions with the numbers 25 & 71
DF[rownames(DF) %in% c(25, 71), c('Label', 'FO')]

# working with the fourth model
# find the Cook's distances for the influencing regions
h <- augment(models.list[[4]], reg.df)
lev <- h[rownames(reg.df) %in% c(25, 71), '.cooksd', drop = F]

# median F-value - threshold for cutting off influencing factors
n <- nrow(reg.df)
p <- nrow(summary(fit.X2.fo)$coeff) - 1
f.median <- qf(1 - 0.5, df1 = p, df2 = n - p)
# the threshold = 1
cut.1 <- 1
# the threshold = 4 / n
cut.4.n <- round(4 / nrow(reg.df), 2)

# compare the calculated values with the thresholds
cbind(leverage = round(lev,2), f.median = round(f.median,2),
      cut.1, cut.4.n)
table1 <- data.frame(cbind(leverage = round(lev,2), f.median = round(f.median,2),
                           cut.1, cut.4.n))
kable(table1)


# 3. Проверка равенства среднего остатков нулю  --------------------------------

# номер модели
i <- 4
# t-тест для среднего
t.test(models.list[[i]]$residuals, mu = 0, alternative = 'two.sided')
table15<- t.test(models.list[[i]]$residuals, mu = 0, alternative = 'two.sided')$p.value
kable(table15,caption = "P-value")
# 4. Checking the constancy of the average residuals ------------------------------------

# model number
i <- 4
# first half of leftovers
res.s1 <- fit.X2.fo$residuals[1:(n / 2)]

# the second half of the residues
res.s2 <- fit.X2.fo$residuals[(n / 2):n]

# t-test for equality of averages
t.test(res.s1, res.s2, alternative = 'two.sided')



# 5. Detection of heteroskedasticity  -----------------------------------------

# model number in the list
i <- 4


# test Breush-pagan ===========================================================
bptest(models.list[[i]])

# добавляем в исходную таблицу h прогнозы, остатки из модели model
h <- augment(models.list[[i]], reg.df)
str(h) # смотрим структуру таблицы h
table16<- bptest(models.list[[i]])$p.value
kable(table16,caption = "P-value test Breush-pagan")

###################################################

# тест Уайта ===================================================================
# Во вспомогательной регрессии e^2 зависят от X и X^2
# для моделей 1-2 X: Rural.2011; для моделей 3-4 X:
bptest(models.list[[i]], data = h, 
       varformula = ~ x2 + I(x2^2))

# тест Голдфельда-Квандта ======================================================
gqtest(models.list[[i]], order.by = ~ x2, 
       data = h, fraction = 0.2)


# Тест Глейзера ================================================================
# вектор степеней независимой переменной
beta.vector <- seq(-1, 1.5, by = 0.05)
beta.vector <- beta.vector[beta.vector != 0]

# строим вспомогательные регрессии, и если коэффициент модели 
#  значим, выводим p-значение и степень.
#  для моделей 1-2 X: Rural.2011; для моделей 3-4 X: Injury.2011
for (j in 1:length(beta.vector)) {
  gl.test <- lm(abs(.std.resid) ~ I(x2^beta.vector[j]), data = h)
  if (summary(gl.test)$coef[2, 4] < 0.05) {
    # если найдена значимая модель по тесту Глейзера,
    #  появится сообщение в консоли
    message(paste0('! >>> Модель значима >>> ', 
                   'beta = ', round(beta.vector[j], 2), 
                   'p-value = ', round(summary(gl.test)$coef[2, 4], 4)))
  } else {
    # если модель незначима, тоже пишем в консоль
    message(paste0('Модель для beta = ', round(beta.vector[j], 2), 
                   ' незначима'))
  }
}



# 6. Обнаружение автокорреляции  -----------------------------------------------

# номер модели в списке
i <- 4

# тест Дарбина-Уотсона на автокорреляцию
dwtest(models.list[[i]], alternative = 'two.sided')

# автокорреляционный коэффициент первого порядка для остатков
n <- nrow(reg.df)
cor.test(x = models.list[[i]]$residuals[1:(n - 1)],
         y = models.list[[i]]$residuals[2:n])
table17<- dwtest(models.list[[i]], alternative = 'two.sided')$p.value
kable(table17,caption = "P-value DWtest")


# 7. Переоценка параметров модели с учётом ошибок  -----------------------------

# оценки параметров модели по МНК. для примера: модель 1
i <- 1

# исходные коэффициенты и их стандартные ошибки
coeftest(models.list[[i]])

# робастные оценки стандартных ошибок моделей
# vcovHC(): оценка ковариационной матриц, устойчивая к гетероскедастичности
# vcovHAC(): оценка ковариационной матриц, устойчивая к гетероскедастичности
#  и автокорреляции
coeftest(models.list[[i]], vcov. = vcovHAC(models.list[[i]])) # гетероскедастичность и автокорреляция
# NB: сами оценки параметров не меняются,
#  меняются их стандартные ошибки, и выводы по значимости могут измениться


# 8. Обнаружение мультиколлинеарности  -----------------------------------------

# VIF-тест на мультиколлинеарность факторов 
#  NB: применяется для множественной регрессии с непрерывными факторами
round(vif(models.list[[2]]), 2)
table18<- round(vif(models.list[[2]]), 2)
kable(table18,caption = "VIF-тест")