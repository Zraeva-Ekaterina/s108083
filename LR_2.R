library('stats')      
library('Hmisc')          
library('corrplot')       
library('nortest')        
library('knitr')

# 1. ------------------------------------------------------------


load('Example.RData')

ls()

dim(reg.df)
head(reg.df)
str(reg.df)
dim(reglog.df)
head(reglog.df)
str(reglog.df)

# 2.  ------------------------------------------------


fit.1 <- lm(y ~ x1 + x2, 
            data = reg.df)
summary(fit.1)  
fit.2 <- lm(y ~ x1, 
            data = reg.df)
summary(fit.2)  fit.X1.fo <- lm(y ~ x1 * FO , 
                data = reg.df)
summary(fit.X1.fo)
X.matrix <- model.matrix(y ~ x1 * FO, data = reg.df)
head(X.matrix)

data.fit <- cbind(y = reg.df$y, 
                  data.frame(X.matrix)[, -1])
head(data.fit)
tail(data.fit)

data.fit.X1.fo <- data.fit
source('https://raw.githubusercontent.com/aksyuk/R-Practice-basics/master/user_functions/removeFactorsByPValue.R')

fit.X1.fo <- removeFactorsByPValue(data = data.fit, 
                                   y.var.name = 'y')
summary(fit.X1.fo)
fit.X2 <- lm(y ~ x2, 
             data = reg.df)
summary(fit.X2)

fit.X2.fo <- lm(y ~ FO + x2, 
                data = reg.df)
summary(fit.X2.fo)

X.matrix <- model.matrix(y ~ x2 * FO, data = reg.df)
data.fit <- cbind(y = reg.df$y, 
                  data.frame(X.matrix)[, -1])
head(data.fit)
tail(data.fit)

data.fit.X2.fo <- data.fit

fit.X2.fo <- removeFactorsByPValue(data = data.fit, 
                                   y.var.name = 'y')
summary(fit.X2.fo)
fit.X2.fo <- removeFactorsByPValue(data = data.fit, 
                                   y.var.name = 'y',
                                   p.adj.method = 'bonferroni')
summary(fit.X2.fo)

fit.X2.fo <- removeFactorsByPValue(data = data.fit, 
                                   y.var.name = 'y',
                                   p.adj.method = 'bonferroni',
                                   alpha = 0.10)
summary(fit.X2.fo)



# 3. --------------------------------------------------------

anova(fit.2, fit.X1.fo)

anova(fit.2, fit.X2.fo)
table7 <- data.frame(anova(fit.2, fit.X1.fo))
kable(table7)
table8 <- data.frame(anova(fit.2, fit.X2.fo))
kable(table8)
str(summary(fit.2))
summary(fit.2)$adj.r.squared
summary(fit.2)$fstatistic
summary(fit.2)$fstatistic[1]
summary(fit.2)$sigma

models.list <- list(fit.2, fit.X1.fo, fit.X2, fit.X2.fo)
names(models.list) <- c('fit.2', 'fit.X1.fo', 'fit.X2', 'fit.X2.fo')

df.goodness.of.fit <- data.frame(Model = names(models.list), 
                                       R.2.skorr = 0,
                                       F.rasch = 0,
                                       standart.oshibka = 0)
for (i in 1:length(models.list)) {
  df.goodness.of.fit[i, 'R.2.скорр'] <- 
    round(summary(models.list[[i]])$adj.r.squared, 3)
  df.goodness.of.fit[i, 'F.расч'] <- 
    round(summary(models.list[[i]])$fstatistic[1], 2)
  df.goodness.of.fit[i, 'Станд.Ошибка'] <- 
    round(summary(models.list[[i]])$sigma, 1)
}
df.goodness.of.fit
table3 <- data.frame(df.goodness.of.fit)
kable(table3)

# 4. ------------------------------------------------

fitlog.1 <- lm(y ~ x1 + x2 + x4, 
            data = reglog.df)
summary(fitlog.1)  
fitlog.2 <- lm(y ~ x1 + x2, 
            data = reglog.df)
round(summary(fitlog.2)$coef,4) 
fitlog.X1 <- lm(y ~ x1, 
             data = reglog.df)
summary(fitlog.X1)
FO<-reg.df[2]
reglog.df<-c(reglog.df,FO)
fitlog.X1.fo <- lm(y ~ x1 * FO , 
                data = reglog.df
                )
summary(fitlog.X1.fo)
X.matrixlog <- model.matrix(y ~ x1 * FO, data = reglog.df)
head(X.matrixlog)

data.fitlog <- cbind(y = reglog.df$y, 
                  data.frame(X.matrixlog)[, -1])
head(data.fitlog)
tail(data.fitlog)

data.fitlog.X1.fo <- data.fitlog
source('https://raw.githubusercontent.com/aksyuk/R-Practice-basics/master/user_functions/removeFactorsByPValue.R')

fitlog.X1.fo <- removeFactorsByPValue(data = data.fitlog, 
                                   y.var.name = 'y')
summary(fitlog.X1.fo)
fitlog.X2 <- lm(y ~ x2, 
             data = reglog.df)
summary(fit.X2)

fitlog.X2.fo <- lm(y ~ FO + x2, 
                data = reglog.df)
summary(fitlog.X2.fo)

X.matrixlog <- model.matrix(y ~ x2 * FO, data = reglog.df)
data.fitlog <- cbind(y = reglog.df$y, 
                  data.frame(X.matrixlog)[, -1])
head(data.fitlog)
tail(data.fitlog)

data.fitlog.X2.fo <- data.fitlog

fitlog.X2.fo <- removeFactorsByPValue(data = data.fitlog, 
                                   y.var.name = 'y')
summary(fitlog.X2.fo)

fitlog.X2.fo <- removeFactorsByPValue(data = data.fitlog, 
                                   y.var.name = 'y',
                                   p.adj.method = 'bonferroni')
summary(fitlog.X2.fo)

fitlog.X2.fo <- removeFactorsByPValue(data = data.fitlog, 
                                   y.var.name = 'y',
                                   p.adj.method = 'bonferroni',
                                   alpha = 0.10)
summary(fitlog.X2.fo)


# 5.  --------------------------------------------------------

anova(fitlog.X1, fitlog.X1.fo)

anova(fitlog.X1, fit.X2.fo)
table2 <- data.frame(anova(fitlog.X1, fitlog.X1.fo))
kable(table2)
table9 <- data.frame(anova(fitlog.X1, fit.X2.fo))
kable(table9)
str(summary(fitlog.X1))
summary(fitlog.X1)$adj.r.squared
summary(fitlog.X1)$fstatistic
summary(fitlog.X1)$fstatistic[1]
summary(fitlog.X1)$sigma

modelslog.list <- list(fitlog.X1, fitlog.X1.fo, fitlog.X2, fitlog.X2.fo)
names(modelslog.list) <- c('fitlog.X1', 'fitlog.X1.fo', 'fitlog.X2', 'fitlog.X2.fo')

df.goodness.of.fitlog <- data.frame(Model = names(modelslog.list), 
                                       R.2.scorr = 0,
                                       F.rasch = 0,
                                       standart.oshibka = 0)
for (i in 1:length(modelslog.list)) {
  df.goodness.of.fitlog[i, 'R.2.скорр'] <- 
    round(summary(modelslog.list[[i]])$adj.r.squared, 3)
  df.goodness.of.fitlog[i, 'F.расч'] <- 
    round(summary(modelslog.list[[i]])$fstatistic[1], 2)
  df.goodness.of.fitlog[i, 'Станд.Ошибка'] <- 
    round(summary(modelslog.list[[i]])$sigma, 1)
}
df.goodness.of.fitlog

table1 <- data.frame(df.goodness.of.fitlog)
kable(table1)
save(list = c('data.fit.X1.fo', 'data.fit.X2.fo', 'DF', 'reg.df',
              'models.list', 'data.fitlog.X1.fo', 'data.fitlog.X2.fo', 'DF', 'reglog.df',
              'modelslog.list'), 
     file = 'Example_model.RData')
