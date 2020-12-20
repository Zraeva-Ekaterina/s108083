library('Hmisc')         
library('corrplot')
library('nortest') 
library('knitr')
# 1.Importing data  ------------------------------------------------------------
DF<-read.csv2("Data_Zraeva.csv",
              encoding = "UTF-8",
              stringsAsFactors = F,  
              na.strings = '')
View(DF)
dim(DF)

reg.df <- DF[as.numeric(row.names(DF)) < 1000, -1]

reg.df <- na.omit(reg.df)

reglog.df<-log10(reg.df[3:7])
# 2. Analysis of data distribution ----------------------------------------------
# Histogram ==================================================================
par(mfrow = c(3, 2))           
par(oma = c(0, 0, 1.5, 0))    
par(mar = c(4, 4, 0.5, 0.5)) 


for (i in 3:7) {
  
  x <- reg.df[, i]
  
 
  hist(x,
       freq = F,          
       col = 'wheat',      
       xlab = colnames(reg.df)[i],    
       ylab = 'Density',            
       main = '')                     
  
 
  curve(dnorm(x, mean = mean(x), sd = sd(x)), col = 'darkblue', 
        lwd = 2, add = TRUE)
}


title(main = 'Histograms of the distribution of indicators', 
      outer = TRUE, cex = 1.5)



par(mfrow = c(1, 1))

# Normality tests Shapiro Wilka ========================================================
shapiro.test(reg.df$y)


apply(reg.df[, 3:7], 2, shapiro.test)


str(shapiro.test(reg.df$y))


apply(reg.df[, 3:7], 2, function (x) {
  round(shapiro.test(x)$statistic, 2)
})
peremennaia <- sapply(reg.df[, 3:7], function (x) { round(shapiro.test(x)$statistic, 2)})
table <- data.frame(peremennaia)
kable(table)

#3. Analysis of linear relationships  ---------------------------------------------
# Scatter charts =============================================================
pairs(reg.df[3:7],     
      pch = 21,        
      col = rgb(0, 0, 1, alpha = 0.4),  
      bg = rgb(0, 0, 1, alpha = 0.4),   
      cex = 1.1)                

# Correlation matrix  ======================================================
matrix.cor <- cor(reg.df[3:7])


matrix.p <- rcorr(as.matrix(reg.df[3:7]))$P


corrplot(matrix.cor,  
         method = c("shade"),
         addshade = "all", 
         order = 'original',   
         
         diag = F,           
         p.mat = matrix.p,  
         insig = 'blank',    
         sig.level = 0.05) 

# 4. Analysis of data distribution for log ----------------------------------------------
# Histogram for log==================================================================

par(mfrow = c(3, 2))
par(oma = c(0, 0, 1.5, 0))
par(mar = c(4, 4, 0.5, 0.5))  


for (i in 1:5) {
 
  x <- reglog.df[, i]
  
 
  hist(x,
       freq = F,           
       col = 'wheat',      
       xlab = colnames(reglog.df)[i],    
       ylab = 'Density',            
       main = '')                      
  
 
  curve(dnorm(x, mean = mean(x), sd = sd(x)), col = 'darkblue', 
        lwd = 2, add = TRUE)
}


title(main = 'Histograms of the distribution of indicators', 
      outer = TRUE, cex = 1.5)



par(mfrow = c(1, 1))

# Normality tests Shapiro-Wilka for log========================================================
shapiro.test(reglog.df$y)


apply(reglog.df[, 1:5], 2, shapiro.test)


str(shapiro.test(reglog.df$y))


apply(reglog.df[, 1:5], 2, function (x) {
  round(shapiro.test(x)$statistic, 2)
})
peremennaialog <- sapply(reglog.df[, 1:5], function (x) { round(shapiro.test(x)$statistic, 2)})
table1 <- data.frame(peremennaialog)
kable(table1)

#5. Analysis of linear relationships for log  ---------------------------------------------
# Scatter charts for log=============================================================

pairs(reglog.df[1:5],   
      pch = 21,         
      col = rgb(0, 0, 1, alpha = 0.4),   
      bg = rgb(0, 0, 1, alpha = 0.4),   
      cex = 1.1)               
# Correlation matrix for log ======================================================

matrix.cor <- cor(reglog.df[1:5])


matrix.p <- rcorr(as.matrix(reglog.df[1:5]))$P


corrplot(matrix.cor, 
         method = c("shade"),
         addshade = "positive", 
         order = 'original',  
        
         diag = F,           
        
         p.mat = matrix.p,  
         insig = 'blank',    
         sig.level = 0.05)   

# 6. Keeping your workspace ------------------------------------------

ls()

save(list = c('DF', 'reg.df', 'reglog.df'), file = 'Example.RData')