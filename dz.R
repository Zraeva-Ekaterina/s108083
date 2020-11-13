#task5 
#1
set.seed(11)
x<- rnorm(n = 300,mean = 12,sd = 12)
y<-100-12*x+rnorm(300,0,5)

#2-3
png("plot-ex05.png",866,553)
par(fig=c(0.25,0.8,0.35,1))
plot(x,y)#graf
par(fig=c(0.25,0.8,0,0.5),new=TRUE)
boxplot(x,horizontal=TRUE)
par(fig=c(0,0.3,0.35,1),new=TRUE)
boxplot(y)
dev.off()

par(mfrow=c(1,1))

#task6

#1
png("plot-ex06.png",866,553)
par(mfrow=c(2,1))
#histogram of x
hist(x,freq = F,col = "grey")
#fact plotnost
lines(density(x),col="darkgreen", lwd=3)
curve(dnorm(x,12,12), col = "red",lwd=2,add=T)

#2
hist(y,freq = FALSE,col = "grey")
curve(dnorm(x, mean=mean(y),sd=sd(y)), col = "darkgreen",lwd=2,add=T)
lines(density(y),col="red", lwd=3)

par(mfrow=c(1,1))
dev.off()
getwd()

#task7
png("plot-ex07.png",866,553)
par(mfrow=c(1,2))

y2007<-c(8884.7,11054.3,3555.7,3945.2)
y2017<-c(22946.6,22903.4,20230.8,6806.6)
names(y2007)<-c("Налог на пр.орг.","НДФЛ","Н. на имущ.","Безвозвр. поступ.")
names(y2017)<-c("Налог на пр.орг.","НДФЛ","Н. на имущ.","Безвозвр. поступ.")
pie(y2007,main="2007")
pie(y2017,main="2017")
par(mfrow=c(1,1))
dev.off()