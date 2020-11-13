task8<-function(xmean,xsd,emean,esd)#obyavlenie f
  {
  set.seed(11)#nachalnoe sostoyanie dlya generatora sl. chisel
  x<- rnorm(n = 300,xmean,xsd)#gener. x
  y<-100-12*x+rnorm(300,emean,esd)# y
  par(fig=c(0.25,0.8,0.35,1))#nastroykb graf 1
  plot(x,y)#graf 1
  par(fig=c(0.25,0.8,0,0.5),new=TRUE)#nastroykb graf 2
  boxplot(x,horizontal=TRUE) #graf 2
  par(fig=c(0,0.3,0.35,1),new=TRUE)#nastroykb graf 3
  boxplot(y)#graf3
  
  par(mfrow=c(1,1))#vozvrat standart nastroek
  }

