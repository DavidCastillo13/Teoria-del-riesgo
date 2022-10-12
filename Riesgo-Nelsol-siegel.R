
###### Nelson siegel
######### Autores ############
##### Juan Castillo   ########
##### Carlos Gonzales ########


library(lmtest)
library(Metrics)

tasa = c(0.42,0.775,1.235,1.72,1.85,1.8442,2.2849,2.796,3.01) ## Tasas en 100%

tau= 3
x1=c()
x2=c()
for (i in 1:9) {
  meses= c(1,3,6,9,12,24,60,120,360)
  años=meses/12
  x1[i] = (1-exp(-años[i]/tau))/(años[i]/tau)
  x2[i] = ((1-exp(-años[i]/tau))/(años[i]/tau))-exp(-años[i]/tau)
}


m1=lm(tasa ~ x1+x2)
summary(m1)

ns=c()
for (i in 1:9) {
  ns[i]=m1$coefficients[1] + m1$coefficients[2]*x1[i] + m1$coefficients[3]*x2[i]
}

plot(años,ns, type = "l", main = "Curva estructura a termino", ylab = "tasa spot", xlab = "años" , ylim=c(0,3), xlim = c(años[1],años[9]))
lines(años,tasa,type="l",col="red",lty=2)
legend("topleft", legend = c("Nelson-Siegel","Real"),
       col=c("black", "red"),
       lty = c("solid", "solid"), bty = "n")

###### Pruebas sobre los residuales 

t.test(m1$residuals)
shapiro.test(m1$residuals)
bptest(m1) 

plot(tasa,años, type = "l")

rmse(tasa, ns)
12*5
#### Gracias por la lectura

  