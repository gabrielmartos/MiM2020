library(MASS)
require(splines) || install.packages('splines'); library(splines)

data(mcycle)
attach(mcycle)
regpol = lm(accel ~ poly(times,9))
par(mar=c(4,4,1,1))
plot(times,accel, pch = 20, xlab = 'X', ylab ='Y')
points(times, regpol$fitted.values, type='l', col=rgb(0,0,1,1) , lwd = 4, lty = 2)

spline = lm(accel ~ bs(times, df = 10 ,degree = 3)) 
points(times,spline$fitted.values, type='l', col=rgb(1,0,0,0.5) , lwd = 4, lty = 2)




x  = seq(0,1,by=0.01)
knots=c(0.25,0.5,0.75)
par(mar = c(5,5,1,1))
plot(x,x,type = 'l', xlab = 'x', ylab =expression(paste('h(x,',nu,')')), 
     col = 'gray', cex.lab = 2)
points(x,x^2,type = 'l', col = 'gray')
points(x,x^3,type = 'l', col = 'gray')
points(x,pmax(0,x-knots[1])^3,type = 'l', xlab = 'x', ylab =expression(phi), col = 'red', lwd = 2)
points(x,pmax(0,x-knots[2])^3,type = 'l', xlab = 'x', ylab =expression(phi), col = 'blue', lwd = 2)
points(x,pmax(0,x-knots[3])^3,type = 'l', xlab = 'x', ylab =expression(phi), col = 'green' ,lwd = 2)
abline(v = knots[1], col = 'red', lty = 3)
abline(v = knots[2], col = 'blue', lty = 3)
abline(v = knots[3], col = 'green', lty = 3)