
library(spatstat)
library(maptools)

park <- readShapePoints("shape/Parks")#readShapeSpatial(file.shp) works same
class(park)
plot(park)
summary(park)
pks <- as.ppp(park)# In this conversion, the data frame of additional variables in x will become the marks of the point pattern z

### Quadrat analysis
par(mfrow=c(2,2))
par(mar=c(5,3,3,2)+0.1)#mar is number of lines of margin, c(bottom, left, top, right), +3.5 is magnification
plot(park, main="Quadrat counts", pch=1, col="darkgreen")
tab <- quadratcount(pks, 4)
plot(tab, add=TRUE, lty=2, cex=2, col="blue")

plot(park, pch=1)  
title(main=expression(chi^2 * " test"), cex.main=2)
tes <- quadrat.test(pks, method="Chisq",4)#spatial randomness test for point pattern by quadrat counts
tes
plot(tes, add=TRUE, col="red", cex=1.5, lty=2, lwd=3)
title(sub=paste("p-value =", signif(tes$p.value,3)), cex.sub=1.4)
### quadrat.test {spatstat}
### If method="Chisq" then a chi^2 test of goodness-of-fit is performed by comparing 
### the test statistic to the chi^2 distribution with m-k degrees of freedom, 
### where m is the number of quadrats and k is the number of fitted parameters (equal to 1 for quadrat.test.ppp). 
### The default is to compute the two-sided p-value, so that the test will be declared significant 
### if X^2 is either very large or very small. One-sided p-values can be obtained by specifying the alternative. 
### An important requirement of the chi^2 test is that the expected counts in each quadrat be greater than 5.

tes2 <-quadrat.test(pks, method="M", nsim=4999, conditional=FALSE)
tes2
plot(tes2, main=expression("Monte Carlo"), cex.main=2)
title(sub=paste("p-value =", signif(tes2$p.value,3)), cex.sub=1.4)
### If method="MonteCarlo" then a Monte Carlo test is performed, obviating the need for all expected counts to be at least 5. 
### In the Monte Carlo test, nsim random point patterns are generated from the null hypothesis 
### (either CSR or the fitted point process model). The Pearson X^2 statistic is computed as above. 
### The p-value is determined by comparing the X^2 statistic for the observed point pattern, with the values obtained from the simulations. 
### Again the default is to compute the two-sided p-value.