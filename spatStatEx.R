### Bern Romey HW1 spatstat analysis & examples
#demo(spatstat)
library(spatstat)
library(maptools)

park <- readShapePoints("shape/Parks")#readShapeSpatial(file.shp) works same
class(park)
plot(park)
summary(park)
pks <- as.ppp(park)# In this conversion, the data frame of additional variables in x will become the marks of the point pattern z

### Quadrat analysis
par(mfrow=c(2,1))
par(mar=c(5,3,3,2)+0.1)#mar is number of lines of margin, c(bottom, left, top, right), +3.5 is magnification
plot(park, main="Quadrat counts", pch=1, col="darkgreen")
tab <- quadratcount(pks, 4)
plot(tab, add=TRUE, lty=2, cex=2, col="blue")

plot(park, pch=1)  
title(main=expression(chi^2 * " test"), cex.main=2)
tes <- quadrat.test(pks, method="Chisq",4)#spatial randomness test for point pattern by quadrat counts
tes
plot(tes, add=TRUE, col="Darkgreen", cex=1.5, lty=2, lwd=3)
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

par(mar=c(5,3,3,2)+0.1)
tesk <- cdf.test(pks, "x")
tesk
plot(tesk)

######
plot(cells, add=TRUE)
plot(park)
te <- scan.test(pks, 0.1, method="poisson")

plot(te, main=c("Scan Statistic for pks data",
        paste("p-value =", signif(te$p.value,3))))

plot(pks, add=TRUE)
te

#####
K <- Kest(pks)
plot(K, main="K function for Parks", legendmath=TRUE)
en <- envelope(pks, fun=Kest, nsim=10, correction="translate")
plot(en, main="Envelopes of K function based on CSR", shade=c("hi", "lo"))

pc <- pcf(pks)
plot(pc, main="Pair correlation function")

###Nearest neighbor
par(mar=0.2+c(1,1,3,1))
plot(park, main="Nearest neighbor", bty="n", pch=2, cex.main=2,col=211)
box(lwd=2) 
m <- nnwhich(pks)
b <- pks[m]
arrows(pks$x, pks$y, b$x, b$y,
        angle=12, length=0.1, col="darkgreen")


# nearest neighbours of each type
head(nnwhich(ants, by=marks(ants)))



plot(pks %mark% nndist(pks),
     markscale=1, main="Stienen diagram", legend=FALSE, fg="blue")


plot(Gest(pks),
    main=c("Nearest neighbour distance function G", "Gest(pks)"),
    legendmath=TRUE)

Z <- distmap(pks, dimyx=512)
plot(pks$window, main="Distance map of parks") 
plot(Z, add=TRUE)
points(pks, col="red")

plot(Fest(pks),
       main=c("Empty space function F", "Fest(pks)"),
       legendmath=TRUE)

plot(Jest(pks), main=c("J-function", "J(r)=(1-G(r))/(1-F(r))"))

X <- pks
X <- X[sample(1:npoints(X))]
Z <- nnfun(X)
plot(as.owin(X), main="Nearest neighbour map") 
plot(Z, add=TRUE)
points(X, col="black")


plot(allstats(swedishpines)) #only works on unmarked patterns


#####

#pks.extra$plotit(main="pks Pines")
  
L <- localL(pks)
pL <- plot(L, lty=1, col=1, legend=FALSE,
      main=c("neighbourhood density functions",
      "for pks"), cex.main=0.8)
  
parsave <- par(mfrow=c(1,2))
#pks.extra$plotit()

par(pty="s")
plot(L, iso007 ~ r, main="point B")
par(mar=0.2+c(1,1,3,1))

#pks.extra$plotit() 
L12 <- localL(pks, rvalue=12)
P12 <- pks %mark% L12
Z12 <- Smooth(P12, sigma=5, dimyx=128)

plot(Z12, col=topo.colors(128),
       main=c("smoothed", "neighbourhood density"),
       cex.main=0.8)

contour(Z12, add=TRUE)
points(pks, pch=16, cex=0.5)

###Density plot
plot(density(pks))
plot(park)
