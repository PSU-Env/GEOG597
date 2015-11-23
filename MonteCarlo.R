####Bern GEOG597 Spatial analysis
library(spatstat)
demo(spatstat)
#demo(data)

library(maptools)
park <- readShapePoints("shape/Parks")#readShapeSpatial(file.shp) works same
class(park)
plot(park)
summary(park)
pks <- as.ppp(park)# In this conversion, the data frame of additional variables in x will become the marks of the point pattern z

pk <- as(pks, "ppp")
#pk.dta <- slot(pks, "data")
class(pk)

K <- Kest(pk)
plot(K, main="K function for Parks", legendmath=TRUE)
en <- envelope(pk, fun=Kest, nsim=10, correction="translate")
plot(en, main="Envelopes of K function based on CSR", shade=c("hi", "lo"))

plot(pk, cols=c("green", "blue"), main="Multitype point pattern")

###Near neigbor
X <- unique(unmark(pk))
plot(X, "Park Nearest Neighbor", pch=".")
m <- nnwhich(pks)
b <- pks[m]
arrows(pks$x, pks$y, b$x, b$y,#nearest neighbor
         angle=12, length=0.1, col="red")

###
plot(pks %mark% nndist(pks),
       markscale=1, main="Stienen diagram", legend=FALSE, fg="blue")


plot(pks, markscale=0.09, main="markscale=0.09")           
par(opa)
plot(pks, pch=21, cex=1,
       bg=colourmap(terrain.colors(128), range=c(0,80)),
       main="colourmap for numeric mark values")

###quadrant counts
par(mar=c(3,3,3,2)+0.1)
plot(pks, main="Quadrat counts", pch="+")

tab <- quadratcount(pks, 4)
plot(tab, add=TRUE, lty=2, cex=2, col="blue")
par(mar=c(5,3,3,2)+0.1)
plot(pks, main="", pch="+")

###Chi-sq
title(main=expression(chi^2 * " test"), cex.main=2)
tes <- quadrat.test(pks, 3)
tes
plot(tes, add=TRUE, col="red", cex=1.5, lty=2, lwd=3)
title(sub=paste("p-value =", signif(tes$p.value,3)), cex.sub=1.4)
par(mar=c(4,4,3,2)+0.1)
tesk <- cdf.test(nztrees, "x")
tesk
plot(tesk)

###Kernel smoothed intensity pattern
Z <- density(pks, 1200)
plot(Z, main="Kernel smoothed intensity of point pattern")

###Nearest Neighbours
pc <- pcf(pks)
plot(pc, main="Pair correlation function")
plot(pks, main="nearest neighbours")

###Locally-scaled K function
par(mfrow=c(1,2))
X <- unmark(pks)
plot(X, "Parks data")
lam <- predict(ppm(X ~x))
plot(Kscaled(X, lam), xlim=c(0, 1.5), main="Locally-scaled K function")

##
te <- scan.test(pks, 0.1, method="poisson")
plot(te, main=c("Scan Statistic for parks data",
                paste("p-value =", signif(te$p.value,3))))
plot(pks, add=TRUE)
te

##
miplot(pks, main="Morishita Index plot", pch=16, col="blue")





