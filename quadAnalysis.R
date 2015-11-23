### Erin, copy your parks shapefiles from lab 1 and put them in a folder called shape, 
### then run the following code for quadrat chi^2 analysis.

#### ~~~Data~~~ ####
library(spatstat)
library(maptools) #used to convert shape file to sp format used in spatstat package
park <- readShapePoints("shape/Parks") #directory that shape file is stored
class(park) # displays the class of the shape file
plot(park) # basic plot of shapfile
summary(park) # Descriptive information
pks <- as.ppp(park)# Converts shapefile to spatstat format, the data frame of additional variables in x will become the marks of the point pattern z

#### ~*~ Quadrat analysis ~*~ ####
par(mar=c(5,3,3,2)+0.1)
plot(park, main="Quadrat counts", pch=1, col="darkgreen")
tab <- quadratcount(pks, 4) #creates quadrants and counts the number of observation in each quadrant.
plot(tab, add=TRUE, lty=2, cex=2, col="blue")

plot(park, pch=1, col="grey52")  
title(main=expression(chi^2 * " test"), cex.main=2)
tes <- quadrat.test(pks, method="Chisq",4) #spatial randomness test for point pattern by quadrat counts. 
tes
plot(tes, add=TRUE, col="Darkgreen", cex=1.5, lty=2, lwd=3) #Includes cell count, mean, and Z-score for each cell
title(sub=paste("p-value =", signif(tes$p.value,3)), cex.sub=1.4) #Includes poisson distribution p-value

###Nearest neighbor plot
par(mar=0.2+c(1,1,3,1))
plot(park, main="Nearest neighbor", bty="n", pch=2, cex.main=2,col=211)
box(lwd=2) 
m <- nnwhich(pks)
b <- pks[m]
arrows(pks$x, pks$y, b$x, b$y,
       angle=12, length=0.1, col="darkgreen")

### Nearest neighbor: K Function - average number of evenst inside a circle of radius d centered on an event.
K <- Kest(pks)
plot(K, main="K function for Parks data", legendmath=TRUE)
en <- envelope(pks, fun=Kest, nsim=10, correction="translate")
plot(en, main="K function based on CSR", shade=c("hi", "lo")) #CSR: complete spatial randomness
### Where the black observed line falls below the grey envelope we may infer that points are 
### further apart than would be expected by chance and vice versa above the envelope.

