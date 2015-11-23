### Bern Romey, Lab 4, Spatial Multivariate Regression

data <- read.csv("county_water.csv")
head(data)
dat <-data[,c(4:20)]

lin <- lm(TWA_AREA ~ .,data=dat)
summary(lin)
coefficients(lin) # model coefficients
# confint(lin, level=0.95) # CIs for model parameters 
# fitted(lin) # predicted values
# residuals(lin) # residuals
#anova(lin) # anova table 
#vcov(lin) # covariance matrix for model parameters 
#influence(lin) # regression diagnostic

dt<-data[,c(5:7,10,13,20)]
head(dt)

lshap <- lapply(dt, shapiro.test) #shapiro test for normality; used to select best norm for lm 
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres) #transposed

par(mfrow = c(2,1))
boxplot(dt)
cor.matrix(dt) # source cor.matrix.r function, for pearson correlation coefficient

###none are normal, log transform
ln <- log(dt+.001)
boxplot(ln)
cor.matrix(ln) #matrix correlation plot

lshap <- lapply(ln, shapiro.test) #shapiro test for normality; used to select best norm for lm 
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres) #transposed
### After log transformation, T_MAX, and POP data look to be the only normal distributions.

##### --linear model analysis-- ####

mod <- lm(TWA_AREA ~ .,data=ln) #Multi Linear regression with all variables with log transformed data
summary(mod) # what vaiables are significant

attach(ln) #makes available so dont need to reference object, just header
par(mfrow = c(2,2))
plot(TWA_AREA~Per_Urban+Per_irriga+T_MAX+PPT_ANL_01)
detach(ln) #removes ln data

lm <- lm(TWA_AREA ~ Per_Urban + Per_irriga, data = ln) #Reduced multilinear model
summary(lm)
coefficients(lm) # model coefficients
confint(lm, level=0.95) # CIs for model parameters 
#fitted(lm) # predicted values
#residuals(lm) # residuals
anova(lm) # anova table 
vcov(lm) # covariance matrix for model parameters 
#influence(lm) # regression diagnostic

par(mfrow = c(3,2))
plot(lm, which = 1:6)
### Assumption (~a~).  Random errors have a mean of zero, linearity.  Residuals should be normaly distributed about 
### the zero line for the residuals vs fitted (y-hat) plot.  In this chase they are are pretty close, 
### thus linearity assumption is met.
### Assumption (~b~).  Random error terms uncorrelated. If correlated, the multi-collinearity exists.  Use VIF test above.
### Assumption (~c~). Random error terms have a constant variance (homoscedasticity). Scale-locatin plot should be uniform from 
### left to right.  In this case it is not, slight horn shape. Also shows up in the Residuals vs Fitted plot.
### Assumption (~d~).  Random error tersm follow a normal distribution.  Residuals Q-Q plot. witht he exception
### of three observation, they are normaly distributed.
### Cooks distance shows what values are outliers and may bias the model.  Recommended to remove 199, then re-run model

library(lmtest) #tests for regression
gqtest(lm) # Goldfeld-Quandt test for homoscedasticity, if p-value < 0.05 reject null of homoscedastisity, there is some heteroscedastisity
dwtest(lm) # Autocorrelation test

##### ~reduced~ ####
ln.r<-ln[-c(141,152,199),] #removed observations
lm.r <- lm(TWA_AREA ~ Per_Urban + Per_irriga, data = ln.r) #Reduced multilinear model
confint(lm.r) # Confidence interval
par(mfrow = c(3,2))
plot(lm.r, which = 1:6)

summary(lm.r) # Intercept, slope, R^2, residuals standard error
anova(lm.r)

require(gvlma) # global validation, includes hetroscedasticity
gvmodel <- gvlma(lm.r) 
summary(gvmodel)  

require (car) #Variance inflation factor:  If > 3, then multi-colinearity may be a problem, if > 10 severe
vif(lm.r)

### Compare models, are they significantly different?
anova(lm, mod) 

infl <- influence.measures(lm.r)
which(apply(infl$is.inf, 1, any)) # which observations are influential

summary(infl) # only the influential ones
infl          # all, with influance column




