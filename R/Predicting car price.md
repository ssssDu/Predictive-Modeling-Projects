# The project aims to predict car price given multiple variables

#### First, import packages and load the dataset
library(lattice)
library(caret)
library(ggplot2)
attach(Cars)
library(glmnet)
library(tree)
library(pls)
library(randomForest)
library(gbm)
library(readr)

Cars <- read_csv("~/Downloads/Cars.csv")

#### Clean the dataset
unique(Cars$trim)
Cars$x1 <- NULL
unique(Cars$isOneOwner)
unique(Cars$color)  
unique(Cars$displacement)
unique(Cars$fuel)  
unique(Cars$state)
unique(Cars$soundSystem)
unique(Cars$wheelType)
table(subTrim)
#Hybrid   unsp 
#190  29276 
table(wheelSize)
#16    17    18    19    20    21    22  unsp 
#107   149  1774  1297   813     2    31 25293 
Cars$X1 <- NULL

#### Let's have a brief view of variables in car dataset and their distributions
summary(Cars)

#trim               condition              isOneOwner            mileage            year     
#Length:29466       Length:29466       Length:29466       Min.   :     1   Min.   :1988  
#Class :character   Class :character   Class :character   1st Qu.:    14   1st Qu.:2007  
#Mode  :character   Mode  :character   Mode  :character   Median : 26118   Median :2012  
                                                          #Mean   : 40376   Mean   :2010  
                                                          #3rd Qu.: 68176   3rd Qu.:2015  
                                                          #Max.   :488525   Max.   :2015  
                                                          #NA's   :4                      
#color            displacement           fuel              state              region         
#Length:29466       Length:29466       Length:29466       Length:29466       Length:29466      
#Class :character   Class :character   Class :character   Class :character   Class :character  
#Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character  
                                                                                      
#soundSystem         wheelType          wheelSize          featureCount        price       
#Length:29466       Length:29466       Length:29466       Min.   :  0.00    Min.   :   599  
#Class :character   Class :character   Class :character   1st Qu.: 18.00    1st Qu.: 28995  
#Mode  :character   Mode  :character   Mode  :character   Median : 53.00    Median : 56991  
                                                           #Mean   : 46.48   Mean   : 67000  
                                                           #3rd Qu.: 70.00   3rd Qu.:108815  
                                                           #Max.   :132.00   Max.   :299000  
                                                                             #NA's   :1   
 #### Get ride of NAs
Carsom = na.omit(Cars)  
summary(Carsom)

#trim            condition          isOneOwner           mileage            year     
#Length:29461       Length:29461       Length:29461       Min.   :     1   Min.   :1988  
#Class :character   Class :character   Class :character   1st Qu.:    14   1st Qu.:2007  
#Mode  :character   Mode  :character   Mode  :character   Median : 26119   Median :2012  
                                                          #Mean   : 40377   Mean   :2010  
                                                          #3rd Qu.: 68176   3rd Qu.:2015  
                                                          #Max.   :488525   Max.   :2015  

#color           displacement           fuel              state              region         
#Length:29461       Length:29461       Length:29461       Length:29461       Length:29461      
#Class :character   Class :character   Class :character   Class :character   Class :character  
#Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character  

#soundSystem         wheelType          wheelSize          featureCount        price       
#Length:29461       Length:29461       Length:29461       Min.   :  0.00   Min.   :   599  
#Class :character   Class :character   Class :character   1st Qu.: 18.00   1st Qu.: 28995  
#Mode  :character   Mode  :character   Mode  :character   Median : 53.00   Median : 56991  
                                                          #Mean   : 46.49   Mean   : 67007  
                                                          #3rd Qu.: 70.00   3rd Qu.:108815  
                                                          #Max.   :132.00   Max.   :299000 


#### Create a new table with all dummies
Cardum <- dummyVars(~.- region, data = Carsom, levelsOnly = TRUE)
dummy <- predict(Cardum, Carsom)
newcarsdata = data.frame(dummy)

#### Scale columns with comparatively big numeric value for our new data
newcarsdata$mileage <- scale(newcarsdata$mileage)
newcarsdata$year <- scale(newcarsdata$year)
newcarsdata$featureCount <- scale(newcarsdata$featureCount)

#### Get rid of region and subTrim
Carsom$region <- NULL  
Carsom$subTrim <- NULL

#### Build regression over all variables left
set.seed(94)
lmom = lm(Carsom$price~. + I(year ^ 2)  + I(mileage ^ 2) + mileage * year + year * isOneOwner + mileage * isOneOwner + mileage * condition + condition * year + displacement * year, data = Carsom)
plot(lmom)

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/Rplot02.png" />
<br>

x = model.matrix(~. - price + I(mileage ^ 2) + I(year ^ 2) + year * mileage + year * isOneOwner  + mileage * isOneOwner + mileage * condition + condition * year + displacement * year , data=Carsom)[,-1]
y = Carsom$price

#### Now split train and test dataset
n = nrow(x)
traindata = sample(1:n,20000)
testdata =  (-traindata)
y.test = y[testdata]

#### Lasso on training data
lasso.fit = glmnet(x[traindata,], y[traindata],standardize = FALSE,alpha = 1)
lasso.fit$lambda
cv.lasso = cv.glmnet(x[traindata,], y[traindata],alpha = 1) 
selamb = cv.lasso$lambda.1se #363.0681
minlamb = cv.lasso$lambda.min #18.49519

#### Check Lasso accuracy on test data
lasso.pred = predict(lasso.fit, s = minlamb, newx = x[testdata,])
sqrt(mean((lasso.pred - y.test)^2)) #8401.997

#### Get Lasso coefficients on variables
lasso_co = predict(lasso.fit, type = "coefficients", s = minlamb)

#### Show the cross-validation graph
plot(log(cv.lasso$lambda),sqrt(cv.lasso$cvm),main="Lasso CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(selamb),lty=2,col=2,lwd=2)

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/Screen%20Shot%202018-12-12%20at%201.27.59%20PM.png" />
<br>

#### Ridge on training data
ridge.fit = glmnet(x,y,standardize = FALSE,alpha = 0)
cv.ridge = cv.glmnet(x[traindata,], y[traindata],alpha = 0) 
ridgelamb1se = cv.ridge$lambda.1se
ridgelamb = cv.ridge$lambda.min

#### Check Ridge accuracy on test data
ridge.pred = predict(ridge.fit, s = ridgelamb, newx = x[testdata,])
sqrt(mean((ridge.pred - y.test)^2)) #11926.93

#### Get Ridge coefficients on variables
ridge_co = predict(ridge.fit, type = "coefficients", s = ridgelamb)

#### Show the cross-validation graph
plot(log(cv.ridge$lambda),sqrt(cv.ridge$cvm),main="Ridge CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(ridgelamb1se),lty=2,col=2,lwd=2)

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/Screen%20Shot%202018-12-12%20at%201.28.03%20PM.png" />
<br>

#### New data frame after variable selection
xdf = data.frame(x)
xdf[3:7] <- list(NULL)
xdf$trim600 <- NULL
xdf[12:20] <- list(NULL)
xdf[13:14] <- list(NULL)
xdf[14:19] <- list(NULL)
xdf[16:17] <- list(NULL)
xdf$displacement5.8.L <- NULL
xdf[19:20] <- list(NULL)
xdf$fuelHybrid <- NULL
xdf[21:28] <- list(NULL)
xdf[23:38] <- list(NULL)
xdf[24:26] <- list(NULL)
xdf$stateNM <- NULL
xdf$stateNM <- NULL
xdf[27:35] <- list(NULL)
xdf[28:34] <- list(NULL)
xdf[28:30] <- list(NULL)
xdf$soundSystemPremium <- NULL
xdf[30:32] <- list(NULL)
xdf$wheelSize17 <- NULL
xdf[34:36] <- list(NULL)
xdf$isOneOwnert.year <- NULL
xdf$conditionNew.mileage <- NULL
xdf$conditionUsed.year <- NULL
xdf$year.displacement3.5.L <- NULL
xdf$year.displacement3.7.L <- NULL
xdf$year.displacement4.3.L <- NULL
xdf$year.displacement5.8.L <- NULL
xdf$year.displacement6.3.L <- NULL
xdf$year.displacement8.0.L <- NULL
xdfprice = data.frame(xdf, Carsom$price)

#### PCR & Cross-validation
pcr.fit = pcr(xdfprice$Carsom.price ~., data = xdfprice, subset = traindata ,scale = FALSE,validation = "CV")
validationplot(pcr.fit,val.type = "MSEP")

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/Screen%20Shot%202018-12-12%20at%201.27.42%20PM.png" />
<br>

#### Check PCR accuracy on test data
pcr.pred = predict(pcr.fit, xdfprice[testdata,], ncomp = 40)
sqrt(mean((pcr.pred - y.test) ^ 2)) ###8849.139

#### Fit PCR model
pcrnew.fit = pcr(xdfprice$Carsom.price ~., data = xdfprice ,  scale = FALSE, ncomp = 40)
#summary(pcrnew.fit)

#### PLS & Cross-validation
pls.fit = plsr(xdfprice$Carsom.price ~., data = xdfprice, subset = traindata ,scale = FALSE,validation = "CV")
validationplot(pls.fit,val.type = "MSEP")

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/Screen%20Shot%202018-12-12%20at%201.27.36%20PM.png" />
<br>

#### Check PLS accuracy on test data
pls.pred = predict(pls.fit, xdfprice[testdata,], ncomp = 12)
sqrt(mean((pls.pred - y.test) ^ 2)) #8770.457

#### Fit PLS model
plsnew.fit = plsr(xdfprice$Carsom.price ~., data = xdfprice ,  scale = FALSE, ncomp = 12)
#summary(plsnew.fit)

#### Random Forest Model
xminus = model.matrix(~., Carsom)[,-1]
dataxminus = data.frame(xminus)
bag.cars = randomForest(dataxminus$price ~. , data = dataxminus, subset = traindata, mytr = 40,ntree = 100,importance = TRUE)
importance(bag.cars) 
varImpPlot(bag.cars)

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/importance.rf.png" />
<br>

#### Check Random Forest accuracy on test data
yhat.bag = predict(bag.cars, newdata = dataxminus[-traindata,])
cartree.test = dataxminus[-traindata, "price"]
sqrt(mean((yhat.bag - cartree.test)^2)) ###6775.446 ntree = 100
plot(yhat.bag,cartree.test)
abline(0,1,col='red',lwd=2)

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/Screen%20Shot%202018-12-12%20at%201.15.07%20PM.png" />
<br>

#### Boosting Model
boost.cars = gbm(price ~ . , data = dataxminus[traindata,], distribution = "gaussian", n.tree = 1000, interaction.depth = 4, shrinkage=.2,verbose = FALSE)
summary(boost.cars)

##var      rel.inf
##year 5.354197e+01
##mileage 3.693464e+01
#trim550 3.609036e+00
#displacement4.6.L 1.806333e+00
#conditionUsed 8.766579e-01
plot(boost.cars, i = "year")

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/Rplot04.png" />
<br>

plot(boost.cars, i = "mileage")

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/Rplot05.png" />
<br>

#### Check Boosting accuracy on test data
yhat.boost = predict(boost.cars, newdata = dataxminus[-traindata,], n.trees = 1000)
cartree.test = dataxminus[-traindata, "price"]
sqrt(mean((yhat.boost - cartree.test)^2)) ###6952


## Random Forest model has the best performance in this project

<br>
<image height="300" src= "https://github.com/ssssDu/Predictive-Modeling-Projects/blob/master/R/Pictures/Screen%20Shot%202018-12-12%20at%201.42.34%20PM.png" />
<br>


