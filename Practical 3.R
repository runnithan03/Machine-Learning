#Exercise 3.1: Initial Data Analysis
#a
install.packages("faraway")
library("faraway")

#b
# hipcenter would be a good response variable, as it is the only variable 
# in the dataset that considers the location of a person within the car.  
# This can be used as a proxy for where different drivers will position the 
# seat, which is the quantity of interest for car designers.

#c
# Many of the predictors could be highly correlated.
# Examples include:
# - height and weight (a whole analysis could be done on this 
# pairing), 
# - lower leg length and thigh length - I would suspect people with 
# longer lower legs also have longer thighs, but maybe I'm wrong.
# - Ht and HtShoes - these two variables measure height bare foot and in 
# shoes - given that people's heights are in general not going to increase 
# much by putting on a pair of shoes (and possibly by roughly a similar 
# amount for each person), these variables are likely to be (very) highly 
# correlated.  I would question including both of these variables 
# in an analysis, although our computational analyses will confirm whether 
# this is the case.
# 
# Perhaps Age is unlikely to be too highly correlated with any of the other 
# variables.
# Note that the aim of this question is to get you to think about the 
# dataset before we go headlong into applying computational methods, since 
# this is what a statistician/data scientist/machine learner should do!

#d
dim(seatpos) ##38 9 
sum(is.na(seatpos)) #no missing data

#e
# We can calculate the correlation of `hipcenter` to the other variables.
# Note that the response is the ninth variable in this dataset.
cor(seatpos[,9], seatpos[,-9])

# From this we see that `hipcenter` has a reasonable negative correlation 
# with most of the predictors, apart from age.

# We display the correlation matrix across the eight predictors as follows.  
cor( seatpos[,-9] )

# Many of the predictors are reasonably highly correlated with each other, 
# except for Age.  
# As suspected the pairing with greatest correlation is Ht and HtShoes.

# We could obtain a pairs plot across the dataset.
pairs( seatpos, pch = 16, col = 2 )

# These support the comments and correlations seen above.

# I would hazard a guess that the fixed location in the car is behind the 
# driver, since the distance from it to the drivers hips gets smaller as 
# the driver's various dimensions (in general) get larger.  Larger drivers 
# are likely to move their seat back, so to make the distance shorter, the 
# fixed position should be behind the driver.  
# Note: these are my interpretations of the dataset, but again the purpose 
# of this question is about getting you to think logically about the data 
# you have been presented with.  
# This can sometimes be harder than the computational side of things.  
# In particular it may help to catch any coding errors (and potential 
# illogical reasonings and conclusions) later! 

#Exercise 3.2: Ridge Regression
#a
y <- seatpos$hipcenter

#b
x = model.matrix(hipcenter~., seatpos)[,-1]
x
#c
library("glmnet")
ridge = glmnet(x, y, alpha = 0, nlambda = 200)

#d
plot(ridge, xvar = 'lambda')

#Exercise 3.3: Lasso Regression
#model fitting of remaining variables for hipcenter using lasso regression
lasso = glmnet(x, y)
par(mfrow = c(1,2)) #remember this puts both plots in 1 image 
plot(lasso, xvar = 'lambda')
plot(lasso)

# We can see which values of lambda were in the grid.
lasso$lambda

# We can see which variables were included under each of those.
lasso$beta

# In particular, high values of lambda leads to inclusion of Ht only.  
# Other variables start to come in; leg, followed by Age, 
# followed by Thigh, as lambda gets smaller.  
# It is interesting that Age comes in third, despite having a reasonably 
# small correlation with the response.  
# However, this may be as expected, since the other variables also have 
# high correlations with each other (collinearity), whereas Age may 
# contain alternative predictive information.  
# Interestingly, HtShoes enters only for small enough values of lambda, at 
# which point its coefficients increase whilst the Ht coefficients 
# decrease, eventually to zero.  
# This is to do with the ridiculously high correlation between Ht 
# and HtShoes (spotted in Exercise 3.1) - even for really low values of lambda, 
# we don't need both of these variables.

#Exercise 3.4: Principal Component Analysis
#a
#how to calculate principal component analysis to all the predictors x as in Exercise 3.2 
#with all variables being scaled to have uni variance.
s <- apply(x, 2, sd)       # calculates the column standard deviations
x.s  <- sweep(x, 2, s, "/")  # divides all columns by their standard deviations
seatpos.pr <- prcomp(x.s)

#b
#variance of each of the principal component and the eigenvectors that correspond to the first principal component
seatpos.pr #we get standard deviations (1, ... , p = 8):
#Standard deviations: 2.38184501 1.11210881 0.68098711 0.49087508 0.44070349 0.37306059 0.22437586 0.03985271
#Giving the eigenvectors PC1,...,PC8

#now to get PC1 (i.e. first principal component), rotate the matrix and take the first row
seatpos.pr$rotation #gives only the matrix so we can isolate PC1 (first column)
seatpos.pr$rotation[,1]

#c
#a scree plot is simiply a plot of the principal component analysis
plot(seatpos.pr)
summary(seatpos.pr) #gives us standard deviation, proportion of variance and cumulative proportion for each principal component
#therefore to capture at least 95% of the total variance of the data cloud, therefore 4 components are needed to capture at least 95%
#of the total variance of the data cloud (PC3 cumulative proportion = 0.92171 and PC4 cumulative proportion = 0.95183)

#d
#Compress the data using the number of principal components as in (c), 
#then reconstruct the data and compare the reconstruct data with the original data by looking at Age and weight
seatpos.pr$x

#The $x in prcomp$x refers to the rotated data. Specifically, it contains the principal components, which are the data transformed into the new coordinate system 
#defined by the principal axes. These principal components are the scores of the original data on the principal component axes.
T   <- t(seatpos.pr$x[,c(1,2,3,4)])  #compressed using 4 PCs
T
ms <- colMeans(x.s)
ms

R   <- t(ms + seatpos.pr$rot[,c(1,2,3,4)]%*% T) #reconstruction 
plot(rbind(x.s[,1:2], R[,1:2]), col=c(rep(1,38),rep(3,38))) #whatdoes this line do?, especially the slicing!

#3.5: Principal Component Regression
#a
install.packages("pls")
library(pls)

#If numeric vector, XX is scaled by dividing each variable with the corresponding element of scale. If scale is TRUE, XX is scaled by dividing each variable by its sample standard deviation. 
#If cross-validation is selected, scaling by the standard deviation is done for every segment.
#CV gives cross-validation and selects the number of principal components
pcr.fit <- pcr(hipcenter~., data = seatpos, scale = TRUE, validation = CV)

summary(pcr.fit)

#plotting the mean squared validation error is done using:
#b
validationplot( pcr.fit, val.type = 'MSEP' )
#MSEP corresponds to mean square validation error

#c
coef(pcr.fit)

#Exercise 3.6: Predictive Performance of Methods
#need to go over it all to be honest, probably need to cover that section of lectures!

# Load R libraries.
library(leaps)

# Predict function for regsubsets
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

repetitions = 50
cor.bss = c()
cor.ridge = c()
cor.lasso = c()
cor.pcr = c()

set.seed(1)                
for(i in 1:repetitions){
  # Step (i) data splitting
  training.obs = sample(1:38,  28)
  y.train = seatpos$hipcenter[training.obs]
  x.train = model.matrix(hipcenter~., seatpos[training.obs, ])[,-1]
  y.test = seatpos$hipcenter[-training.obs]
  x.test = model.matrix(hipcenter~., seatpos[-training.obs, ])[,-1]
  
  # Step (ii) training phase
  bss.train = regsubsets(hipcenter~., data=seatpos[training.obs,], nvmax=8)
  min.cp = which.min(summary(bss.train)$cp)
  ridge.train = cv.glmnet(x.train, y.train, alpha = 0, nfolds = 5)
  lasso.train = cv.glmnet(x.train, y.train, nfold = 5)
  pcr.train = pcr(hipcenter~., data =seatpos[training.obs,], 
                  scale = TRUE, validation="CV")
  min.pcr = which.min(MSEP(pcr.train)$val[1,1, ] ) - 1
  
  # Step (iii) generating predictions
  predict.bss = predict.regsubsets(bss.train, seatpos[-training.obs, ], min.cp)
  predict.ridge = predict(ridge.train, x.test, s = 'lambda.min')
  predict.lasso = predict(lasso.train, x.test, s = 'lambda.min')
  predict.pcr = predict(pcr.train,seatpos[-training.obs, ], ncomp = min.pcr )
  
  # Step (iv) evaluating predictive performance
  cor.bss[i] = cor(y.test, predict.bss)
  cor.ridge[i] = cor(y.test, predict.ridge)
  cor.lasso[i] = cor(y.test, predict.lasso)
  cor.pcr[i] = cor(y.test, predict.pcr)
}

# Plot the resulting correlations as boxplots.
boxplot(cor.bss, cor.ridge, cor.lasso, cor.pcr, 
        names = c('BSS','Ridge', 'Lasso', 'PCR'), 
        ylab = 'Test correlation', col = 2:5)