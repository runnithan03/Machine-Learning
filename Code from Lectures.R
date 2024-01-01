install.packages("carData")
library(carData)
data(UN)
options(scipen=999)

# Remove missing data
newUN<-na.omit(UN) 

str(newUN)

fit<- lm(infantMortality ~ ppgdp, data=newUN)
summary(fit)
# poor fit and only has an R^2 0.2656

plot(newUN$infantMortality ~ newUN$ppgdp, xlab="GDP per Capita", ylab="Infant mortality (per 1000 births)", pch=16, col="cornflowerblue")
abline(fit,col="red")

plot(fit,which=1,pch=16,col="cornflowerblue")

fit1<- lm(log(infantMortality) ~ log(ppgdp), data=newUN)
summary(fit1)

plot(log(newUN$infantMortality) ~ log(newUN$ppgdp), xlab="GDP per Capita", ylab="Infant mortality (per 1000 births)", pch=16, col="cornflowerblue")
abline(fit1,col="red")

plot(fit1,which=1,pch=16,col="cornflowerblue")


par(mfrow=c(2,2))
# before the log  transformation.
plot(fit, which = 2,pch=16, col="cornflowerblue")
hist(resid(fit),col="cornflowerblue",main="")
# after the log  transformation.
plot(fit1, which = 2, pch=16, col="hotpink3")  
hist(resid(fit1),col="hotpink3",main="")

library(ISLR)

?Hitters
#Data description, salary is the response

names(Hitters)
head(Hitters)       
dim(Hitters)

sum( is.na( Hitters ) )
sum( is.na( Hitters$Salary ) )

Hitters = na.omit( Hitters )
dim( Hitters )
sum( is.na( Hitters ) ) 

#install.packages('leaps')
library(leaps)
#performs step 2 of the sesearch algorithms

best = regsubsets(Salary~., Hitters)
#the . part of Salary~. indicates that we wish to consider 
#all possible predictors in the subset selection method

summary(best)
# The asterisks `*` indicate variables which are included in model 
# $M_k, k = 1,...,19$ as discussed in the subset selection algorithm
# For instance, we see that for the model with one predictor that variable is `CRBI`, 
# while for the model with four predictors the selected variables 
# are `Hits`, `CRBI`, `DivisionW` and `PutOuts`. 
# `summary()` command displayed output up to the best model with 
# 8 predictors; this is the default option in `regsubsets()`. 


best = regsubsets(Salary~., data = Hitters, nvmax = 19)
# nvmax for display number of best models 
results = summary(best)
names(results)
# performance information

results$rsq

#putting them together
RSS = results$rss
r2 = results$rsq
Cp = results$cp
BIC = results$bic
Adj_r2 = results$adjr2

cbind(RSS, r2, Cp, BIC, Adj_r2)

par(mfrow = c(1, 2))
plot(RSS, xlab = "Number of Predictors", ylab = "RSS", 
     type = "l", lwd = 2)
plot(r2, xlab = "Number of Predictors", ylab = "R-square", 
     type = "l", lwd = 2)

#different criteria choose different "best" models
which.min(Cp)
which.min(BIC)
which.max(Adj_r2)

par(mfrow = c(1, 3))
plot(Cp, xlab = "Number of Predictors", ylab = "Cp", 
     type = 'l', lwd = 2)
points(10, Cp[10], col = "red", cex = 2, pch = 8, lwd = 2)
plot(BIC, xlab = "Number of Predictors", ylab = "BIC", 
     type = 'l', lwd = 2)
points(6, BIC[6], col = "red", cex = 2, pch = 8, lwd = 2)
plot(Adj_r2, xlab = "Number of Predictors", ylab = "Adjusted RSq", 
     type = "l", lwd = 2)
points(11, Adj_r2[11],  col = "red", cex = 2, pch = 8, lwd = 2)


par( mfrow = c(1,1) )
#respecifying the window splitting
plot(best, scale = "bic")
#The top row corresponds to the best model, 
#while the bottom row to the worst model according to 
#the chosen criterion. White squares correspond to variables 
#that are excluded under each model.

#extract the model coefficients
coef(best,10)
coef(best,6)
coef(best,11)


### Forward Stepwise Selection
fwd = regsubsets(Salary~., data = Hitters, nvmax = 19, 
                 method = "forward")
summary(fwd)


coef(best, 6)
coef(fwd, 6)
coef(best, 7)
coef(fwd, 7)
#The results for the model with 6 predictors are in agreement 
#the results for the models with 7 predictors are different.


### Best Subset Selection: Validation

predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

set.seed(10)          # for the results to be reproducible   
dim(Hitters)
training.obs = sample(1:263, 175)
Hitters.train = Hitters[training.obs,  ]
Hitters.test = Hitters[-training.obs,  ]
dim(Hitters.train)
dim(Hitters.test)

best.val = regsubsets(Salary~., data = Hitters.train, nvmax = 19)

val.error<-c()
for(i in 1:19){
  pred = predict.regsubsets(best.val, Hitters.test, i)
  val.error[i] = mean((Hitters.test$Salary - pred)^2)
}

val.error

which.min(val.error)


#What about inference?
#Note that the aim of subset selection is to find which predictors
# should be included within our model, not to identify the specific model
# (including coefficient values). 
# Therefore, it is important to note that there is a final step 
# to fitting a model by validation, and that is to train the 
# same 10-predictor model to the entire data. 
# This will yield different coefficient values to 
# the current coefficients. The current coefficients should not be used 
# because these were “learned” from the reduced training sample 
# (that is, we didn’t use all of the available data).

coef(best.val, 10)

ls10 = lm(Salary ~ AtBat + Hits + Runs + Walks + CAtBat + CRuns + CRBI 
          + CWalks + Division + PutOuts, data = Hitters)
summary(ls10)

confint(ls10, 1:11)


# Variance of selection based on validation
# When using validation and cross-validation for best subset 
# selection (or any stepwise method) it is important to understand 
# that results depend on the random splitting between training and 
# testing samples. This is generally a drawback which is 
# not discussed a lot, especially for the simple validation approach. 
# The below code implements the validation approach presented 
# previously but now starting from 50 different random seeds, 
# storing the number of predictors in the selected model each time.

min.valid = c()

for(n in 1:50){
  set.seed(n)          
  training.obs = sample(1:263, 175)
  Hitters.train = Hitters[training.obs,  ]
  Hitters.test = Hitters[-training.obs,  ]
  
  best.val = regsubsets(Salary~., data = Hitters.train, nvmax = 19)
  
  val.error<-c()
  for(i in 1:19){
    pred = predict.regsubsets(best.val, Hitters.test, i)
    val.error[i] = mean((Hitters.test$Salary - pred)^2)
  }
  val.error
  min.valid[n] = which.min(val.error)
}

# The histogram below shows that the final selection of 
# the best model can vary a lot, even in terms of the 
# number of predictors! Note that in addition, the histogram 
# doesn’t tell the whole story because each time, say, 
# a 10-predictor model is chosen, it may be choosing different 
# predictors. In this case we can see that the validation method 
# is most often choosing a model with 8, 9 or 10 predictors.

hist(min.valid, col = 3, breaks = seq( from = 0.5, to = 19.5, length = 20 ), 
     xlab = 'Number of Predictors', 
     main = 'Best Subset Selection with validation')
abline(v = mean(min.valid), col = 2, lwd = 4)
legend('topright', legend=c('Average selection'),bty = 'n', lty = 1, 
       lwd = 4, col = 2)


#Cross-validation
# we here use the full dataset in the initial regsubsets()
# command (step 2), and cross-validation for step 3 only.

best = regsubsets(Salary~., data = Hitters, nvmax = 19)

#Since this would involve manually specifying 19 models
#using lm(), we will instead demonstrate how to obtain 
# a K-fold cross-validation error for just 3 of the models
# suggested by regsubsets() only.

coef(best,10) # Cp
coef(best,6)  # BIC
coef(best,11) # adj-Rsq

ls10 = lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + CWalks +
            Division + PutOuts + Assists, data = Hitters)
ls6 = lm(Salary ~ AtBat + Hits + Walks + CRBI + Division + PutOuts, 
         data = Hitters)
ls11 = lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + CWalks +
            + League + Division + PutOuts + Assists, data = Hitters)


# Common choices in practice are 5 and 10, 
# leading to 5-fold and 10-fold CV. In this example we will use 10 folds. 
# Of course, often the sample size n will not be perfectly divisable 
# by K (as in our case where n=263 and K=10). 
# This does not matter, since it is not a problem if some folds 
#have fewer observations.

k = 10
folds = cut(1:263, breaks=10, labels=FALSE)
folds

table(folds)

# It is then very important to randomly re-shuffle this vector, 
# because we do not want the fold creation procedure to be deterministic. 
# We can do this very easily in R using once again the command sample().

set.seed(2)
folds = sample(folds)
folds


# Finally, we need to create a matrix this time with K=10 rows and 3 columns. 
# The validation MSEs of the folds for model ls10 will be stored in the first
# column of this matrix, the validation MSEs of the folds for model ls6 
# will be stored in the second column of this matrix, 
# and the validation MSEs of the folds for model ls11 will be stored 
# in the final column.

cv.errors = matrix(NA, nrow = k, ncol = 3, 
                   dimnames = list(NULL, c("ls10", "ls6", "ls11")))
cv.errors 

# data = Hitters[folds!=i, ] is doing. In R != means “not equal”. 
for(i in 1:k){
  ls10 = lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + CWalks +
              Division + PutOuts + Assists, data = Hitters[folds!=i, ] )
  ls6 = lm(Salary ~ AtBat + Hits + Walks + CRBI + Division + PutOuts, 
           data = Hitters[folds!=i, ])
  ls11 = lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + CWalks +
              + League + Division + PutOuts + Assists, 
            data = Hitters[folds!=i, ])
  
  pred10 <- predict( ls10, newdata = Hitters[folds==i, ] )
  pred6 <- predict( ls6, newdata = Hitters[folds==i, ] )
  pred11 <- predict( ls11, newdata = Hitters[folds==i, ] )
  
  cv.errors[i,] = c( mean( (Hitters$Salary[folds==i]-pred10)^2), 
                     mean( (Hitters$Salary[folds==i]-pred6)^2), 
                     mean( (Hitters$Salary[folds==i]-pred11)^2) )
}
cv.errors


cv.mean.errors <- colMeans(cv.errors)
cv.mean.errors

#We may choose ls6 as it has slightly smaller average cross-validation error,
# although in this case there is not that much in it. 
# Since we also tend to favour models with fewer predictors as 
#they tend to be more interpretable, we would probably choose ls6.

library(ISLR)
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
library(glmnet)

# Initially, we will make use of function `glmnet()` which implements 
# ridge regression without cross-validation, but it does give a range of 
# solutions over a grid of $\lambda$ values. The grid is produced automatically 
# - we do not need to specify anything. Of course, if we want to 
# we can define the grid manually. Let's have a look first at the 
#help document of `glmnet()`.

?glmnet

# A first important thing to observe is that `glmnet()` requires a different 
# syntax than `regsubsets()` for declaring the response variable and 
# the predictors. Now the response should be a vector and the predictor 
# variables must be stacked column-wise as a matrix. 
# This is easy to do for the response. For the predictors we have to make use 
# of the `model.matrix()` command. Let us define as `y` the response and 
# as `x` the matrix of predictors.

y = Hitters$Salary
x = model.matrix(Salary~., Hitters)[,-1] 
# Here we exclude the first columnbecause it corresponds to the intercept.
head(x)

# See how the command `model.matrix()` automatically transformed 
# the categorical variables in `Hitters` with names `League`, `Division` 
# and `NewLeague` to dummy variables with names `LeagueN`, `DivisionW` and 
# `NewLeagueN`.
# Other important things that we see in the help document are that 
# `glmnet()` uses a grid of 100 values for $\lambda$ (`nlambda = 100`) 
# and that parameter `alpha` is used to select between ridge implementation 
# and lasso implementation. The default option is `alpha = 1` which 
# runs lasso (see Chapter \@ref(Lasso-Regression)) - 
# for ridge we have to set `alpha = 0`. 
# Finally we see the default option `standardize = TRUE`, 
# which means we do not need to standardize the predictor variables manually.

ridge = glmnet(x, y, alpha = 0)
names(ridge)
ridge$lambda
dim(ridge$beta)
coef(ridge)[,1:3]

plot(ridge, xvar = 'lambda')

library(ISLR)
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
library(glmnet)

y = Hitters$Salary
x = model.matrix(Salary~., Hitters)[,-1]
head(x)

lasso = glmnet(x, y)
names(lasso)
lasso$lambda

dim(lasso$beta)
lasso$beta[,1:3]

coef(lasso)[,1:3]  

plot(lasso, xvar = 'lambda')

library(ISLR)
Hitters = na.omit(Hitters)
dim(Hitters)

sum(is.na(Hitters))

#Ridge regression

y = Hitters$Salary
x = model.matrix(Salary~., Hitters)[,-1]
# Here we exclude the first column because it corresponds to the intercept term.

head(x)

library(glmnet)
set.seed(1)

?cv.glmnet

ridge.cv = cv.glmnet(x, y, alpha=0)

names( ridge.cv )

ridge.cv$lambda.min
#lambda.min provides the optimal value of λ as suggested by the cross-validation MSEs.

ridge.cv$lambda.1se 
# lambda.1se provides a different value of λ, known as the 1-standard error (1-se)λ.
# This is the maximum value that λ can take while still falling 
# within the one standard error interval of the minimum_CV λ.

# We see that in this case the 1-se λ is much larger than the min-CV λ.

#This means that we expect the coefficients under 1-se λ to be much smaller.

round(cbind(
  coef(ridge.cv, s = 'lambda.min'),
  coef(ridge.cv, s = 'lambda.1se')),  
  digits = 3 )

plot(ridge.cv)
abline( h = ridge.cv$cvup[ridge.cv$index[1]], lty = 4 )


# We can also plot the ridge regularisation paths and
# add the minimum-CV and 1-se λ values to the plot.
ridge = glmnet(x, y, alpha = 0)
plot(ridge, xvar = 'lambda')
abline(v = log(ridge.cv$lambda.min), lty = 3) # careful to use the log here  
# and below
abline(v = log(ridge.cv$lambda.1se), lty = 3)

# run with different seeds.....
# Important note: Any method/technique that relies on 
# validation/cross-validation is subject to variability. 
# If we re-run this code under a different random seed results will change.

beta.1se =   coef( ridge.cv, s = 'lambda.1se' )[-1]
rank.coef = sort( abs( beta.1se ), decreasing = TRUE )
rank.coef

# Lasso regression

set.seed(1)
lasso.cv = cv.glmnet(x, y)
lasso.cv$lambda.min

lasso.cv$lambda.1se 

round(cbind(
  coef(lasso.cv, s = 'lambda.min'),
  coef(lasso.cv, s = 'lambda.1se')),  
  3)


par(mfrow=c(1,2))
plot(lasso.cv)
lasso = glmnet(x, y)
plot(lasso, xvar = 'lambda')
abline(v = log(lasso.cv$lambda.min), lty = 3) # careful to use the log here  
# and below
abline(v = log(lasso.cv$lambda.1se), lty = 3)



repetitions = 50
mse.1 = c()
mse.2 = c()

set.seed(1)                
for(i in 1:repetitions){
  
  # Step (i) data splitting
  training.obs = sample(1:263,  175)
  y.train = Hitters$Salary[training.obs]
  x.train = model.matrix(Salary~., Hitters[training.obs, ])[,-1]
  y.test = Hitters$Salary[-training.obs]
  x.test = model.matrix(Salary~., Hitters[-training.obs, ])[,-1]
  
  # Step (ii) training phase
  lasso.train = cv.glmnet(x.train, y.train)
  
  # Step (iii) generating predictions
  predict.1 = predict(lasso.train, x.test, s = 'lambda.min')
  predict.2 = predict(lasso.train, x.test, s = 'lambda.1se')
  
  # Step (iv) evaluating predictive performance
  mse.1[i] = mean((y.test-predict.1)^2)
  mse.2[i] = mean((y.test-predict.2)^2)
}

par(mfrow=c(1,1))
boxplot(mse.1, mse.2, names = c('min-CV lasso','1-se lasso'), 
        ylab = 'Test MSE', col = 7)

fuelcons <- read.table("https://www.maths.dur.ac.uk/users/hailiang.du/data/FUEL.dat", header=TRUE)
fuel <- fuelcons[,c("TAX", "DLIC", "INC", "ROAD")]
#TAX =Gasoline state tax rate, cents per gallon
#DLIC = Number of licenced drivers per 1000 people in the state
#INC = Per capita personal income for the year 2000 (in $1000s) for the state
#ROAD = Miles of federal-aid highway (in 1000s) for the state
#FUEL = Gallons of gasoline sold for road use per capita

fuel.pr <- prcomp(fuel)
plot(fuel.pr)
# A simple graphical tool illustrating this decomposition is the `r red("scree plot")`
#, which plots $\lambda_j$ vs. $j$.


#We can create it by hand? Let's look at the output of the `prcomp` function:

fuel.pr

#Here, the item `Standard deviations` contains the (ordered) values \sqrt{\lambda_j}
# and the item `Rotation` is the same as  \Gamma$. Why rotation??

#Hence, we get the \lambda_j via
fuel.pr$sdev^2

#which is immediately confirmed to be the same as
eigen(var(fuel))$values

#and the proportions of variance explained are given by
fuel.pr$sdev^2/ sum(fuel.pr$sdev^2)


summary(fuel.pr)

###################

fuel.pr1 <- prcomp(fuel, scale=TRUE)
fuel.pr1
plot(fuel.pr1)
summary(fuel.pr1)
sum(fuel.pr1$sdev^2)

fuelcons <- read.table("https://www.maths.dur.ac.uk/users/hailiang.du/data/FUEL.dat", header=TRUE)
fuel <- fuelcons[,c("TAX", "DLIC", "INC", "ROAD")]
#TAX =Gasoline state tax rate, cents per gallon
#DLIC = Number of licenced drivers per 1000 people in the state
#INC = Per capita personal income for the year 2000 (in $1000s) for the state
#ROAD = Miles of federal-aid highway (in 1000s) for the state
#FUEL = Gallons of gasoline sold for road use per capita

fuel.pr <- prcomp(fuel)

# Work firstly with a single observation. Say, case 1:
x1 <- fuel[1,]        
mode(x1)        # wrong data type: lists can't be used for computation     
x1<- as.numeric(x1)
mode(x1)                # that does it.

m <- colMeans(fuel) 
t1 <- t(fuel.pr$rot[,c(1,2)]) %*% (x1-m)   #  Score for case 1
r1 <- m+ fuel.pr$rot[,c(1,2)]%*% t1   # Reconstruction for case 1

x1
t1
r1

plot(rbind(fuel,t(r1)), col=c(2, rep(1,47),3))# red=original, green=approx

# For the full data matrix, we make use of the scores delivered by prcomp: 

# fuel.pr$x # is the same as:
# t(fuel.pr$rot[,]) %*% (t(fuel)-m)

T   <- t(fuel.pr$x[,c(1,2)])               # All scores
R   <- t(m + fuel.pr$rot[,c(1,2)]%*% T)    # All reconstructed values

plot(rbind(fuel, R), col=c(rep(1,48),rep(3,48)))
# black=original, green=approx

boxplot(fuel)


s <- apply(fuel, 2, sd)       # calculates the column standard deviations
fuel.s  <- sweep(fuel, 2, s, "/")   # divides all columns by their standard deviations

boxplot(fuel.s) 

fuel.pr1 <- prcomp(fuel.s)
#  (equivalent to fuel.pr1 in the example in previous section).

plot(fuel.pr1) # suggests d=3 

ms <- colMeans(fuel.s) # calculates means of scaled data set

Ts <- t(fuel.pr1$x[,c(1,2,3)])              # 3d-scores for scaled data
Rs <- t(ms + fuel.pr1$rot[,c(1,2,3)]%*% Ts) # Reconstructed scaled data

plot(rbind(fuel.s, Rs), col=c(rep(1,48),rep(3,48)))# black=original, green=approx

library(pls) # for performing PCR
library(ISLR) # for Hitters dataset
set.seed(1)
pcr.fit = pcr( Salary ~ ., data = Hitters, scale = TRUE, validation = "CV" )
summary(pcr.fit)

validationplot( pcr.fit, val.type = 'MSEP' )


min.pcr = which.min( MSEP( pcr.fit )$val[1,1, ] ) - 1
min.pcr

coef(pcr.fit, ncomp = min.pcr)

head( predict( pcr.fit, ncomp = min.pcr ) )



coef.mat = matrix(NA, 19, 19)
for(i in 1:19){
  coef.mat[,i] = pcr.fit$coefficients[,,i]
}

plot(coef.mat[1,], type = 'l', ylab = 'Coefficients', 
     xlab = 'Number of components', ylim = c(min(coef.mat), max(coef.mat)))
for(i in 2:19){
  lines(coef.mat[i,], col = i)
}

abline(v = min.pcr, lty = 3)


PVE <- rep(NA,19)
for(i in 1:19){ PVE[i]<- sum(pcr.fit$Xvar[1:i])/pcr.fit$Xtotvar }
barplot( PVE, names.arg = 1:19, main = "scree plot", 
         xlab = "number of PCs", 
         ylab = "proportion of variance explained" )

library(MASS)
help(Boston)
#The response variable is median value of owner-occupied homes in Boston (medv).

sum(is.na(Boston))
names(Boston)

# We will first analyse medv with respect to the predictor lstat 
#(percentage of lower status population).
head(cbind(Boston$medv, Boston$lstat))

#For convenience we will just name the response y and the predictor x. 
#We will also pre-define the labels for the x and y-axes that 
# we will use repeatedly in figures throughout this practical demonstration.

y = Boston$medv
x = Boston$lstat
y.lab = 'Median Property Value'
x.lab = 'Lower Status (%)'


plot( x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
      main = "", bty = 'l' )

#Let us start by fitting to the data a degree-2 polynomial 
# using the command lm() and summarising the results using summary().
poly2 = lm( y ~ x + I(x^2) )
summary( poly2 )

# Important note: Above we see that we need to enclose x^2 within 
# the envelope function I(). This is because x^2 is a function of x 
# and when we use a function (any function) of a predictor in lm() 
# we need to do that, otherwise we get wrong results! 
#Besides that we see the usual output from summary().

poly2 = lm(y ~ poly(x,  2,  raw = TRUE))
summary(poly2)

#The argument `raw = TRUE` above is used in order to get the same 
# coefficients as previously. If we do not use this argument 
# the coefficients will differ because they will be calculated 
#on a different (orthogonal) basis. 

# However, this is not that important because with polynomial 
# regression it is often the case that we are not interested 
# in the regression coefficients as the model becomes more complex. 
# In terms of fitting the curve `poly(x,  2,  raw = TRUE))` 
# and `poly(x,  2))` will give the same result!
poly2 = lm(y ~ poly(x,  2))
summary(poly2)
poly2 = lm(y ~ poly(x,  2,  raw = TRUE))


#Now, lets see how we can produce a plot similar to those 
# shown in the Wage data example.
# A first important step is that we need to create an object, 
# which we name sort.x, which has the sorted values of predictor 
# x in a ascending order. Without sort.x we will not be able to produce the plots!
# Then, we need to use predict() with sort.x as input 
#in order to proceed to the next steps.

sort.x = sort(x)
sort.x[1:10] 

pred2 = predict(poly2, newdata = list(x = sort.x), se = TRUE)
names(pred2)

# The object pred2 contains fit, which are the fitted values, and se.fit, 
# which are the standard errors of the mean prediction, 
# that we need in order to construct the approximate 95% 
# confidence intervals (of the mean prediction). 
# With this information we can construct the confidence intervals 
# using cbind(). 

se.bands2 = cbind( pred2$fit - 2 * pred2$se.fit, 
                   pred2$fit + 2 * pred2$se.fit )
se.bands2[1:10,] # the first 10 confidence intervals of the curve


# Below we use lines() for pred2$fit because this is a vector, 
#but for se.bands2, which is a matrix, we have to use matlines().

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-2 polynomial", bty = 'l')
lines(sort.x, pred2$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands2, lwd = 1.4, col = "red", lty = 3)


# The code below produces a plot of degree-2 up to degree-5 polynomial fits.

poly3 = lm(y ~ poly(x,  3))
poly4 = lm(y ~ poly(x,  4))
poly5 = lm(y ~ poly(x, 5))

pred3 = predict(poly3, newdata = list(x = sort.x), se = TRUE)
pred4 = predict(poly4, newdata = list(x = sort.x), se = TRUE)
pred5 = predict(poly5, newdata = list(x = sort.x), se = TRUE)

se.bands3 = cbind(pred3$fit + 2*pred3$se.fit, pred3$fit-2*pred3$se.fit)
se.bands4 = cbind(pred4$fit + 2*pred4$se.fit, pred4$fit-2*pred4$se.fit)
se.bands5 = cbind(pred5$fit + 2*pred5$se.fit, pred5$fit-2*pred5$se.fit)


par(mfrow = c(2,2))
# Degree-2
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-2 polynomial", bty = 'l')
lines(sort.x, pred2$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands2, lwd = 2, col = "red", lty = 3)

# Degree-3
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-3 polynomial", bty = 'l')
lines(sort.x, pred3$fit, lwd = 2, col = "darkviolet")
matlines(sort.x, se.bands3, lwd = 2, col = "darkviolet", lty = 3)

# Degree-4
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-4 polynomial", bty = 'l')
lines(sort.x, pred4$fit, lwd = 2, col = "blue")
matlines(sort.x, se.bands4, lwd = 2, col = "blue", lty = 3)

# Degree-5
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-5 polynomial", bty = 'l')
lines(sort.x, pred5$fit, lwd = 2, col = "black")
matlines(sort.x, se.bands5, lwd = 2, col = "black", lty = 3)

# For this data it is not clear which curve fits better. 
# All four curves look reasonable given the data that we have available. 
# Without further indications, one may choose the degree-2 polynomial 
# since it is simpler and seems to do about as well as the others. 
# However, if we want to base our decision on a more formal procedure, 
# rather than on a subjective decision, one option is to use the classical 
# statistical methodology of analysis-of-variance (ANOVA). 
# Specifically, we will perform sequential comparisons based on the F-test, 
# comparing first the linear model vs. the quadratic model 
# (degree-2 polynomial), then the quadratic model vs. 
# the cubic model (degree-3 polynomial) and so on. 
# We therefore have to fit the simple linear model, 
# and we also choose to fit the degree-6 polynomial 
# to investigate the effects of an additional predictor as well. 
# We can perform this analysis in RStudio using the command anova() 
#as displayed below.

poly1 = lm(y ~ x)
poly6 = lm(y ~ poly(x, 6))
anova(poly1, poly2, poly3, poly4, poly5, poly6)

# The hypothesis that is checked at each step is that 
# the decrease in RSS is not significant. The hypothesis 
# is rejected if the p-value (column Pr(>F)) is smaller 
# than a given significance level (say 0.05). 
# If the hypothesis is rejected then we move on to the next comparison. 
# Based on this reasoning and the results above we would 
# select the 5-degree polynomial.

# we could also try polynomial regression with multiple predictors. 
# For example, we include average number of rooms per dwelling 
# as one of the predictors

x1=Boston$lstat
x2=Boston$rm
?Boston

polym1 <- lm(y ~ poly(x1, 2) + poly(x2 , 2) + x1:x2)
summary(polym1)
# we could use polym() makes things easier for us
polym2=lm(y ~ polym(x1, x2, degree=2) )
summary(polym2)

library("MASS")
y = Boston$medv
x = Boston$lstat
y.lab = 'Median Property Value'
x.lab = 'Lower Status (%)'

# For step function regression we can make use of the command cut(), 
# which automatically assigns samples to intervals given a specific
#number of intervals. 

table(cut(x, 2))

# Note that cut(x, 2) generated 2 intervals, 
#but this means there is only 1 cutpoint. 
# The number of cutpoints is naturally one less than the number of intervals,
# but it is important to be aware that cut requires 
#specification of the number of required intervals.

# So, we can use cut() within lm() to easily fit regression 
# models with step functions. Below we consider 4 models with 1, 2, 3 
# and 4 cutpoints (2, 3, 4 and 5 intervals) respectively.

step2 = lm(y ~ cut(x, 2))
step3 = lm(y ~ cut(x, 3))
step4 = lm(y ~ cut(x, 4))
step5 = lm(y ~ cut(x, 5))


pred2 = predict(step2, newdata = list(x = sort(x)), se = TRUE)
pred3 = predict(step3, newdata = list(x = sort(x)), se = TRUE)
pred4 = predict(step4, newdata = list(x = sort(x)), se = TRUE)
pred5 = predict(step5, newdata = list(x = sort(x)), se = TRUE)

se.bands2 = cbind(pred2$fit + 2*pred2$se.fit, pred2$fit-2*pred2$se.fit)
se.bands3 = cbind(pred3$fit + 2*pred3$se.fit, pred3$fit-2*pred3$se.fit)
se.bands4 = cbind(pred4$fit + 2*pred4$se.fit, pred4$fit-2*pred4$se.fit)
se.bands5 = cbind(pred5$fit + 2*pred5$se.fit, pred5$fit-2*pred5$se.fit)

par(mfrow = c(2,2))

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "1 cutpoint", bty = 'l')
lines(sort(x), pred2$fit, lwd = 2, col = "red")
matlines(sort(x), se.bands2, lwd = 1.4, col = "red", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "2 cutpoints", bty = 'l')
lines(sort(x), pred3$fit, lwd = 2, col = "darkviolet")
matlines(sort(x), se.bands3, lwd = 1.4, col = "darkviolet", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "3 cutpoints", bty = 'l')
lines(sort(x), pred4$fit, lwd = 2, col = "blue")
matlines(sort(x), se.bands4, lwd = 1.4, col = "blue", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "4 cutpoints", bty = 'l')
lines(sort(x), pred5$fit, lwd = 2, col = "black")
matlines(sort(x), se.bands5, lwd = 1.4, col = "black", lty = 3)


# Note that we do not necessarily need to 
# rely on the automatic selections of cutpoints 
# used by cut(). We can define the intervals if we want to. 
# For instance, if we want cutpoints at 
#10, 20 and 30 we can do the following

breaks4 = c(min(x), 10, 20, 30, max(x))
table(cut(x, breaks = breaks4))

step.new4 = lm(y ~ cut(x, breaks = breaks4))
summary(step.new4)

library(MASS)
y = Boston$medv
x = Boston$lstat
y.lab = 'Median Property Value'
x.lab = 'Lower Status (%)'
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "", bty = 'l')

## Regression Splines ##

library(splines)

summary(x)
cuts = summary(x)[c(2, 3, 5)] 
cuts

sort.x = sort(x)

#For a start lets fit a linear spline using our selected placement 
# of knots. For this we can use command lm() and inside it we use 
# the command bs() in which we specify degree = 1 for a linear spline 
# and knots = cuts for the placement of the knots at the 
# three percentiles. We also calculate the corresponding fitted 
# values and confidence intervals exactly in the same way 
#we did in previous practical demonstrations.

?bs
spline1 = lm(y ~ bs(x, degree = 1, knots = cuts))
pred1 = predict(spline1, newdata = list(x = sort.x), se = TRUE)
se.bands1 = cbind(pred1$fit + 2 * pred1$se.fit, 
                  pred1$fit - 2 * pred1$se.fit)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Linear Spline", bty = 'l')
lines(sort.x, pred1$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands1, lwd = 2, col = "red", lty = 3)

# We know that splines have (d+1)+K degrees of freedom, where  
# d is the degree of the polynomial. So in this case we have 1+1+3 = 5 
# degrees of freedom. Selecting df = 5 in bs() will 
# automatically use 3 knots placed at the 25th, 50th and 75th percentiles. 
spline1df = lm(y ~ bs(x, degree = 1, df = 5))
pred1df = predict(spline1df, newdata = list(x = sort.x), se = TRUE)
se.bands1df = cbind(pred1df$fit + 2 * pred1df$se.fit, pred1df$fit - 2 * pred1df$se.fit)

par(mfrow = c(1, 2))
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Linear Spline (with knots)", bty = 'l')
lines(sort.x, pred1$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands1, lwd = 2, col = "red", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Linear Spline (with df)", bty = 'l')
lines(sort.x, pred1df$fit, lwd = 2, col = "darkred")
matlines(sort.x, se.bands1df, lwd = 2, col = "red", lty = 3)

# degree 2

spline2 = lm(y ~ bs(x, degree = 2, df = 6))
pred2 = predict(spline2, newdata = list(x = sort.x), se = TRUE)
se.bands2 = cbind(pred2$fit + 2 * pred2$se.fit, pred2$fit - 2 * pred2$se.fit)

spline3 = lm(y ~ bs(x, degree = 3, df = 7))
pred3 = predict(spline3, newdata = list(x = sort.x), se = TRUE)
se.bands3 = cbind(pred3$fit + 2 * pred3$se.fit, pred3$fit - 2 * pred3$se.fit)

par(mfrow = c(1,3))
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Linear Spline", bty = 'l')
lines(sort.x, pred1$fit, lwd = 2, col = "darkred")
matlines(sort.x, se.bands1, lwd = 2, col = "darkred", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Quadratic Spline", bty = 'l')
lines(sort.x, pred2$fit, lwd = 2, col = "darkgreen")
matlines(sort.x, se.bands2, lwd = 2, col = "darkgreen", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Cubic Spline", bty = 'l')
lines(sort.x, pred3$fit, lwd = 2, col = "darkblue")
matlines(sort.x, se.bands3, lwd = 2, col = "darkblue", lty = 3)

library(MASS)
y = Boston$medv
x = Boston$lstat
y.lab = 'Median Property Value'
x.lab = 'Lower Status (%)'

names(Boston)
# Let’s say that we want to use predictors lstat, indus and chas 
#for the analysis (use ?Boston again to check what these refer to).

library(gam)

gam = gam( medv ~ bs( lstat, degree = 3, df = 5 ) + s( indus, df = 5 ) + chas, 
           data = Boston )
par( mfrow = c(1,3) )
plot( gam,  se = TRUE, col = "blue" )

Boston1 = Boston
Boston1$chas = factor(Boston1$chas)

gam1 = gam( medv ~ bs( lstat, degree = 3, df = 5 ) + s( indus, df = 5 ) + chas, 
            data = Boston1 )
par(mfrow = c(1,3))
plot(gam1,  se = TRUE, col = "blue")

preds <- predict( gam1, 
                  newdata = data.frame( chas = "0", indus = 3, lstat = 5 )  )
preds

## Natural Splines ##

spline.ns1 = lm(y ~ ns(x, df = 1))
pred.ns1 = predict(spline.ns1, newdata = list(x = sort.x), se = TRUE)
se.bands.ns1 = cbind(pred.ns1$fit + 2 * pred.ns1$se.fit, 
                     pred.ns1$fit - 2 * pred.ns1$se.fit)

spline.ns2 = lm(y ~ ns(x, df = 2))
pred.ns2 = predict(spline.ns2, newdata = list(x = sort.x), se = TRUE)
se.bands.ns2 = cbind(pred.ns2$fit + 2 * pred.ns2$se.fit, 
                     pred.ns2$fit - 2 * pred.ns2$se.fit)

spline.ns3 = lm(y ~ ns(x, df = 3))
pred.ns3 = predict(spline.ns3, newdata = list(x = sort.x), se = TRUE)
se.bands.ns3 = cbind(pred.ns3$fit + 2 * pred.ns3$se.fit, 
                     pred.ns3$fit - 2 * pred.ns3$se.fit)

spline.ns4 = lm(y ~ ns(x, df = 4))
pred.ns4 = predict(spline.ns4, newdata = list(x = sort.x), se = TRUE)
se.bands.ns4 = cbind(pred.ns4$fit + 2 * pred.ns4$se.fit, 
                     pred.ns4$fit - 2 * pred.ns4$se.fit)

par(mfrow = c(2, 2))

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (1 df)", bty = 'l')
lines(sort.x, pred.ns1$fit, lwd = 2, col = "darkred")
matlines(sort.x, se.bands.ns1, lwd = 2, col = "darkred", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (2 df)", bty = 'l')
lines(sort.x, pred.ns2$fit, lwd = 2, col = "darkgreen")
matlines(sort.x, se.bands.ns2, lwd = 2, col = "darkgreen", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (3 df)", bty = 'l')
lines(sort.x, pred.ns3$fit, lwd = 2, col = "darkblue")
matlines(sort.x, se.bands.ns3, lwd = 2, col = "darkblue", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (4 df)", bty = 'l')
lines(sort.x, pred.ns4$fit, lwd = 2, col = "brown")
matlines(sort.x, se.bands.ns4, lwd = 2, col = "brown", lty = 3)

par(mfrow = c(1,2))
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Cubic Spline", bty = 'l')
lines(sort.x, pred3$fit, lwd = 2, col = "blue")
matlines(sort.x, se.bands3, lwd = 2, col = "blue", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (3 df)", bty = 'l')
lines(sort.x, pred.ns3$fit, lwd = 2, col = "darkblue")
matlines(sort.x, se.bands.ns3, lwd = 2, col = "darkblue", lty = 3)

## Smoothing Splines ##

smooth1 = smooth.spline(x, y, df = 3)
smooth2 = smooth.spline(x, y, cv = TRUE)

par(mfrow = c(1,2))
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Smoothing Spline (3 df)", bty = 'l')
lines(smooth1, lwd = 2, col = "brown")

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Smoothing Spline (CV)", bty = 'l')
lines(smooth2, lwd = 2, col = "darkorange")

## `Wage` Data ##

library("ISLR")
library("splines") # Required for regression and natural splines.
agelims=range(Wage$age)
age.grid=seq(from=agelims[1],to=agelims[2])

set.seed(1)

# Number of data points - change this to investigate 
# small, medium and large samples.
n <- 200 

# Take the a sample of n points from the data.
ind = sample(1:3000, n)
Wage1 = Wage[ind,]  # Label subset of data as Wage1.

# Cubic Spline
fitbs = lm(wage~bs(age, degree = 3, knots = c(30,40,60)), data = Wage1)
predbs = predict(fitbs, newdata = list(age = age.grid), se = T)

# Natural Spline
fitns = lm(wage~ns(age, knots = c(30,40,60)), data = Wage1)
predns = predict(fitns, newdata = list(age = age.grid), se = T)

# Smoothing Spline
fitss = smooth.spline(Wage1$age, Wage1$wage, cv = TRUE) 

# Generate the Plots.
par(mfrow=c(1,3))

# Cubic Spline
plot(Wage1$age, Wage1$wage, col = "darkgray", pch = 19, main = 'Cubic spline', 
     bty = 'l', xlab = 'age', ylab = 'wage', cex.lab = 1.4)
lines(age.grid, predbs$fit, lwd = 2, col = 'darkgreen')
abline(v = c(30,40,60), lty = 'dashed')

# Natural Spline
plot(Wage1$age, Wage1$wage, col = "darkgray", pch = 19, 
     main = 'Natural cubic spline', bty = 'l', 
     xlab = 'age', ylab = 'wage', cex.lab = 1.4)
lines(age.grid, predns$fit, lwd = 2, col = 'darkviolet')
abline(v = c(30,40,60), lty = 'dashed')

# Smoothing Spline
plot(Wage1$age, Wage1$wage, col = "darkgray", pch = 19, 
     main = 'Smoothing spline', bty = 'l', 
     xlab = 'age', ylab = 'wage', cex.lab = 1.4)
lines(fitss, lwd = 2, col = 'brown')

