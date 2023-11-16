#Exercise 3.1
#a
install.packages("faraway")
library("faraway")

#b
#seat position could be a good response variable

#solution:
# hipcenter would be a good response variable, as it is the only variable 
# in the dataset that considers the location of a person within the car.  
# This can be used as a proxy for where different drivers will position the 
# seat, which is the quantity of interest for car designers.

#c
#height and weight. should be more

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
dim(seatpos)
is.na(seatpos) #use sum(is.na(seatpos))

#e
pairs( seatpos, pch = 16, col = 2 ) #you can calculate specific correlation using the cor function
cor(seatpos[,9], seatpos[,-9]) 
#rough line of correlation from bottom left to top right

#there conclusion:
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

#Exercise 3.2
#a
y <- seatpos$hipcenter

#b
x = model.matrix(hipcenter~., seatpos)[,-1]

#c
library("glmnet")
ridge = glmnet(x, y, alpha = 0, nlambda = 200)

#d
plot(ridge, xvar = 'lambda')

#Exercise 3.3
lasso = glmnet(x, y)
par(mfrow = c(1,2)) #remember this puts both plots in 1 image 
plot(lasso, xvar = 'lambda')
plot(lasso)

lasso$lambda
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

#Exercise 3.4
#a
