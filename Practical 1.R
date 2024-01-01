#exercise 1.1
data(faithful)

w <- faithful[,2]
d <- faithful[,1]

plot(x=d,y=w)
cor(w,d)
#there is strong evidence of a linear relationship because 
#pmcc is close to 1 and there is a line of best fit
#however data is clustered towards edges so may not be linear?

#exercise 1.2 
model <- lm(w~d)
model$coefficients # same as coef(model)
fitted(model) # same as fitted.values(model)
resid(model) # same as residuals(model)

#exercise 1.3 
#a
coef <- coef(model)

#extracting coefficients as vectors
beta0hat <- coef[1]
beta1hat <- coef[2]

#b
lsq.resid <- resid(model) #extracts vector of residuals from model
lsq.Q <- sum(lsq.resid^2)
lsq.Q #sum of squares of the residuals

#summary(model) helps us extract:
#residuals
#coefficients: (Intercept), Estimate, Std. Error, t value, Pr(>|t|)
#residual standard error - se (the square root of the unbiased estimate of the residual variance)
#multiple R-squared - the R^2 value defined in lectures as the squared correlation
#coefficient and is a measure of goodness of fit (gof)

#We can also use the summary to access the individual elements of the regression summary output. If we save the results of a call to the summary function of a lm object as summ:
#summ$residuals – extracts the regression residuals as resid above
#summ$coefficients – returns the  p×4 coefficient summary table with columns for the estimated coefficient, its standard error, t-statistic and corresponding (two-sided) p-value.
#summ$sigma – extracts the regression standard error
#summ$r.squared – returns the regression  R^2

#exercise 1.4
#a
summary <- summary(model)
summary
#b
#coefficient estimates and their standard errors:
summary$coefficients
#residual standard error:
summary$sigma
#regression:
rsq <- summary$r.squared
rsq

#c
se <- summary$sigma
se

#d
#these are the values for the test that b0
#and b1 = 0 or not, 
#they are the respective estimate over the respective standard error
#the p-values tell us that the test I have said is 
#rejected for any significance test

#33.47440/1.1548735 - check for t value calculation

#e
#the coefficients are significant because of the (***)
#meaning there is sufficient to suggest both estimates are
#not equal to zero, so what happens to regression?

#solution:
# (d) & (e)
# The column t value shows you the t-test associated with testing the significance 
# of the parameter listed in the first column. 
# Both coefficients are significant, as p-values are very small.

#f
rsq <- summary$r.squared
rsq
#The coefficient of determination, or R2 , is a measure that provides 
#information about the goodness of fit of a model.

#g
#no it doesn't the value earlier was 0.9008112
#this was 0.8114608
#the solutions say we need to square this value, so:
cor(w,d) #0.9008112
cor(w,d)^2 #0.8114608
#this is the same as the rsquared value calculated earlier

#exercise 1.5
#a
#they do it using matrices and dimensions
summary(model)$coefficients
se.beta1 <- summary(model)$coefficients[2,2]
se.beta1
#b
ts <- (beta1hat-0)/se.beta1
#they then remove "d" name from summary feature using unname function
ts <- unname(ts)
ts

#c
n <- length(w)
#length of the column!
#this uses page 61 and 62 of pdf notes
#df = n - (k+1), we are only testing beta1, 
#so k = 1 -> df = n - 2
pt(ts,n-2)
2*(1-pt(ts,n-2))
#then use the last picture on page 61 to get the probability
#in pt, lower.tail is set as true normally so, pt(ts, n-2) gives the CDF 
#of the student's t-distribution for a quantile of ts with n-2 df, so the area 
#we want is the rejection region in the picture used at the bottom of 
#page 61 in the ML pdf notes, hence the 2(1-pt(ts, n-2)).

#d
summary$coefficients
#this is 2 x 4
summary$coefficients[2,3]  #t-value in the regression summary
#because the t-values are so high and the test statistic 
#is so high, these regression coefficients will always be significant

#e
#I didn't get this, remember qt does inverse t-dist.
beta1hat + c(-1,1)*qt(0.975,n-2)*se.beta1
#df is calculated by df = n - (k+1), we are only 
#testing beta1, so k = 1 -> df = n - 2, and c(-1,1) 
#does the plus minus in the confidence interval (CI)
#to get the 95% CI, we have this as 100(1-0.05), so alpha = 0.05
#so t(alpha/2) = t(0.025), this is the same as qt(1-0.025, n-2)
#which is qt(0.975, n-2 ) as stipulated

#f
#to get CI using function, do:
confint(model, level = 0.95) #gives CI for both beta coefficients

#for the slope parameter
confint(model,level=0.95)[2,] #same as: confint(model, parm = "d", level = 0.95)

#Suppose now that we are interested in some new point  X=x∗,
#and at this point we want to predict: (a) the location of the regression line, 
#and (b) the value of  Y

#exercise 1.6
#first part
newdata1 <- data.frame(d=3)
predict(model,newdata = newdata1, interval = "confidence",level = 0.95)
#second part
predict(model,newdata = newdata1, interval = "prediction",level = 0.95)
#prediction interval is wider than confidence interval

#SLR is simple linear regression
#To assess the SLR assumptions, we inspect the residual plot for particular features:

#Even and random scatter of the residuals about  0
#is the expected behaviour when our SLR assumptions are satisfied.

#Residuals shown evidence of a trend or pattern — The presence of a clear pattern or trend in the residuals suggests that  
#E[ϵ]i≠0 and  Cov[ϵi,ϵj]≠0. There is clearly structure in the data that is not explained by the regression model, and so a simple linear model is not adequate for explaining the behaviour of  Y.

#Spread of the residuals is not constant — if the spread of the residuals changes substantially as  
#x (or  yhat) changes then clearly our assumption of constant variance is not upheld.

#A small number of residuals are very far from the others and  0
#— observations with very large residuals are known as outliers. Sometimes these points can be explained through points with particularly high measurement error, or the effect of another variable which should be included in the model. Their presence could signal problems with the data, a linear regression being inadequate, or a violation of Normality.

#exercise 1.7
par(mfrow = c(1,2))
#plot of residuals against fitted values, and the residuals against the eruption durations side-by-side.
plot(x=model$fitted.values,y=model$residuals)
plot(x=d,y=model$residuals) 
#remember model$fitted.values is the same as fitted(model)
#and model$residuals is the same as resid(model)

#same plot, is it because the fitted values correspond to d?
#not appropriate because of non-linear behaviour

#In order to state confidence intervals for the coefficients and for predictions made using this model, 
#in addition to the assumptions tested above, we also require that the regression errors are normally distributed. 
#We can check the normality assumption using quantile plots.

#exercise 1.8
par(mfrow = c(1,2))
hist(x=model$residuals) #or resid(model)
#remember qqnorm gives the normal quantile plot
qqnorm(model$residuals) 

#do seem normally distributed as qqplot line is roughly straight
