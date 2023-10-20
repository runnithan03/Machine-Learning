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
model$coefficients

#exercise 1.3 
#my attempt!
#a
fitted(model) #can use fitted.values(model)
#b
residuals(model) #can use resid(model)
lsq.Q

#RTQ
#soln.
coef <- coef(model)
beta0hat <- coef[1]
beta1hat <- coef[2]

resid <- resid(model)
lsq.Q <- sum(resid^2)
lsq.Q

#exercise 1.4
#a
summary <- summary(model)
summary
#b
#located them all!
#c
se <- summary$sigma
se
#d
#these are the values for the test that b0
#and b1 = 0 or not, they are the respective estimate
#over the respective standard error
#the p-values tell us that the test I have said is 
#rejected for any significance test

#e
#the coefficients are significant because of the (***)

#f
rsquared <- summary$r.squared
rsquared

#g
#no it doesn't the value earlier was 0.9008112
#this was 0.8114608
#the solutions say we need to square this value, so:
cor(w,d)^2
#this is the same as the rsquared value calculated earlier

#exercise 1.5
#a
se.beta1 <- summary$coefficients[4]
#b
ts <- beta1hat/se.beta1
ts #yes, it agrees!
#c
n <- length(w)
2*(1-pt(ts,n-2))
#d
#because the t-values are so high and the test statistic 
#is so high, these values will always be significant

#e
#I didn't get this, remember qt does inverse t-dist.
beta1hat + c(-1,1)*qt(0.975,n-2)
#my attempt
confint(beta1hat,parm,level = 0.95)

#solution:
confint(model,level=0.95)[2,]

#exercise 1.6
#myattempt
predict(model,d=3, level = 0.95)
#soln.
newdata1 <- data.frame(d=3)
predict(model,newdata = newdata1, interval = "confidence",level = 0.95)
#second part
predict(model,newdata = newdata1, interval = "prediction",level = 0.95)

#exercise 1.7
plot(x=model$fitted.values,y=model$residuals)
plot(x=d,y=model$residuals)
#same plot, is it because the fitted values correspond to d?
#not appropriate because of non-linear behaviour

#exercise 1.8
hist(x=model$residuals)
#remember qqnorm gives the normal quantile plot
qqnorm(model$residuals)

#do seem normally distributed as qqplot line isn't straight
