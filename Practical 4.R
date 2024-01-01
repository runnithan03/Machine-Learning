install.packages("faraway")
library("faraway")

#Exercise 4.1: Polynomial and step-wise function regression
#a
y = seatpos$hipcenter
x = seatpos$Ht

plot(x = x, y = y)
#linear polynomial might be appropriate? #they say 2nd order

#b
#first order polynomial fitting:
model1 <- lm(y~x)
model1

summary(model1)

model2 <- poly(x=x, y = y, degree = 2)
model2

summary(model2) #summary for this is not very clear

#c