install.packages("faraway")
library("faraway")

names(fat)
?fat
str(fat)

head(fat)
summary(fat)

sum(is.na(fat))

fat1 <- fat[,-c(2,3,8)] 
# siri is variable 2, density variable 3 and free variable 8. 

#2.1
#a
dim(fat1) #18 columns in fat, 252 rows

#b
#pairwise scatterplot
pairs(fat1)

#c
#need to get explained before the exam because wtf!
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0(" ", r)
  text(0.5, 0.5, txt, cex = 0.8)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
# Create the plots
pairs(fat1, 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

install.packages("corrplot")
library(corrplot)
#checking correlation between variables
corrplot(cor(fat1), method = "number", type = "upper", diag = FALSE)

corrplot.mixed(cor(fat1), upper = "ellipse", lower = "number",number.cex = .7)

#Exercise 2.2
#a
reg1 <- lm(fat1$brozek~fat1$adipos)
#b
summary(reg1)
#c
names(reg1)

#Exercise 2.3
#a
confint(reg1)
#b
#to do an estimate, make a data frame which desired input
newdata= data.frame(adipos=22)

#my prediction interval isn't coming out right!
predict(reg1,data.frame(adipos=22), interval="confidence",  level = 0.95)
predict(reg1,data.frame(adipos=22), interval="prediction",  level = 0.95)

#Exercise 2.4
#a
plot(x=fat1$adipos, y=fat1$brozek)
#can see some positive correlation

#b
#to add the abline, apply it on the regression line
#lwd gives line width and pch gives dot types

plot(fat1$adipos, fat1$brozek, pch=16, col="cornflowerblue")
abline(reg1,lwd=3,col="red")

# (c)
par(mfrow=c(1,2)) # to have two plots side-by-side

#what does which do?
plot(reg1, which=1,  pch=16, col="cornflowerblue")
plot(reg1, which=2,  pch=16, col="cornflowerblue")

#Exercise 2.5
#a
reg2 <- lm(brozek ~ adipos + age, data = fat1)

#b
reg3 <- lm(brozek~., data = fat1)

#c
reg4 <- lm(brozek~.-age, data=fat1)

#d
summary(reg1)$r.sq
?summary.lm

#e
install.packages("car")
library(car)
vif(reg3)

#using interaction term:
summary(lm(brozek~chest*abdom, data=fat1))

#non-linear transformation of the predictors
summary(lm(brozek~adipos+I(adipos^2), data=fat1))

#other types of transformations:
summary(lm(brozek~log(adipos), data=fat1))

#Exercise 2.6
#a
install.packages("leaps")
library("leaps")

#my attempt
fwd <- regsubsets(x=reg1)

#solution:
fwd <- regsubsets(brozek~., data = fat1, method = "forward", nvmax=14)

#b
results <- summary(fwd)
results

#extracting results for criteria:
RSS = results$rss
r2 = results$rsq
Cp = results$cp
BIC = results$bic
Adj_r2 = results$adjr2

# Combine the calculated criteria values above into a single matrix.
criteria_values <- cbind(RSS, r2, Cp, BIC, Adj_r2)
criteria_values

#c
#i) 
which.min(Cp)
#ii)
which.min(BIC)
#iii)
which.max(Adj_r2)

# The model with 8 predictors (minimum Cp and maximum adjusted R-squared) 
# includes additionally the predictors age, neck, hip and thigh.

#(d) #I don't understand this stage - learn this for the exam
#make sure to check 2.7 too!
par(mfrow = c(1, 3))
plot(Cp, xlab = "Number of Predictors", ylab = "Cp", type = 'l', lwd = 2)
points(8, Cp[8], col = "red", cex = 2, pch = 8, lwd = 2)
plot(BIC, xlab = "Number of Predictors", ylab = "BIC", type = 'l', lwd = 2)
points(4, BIC[4], col = "red", cex = 2, pch = 8, lwd = 2)
plot(Adj_r2, xlab = "Number of Predictors", ylab = "Adjusted RSq", 
     type = "l", lwd = 2)
points(8, Adj_r2[8],  col = "red", cex = 2, pch = 8, lwd = 2)

par(mfrow = c(1, 1))
plot(fwd, scale = "Cp")

#Exercise 2.7
## SOLUTION
best = regsubsets(brozek~., data = fat1, nvmax = 14)
bwd = regsubsets(brozek~., data = fat1, method = 'backward', nvmax = 14)

which.min(summary(best)$cp)
which.min(summary(best)$bic)
which.max(summary(best)$adjr2)
which.min(summary(bwd)$cp)
which.min(summary(bwd)$bic)
which.max(summary(bwd)$adjr2)

# Yes, the three optimal models (under each of the criteria Cp, BIC and 
# adj-R-squared) for each of forward stepwise, backward stepwise and 
# best subset selections all have the same number of predictors.

# Cp
coef(fwd,8) 
coef(best,8) 
coef(bwd,8)

# BIC
coef(fwd,4) 
coef(best,4) 
coef(bwd,4)

# In addition,  the predictors are also the same.

# adjusted R squared is not needed because it has also 8 predictors