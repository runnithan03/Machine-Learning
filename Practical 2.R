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
dim(fat1) #15 columns in fat, 252 rows
#this represents 15 variables and 252 observations

#b
#pairwise scatterplot
pairs(fat1)

#c
#need to get explained before the exam because wtf!
#think GPT breakdown makes sense
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0(" ", r)
  text(0.5, 0.5, txt, cex = 0.8)
}

#GPT breakdown of panel.cor
#x and y are expected to be numerical vectors
#the first line of the function saves the current graphical parameters (specifically the usr parameter, which defines the plotting region) 
#and ensures that these parameters are restored when the function exits. This is a way to avoid side effects on the global 
#plotting environment after the function is executed.

#The next line sets the plotting region to be a unit square. The usr parameter is set to a vector c(0, 1, 0, 1), which means the x-axis will 
#range from 0 to 1 and the y-axis will range from 0 to 1. This is a standard setup for displaying correlation coefficients.

#Then we compute the correlation coefficient between x and y using the cor function. The result is then rounded to two decimal places.
#And then, we create a string from the correlation coefficient r, prefixed with a space for formatting purposes.

#Finally, we consider how we demonstrate the text: we display the text (the correlation coefficient) at the center of the unit square 
#(0.5, 0.5 are the coordinates for the center). The cex = 0.8 argument adjusts the size of the text.

# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}

#the line uses the points function to add points to a plot. The x and y arguments are the coordinates of the points. The pch = 19 argument 
#specifies the plotting character (or symbol) to use for the points. In R, pch = 19 corresponds to solid circles.

# Create the plots
pairs(fat1, 
      lower.panel = panel.cor,
      upper.panel = upper.panel)
#The lower.panel argument specifies the function to use for the lower triangle of the scatterplot matrix. 
#In your case, panel.cor is provided as the function for the lower panels.

#The upper.panel argument specifies the function to use for the upper triangle of the scatterplot matrix. 
#Here, upper.panel is the provided function, which fills all the points and colours them black

#the corrplot package does the above in a nicer way
install.packages("corrplot")
library(corrplot)

#checking correlation between variables
corrplot(cor(fat1), method = "number", type = "upper", diag = FALSE)
#cor(fat1) computes the correlation matrix for the fat1 dataset. The cor function calculates the 
#pairwise correlation coefficients between all variables (columns) in fat1.
#The result is a matrix where each element (i, j) represents the correlation coefficient between the i-th and j-th variables of the dataset.
#corrplot is a function used to create a visual representation of a correlation matrix.
#method = "number": This argument specifies that the correlation coefficients should be displayed as numbers in the plot.
#type = "upper": This argument indicates that only the upper triangle of the correlation matrix should be plotted. 
#In a correlation matrix, the lower and upper triangles are mirror images (since correlation is symmetric), so plotting just one triangle can make the plot less cluttered and easier to read.
#diag = FALSE: This argument specifies that the diagonal of the matrix (which would just be 1s, as they are the correlation of each variable with itself) should not be displayed.

corrplot.mixed(cor(fat1), upper = "ellipse", lower = "number",number.cex = .7)
#cor(fat1) computes the correlation matrix for the fat1 dataset. The cor function calculates the 
#pairwise correlation coefficients between all variables (columns) in fat1.
#The result is a matrix where each element (i, j) represents the correlation coefficient between the i-th and j-th variables of the dataset.
#corrplot.mixed is a function used to create a visual representation of a correlation matrix with different methods for the upper and lower triangles.
#upper = "ellipse": This argument specifies that the upper triangle of the matrix should be represented using ellipses. 
#The shape and orientation of the ellipses indicate the strength and direction of the correlation. 
#For example, an ellipse that is oriented along the diagonal from the bottom left to the top right indicates a positive correlation, 
#while an ellipse oriented from the top left to the bottom right indicates a negative correlation. The narrower the ellipse, the stronger the correlation.
#lower = "number": This argument indicates that the lower triangle of the matrix should display the actual numerical values of the correlation coefficients.
#number.cex = .7: This argument sets the size of the numbers in the plot. cex stands for character expansion factor, and .7 means that the numbers will be at 70% of the default size.

#Exercise 2.2
#a
reg1 <- lm(fat1$brozek~fat1$adipos) #or reg1 <- lm(brozek ~ adipos, data = fat1)
#Remember: a response variable goes on the left hand side and predictor variables go on the right hand side

#b
summary(reg1)

#c
names(reg1)
coef(reg1) #or reg1$coefficients

#Exercise 2.3
#a
confint(reg1, level = 0.95) #95% confidence interval is a default
#b
#to do an estimate, make a data frame with the desired input
newdata= data.frame(adipos=22)

#my prediction intervals come out wrong - check on this!
predict(reg1,newdata=newdata, interval="confidence",  level = 0.95)
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
#the above abline outlines the linear model of reg1 with line width 3 and colour red

# (c)
par(mfrow=c(1,2)) # to have two plots side-by-side
#what does which do?
plot(reg1, which=1,  pch=16, col="cornflowerblue")
#which=1 typically corresponds to a plot of residuals against fitted values. 
#This plot is useful for checking the assumption of homoscedasticity (constant variance) of residuals, which is an important assumption in linear regression analysis.

plot(reg1, which=2,  pch=16, col="cornflowerblue")
#When you use which=2, it typically generates a Q-Q plot (Quantile-Quantile plot) of the residuals. This plot is used to assess whether the residuals of the model are normally distributed, 
#which is an important assumption in linear regression analysis.

#Exercise 2.5
#a
reg2 <- lm(brozek ~ adipos + age, data = fat1)
summary(reg2)
#response on LHS and predictors on RHS

#b
reg3 <- lm(brozek~., data = fat1)
summary(reg3)
#neat shortcut to take everything else

#c
reg4 <- lm(brozek~.-age, data=fat1)
summary(reg4)
#neat shortcut to take everything else except age 

#d
?summary.lm
summary(reg1)$r.sq
summary(reg2)$r.sq
summary(reg3)$r.sq
summary(reg4)$r.sq #or summary(reg4)$r.squared
summary(reg4)$adj.r.squared #this gives the adjusted R Squared

#e
install.packages("car")
library(car)
vif(reg3)
#the Variance Inflation Factor (VIF) is used to assess 
#if there is any relationship between the independent variables.
#This assesses multicollinearity. Multicollinearity refers to a situation when two or more predictor variables in our
#multiple regression model are highly (linearly) correlated.
#VIF = 1 indicates that there is no correlation between ith predictor variable and the other predictor variables.
#As a rule of thumb if VIF > 10, then multicollinearity could be a problem.

#Multicollinearity: How to fix?
#Ignore: if the model is going to be used for prediction only.
#Remove: e.g. see if the variables are providing the same information.
#Combine: combining highly correlated variables.
#Advanced: e.g. Principal Components Analysis, Partial Least Squares.


#The VIF for age (2.250902) suggests that age has some linear relationship with other predictor variables in your model, 
#but this relationship is not so strong as to cause severe concerns about the reliability of the regression coefficients. 

#using interaction term:
summary(lm(brozek~chest*abdom, data=fat1))

#we can also include a non-linear transformation of the predictors, using the function I().
#For example, we can regress the response variable brozek on both adipos and adipos^2 as follows.
summary(lm(brozek~adipos+I(adipos^2), data=fat1))

#other types of transformations, e.g. taking the log of adipos:
summary(lm(brozek~log(adipos), data=fat1))

#Please review the practical demonstration 5.4) presented in the lecture.

#Practical Demonstration 5.4
#5.4.1 Data Handling
install.packages('ISLR')
library(ISLR)
?Hitters

names(Hitters)
head(Hitters)
dim(Hitters)

#Prior to proceeding to any analysis we must check whether the data contains any missing values. 
#To do this we can use a combination of the commands sum() and is.na(). 

sum( is.na( Hitters ) ) # checks for missing data in the entire dataframe.
sum( is.na( Hitters$Salary ) ) # checks for missing data in the response only.

Hitters = na.omit( Hitters ) # removes entries to the dataframe with any data missing.
dim( Hitters )
sum( is.na( Hitters ) ) # check that there is now no missing data in the dataframe.

#5.4.2 Best Subset Selection

#Now we are ready to start with best subset selection. There are different packages in R which perform best subset as well as stepwise selections. 
#We will use the function regsubsets() from library leaps (again install.packages('leaps') is needed if not already installed). 
#regsubsets() essentially performs step 2 of the algorithm presented in Section 5.1, although yields results that make step 3 relatively straightforward. 
#Note that the . part of Salary~. indicates that we wish to consider all possible predictors in the subset selection method. 
#We will store the results from best subset in an object called best (any name would do) and summarize the results via summary(), which outputs the best models of different dimensionality.

library(leaps)
demo_best = regsubsets(Salary~., Hitters)
summary(demo_best)

#The asterisks * indicate variables which are included in model Mk,k=1,...,19 as discussed in the subset selection algorithm of Section 5.1. 
#For instance, we see that for the model with one predictor that variable is CRBI, while for the model with four predictors the selected variables are Hits, CRBI, DivisionW and PutOuts. 
#To interpret the asterisks, you look at them horizontally, so notice how for the top row, CRBI is the only variable with an asterik, meaning for a model with only one predictor variable, the best 
#regression model is produced by using CRBI, the same applies for 4 asteriks, which has the variables: Hits, CRBI, DivisionW and PutOuts (row 4)
#As we see the summary() command displayed output up to the best model with 8 predictors; this is the default option in regsubsets(). 

#To change this we can use the extra argument nvmax. Below we re-run the command for all best models (19 in total). Now we also store the output of summary() in an object called results and further 
#investigate what this contains via names(). As we can see there is a lot of useful information! Of particular use to us, we can see that it returns  R2, RSS, adjusted-R2,  Cp and BIC. 
#We can easily extract these quantities; for instance below we see the R2 values for the 19 models.

prac_best = regsubsets(Salary~., data = Hitters, nvmax = 19)
prac_results = summary(prac_best)
names(prac_results)

prac_results$rsq

#Lets store these quantities as separate objects. We can also stack them together into one matrix as done below and see the values.
prac_RSS = prac_results$rss
prac_r2 = prac_results$rsq
prac_Cp = prac_results$cp
prac_BIC = prac_results$bic
prac_Adj_r2 = prac_results$adjr2

cbind(prac_RSS, prac_r2, prac_Cp, prac_BIC, prac_Adj_r2)

#We know that RSS should steadily decrease and that  R2 increase as we add predictors. Lets check that by plotting the values of RSS and  R2
#We would like to have the 2 plots next to each other so do par(mfrow(c(1,2))), indicating we require the plotting window to contain 1 row and 2 columns, before calling the command plot().
par(mfrow = c(1, 2))
plot(prac_RSS, xlab = "Number of Predictors", ylab = "RSS", 
     type = "l", lwd = 2)
plot(prac_r2, xlab = "Number of Predictors", ylab = "R-square", 
     type = "l", lwd = 2)

#now let's find how many predictors are included in the optimal models under Cp, BIC and adjusted-R2 and produce some plots illustrating this information.
which.min(prac_Cp) #10
which.min(prac_BIC) #6
which.max(prac_Adj_r2) #11

par(mfrow = c(1, 3))
plot(prac_Cp, xlab = "Number of Predictors", ylab = "Cp", 
     type = 'l', lwd = 2)
points(10, prac_Cp[10], col = "red", cex = 2, pch = 8, lwd = 2)
plot(prac_BIC, xlab = "Number of Predictors", ylab = "BIC", 
     type = 'l', lwd = 2)
points(6, prac_BIC[6], col = "red", cex = 2, pch = 8, lwd = 2)
plot(prac_Adj_r2, xlab = "Number of Predictors", ylab = "Adjusted RSq", 
     type = "l", lwd = 2)
points(11, prac_Adj_r2[11],  col = "red", cex = 2, pch = 8, lwd = 2)

#Cp and adjusted-R2 select 10 and 11 predictors respectively, while the optimal BIC model has only 6 predictors. In the code above we use the command points() to highlight the optimal model; 
#the first two arguments of this command correspond to x-axis and y-axis co-ordinates. So, for example, we know that the optimal Cp model is the one with 10 predictors; therefore the first argument 
#is set to 10 and the second argument is set to Cp[10] (i.e., Cp at its minimum).

#The regsubsets() in-built function for plotting the results is very convenient when the output from summary() is difficult to read due to there being many possible predictors. 
#Below we see the visualisation for the BIC models. Remark: Because previously we used par(mfrow(1,3)) we have to either close the plotting panel by typing dev.off() 
#(this returns the window plot to the default state), or respecifying the window splitting using par( mfrow = c(1,1) ).

par(mfrow = c(1,1)) #dev.off()
plot(prac_best, scale = "bic")

#The top row corresponds to the best model, while the bottom row to the worst model according to the chosen criterion. White squares correspond to variables that are excluded under each model.
#Finally, we extract the model coefficients for any model we choose via the command coef. For instance, the best models under each approach are the following.
coef(prac_best,10) #Cp
coef(prac_best,6) #BIC
coef(prac_best,11) #adj-Rsq

#5.4.3 Forward Stepwise Selection
prac_fwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, 
                      method = "forward")
#method = "forward" is the only difference between prac_fwd and prac_best
summary(prac_fwd)

#The analysis will then be essentially identical to the previous analysis for best subset selection.
#One interesting thing to check is whether the results from best subset and forward stepwise selection 
#are in agreement (we know that this will not necessarily be the case). For example, let’s have a look at the models with 6 and 7 predictors.

coef(prac_best, 6)
coef(prac_fwd, 6)

coef(prac_best, 7)
coef(prac_fwd, 7)

#So, the results for the model with 6 predictors are in agreement (which is good to know because this would be one of the three models one would likely select), 
#but as we see the results for the models with 7 predictors are different.

#5.4.4 Best Subset Selection: Validation
#In this section, we consider applying best subset selection not based on model selection criteria but using the validation approach.
#As we know, for validation (and cross-validation) we need to be able to predict from our linear models. Unfortunately, 
#regsubsets() doesn’t have its own built-in function for that (most similar types of R functions have). 

#we will just make use of the function predict.regsubsets below which will do the predictions for us. The important aspects of the function are the arguments. 
#object should be the result of a call to the function regsubsets() (for example the object best in Section 5.4.2 or fwd in Section 5.4.3), newdata should be a dataframe of data at which we wish to predict at, 
#and id indicates that we wish to use the resulting model from regsubsets with id number of predictors (so in this case could range from 1,2,…19).
#The output is the response prediction for each combination of inputs. Note that if you wish to use this function in your own code you will need to define it by copy and pasting this code segment.

predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

#The validation approach is based on splitting the data once into a training sample and a validation or testing sample. We also need to decide how to split the data. 
#A common approach is to use 2/3 of the sample for training and 1/3 of the sample for testing (although it does not have to be so). Note the first line: set.seed. 
#If we removed this command, then every time we ran the following code we could get different results (for example, variables selected) due to the effect of the random sample (do feel free to try this, or alternatively changing the seed number). 
#set.seed ensures a particular random state for any particular integer value, thus making the code reproducible (i.e. the same every time).

set.seed(10) #for the results to be reproducible
#another set.seed is put later on to make a new random bit of data but then to make sure that random bit varies from set.seed(10)
dim(Hitters)

training.obs = sample(1:263, 175)
Hitters.train = Hitters[training.obs,  ]
Hitters.test = Hitters[-training.obs,  ]
dim(Hitters.train)

dim(Hitters.test)
#validation is then executed as follows. We first apply regsubsets() to the training set. By doing so note how we are only using the training data for both steps 2 and 3 of the algorithm in Section 5.1.

best.val = regsubsets(Salary~., data = Hitters.train, nvmax = 19)
#Then we create an empty vector called val.error where we will store the 19 (because we have 19 models) validation MSEs. We then use a for loop inside which we (i) generate predictions using predict.regsubsets() and (ii) calculate the validation MSEs. 
#At the end we just locate which model produces the lowest MSE. The code is the following.

val.error<-c()
for(i in 1:19){
  pred = predict.regsubsets(best.val, Hitters.test, i)
  val.error[i] = mean((Hitters.test$Salary - pred)^2)
}
val.error
which.min(val.error)
#In this instance we see that the model with 10 predictors is selected as the best. This was also the model selected by  Cp in Section 5.4.2.

#5.4.5 What about inference?
#Note that the aim of subset selection is to find which predictors should be included within our model, not to identify the specific model (including coefficient values). 
#Therefore, it is important to note that there is a final step to fitting a model by validation, and that is to train the same 10-predictor model to the entire data. 
#This will yield different coefficient values to the current coefficients. The current coefficients should not be used because these were “learned” from the reduced training sample (that is, we didn’t use all of the available data).
#For the purpose of fitting a specific model (with specified predictors) we use the command lm() in R.

coef(best.val, 10) # Check which variables to use in the lm.
ls10 = lm(Salary ~ AtBat + Hits + Runs + Walks + CAtBat + CRuns + CRBI 
          + CWalks + Division + PutOuts, data = Hitters)
#As we have seen the output from regsubsets() relates to the model selection procedure; i.e. we get the values of Cp,BIC, adjusted-R2.
#We can also extract model specific regression coefficients via the command coef(). 
#However, the lm object above is required for quantities such as standard errors, confidence and prediction intervals etc., for example, recalling that summary() provides a summary, and confint() provides confidence intervals. 
#In addition, we can now use this model for prediction purposes using the built-in predict function for linear models (which is distinct to the predict.regsubsets function we defined above).

summary(ls10)
confint(ls10, 1:11)
test.data <- data.frame( "AtBat" = 322, "Hits" = 90, "HmRun" = 14, 
                         "Runs" = 40, "RBI" = 22, "Walks" = 40, 
                         "Years" = 4, "CAtBat" = 3000, "CHits" = 830, 
                         "CHMRun" = 100, "CRuns" = 250, "CRBI" = 600, 
                         "CWalks" = 300, "League" = "N", "Division" = "W", 
                         "PutOuts" = 600, "Assists" = 60, "Errors" = 7, 
                         "newLeague" = "N" )
#predicting using linear model!
predict( ls10, newdata = test.data )

#5.4.6 Variance of selection based on validation

#When using validation and cross-validation for best subset selection (or any stepwise method) it is important to understand that results depend on the random splitting between training and testing samples. 
#This is generally a drawback which is not discussed a lot, especially for the simple validation approach. The below code implements the validation approach presented previously but now starting from 50 different random seeds, 
#storing the number of predictors in the selected model each time.
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

#The histogram below shows that the final selection of the best model can vary a lot, even in terms of the number of predictors! Note that in addition, the histogram doesn’t tell the whole story because each time, say, 
#a 10-predictor model is chosen, it may be choosing different predictors. In this case we can see that the validation method is most often choosing a model with 8, 9 or 10 predictors.

hist(min.valid, col = 3, breaks = seq( from = 0.5, to = 19.5, length = 20 ), 
     xlab = 'Number of Predictors', 
     main = 'Best Subset Selection with validation')
abline(v = mean(min.valid), col = 2, lwd = 4)
legend('topright', legend=c('Average selection'),bty = 'n', lty = 1, 
       lwd = 4, col = 2)

#5.4.7 Cross-validation
#Now lets turn our attention to the K-fold cross-validation (CV) approach. This procedure is generally preferable to simple validation because it utilizes the entire sample for training as well as for testing.
#In contrast to the application of validation in Section 5.4.4, where we used the training data for both steps 2 and 3 of the algorithm presented in Section 5.1, we here use the full dataset in the initial regsubsets() command (step 2), 
#and cross-validation for step 3 only. Therefore step 2 proceeds as before:
prac_best = regsubsets(Salary~., data = Hitters, nvmax = 19) #same definition as above!

#Since step 3 involves fitting models with specified predictors, we need to once again use the lm() function in R. Since this would involve manually specifying 19 models using lm(), we will instead demonstrate 
#how to obtain a K-fold cross-validation error for just 3 of the models suggested by regsubsets() only.

#In particular, we are going to answer the following question... which of the different models chosen under Cp, BIC and adjusted-R2 respectively in Section 5.4.2 should we choose? Recall Cp yielded the model with 10 variables,
#BIC the one with 6, and adjusted-R2 the one with 11. Therefore we will calculate the K-fold cross-validation error of the models with 6, 10 and 11 variables suggested by regsubsets() only. Let's remind ourselves of the variables in those 3 models:
coef(prac_best, 10) #Cp
coef(prac_best, 6) #BIC
coef(prac_best, 11) #adj-Rsq

ls10 = lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + CWalks +
            Division + PutOuts + Assists, data = Hitters)
ls6 = lm(Salary ~ AtBat + Hits + Walks + CRBI + Division + PutOuts, 
         data = Hitters)
ls11 = lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + CWalks +
            + League + Division + PutOuts + Assists, data = Hitters)
#so you use which.min or which.max to determine the number of predictors, then use coef to extract which predictors these correspond to,
#and then put that into a linear model with data = Hitters

#Now we turn to calculating K-fold cross-validation errors for these models. A first question is how many folds to use. Common choices in practice are 5 and 10, leading to 5-fold and 10-fold CV. 
#In this example we will use 10 folds. Of course, often the sample size n will not be perfectly divisable by K (as in our case where n=263 and K=10). This does not matter, since it is not a problem if some folds have fewer observations.

#Lets see how we can create folds in R. First we need to form an indicator vector with length equal to 263 containing the numbers 1,2,3,4,5,6,7,8,9,10 roughly the same number of times. This can be done by combining the commands cut() and seq() in R as below. 
#The command table() then tabulates how many of each integer are present in the resulting vector.
k = 10
folds = cut(1:263, breaks=10, labels=FALSE)
folds
#remember 263 is our number of observations
table(folds)

#It is then very important to randomly re-shuffle this vector, because we do not want the fold creation procedure to be deterministic. We can do this very easily in R using once again the command sample().
set.seed(2)
folds = sample(folds)
folds

#Finally, we need to create a matrix this time with K=10 rows and 3 columns. The validation MSEs of the folds for model ls10 will be stored in the first column of this matrix, 
#the validation MSEs of the folds for model ls6 will be stored in the second column of this matrix, and the validation MSEs of the folds for model ls11 will be stored in the final column.

cv.errors = matrix(NA, nrow = k, ncol = 3, 
                   dimnames = list(NULL, c("ls10", "ls6", "ls11")))
cv.errors 

#The NA entries will be replaced with the MSEs as the loop below progresses. The part dimnames = list(NULL, c("ls10", "ls6", "ls11")) just assigns no names to the rows and names the columns "ls10", "ls6" and "ls11" respectively.
#Now for calculating the cross-validation errors we will need a for loop to loop over the folds. The important part is to understand what data = Hitters[folds!=i, ] is doing. In R != means “not equal”. 
#So for example when i=1 we discard the rows of Hitter for which the corresponding fold vector equals 1. We then predict only for the rows for which the fold vector equals i (in Hitters[folds==i, ] and Hitters$Salary[folds==i]). We then store each validation MSE in the appropriate row and column of cv.errors.

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
#We may choose ls6 as it has slightly smaller average cross-validation error, although in this case there is not that much in it. Since we also tend to favour models with fewer predictors as they tend to be more interpretable, we would probably choose ls6.

#Exercise 2.6
#a
install.packages("leaps")
library("leaps")

fwd <- regsubsets(brozek~., data = fat1, method = "forward", nvmax=14)
# the method = "forward" argument specifies that forward selection should be used as the method for subset selection in regression analysis. 
#Forward selection is a stepwise approach to building a regression model, starting with no variables and adding them one by one. 

#This is a method in regression analysis where you fit all possible models with each possible combination of predictor variables and then select the best model based on some criterion. 
#It's particularly useful when you have a relatively large number of predictors and you want to find the best subset of these predictors for your regression model.

#regsubsets fits a regression model for every possible combination of the predictor variables. For example, if you have 3 predictors, it will fit models for each predictor individually, 
#each pair of predictors, and all three predictors together, resulting in 7 different models (2^3 - 1).

# The function then evaluates each model based on a selection criterion. Common criteria include the adjusted R-squared, the Bayesian Information Criterion (BIC), the Akaike Information Criterion (AIC),
# Mallows' Cp, and others. These criteria balance model fit with model complexity (number of predictors).

#The output of regsubsets includes details about which variables are included in each model and the performance of each model according to the selection criteria. 
#This allows you to compare models and choose the one that best meets your analysis goals.

#b
results <- summary(fwd)
results

#extracting results for criteria:
#extract the following statistics for the model of each size: RSS,  
#R2, Cp, BIC and adjusted  R2. Combine them into a single matrix.
RSS = results$rss
r2 = results$rsq
Cp = results$cp
BIC = results$bic
Adj_r2 = results$adjr2

# Combine the calculated criteria values above into a single matrix.
criteria_values <- cbind(RSS, r2, Cp, BIC, Adj_r2)
criteria_values

#c
#extracts the number of predictors in the minimum/ maximum statistic specified
#i) 
which.min(Cp)
#ii)
which.min(BIC)
#iii)
which.max(Adj_r2)

coef(fwd, 4)
# Looking at the summary above, the model with 4 predictors (minimum BIC) 
# includes the predictors weight, abdom, forearm and wrist.

coef(fwd, 8)
# The model with 8 predictors (minimum Cp and maximum adjusted R-squared) 
# includes additionally the predictors age, neck, hip and thigh.

#(d)
#make sure to check 2.7 too!
par(mfrow = c(1, 3))
#plotting Cp against number of predictors highlighting the optimal model appropriately on each plot using a big red point.
plot(Cp, xlab = "Number of Predictors", ylab = "Cp", type = 'l', lwd = 2) #l stands for line
points(8, Cp[8], col = "red", cex = 2, pch = 8, lwd = 2)
#plotting BIC against number of predictors highlighting the optimal model appropriately on each plot using a big red point.
plot(BIC, xlab = "Number of Predictors", ylab = "BIC", type = 'l', lwd = 2)
points(4, BIC[4], col = "red", cex = 2, pch = 8, lwd = 2)
#plotting Adjusted RSq against number of predictors highlighting the optimal model appropriately on each plot using a big red point.
plot(Adj_r2, xlab = "Number of Predictors", ylab = "Adjusted RSq", 
     type = "l", lwd = 2)
points(8, Adj_r2[8],  col = "red", cex = 2, pch = 8, lwd = 2)

#(e)
#This visualises the model obtained by Cp
par(mfrow = c(1, 1))
plot(fwd, scale = "Cp")

#Exercise 2.7
best = regsubsets(brozek~., data = fat1, nvmax = 14)
# the nvmax parameter specifies the maximum number of variables to include in the models that are being considered
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

#So, which.min and which.max outline the number of predictors for Cp, BIC and adjusted-R2
#Coef then tells us which specific predictors are in the model outlined, 
#we need to say the number of predictors which.min or which.max outline

