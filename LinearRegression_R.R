#Invoke MASS package which comprised of large collection of data sets and functions
library(MASS)

#The MASS library contains Boston dataset which records median house value using 13 predictors
#Some predictors are rm(average no. of rooms per house), age(average age of houses), and lstat(percent of households with low socioeconomic status)

attach(Boston)

names(Boston)

library(corrplot)

# Correlation matrix shows the linear relationship between variables of the Boston dataset
corr_matrix<-cor(Boston)
corrplot(corr_matrix, type="upper")

#train and test data split
data(Boston)
smp_size<-floor(0.75*nrow(Boston))
set.seed(12)
train_ind<-sample(seq_len(nrow(Boston)), size=smp_size)
train<-Boston[train_ind, ]
test<-Boston[-train_ind, ]

#Model build 
lm.fit = lm(medv~lstat, data=train)

# summary of the model that shows coefficients, R-squared and p-value 
summary(lm.fit)

# Scatter plot along with the trend line
plot(train$lstat,train$medv)

abline(lm.fit, lwd = 3, col = "red")

par(mfrow=c(2,2))
plot(lm.fit)

#predict test data
test$medv <- NULL
test_medv <- predict(lm.fit,test)

test <- cbind(test, as.data.frame(test_medv))

