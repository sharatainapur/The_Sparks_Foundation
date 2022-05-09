library(Metrics)

#Reading the Dataset
dat=read.csv("D:\\The Sparks Foundation\\student_scores - student_scores.csv")

#Printing the Dataset
dat

#Dimensions of the Dataset
dim(dat)
n=nrow(dat)
n

set.seed(905)
#Train-Test split using sample method.
i.train <- sample(1:n, 0.8*n, replace = FALSE)

#Train and the Test dataset
x.train<-dat[i.train,]
x.test<-dat[-i.train,]

#Columns in the Dataset
colnames(x.train)

#Attaching the dataset
attach(dat)

#Defined Scatter Plot
plot(Hours,Scores, xlab="Number of Study Hours", ylab="Percentage Score", main="Student Study Hours vs Percentage", pch=20, cex=1.1, col="blue")

#Scatter Plot
pairs(dat)

#Linear_Regression Model - Scores regressed over No of hours of study.
linear_model <- lm(formula = Scores~Hours, data=x.train)
plot(x.train$Hours,x.train$Scores, xlab="Number of Study Hours", ylab="Percentage Score", main="Student Study Hours vs Percentage", pch=20, cex=1.1, col="blue")
abline(linear_model, col="blue")

#Model Summary and related outputs
summary(linear_model)
linear_model$coefficients
linear_model$fitted.values

#Computing the Performance Metrics
observed <- x.test
score_predicted <- predict(linear_model,x.test, type="response")

#MAE
mae(observed$Scores, score_predicted)
mae(observed$Scores, predict(linear_model,x.test))
cbind(observed,score_predicted)

#Predicitng the Percentage for hours of study = 9.25
newd <- data.frame(Hours = 9.25)
predict(linear_model,newd, type = "response")
#The predicted percentage would be ~ 93.91.

#MSE
mean(linear_model$residuals^2)

#RMSE
sqrt(mean(linear_model$residuals^2))

