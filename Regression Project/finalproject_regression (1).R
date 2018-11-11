#cleaning dataset

summary(communities)
commdf=as.data.frame(communities)

is.na(commdf)<-communities=="?"
summary(commdf)
myData=commdf
NumRows = dim(myData)[1]
NumCols = dim(myData)[2]
#Removing columns with more than 25% missing data
tol = .25
for(c in NumCols:1)
{
  if(sum(is.na(myData[,c])) > tol*NumRows) myData[,c]=NULL
}
summary(myData)
myData=myData[,-2]
myData=myData[,-28]
summary(myData)

#cross validation with 75-25 split on the dataset
library(caTools)
sampledData <- sample.split(myData$V128, SplitRatio = 0.75)
training_set <- subset(myData, sampledData == TRUE)
testing_set <- subset(myData, sampledData == FALSE)

#training the model and making predictions on testing dataset
lr.model = glm(V128~., data=training_set,family="binomial")
lr.pred = predict(lr.model,newdata = testing_set, type="response")
test<-t(lr.pred)
test1=t(test)
res=testing_set[,-102]
output=cbind(res,test1)
head(output)
#MSE for glm
library(Metrics)
mse(testing_set[,102], test1)
#0.01641382

#regression - ridge and lasso 

x=model.matrix(v128~.,output)
y=output$v128
#lambda values
grid=10^seq(10,-2,length=100)

#Ridge Regression 

library(glmnet)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
set.seed (1)
cv.out =cv.glmnet(x,y,alpha =0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam
#4.92564e-05
min(cv.out$cvm)
#0.001360499

#predictions using best lambda
x_test=model.matrix(V128~.,testing_set)
y_test=testing_set$V128
ridge.predi = predict(ridge.mod, s = bestlam, newx = x_test)
mse(y_test, ridge.predi)
#0.0161367

#Lasso Regression 
lasso.mod =glmnet (x,y,alpha =1, lambda =grid)
plot(lasso.mod)
set.seed (1)
cv.out1 =cv.glmnet (x,y,alpha =1)
plot(cv.out1)
bestlam1 =cv.out1$lambda.min
bestlam1
#4.92564e-05
min(cv.out1$cvm)
#predictions using best lambda
lass.predi = predict(lasso.mod, s = bestlam1, newx = x_test)
mse(y_test, lass.predi)
#0.01752406





