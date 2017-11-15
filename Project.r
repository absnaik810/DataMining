# project.r
# Authors: Pratik Jain, Abhishek Naik
# Visualization of Car Evaluation dataset
# Classification using RandomForest and Decision Tree models
# install.packages("randomForest")
library(randomForest)
library(rpart)
rm(list = ls()) # clear all stored variables
data1 = read.csv("F:/Data mining/project/car.csv", header = T);
N = nrow(data1);

# create test and train data
ind <- sample(c(TRUE, FALSE), N, replace = T, prob=c(0.5, 0.5))
train <- data1[ind, ]
test <- data1[!ind, ]

### Decision Tree
model = rpart(data1$acceptability ~ ., data=data1, method="class")
plot(model)
text(model)

err_count = 0
p = predict(model, test, type="class")
for(i in 1:nrow(test)){
    if(p[i]!=test$acceptability[i])
        err_count = err_count + 1
}
print(err_count/N)
readline()
### Random Forest
model = randomForest(train$acceptability ~ . , data = train)
plot(model)
# text(model)
p = predict(model, newdata = test)

err_count = 0
for(i in 1:nrow(test)){
    if(p[i]!=test$acceptability[i])
        err_count = err_count + 1
}
print(err_count/N)
readline()

mosaicplot(~ lug_boot+acceptability +safety , data = data1, color = TRUE)
#readline()

mosaicplot(~ lug_boot +acceptability +persons , data = data1, color = TRUE)
#readline()
mosaicplot(~ lug_boot+acceptability +maint , data = data1, color = TRUE)
#readline()
mosaicplot(~ lug_boot+acceptability +buying , data = data1, color = TRUE)
#readline()
mosaicplot(~ lug_boot+acceptability +doors , data = data1, color = TRUE)
#readline()

## Convert all data to numeric for box plot
newdata = data1
newdata$buying<-as.character(newdata$buying)
newdata$buying[newdata$buying=="vhigh"] <- "4"
newdata$buying[newdata$buying=="high"] <- "3"
newdata$buying[newdata$buying=="med"] <- "2"
newdata$buying[newdata$buying=="low"] <- "1"
newdata$buying = as.numeric(newdata$buying)

newdata$maint<-as.character(newdata$maint)
newdata$maint[newdata$maint=="vhigh"] <- "4"
newdata$maint[newdata$maint=="high"] <- "3"
newdata$maint[newdata$maint=="med"] <- "2"
newdata$maint[newdata$maint=="low"] <- "1"
newdata$maint = as.numeric(newdata$maint)

newdata$doors<-as.character(newdata$doors)
newdata$doors[newdata$doors=="5more"] <- "5"
newdata$doors[newdata$doors=="4"] <- "4"
newdata$doors[newdata$doors=="3"] <- "3"
newdata$doors[newdata$doors=="2"] <- "2"
newdata$doors[newdata$doors=="1"] <- "1"
newdata$doors = as.numeric(newdata$doors)
#stopp;
newdata$persons<-as.character(newdata$persons)
newdata$persons[newdata$persons=="more"] <- "5"
newdata$persons[newdata$persons=="4"] <- "4"
newdata$persons[newdata$persons=="2"] <- "2"
newdata$persons = as.numeric(newdata$persons)

newdata$lug_boot<-as.character(newdata$lug_boot)
newdata$lug_boot[newdata$lug_boot=="small"] <- "1"
newdata$lug_boot[newdata$lug_boot=="med"] <- "2"
newdata$lug_boot[newdata$lug_boot=="big"] <- "3"
newdata$lug_boot = as.numeric(newdata$lug_boot)

newdata$safety<-as.character(newdata$safety)
newdata$safety[newdata$safety=="low"] <- "1"
newdata$safety[newdata$safety=="med"] <- "2"
newdata$safety[newdata$safety=="high"] <- "3"
newdata$safety = as.numeric(newdata$safety)
#stopp;
newdata$acceptability<-as.character(newdata$acceptability)
newdata$acceptability[newdata$acceptability=="unacc"] <- "1"
newdata$acceptability[newdata$acceptability=="acc"] <- "2"
newdata$acceptability[newdata$acceptability=="good"] <- "3"
newdata$acceptability[newdata$acceptability=="vgood"] <- "4"
newdata$acceptability = as.numeric(newdata$acceptability)

boxplot(acceptability ~ safety, data = newdata, color = TRUE, main="Acceptability Safety", 
  	xlab="safety", ylab="acceptability")
readline()
boxplot(acceptability ~ lug_boot, data = newdata, color = TRUE, main="Acceptability Lug_boot", 
  	xlab="lug_boot", ylab="acceptability")
readline()    
boxplot(acceptability ~ buying, data = newdata, color = TRUE, main="Acceptability Buying", 
  	xlab="buying", ylab="acceptability")