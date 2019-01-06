data <- read.csv("pml-training.csv")
data <- data[,-1] #Remove row numbers 

#Transform data to numeric
data[,6:158] <- apply(data[,6:158], 2, function(x){as.numeric(as.character(x))})

#Remove variables with missing data >90%
round(sum(is.na(data))/prod(dim(data))*100)
count_nas <- apply(data, 2, function(var){
        sum(is.na(var))/length(var)*100
})
data <- data[-which(count_nas>90)]
round(sum(is.na(data))/prod(dim(data))*100)

#Remove non predictive variables
data <- data[,-1:-6]

#Build prediction model
library(caret)
set.seed(666)
trainIndex = createDataPartition(data$classe, p=0.1, list=FALSE)
training = data[trainIndex,]
testing = data[-trainIndex,]

library(randomForest)
mtry <- tuneRF(training[,-53], training[,53], stepFactor = 1.5)
mtry <- as.data.frame(mtry)
mtry_min <- mtry$mtry[which.min(mtry$OOBError)]
fit <- randomForest(classe~., data=training, ntree=100, mtry=mtry_min)

pred <- predict(fit, testing)
accuracy <- confusionMatrix(pred, testing$classe)
accuracy$overall[1]

varImpPlot(fit, pch=19, cex=.6)

#Test against correct submitted answers
test <- read.csv("pml-testing.csv")
submitted <- vector(length = 20)
for (i in 1:20) {
        filename <- paste0("submission/problem_id_", i, ".txt")
        submitted[i] <- (readChar(filename, 1))
}
submitted <- as.factor(submitted)

answers <- predict(fit, test)
sum(submitted == answers) / length(answers)*100
