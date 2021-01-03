install.packages("caret"); install.packages("randomForest"); install.packages("rpart"); 
library(lattice); library(ggplot2); library(caret); library(randomForest); library(rpart); library(rpart.plot);
library(dplyr); library(rattle)
set.seed(2121)

train_data <- read.csv(file.path("C:/Users/User/Documents/R Directory","pml-training.csv"), na.strings=c("NA","#DIV/0!", ""))
test_data <- read.csv(file.path("C:/Users/User/Documents/R Directory","pml-testing.csv"), na.strings=c("NA","#DIV/0!", ""))
train_data<-train_data[,colSums(is.na(train_data)) == 0]
test_data <-test_data[,colSums(is.na(test_data)) == 0]
train_data <-train_data[,-c(1:7)]
test_data <-test_data[,-c(1:7)]
groupByClasse <- train_data %>%
  group_by(classe) %>% 
  summarise(counts = n())
g <- ggplot(groupByClasse, aes(x = classe, y = counts)) + geom_bar(stat = "identity")
g <- g + geom_bar(stat = "identity")
g <- g + ggtitle("Total number of records for each group in classe")
g <- g + xlab("classe_Groups")
g <- g + ylab("Total")
plot(g)


index <- createDataPartition(y=train_data$classe, p=0.75, list=FALSE)
train <- train_data[index, ] 
test <- train_data[-index, ]


controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
model_RandForest <- train(classe ~ ., data=train, method="rf",
                          trControl=controlRF)

confusion_rand <- confusionMatrix(predict(model_RandForest, newdata=test), as.factor(test$classe))
confusion_rand
plot(modFitRandForest)

##### The model with random forest ended up having around 99% accuracy which makes it harder for the other models to beat

decision_tree <- train(classe ~ ., data=train, method = "rpart")

# Normal plot
fancyRpartPlot(decision_tree$finalModel, main = "Classification Tree for classe")

confusion_tree <- confusionMatrix(predict(decision_tree, newdata=test), as.factor(test$classe))
confusion_tree

### The accuracy using Classification Tree turns out to be only 49%, given the pre-processing I considered.
### In the end we see that Random Forest is the winner here and can better predict the classe variable's levels.


#### Now it is time to apply the Random Forest Model to the original test data and see how it performs when facing with the data it never used for training the model


predict_TEST<-predict(model_RandForest,newdata = test_data)
predict_TEST
