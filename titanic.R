library(tidyverse)

# read in data
test <- read_csv("test.csv")
train <- read_csv("train.csv")

# treat Pclass and Sex as factors
train$Pclass <- factor(train$Pclass)
train$Sex <- factor(train$Sex)

# split data into train and validation sets
train.split <- sample(1:891, size=445)
train.train <- train[train.split, ]
train.valid <- train[-train.split, ]

# model using just age and sex
train.model <- glm(Survived~Sex+Age, data=train.train, family = "binomial")

predict <-as.vector(predict(train.model, train.valid, type="response"))
probs <- rep(0,446)  # Initialize prediction vector
probs[predict >0.5] <- 1 # p>0.5 -> 1
table(probs, train.valid$Survived)
# 77.8% accuracy - fine

# now make model on full dataset
model <- glm(Survived~Sex+Age, data=train, family = "binomial")

predict <-as.vector(predict(model, test, type="response"))
probs <- rep(0,418)  # Initialize prediction vector
probs[predict >0.5] <- 1 # p>0.5 -> 1

# make predictions for test data
my_predictions <- as.data.frame(cbind(test$PassengerId, probs))
colnames(my_predictions) <- c("PassengerId", "Survived")

# write to csv
write.table(my_predictions, file = "predictions.csv", row.names=F, sep=",")