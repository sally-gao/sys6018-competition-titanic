library(tidyverse)

test <- read_csv("test.csv")
train <- read_csv("train.csv")

train$Pclass <- factor(train$Pclass)
train$Sex <- factor(train$Sex)

train.split <- sample(1:891, size=445)
train.train <- train[train.split, ]
train.valid <- train[-train.split, ]

# Just age and sex
train.model <- glm(Survived~Sex+Age, data=train.train, family = "binomial")

predict1 <-as.vector(predict(train.model, train.valid, type="response"))
probs1 <- rep(0,446)  # Initialize prediction vector
probs1[predict1 >0.5] <- 1 # p>0.5 -> 1
table(probs1, train.valid$Survived) # 77.8%

# Age + sex + parch
train.model2 <- glm(Survived~Sex+Age+Parch, data=train.train, family = "binomial")

predict2 <-as.vector(predict(train.model2, train.valid, type="response"))
probs2 <- rep(0,446)  # Initialize prediction vector
probs2[predict2 >0.5] <- 1 # p>0.5 -> 1
table(probs2, train.valid$Survived) # 77.8%
