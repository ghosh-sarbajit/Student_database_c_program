# cofusion matrix
train_data <- read.csv("./datasets/titanic_train.csv")
test_data <- read.csv("./datasets/titanic_test.csv")


model <- glm(Survived ~ Pclass , family = binomial(link = "logit"), train_data)
p <- predict(model, test_data, type = "response")
summary(p)

p_class <- ifelse(p > 0.50, 1, 0)
table(p_class)

table(p_class, test_data[["Survived"]])


# install.packages("caret")
# install.packages('e1071')
library(caret)
library(e1071)

p_class <- as.integer(p_class)
p_class <- as.factor(p_class)

myfacor <- as.factor(test_data[["Survived"]])
caret::confusionMatrix(p_class, myfacor)


