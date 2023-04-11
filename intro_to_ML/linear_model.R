op <- par(mar = c(4, 4, 4, 4))  #margin formatting

barplot(mtcars$mpg, names.arg = row.names(mtcars), las = 2, ylab = "Fuel
Efficiency in Miles per Gallon")

pairs(mtcars[1:7], lower.panel = NULL)


plot(y = mtcars$mpg, x = mtcars$wt, xlab = "Vehicle Weight",
     ylab = "Vehicle Fuel Efficiency in Miles per Gallon")



# Q1
plot(y = mtcars$mpg, x = mtcars$cyl, xlab = "Vehicle Weight",
     ylab = "Vehicle Fuel Efficiency in Miles per Gallon")

mt.model <- lm(formula = mpg ~ wt, data = mtcars)

coef(mt.model)[2]
coef(mt.model)[1]

#Q2
mpg.hp.model = lm(formula = mpg ~ hp, data = mtcars)
coef(mpg.hp.model)[2]
coef(mpg.hp.model)[1]



plot(y = mtcars$disp , x = mtcars$mpg , xlab = "Engine Size (cubic inches)",
     ylab = "Fuel Efficiency (Miles per Gallon)")


model <- lm(mtcars$mpg ~ mtcars$disp)
coef(model)
coef(model)[2] * 200 + coef(model)[1]


split_size = 0.8

sample_size = floor(split_size * nrow(mtcars))

set.seed(123)
train_indices <- sample(seq_len(nrow(mtcars)), size = sample_size)

train <- mtcars[train_indices, ]
test <- mtcars[-train_indices, ]


model2 <- lm(mpg ~ disp, data = train)

new.data <- data.frame(disp = test$disp)

test$output <- predict(model2, new.data)

sqrt(sum(test$mpg - test$output)^2/nrow(test))

#Q3

model3 <- lm(mpg ~ hp, data = train)

new.data2 <- data.frame(hp = test$hp)

test$output.hp <- predict(model3, new.data2)

sqrt(sum(test$mpg - test$output.hp)^2/nrow(test))


library(e1071)
iris.svm <- svm(Species ~ ., data = iris)
table(iris$Species, predict(iris.svm, iris, type = "class"))

