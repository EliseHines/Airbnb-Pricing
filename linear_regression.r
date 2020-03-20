#Multiple Linear Regression
lm.fit = lm(price ~ neighbourhood_group + number_of_reviews +
minimum_nights + reviews_per_month + availability_365,
data = AirBnBNYC)
summary(lm.fit)


##remove insignificant predictors
lm.fit = lm(price ~ neighbourhood_group + number_of_reviews +
minimum_nights + availability_365, data = AirBnBNYC)
summary(lm.fit)

#########################################
# Training/testing model

n <- nrow(AirBnBNYC)
p <- ncol(AirBnBNYC) #for each of the 10 times I run my model, I change #the seed number to 1, 2, 3,..., 9, 10.
set.seed(1)
train <- sample(n, 0.8*n)

AirBnBNYC.train = AirBnBNYC[train, 1:p]
AirBnBNYC.train.labels <- AirBnBNYC[train, p+1]
AirBnBNYC.test = AirBnBNYC[-train, 1:p]
AirBnBNYC.test.labels <- AirBnBNYC[-train, p+1]


lm.fit = lm(price ~ neighbourhood_group + number_of_reviews + minimum_nights + availability_365, data = AirBnBNYC.train)

lm.probs = predict(lm.fit, AirBnBNYC.test, type="response")
lm.probs[1:10]
predict_err <- AirBnBNYC.test$price-lm.probs
predict_mse <- sum((AirBnBNYC.test$price-lm.probs)^2)/nrow(AirBnBNYC.test)
predict_mse

mean(c(122949.5, 124179.8, 70838.61, 72164.95, 62463.38, 43514.34, 62042.81, 66193.6, 101390.6, 82636.21))

#2d
lm.fit = lm(price ~ neighbourhood_group + number_of_reviews +
minimum_nights + reviews_per_month + availability_365,
data = AirBnBNYC)
summary(lm.fit)