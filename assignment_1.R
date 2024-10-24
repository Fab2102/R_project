############ Task 1 ############ 

# read in the CSV file
data <- read.csv("airbnb.csv", sep=",", header = TRUE)

# look at the data
head(data)

# check type of variables in the dataset
str(data)

# check for missing data
sum(is.na(data))

# eliminate variables which are not meaningful predictors
data <- data[, -c(1)]

# turn categorial variables into factors
data$room_type <- factor(data$room_type)

# remove doubled information (attr_index, rest_index)
data <- data[, -c(14, 16)]



############ Task 2 ############

# descriptive statistics of variables
summary(data)
str(data)

numeric_columns <- sapply(data, is.numeric)
numeric_data <- data[, numeric_columns]
cor(numeric_data)

# visualizing data of variables which seem to be related to realSum
plot(realSum ~ person_capacity, data = data, log = "y")
plot(realSum ~ room_type, data = data, log = "y")
plot(realSum ~ metro_dist, data = data, log = "y")



############ Task 3 ############

# model_1
fitAll <- lm(realSum ~ ., data = data)
summary(fitAll)



############ Task 4 ############

# model_2
fitStepwise <- step(fitAll, direction = "backward")
summary(fitStepwise)



############ Task 5 ############
#AIC(model1)
#AIC(model2)
#AIC(model3) ??????????????

# hallo von flo


############ Task 6 ############
total_rows <- nrow(data)
eighty_percent <- floor(0.8 * total_rows)
set.seed(123)

for_training <- sample(1:total_rows, eighty_percent, replace = FALSE)

train_data = data[for_training,]
test_data = data[-for_training,]

# model1_oos <- lm(___, data = train_data)
# model3_oos <- lm(___, data = train_data)


# model1_oos_stepwise <- step(model1_oos, direction = "backward")
# model3_oos_stepwise <- step(model3_oos, direction = "backward")


# testing_model1 = predict(model1_oos, newdata = test_data)
# testing_model1_MSE = mean((test_data$realSum - testing_model1)^2)

# testing_model1_stepwise = predict(model1_oos_stepwise, newdata = test_data)
# testing_model1_MSE_stepwise = mean((test_data$realSum - testing_model1_stepwise)^2)


# testing_model3 = predict(model3_oos, newdata = test_data)
# testing_model3_MSE = mean((test_data$realSum - testing_model3)^2)

# testing_model3_stepwise = predict(model3_oos_stepwise, newdata = test_data)
# testing_model3_MSE_stepwise = mean((test_data$realSum - testing_model3_stepwise)^2)

############ Task 6 VERBESSERT ############

total_rows <- nrow(data)
eighty_percent <- floor(0.8 * total_rows)
set.seed(123)

for_training <- sample(1:total_rows, eighty_percent, replace = FALSE)

train_data <- data[for_training,]
test_data <- data[-for_training,]

model_full <- lm(realSum ~ ., data = train_data)
model_step <- step(model_full, direction = "backward")

pred_train_model_full <- predict(model_full, newdata = train_data)
pred_train_model_step <- predict(model_step, newdata = train_data)

pred_test_model_full = predict(model_full, newdata = test_data)
pred_test_model_step = predict(model_step, newdata = test_data)

mse_train_model_full <- mean((train_data$realSum - pred_train_model_full)^2)
mse_train_model_step <- mean((train_data$realSum - pred_train_model_step)^2)

mse_test_model_full <- mean((test_data$realSum - pred_test_model_full)^2)
mse_test_model_step <- mean((test_data$realSum - pred_test_model_step)^2)

########:

predictions_full <- predict(model_full, newdata = test_data) # computing the prediction errors in test_data, so we keep the model but interchange the data source

predictions_step <- predict(model_step, newdata = test_data)

mse_train_full <- mean((train_data$realSum - predict(model_full))^2)
mse_test_full <- mean((test_data$realSum - predictions_full)^2)

mse_train_step <- mean((train_data$realSum - predict(model_step))^2)
mse_test_step <- mean((test_data$realSum - predictions_step)^2)
