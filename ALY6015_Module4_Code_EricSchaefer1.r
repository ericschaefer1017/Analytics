
# Install required packages if not already installed to ensure the code runs on any system
if (!require(glmnet)) install.packages('glmnet', dependencies=TRUE)
if (!require(ISLR)) install.packages('ISLR', dependencies=TRUE)
if (!require(MASS)) install.packages('MASS', dependencies=TRUE)

# Load required libraries after confirming installation
library(glmnet)
library(ISLR)
library(MASS)

# Set seed to ensure reproducibility of train/test split for consistent results
set.seed(123)

# Load College dataset because it contains institutional data we need to model graduation rate
data(College)

# Prepare features and response: exclude 'Private' because we're predicting Grad.Rate, convert data frame to matrix for glmnet
x <- model.matrix(Grad.Rate ~ ., College)[, -1]  # Remove intercept column to match glmnet input requirements
y <- College$Grad.Rate

# Split data into training and test sets to enable model evaluation on unseen data
train_index <- sample(1:nrow(x), nrow(x) * 0.7)  # Randomly select 70% of rows for training
x_train <- x[train_index, ]
y_train <- y[train_index]
x_test <- x[-train_index, ]
y_test <- y[-train_index]

# -----------------------------
# Ridge Regression
# -----------------------------

# Perform cross-validation for ridge to estimate optimal lambda values because we need regularization tuning
cv_ridge <- cv.glmnet(x_train, y_train, alpha=0)  # alpha=0 for ridge

# Display lambda.min and lambda.1se because they indicate the optimal and more regularized lambdas
print(paste('Ridge lambda.min:', cv_ridge$lambda.min))
print(paste('Ridge lambda.1se:', cv_ridge$lambda.1se))

# Plot the cv.glmnet object to visualize how error changes across lambda values for interpretation
plot(cv_ridge)

# Fit ridge regression using lambda.min to get coefficient estimates on the training data
ridge_model <- glmnet(x_train, y_train, alpha=0, lambda=cv_ridge$lambda.min)

# Print ridge coefficients to examine which variables have higher or lower influence
print(coef(ridge_model))

# Predict on training set using ridge model to calculate RMSE for training performance
ridge_pred_train <- predict(ridge_model, s=cv_ridge$lambda.min, newx=x_train)
ridge_rmse_train <- sqrt(mean((y_train - ridge_pred_train)^2))
print(paste('Ridge Train RMSE:', ridge_rmse_train))

# Predict on test set to assess generalization and calculate RMSE for test performance
ridge_pred_test <- predict(ridge_model, s=cv_ridge$lambda.min, newx=x_test)
ridge_rmse_test <- sqrt(mean((y_test - ridge_pred_test)^2))
print(paste('Ridge Test RMSE:', ridge_rmse_test))

# -----------------------------
# LASSO Regression
# -----------------------------

# Perform cross-validation for LASSO to estimate optimal lambda values because we want variable selection and shrinkage
cv_lasso <- cv.glmnet(x_train, y_train, alpha=1)  # alpha=1 for LASSO

# Display lambda.min and lambda.1se for LASSO to compare regularization results
print(paste('LASSO lambda.min:', cv_lasso$lambda.min))
print(paste('LASSO lambda.1se:', cv_lasso$lambda.1se))

# Plot the cv.glmnet object for LASSO to visualize how cross-validated error changes with lambda
plot(cv_lasso)

# Fit LASSO regression using lambda.min to obtain coefficients with potential zero reductions
lasso_model <- glmnet(x_train, y_train, alpha=1, lambda=cv_lasso$lambda.min)

# Print LASSO coefficients to see if any predictors are reduced to zero
print(coef(lasso_model))

# Predict on training set using LASSO to calculate training RMSE
lasso_pred_train <- predict(lasso_model, s=cv_lasso$lambda.min, newx=x_train)
lasso_rmse_train <- sqrt(mean((y_train - lasso_pred_train)^2))
print(paste('LASSO Train RMSE:', lasso_rmse_train))

# Predict on test set using LASSO to calculate test RMSE and check for overfitting
lasso_pred_test <- predict(lasso_model, s=cv_lasso$lambda.min, newx=x_test)
lasso_rmse_test <- sqrt(mean((y_test - lasso_pred_test)^2))
print(paste('LASSO Test RMSE:', lasso_rmse_test))

# -----------------------------
# Stepwise Selection Comparison
# -----------------------------

# Fit full linear model to prepare for stepwise regression
lm_full <- lm(Grad.Rate ~ ., data=College)

# Perform stepwise selection using AIC to identify a parsimonious model
step_model <- stepAIC(lm_full, direction='both')

# Print summary of stepwise model to see selected predictors and coefficients
summary(step_model)

# Predict on training set using stepwise model to calculate training RMSE
step_pred_train <- predict(step_model, newdata=College[train_index, ])
step_rmse_train <- sqrt(mean((y_train - step_pred_train)^2))
print(paste('Stepwise Train RMSE:', step_rmse_train))

# Predict on test set using stepwise model to calculate test RMSE
step_pred_test <- predict(step_model, newdata=College[-train_index, ])
step_rmse_test <- sqrt(mean((y_test - step_pred_test)^2))
print(paste('Stepwise Test RMSE:', step_rmse_test))

