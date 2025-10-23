# Load data
uscrime <- read.table("uscrime.txt", header = TRUE)

# Check structure
str(uscrime)

# Load necessary library
library(MASS)

# Fit full model
full_model <- lm(Crime ~ ., data = uscrime)

# Stepwise selection using AIC
stepwise_model <- stepAIC(full_model, direction = "both")

# View summary
summary(stepwise_model)

# Load glmnet
# install.packages("glmnet")
library(glmnet)

# Prepare data
x <- as.matrix(uscrime[, -which(names(uscrime) == "Crime")])  # predictors
y <- uscrime$Crime                                            # response

# Standardize predictors
x_scaled <- scale(x)

# Fit Lasso model
lasso_model <- glmnet(x_scaled, y, alpha = 1)

# Plot coefficient paths
plot(lasso_model, xvar = "lambda", label = TRUE)

# Cross-validation to choose lambda
cv_lasso <- cv.glmnet(x_scaled, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min

# Coefficients at best lambda
coef(cv_lasso, s = "lambda.min")

# Try alpha = 0.5 for a balance between Lasso and Ridge
elastic_net_model <- glmnet(x_scaled, y, alpha = 0.5)

# Cross-validation
cv_enet <- cv.glmnet(x_scaled, y, alpha = 0.5)
best_lambda_enet <- cv_enet$lambda.min

# Coefficients at best lambda
coef(cv_enet, s = "lambda.min")