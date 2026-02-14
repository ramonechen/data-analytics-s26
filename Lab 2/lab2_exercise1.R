library("ggplot2")
library("readr")

# Read dataset from CSV
dataset <- read_csv("~/GitHub/data-analytics-s26/Lab 2/NY-House-Dataset.csv")

# Scatter plot of PROPERTYSQFT and PRICE
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

# Scatter plot of BEDS and BATH
ggplot(dataset, aes(x = BEDS, y = BATH)) +
  geom_point()

# Scatter plot of log-transformed PROPERTYSQFT and PRICE
# These two variables are quite right-skewed
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

# Filter PRICE outlier
dataset <- dataset[dataset$PRICE<195000000,]

# Log transform PRICE and PROPERTYSQFT
dataset$PRICE <- log10(dataset$PRICE)
dataset$PROPERTYSQFT <- log10(dataset$PROPERTYSQFT)

# Create linear model with 1 predictor (PROPERTYSQFT)
model1 <- lm(PRICE ~ PROPERTYSQFT, data = dataset)
summary(model1)

# Plot linear model 1
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Price vs PropertySqFt with Best Fit Line")

model1_residual_df <- data.frame(Fitted = predict(model1), Residuals = residuals(model1))

ggplot(model1_residual_df, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Model 1 Residual Plot", x = "Fitted Values", y = "Residuals")

# Create lienar model with 2 predictors (BEDS, BATH)
model2 <- lm(PRICE ~ BEDS + BATH, data = dataset)
summary(model2)

# Plot linear model 2
ggplot(dataset, aes(x = BATH, y = PRICE)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Price vs Bath with Best Fit Line")

model2_residual_df <- data.frame(Fitted = predict(model2), Residuals = residuals(model2))

ggplot(model2_residual_df, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Model 2 Residual Plot", x = "Fitted Values", y = "Residuals")

# Create linear model with 3 predictors (PROPERTYSQFT, BEDS, BATH)
model3 <- lm(PRICE ~ PROPERTYSQFT + BEDS + BATH, data = dataset)
summary(model3)

# Plot linear model 3
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Price vs PropertySqFt with Best Fit Line")

model3_residual_df <- data.frame(Fitted = predict(model3), Residuals = residuals(model3))

ggplot(model2_residual_df, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Model 2 Residual Plot", x = "Fitted Values", y = "Residuals")

