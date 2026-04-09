library(e1071)
library(class)

setwd("/Users/raymond/Documents/GitHub/data-analytics-s26/Lab 4")

# Read the dataset
wine_data <- read.csv("wine.data", header = FALSE)

# Set column names based on wine.names
names(wine_data) <- c(
  "Type",
  "Alcohol",
  "Malic acid",
  "Ash",
  "Alcalinity of ash",
  "Magnesium",
  "Total phenols",
  "Flavanoids",
  "Nonflavanoid Phenols",
  "Proanthocyanins",
  "Color Intensity",
  "Hue",
  "Od280/od315 of diluted wines",
  "Proline"
)

# Change the data type of the "Type" column from character to factor
wine_data$Type <- as.factor(wine_data$Type)

# Create train/test split
set.seed(42)
sample_index <- sample(seq_len(nrow(wine_data)), 0.7 * nrow(wine_data))
train_data <- wine_data[sample_index, ]
test_data <- wine_data[-sample_index, ]

# Subset of features: Flavanoids, Color Intensity, Proline
selected_features <- c("Flavanoids", "Color Intensity", "Proline")

train_subset <- train_data[, c("Type", selected_features)]
test_subset <- test_data[, c("Type", selected_features)]

# 1. SVM with Linear Kernel
tune_linear <- tune.svm(Type ~ .,
  data = train_subset, kernel = "linear",
  cost = c(0.1, 1, 10, 100)
)
best_linear_model <- tune_linear$best.model

svm_linear_pred <- predict(best_linear_model, test_subset)

# 2. SVM with Radial Kernel
tune_radial <- tune.svm(Type ~ .,
  data = train_subset, kernel = "radial",
  cost = c(0.1, 1, 10, 100),
  gamma = c(0.01, 0.1, 1, 10)
)
best_radial_model <- tune_radial$best.model

svm_radial_pred <- predict(best_radial_model, test_subset)

# 3. kNN model with k = 5
train_features <- train_subset[, selected_features]
test_features <- test_subset[, selected_features]

train_features_scaled <- scale(train_features)
test_features_scaled <- scale(test_features,
  center = attr(train_features_scaled, "scaled:center"),
  scale = attr(train_features_scaled, "scaled:scale")
)

k_val <- 5
knn_pred <- knn(
  train = train_features_scaled, test = test_features_scaled,
  cl = train_subset$Type, k = k_val
)

# 4. Compare the performance
compute_metrics <- function(conf_matrix) {
  precision <- diag(conf_matrix) / colSums(conf_matrix)
  recall <- diag(conf_matrix) / rowSums(conf_matrix)
  f1 <- 2 * precision * recall / (precision + recall)

  # Handle missing values
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  f1[is.na(f1)] <- 0

  data.frame(Precision = precision, Recall = recall, F1_Score = f1)
}

cat("\n============================================\n")
cat("SVM Linear Kernel Performance\n")
conf_matrix_linear <- table(
  Actual = test_subset$Type,
  Predicted = svm_linear_pred
)
print("Contingency Table:")
print(conf_matrix_linear)
print("Metrics:")
print(compute_metrics(conf_matrix_linear))

cat("\n============================================\n")
cat("SVM Radial Kernel Performance\n")
conf_matrix_radial <- table(
  Actual = test_subset$Type,
  Predicted = svm_radial_pred
)
print("Contingency Table:")
print(conf_matrix_radial)
print("Metrics:")
print(compute_metrics(conf_matrix_radial))

cat("\n============================================\n")
cat("kNN Performance\n")
conf_matrix_knn <- table(
  Actual = test_subset$Type,
  Predicted = knn_pred
)
print("Contingency Table:")
print(conf_matrix_knn)
print("Metrics:")
print(compute_metrics(conf_matrix_knn))
