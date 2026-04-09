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

# 1. Compute the PCs and plot the dataset using the 1st and 2nd PCs
wine_features <- wine_data[, -1]
pca_result <- princomp(wine_features, cor = TRUE)

# Plotting PC1 vs PC2
plot(pca_result$scores[, 1], pca_result$scores[, 2],
  col = wine_data$Type, pch = 19,
  xlab = "Principal Component 1", ylab = "Principal Component 2",
  main = "Wine Dataset: PC1 vs PC2"
)

# 2. Identify the variables that contribute the most to the 1st PC
loadings_pc1 <- pca_result$loadings[, 1]
sorted_loadings_pc1 <- sort(abs(loadings_pc1), decreasing = TRUE)
cat("\n--- Variables Contribution to PC1 ---\n")
print(sorted_loadings_pc1)

# 3. Train a classifier model (kNN) using a subset of variables
set.seed(42)
sample_index <- sample(seq_len(nrow(wine_data)), 0.7 * nrow(wine_data))
train_data <- wine_data[sample_index, ]
test_data <- wine_data[-sample_index, ]

# Create model train/test split
train_labels <- train_data$Type
test_labels <- test_data$Type

# Using Flavanoids, Color_intensity, Proline
selected_features <- c("Flavanoids", "Color Intensity", "Proline")
train_subset <- train_data[, selected_features]
test_subset <- test_data[, selected_features]

# Scale features
train_subset_scaled <- scale(train_subset)
test_subset_scaled <- scale(test_subset,
  center = attr(train_subset_scaled, "scaled:center"),
  scale = attr(train_subset_scaled, "scaled:scale")
)

# Create kNN model with k = 5
k_val <- 5
knn_pred_subset <- knn(
  train = train_subset_scaled, test = test_subset_scaled,
  cl = train_labels, k = k_val
)

# 4. Train a classifier model using data projected onto first 2 PCs
train_pca <- pca_result$scores[sample_index, 1:2]
test_pca <- pca_result$scores[-sample_index, 1:2]

knn_pred_pca <- knn(
  train = train_pca, test = test_pca,
  cl = train_labels, k = k_val
)

# 5. Compare the 2 classification models
# Helper function to compute Precision, Recall, and F1 per class
compute_metrics <- function(conf_matrix) {
  precision <- diag(conf_matrix) / colSums(conf_matrix)
  recall <- diag(conf_matrix) / rowSums(conf_matrix)
  f1 <- 2 * precision * recall / (precision + recall)

  # Replace NA values
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  f1[is.na(f1)] <- 0

  data.frame(Precision = precision, Recall = recall, F1_Score = f1)
}

cat("\n============================================\n")
cat("Model 1: kNN with subset of features\n")
conf_matrix_subset <- table(Actual = test_labels, Predicted = knn_pred_subset)
print("Contingency Table:")
print(conf_matrix_subset)
print("Metrics:")
print(compute_metrics(conf_matrix_subset))

cat("\n============================================\n")
cat("Model 2: kNN with first 2 Principal Components\n")
conf_matrix_pca <- table(Actual = test_labels, Predicted = knn_pred_pca)
print("Contingency Table:")
print(conf_matrix_pca)
print("Metrics:")
print(compute_metrics(conf_matrix_pca))
