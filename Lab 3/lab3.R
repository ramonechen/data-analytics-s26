library(class)
library(cluster)

setwd("C:/Users/raymo/Documents/GitHub/data-analytics-s26/Lab 3")

## Exercise 0: Prepare the abalone data by modifying columns and values

# Read abalone data
abalone_df <- read.csv("./abalone/abalone.data", header = FALSE)

# Rename columns to be readable
colnames(abalone_df) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings") 

# Derive age group based in number of rings
abalone_df$age.group <- cut(abalone_df$rings, br=c(0,8,11,35), labels = c("young", "adult", "old"))

# Take copy removing sex and rings
abalone_df <- abalone_df[,c(2:8,10)]

# Convert class labels to strings
abalone_df$age.group <- as.character(abalone_df$age.group)

# Convert back to factor
abalone_df$age.group <- as.factor(abalone_df$age.group)

abalone_df <- na.omit(abalone_df)

View(abalone_df)

## Exercise 1: kNN Models

## Split data between train/test
train_indexes <- sample(4177,0.7*4177)

train_data <- abalone_df[train_indexes,]
test_data <- abalone_df[-train_indexes,]

train_labels <- train_data$age.group
test_labels <- test_data$age.group

train_features_1 <- train_data[, c("length", "diameter", "height")]
test_features_1 <- test_data[, c("length", "diameter", "height")]

train_features_2 <- train_data[, c("whole_weight", "shucked_weight", "viscera_weight", "shell_weight")]
test_features_2 <- test_data[, c("whole_weight", "shucked_weight", "viscera_weight", "shell_weight")]

# Choose an arbitrary initial value for k
k_initial <- 5
model_1_preds <- knn(train = train_features_1, test = test_features_1, cl = train_labels, k = k_initial)
model_2_preds <- knn(train = train_features_2, test = test_features_2, cl = train_labels, k = k_initial)

cat("--- Contingency Table: Model 1 (Dimensions) ---\n")
table_1 <- table(Predicted = model_1_preds, Actual = test_labels)
print(table_1)

cat("\n--- Contingency Table: Model 2 (Weights) ---\n")
table_2 <- table(Predicted = model_2_preds, Actual = test_labels)
print(table_2)

# Calculate accuracy: sum of diagonal (correct predictions) / total observations
accuracy_1 <- sum(diag(table_1)) / sum(table_1)
accuracy_2 <- sum(diag(table_2)) / sum(table_2)

cat("\nAccuracy of Model 1:", round(accuracy_1, 4), "\n")
cat("Accuracy of Model 2:", round(accuracy_2, 4), "\n")

# Select better feature set based on initial accuracy
if(accuracy_1 > accuracy_2) {
  best_train_features <- train_features_1
  best_test_features <- test_features_1
  cat("\nModel 1 (Dimensions) performed better. Tuning k for this subset...\n")
} else {
  best_train_features <- train_features_2
  best_test_features <- test_features_2
  cat("\nModel 2 (Weights) performed better. Tuning k for this subset...\n")
}

# Test a range of k values to find the optimal value
k_values <- 1:30

# Create empty vector to store results
accuracies <- numeric(length(k_values))

# Loop through each k value, train, and calculate accuracy
for(i in seq_along(k_values)) {
  temp_preds <- knn(train = best_train_features, test = best_test_features, cl = train_labels, k = k_values[i])
  temp_table <- table(Predicted = temp_preds, Actual = test_labels)
  
  # Calculate and store accuracy of model
  accuracies[i] <- sum(diag(temp_table)) / sum(temp_table)
}

# Get highest recorded accuracy and corresponding k value
optimal_k <- k_values[which.max(accuracies)]
highest_accuracy <- max(accuracies)

cat("\nThe optimal value for k is:", optimal_k, "\n")
cat("Highest achieved accuracy:", round(highest_accuracy, 4), "\n")

## Exercise 2: k-means and PAM to discover clusters in abalone data

# Extract weight features from exercise 1
weight_features <- abalone_df[, c("whole_weight", "shucked_weight", "viscera_weight", "shell_weight")]

# Test a range of k values to find the optimal value
k_values <- 2:10

# Create empty vectors to store silhouette widths
avg_sil_kmeans <- numeric(length(k_values))
avg_sil_pam <- numeric(length(k_values))

# Calculate average silhouette width for each k
for(i in seq_along(k_values)) {
  k <- k_values[i]
  
  # K-means
  km_res <- kmeans(weight_features, centers = k, nstart = 25)
  sil_km <- silhouette(km_res$cluster, dist(weight_features))
  avg_sil_kmeans[i] <- mean(sil_km[, 3])
  
  # PAM
  pam_res <- pam(weight_features, k = k)
  avg_sil_pam[i] <- pam_res$silinfo$avg.width
}

# Identify the optimal k (the one that maximizes the average silhouette width)
optimal_k_kmeans <- k_values[which.max(avg_sil_kmeans)]
optimal_k_pam <- k_values[which.max(avg_sil_pam)]

cat("Optimal k for K-means:", optimal_k_kmeans, "\n")
cat("Optimal k for PAM:", optimal_k_pam, "\n")

# Train final K-means model
final_kmeans <- kmeans(weight_scaled, centers = optimal_k_kmeans, nstart = 25)

# Train final PAM model
final_pam <- pam(weight_scaled, k = optimal_k_pam)

# Set up the plotting area to show 2 plots side-by-side
par(mfrow = c(1, 2))

# Plot 1: K-means Silhouette
sil_kmeans_final <- silhouette(final_kmeans$cluster, dist(weight_scaled))
plot(sil_kmeans_final, 
     main = paste("K-means Silhouette Plot (k =", optimal_k_kmeans, ")"), 
     col = 1:optimal_k_kmeans, 
     border = NA)

# Plot 2: PAM Silhouette
plot(silhouette(final_pam), 
     main = paste("PAM Silhouette Plot (k =", optimal_k_pam, ")"), 
     col = 1:optimal_k_pam, 
     border = NA)

# Reset plotting area back to normal
par(mfrow = c(1, 1))

