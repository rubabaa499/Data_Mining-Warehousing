# Load required libraries
install.packages("rpart")
library(rpart)

# Loading car dataset 
data <- read.csv("C:/car_data.csv")

# Defining the target variable and features
target_var <- "Purchased"
features <- c("Age" , "AnnualSalary")

# Defining k for k-fold cross-validation
k <- 5

# Splitting the dataset into k folds
set.seed(123)  # For reproducibility
fold_indices <- split(1:nrow(data), cut(1:nrow(data), breaks = k, labels = FALSE))

# Initializing variables to store Accuracy and confusion matrix
accuracy_info_gain <- vector("numeric", length = k)
accuracy_gini <- vector("numeric", length = k)
accuracy_gain_ratio <- vector("numeric", length = k)

confusion_matrices_info_gain <- list()
confusion_matrices_gini <- list()
confusion_matrices_gain_ratio <- list()

# Performing k-fold cross-validation for each criterion
for (i in 1:k) {
  # Extract the current fold's indices
  test_indices <- fold_indices[[i]]
  train_indices <- unlist(fold_indices[-i])
  
  # Create training and testing datasets
  train_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  
  # Fit the decision tree model with Information Gain
  decision_tree_info_gain <- rpart(formula(paste(target_var, "~", paste(features, collapse = "+"))),
                                   data = train_data,
                                   method = "class",
                                   parms = list(split = "information"))
  
  # Fit the decision tree model with Gini Index
  decision_tree_gini <- rpart(formula(paste(target_var, "~", paste(features, collapse = "+"))),
                              data = train_data,
                              method = "class",
                              parms = list(split = "gini"))
  
  # Fit the decision tree model with Gain Ratio
  decision_tree_gain_ratio <- rpart(formula(paste(target_var, "~", paste(features, collapse = "+"))),
                                    data = train_data,
                                    method = "class",
                                    parms = list(split = "gainratio"))
  
  # Make predictions on the test data for each criterion
  predictions_info_gain <- predict(decision_tree_info_gain, test_data, type = "class")
  predictions_gini <- predict(decision_tree_gini, test_data, type = "class")
  predictions_gain_ratio <- predict(decision_tree_gain_ratio, test_data, type = "class")
  
  # Calculate accuracy for each criterion
  accuracy_info_gain[i] <- mean(predictions_info_gain == test_data$Purchased)
  accuracy_gini[i] <- mean(predictions_gini == test_data$Purchased)
  accuracy_gain_ratio[i] <- mean(predictions_gain_ratio == test_data$Purchased)
  
  # Create confusion matrix for each criterion
  confusion_matrices_info_gain[[i]] <- table(Actual = test_data$Purchased, Predicted = predictions_info_gain)
  confusion_matrices_gini[[i]] <- table(Actual = test_data$Purchased, Predicted = predictions_gini)
  confusion_matrices_gain_ratio[[i]] <- table(Actual = test_data$Purchased, Predicted = predictions_gain_ratio)
}

# Calculating the average accuracy for each criterion
average_accuracy_info_gain <- mean(accuracy_info_gain)
average_accuracy_gini <- mean(accuracy_gini)
average_accuracy_gain_ratio <- mean(accuracy_gain_ratio)

# Printing the average accuracy for each criterion
cat("Average Accuracy with Information Gain:", average_accuracy_info_gain, "\n")
cat("Average Accuracy with Gini Index:", average_accuracy_gini, "\n")
cat("Average Accuracy with Gain Ratio:", average_accuracy_gain_ratio, "\n")

# Printing confusion matrices for each criterion
for (i in 1:k) {
  cat("Confusion Matrix for Information Gain (Fold", i, "):\n")
  print(confusion_matrices_info_gain[[i]])
  
  cat("Confusion Matrix for Gini Index (Fold", i, "):\n")
  print(confusion_matrices_gini[[i]])
  
  cat("Confusion Matrix for Gain Ratio (Fold", i, "):\n")
  print(confusion_matrices_gain_ratio[[i]])
}




# decision tree using the "rpart.plot" package
library(rpart.plot)


# Resize the plot
options(repr.plot.width = 10000, repr.plot.height = 5000) 
print("Decision Tree with Information Gain")
rpart.plot(decision_tree_info_gain)


options(repr.plot.width = 10000, repr.plot.height = 5000)  
print("Decision Tree with Gini Index:")
rpart.plot(decision_tree_gini)


options(repr.plot.width = 10000, repr.plot.height = 5000)  
print("Decision Tree with Gain Ratio:")
rpart.plot(decision_tree_gain_ratio)





