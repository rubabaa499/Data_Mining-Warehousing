#Importing Dataset
mydata<-read.csv("C:/diabetes.csv")
mydata

#importing necessary liabrary
library(class)
library(ggplot2)

#Data Summarise
str(mydata)
summary(mydata)
attributes<- names(mydata)
attributes
dataType <- c(typeof(mydata$gender), typeof(mydata$age), typeof(mydata$hypertension),typeof(mydata$heart_disease), 
              typeof(mydata$bmi),  typeof(mydata$HbA1c_level), typeof(mydata$blood_glucose_level),
              typeof(mydata$diabetes))
dataType

head(mydata) 

colSums(is.na(mydata)) #how many instances are missing


#Data Normalization
data_norm <- setdiff(names(mydata), c("gender", "smoking_history"))
mydata[data_norm] <- scale(mydata[data_norm])

head(mydata)
head(data_norm)
data_norm
mydata[data_norm]
colSums(is.na(mydata[data_norm]))



# Setting predictor variables and the target variable
predictor_cols <- names(mydata[data_norm])[-ncol(mydata[data_norm])]
target_col <- names(mydata[data_norm])[ncol(mydata[data_norm])]

set.seed(123)
train_indices <- sample(1:nrow(mydata[data_norm]), round(0.7 * nrow(mydata[data_norm])))
train_data <- mydata[data_norm][train_indices, predictor_cols]
train_labels <- mydata[data_norm][train_indices, target_col]
test_data <- mydata[data_norm][-train_indices, predictor_cols]
test_labels <- mydata[data_norm][-train_indices, target_col]





# Set the value of k (number of neighbors) and distance measures

knn_with_distance_measure <- function(train_data, test_data, train_labels, k,
                                      distance_measure) {
  predicted_labels <- knn(train = train_data, test = test_data, cl = train_labels, k = k, prob =
                            TRUE, use.all = TRUE)
  return(predicted_labels)
}



# Set the values of k
k_values <- c(3, 5, 7)
# Initialize vectors to store accuracies
accuracies <- vector()
# Apply k-NN for each k value and distance measure
for (k in k_values) {
  # Apply k-NN with Euclidean distance
  euclidean_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k,
                                                     "euclidean")
  # Apply k-NN with Manhattan distance
  manhattan_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k,
                                                     "manhattan")
  # Apply k-NN with Maximum distance
  maximum_predictions <- knn_with_distance_measure(train_data, test_data, train_labels, k,
                                                   "maximum")
  # Evaluate the accuracy of the predictions
  accuracy_euclidean <- sum(euclidean_predictions == test_labels) / length(test_labels)
  accuracy_manhattan <- sum(manhattan_predictions == test_labels) / length(test_labels)
  accuracy_maximum <- sum(maximum_predictions == test_labels) / length(test_labels)
  # Store the accuracy
  accuracies <- c(accuracies, accuracy_euclidean, accuracy_manhattan, accuracy_maximum)
  # Print the accuracy for the current k value
  cat("Accuracy for k =", k, "\n")
  cat("Euclidean Distance:", accuracy_euclidean, "\n")
  cat("Manhattan Distance:", accuracy_manhattan, "\n")
  cat("Maximum Distance:", accuracy_maximum, "\n")
  cat("\n")
}


# Create a data frame for accuracies
accuracy_df <- data.frame(Distance = rep(c("Euclidean", "Manhattan", "Maximum"),
                                         length(k_values)),
                          K = rep(k_values, each = 3),
                          Accuracy = accuracies)
accuracy_df

# Plotting the accuracies
ggplot(accuracy_df, aes(x = K, y = Accuracy, color = Distance, group = Distance)) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy of k-NN with Different Distance Measures",
       x = "k",
       y = "Accuracy",
       color = "Distance Measure") +
  theme_minimal()


