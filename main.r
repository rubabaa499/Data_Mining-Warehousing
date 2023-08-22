library(ggplot2)
library(dplyr)

diabetics_df <- read.csv("C:/diabetes.csv")

head(diabetics_df, n=5)



# Calculate the correlation matrix
cor_col = c('age', 'hypertension', 'heart_disease', 'bmi', 'HbA1c_level' , 'blood_glucose_level' , 'diabetes')
cor_matrix <- cor(diabetics_df[cor_col])

# Convert the correlation matrix to a data frame
cor_df <- as.data.frame(as.table(cor_matrix))

# Create a heatmap using ggplot2 with wider cells and labels
ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(width = 0.8, height = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Freq)), size = 3, color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Split the data into training and testing sets
set.seed(123)

# Remove blood pressure and skin thickness insulin columns

colums = c('age', 'hypertension', 'heart_disease', 'bmi', 'HbA1c_level' , 'blood_glucose_level' , 'diabetes')

diabetics_df <- diabetics_df[colums]


train_X <- diabetics_df[1:70, 1:7]
train_Y <- diabetics_df[1:70, 7]

test_X <- diabetics_df[71:100, 1:7]
test_Y <- diabetics_df[71:100, 7]

# dimensions of the training and testing sets
dim(train_X)
head(train_Y)
dim(test_X)
head(test_Y)



# Euclidean distance
euclidean_distance <- function(x, y) {
  sqrt(sum((x - y)^2))
}

# Manhattan distance
manhattan_distance <- function(x, y) {
  sum(abs(x - y))
}

# Chebyshev distance
chebyshev_distance <- function(x, y) {
  max(abs(x - y))
}


knn <- function(train_X, train_Y, test_X, test_Y, k, distance_func) {
  nearest_neighbors <- data.frame()
  
  for (i in 1:nrow(test_X)) {
    distances <- data.frame()
    for (j in 1:nrow(train_X)) {
      distance <- distance_func(test_X[i, ], train_X[j, ])
      distances <- rbind(distances, data.frame(distance))
    }
    distances <- cbind(distances, train_Y)
    colnames(distances) <- c("distance", "class")
    distances <- distances[order(distances$distance), ]
    nearest_neighbors <- rbind(nearest_neighbors, distances[1:k, ])
  }
  
  nearest_neighbors <- cbind(nearest_neighbors, test_Y)
  
  colnames(nearest_neighbors) <- c("distance", "class", "actual_class")
  
  return(nearest_neighbors)
}


accuracy <- function(nearest_neighbors) {
  correct <- 0
  for (i in 1:nrow(nearest_neighbors)) {
    if (nearest_neighbors[i, 2] == nearest_neighbors[i, 3]) {
      correct <- correct + 1
    }
  }
  return(correct/nrow(nearest_neighbors))
}


confusion_matrix <- function(nearest_neighbors) {
  true_positives <- nearest_neighbors[nearest_neighbors$class == 1 & nearest_neighbors$actual_class == 1, ]
  false_positives <- nearest_neighbors[nearest_neighbors$class == 1 & nearest_neighbors$actual_class == 0, ]
  true_negatives <- nearest_neighbors[nearest_neighbors$class == 0 & nearest_neighbors$actual_class == 0, ]
  false_negatives <- nearest_neighbors[nearest_neighbors$class == 0 & nearest_neighbors$actual_class == 1, ]
  
  return(list(true_positives = nrow(true_positives),
              false_positives = nrow(false_positives),
              true_negatives = nrow(true_negatives),
              false_negatives = nrow(false_negatives)))
  
}


k_range <- 1:3

accuracy_df <- data.frame()
confusion_matrix_df <- data.frame()

for (k in k_range) {
  # Euclidean distance
  nearest_neighbors_ec <- knn(train_X, train_Y, test_X, test_Y, k, euclidean_distance)
  euclidean_accuracy <- accuracy(nearest_neighbors_ec)
  euclidean_matrix <- confusion_matrix(nearest_neighbors_ec)
  
  # # Manhattan distance
  nearest_neighbors_mh <- knn(train_X, train_Y, test_X, test_Y, k, manhattan_distance)
  manhattan_accuracy <- accuracy(nearest_neighbors_mh)
  manhattan_matrix <- confusion_matrix(nearest_neighbors_mh)
  
  # Chebsynev distance
  nearest_neighbors_ch <- knn(train_X, train_Y, test_X, test_Y, k, chebyshev_distance)
  Chebsynev_accuracy <- accuracy(nearest_neighbors_ch)
  Chebsynev_matrix <- confusion_matrix(nearest_neighbors_ch)
  
  
  confusion_matrix_df <- rbind(confusion_matrix_df, data.frame(k, euclidean_matrix, manhattan_matrix, Chebsynev_matrix))
  accuracy_df <- rbind(accuracy_df, data.frame(k, euclidean_accuracy, manhattan_accuracy, Chebsynev_accuracy))
}

accuracy_df


# plot 3 lines on the same plot and width full width

ggplot(accuracy_df, aes(x = k)) +
  geom_line(aes(y = euclidean_accuracy, color = "euclidean")) +
  geom_line(aes(y = manhattan_accuracy, color = "manhattan")) +
  geom_line(aes(y = Chebsynev_accuracy, color = "Chebsynev")) +
  labs(title = "Accuracy vs K", x = "K", y = "Accuracy") +
  theme_bw(
    base_size = 14,
    base_family = "sans"
  ) +
  theme(
    legend.position = "bottom"
  )


# best k value for every distance function

accuracy_df[which.max(accuracy_df$euclidean_accuracy), ]
accuracy_df[which.max(accuracy_df$manhattan_accuracy), ]
accuracy_df[which.max(accuracy_df$Chebsynev_accuracy), ]


# plot confusion matrix for each k and each distance metric
confusion_matrix_df
