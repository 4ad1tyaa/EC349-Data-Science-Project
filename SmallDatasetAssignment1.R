# ASSIGNMENT 1 - EC349

# Part 1: "You must split the User Reviews data into a training and a test dataset. The test dataset must contain 10,000 randomly drawn observations using the "caret" package in R.

# Loading Small User Reviews Dataset File
load("/Users/god/Desktop/God/3rdYearUni/DataScience/RStuff/Term1Assignment/ProvidedMaterial/yelp_review_small.Rda")

# Installing Relevant Packages and Loading (if needed)
options(repos = c(CRAN = "https://cloud.r-project.org/"))

if (!require("caret", character.only = TRUE)) {
  install.packages("caret")
}
library(caret)

if (!require("randomForest", character.only = TRUE)) {
  install.packages("randomForest")
}
library(randomForest)

# Creating Training and Test Datasets
total_observations <- nrow(review_data_small)
test_size <- 10000
test_fraction <- test_size / total_observations

split_sets <- createDataPartition(review_data_small$stars, p = test_fraction, list = FALSE)

training_dataset <- review_data_small[-split_sets, ]
test_dataset <- review_data_small[split_sets, ]

# Seeing 10,002 observations in the test dataset, as such will be randomly selecting 10,000 observations from the test dataset
sampling_test_dataset <- sample(nrow(test_dataset), 10000)
new_test_dataset <- test_dataset[sampling_test_dataset, ]

# Performing check to see if there are the correct number of observations in both datasets
cat("Training Dataset Observations: ", nrow(training_dataset), "\n")
cat("Test Dataset Observations: ", nrow(new_test_dataset), "\n")

# ---------------------------------------------------------------

# Part 2: Predicting User Reviews for those 10,000 observations - opting for RandomForest

# Loading Small User Data Dataset File
load("/Users/god/Desktop/God/3rdYearUni/DataScience/RStuff/Term1Assignment/ProvidedMaterial/yelp_user_small.Rda")

# Merging User Data Dataset with User Reviews Dataset
combined_data <- merge(training_dataset, user_data_small, by = "user_id")

# Cleaning data to make it easier to manipulate in future (handling missing values, categorical variables etc.)
combined_data[is.na(combined_data)] <- apply(combined_data, 2, function(x) ifelse(is.numeric(x), median(x, na.rm = TRUE), x))
combined_data$yelping_since <- as.Date(combined_data$yelping_since)
combined_data$years_yelping <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(combined_data$yelping_since, "%Y"))
combined_data <- combined_data[, !(names(combined_data) %in% c("name", "yelping_since", "elite", "friends", "text", "date"))]


# Taking a subset for faster processing 
set.seed(123)
sample_index <- sample(1:nrow(combined_data), size = 0.5 * nrow(combined_data))
subset_data <- combined_data[sample_index, ]

# Building a Random Forest model with adjusted parameters to train
my_model <- randomForest(stars ~ ., data = subset_data, ntree = 200, mtry = round(sqrt(ncol(subset_data))), do.trace = 10)

# Evaluation on training dataset
print(my_model)

# Predictions + MSE on training dataset
predictions <- predict(my_model, subset_data)
mse <- mean((subset_data$stars - predictions)^2)
cat("The Mean Squared Error is: ", mse, "\n")

# Preparing test dataset
test_combined_data <- merge(new_test_dataset, user_data_small, by = "user_id")
test_combined_data$yelping_since <- as.Date(test_combined_data$yelping_since)
test_combined_data$years_yelping <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(test_combined_data$yelping_since, "%Y"))
test_combined_data <- test_combined_data[, !(names(test_combined_data) %in% c("name", "yelping_since", "elite", "friends", "text", "date"))]

# Making Predictions
test_dataset_predictions <- predict(my_model, test_combined_data)

# Examining Predictions + Calculating MSE on test dataset
test_mse <- mean((test_combined_data$stars - test_dataset_predictions)^2)
cat("Test Mean Squared Error is: ", test_mse, "\n")

head(test_dataset_predictions)
comparison_df <- data.frame(Actual = test_combined_data$stars, Predicted = test_dataset_predictions)
head(comparison_df)
summary(test_dataset_predictions)
plot(comparison_df$Actual, comparison_df$Predicted, main = "Predicted vs Actual Stars", xlab = "Actual Stars", ylab = "Predicted Stars", pch = 19)
abline(0, 1, col = "red")










