# ASSIGNMENT 1 - EC349

# Part 1: "You must split the User Reviews data into a training and a test dataset. The test dataset must contain 10,000 randomly drawn observations using the "caret" package in R.

# Installing Packages + Storing Packages in Libraries
install.packages("caret")
install.packages("jsonlite")
library(caret)
library(jsonlite)

# Loading User Reviews dataset, making it easier to call in future lines, and checking to see if any data has been incorrectly formatted
filepath <- "/Users/god/Desktop/God/3rdYearUni/DataScience/RStuff/Term1Assignment/ProvidedMaterial/yelp_academic_dataset_review.json"
review_data <- jsonlite:: stream_in(file(filepath))

str(review_data)
summary(review_data)

# Splitting data as per instruction
set.seed(1)

size_test_dataset <- 10000
size_user_reviews_dataset <- nrow(review_data)
proportion_of_test <- size_test_dataset / size_user_reviews_dataset

split_data <- createDataPartition(review_data$stars, p = proportion_of_test, list = FALSE, times = 1)

training_dataset <- review_data[-split_data,]
test_dataset <- review_data[split_data,]

# Was getting 10,002 observations instead, so am randomly removing 2 rows from the test dataset, and ensuring that this is reproducible
set.seed(1)
actual_test_dataset <- test_dataset[-sample(nrow(test_dataset),2),]

# Performing check to see if there are the correct number of observations in both datasets
cat("Training Dataset Observations:", nrow(training_dataset), "\n")
cat("Test Dataset Observations:", nrow(actual_test_dataset), "\n")


