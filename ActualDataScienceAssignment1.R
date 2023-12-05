# ASSIGNMENT 1 - EC349

# Part 1: "You must split the User Reviews data into a training and a test dataset. The test dataset must contain 10,000 randomly drawn observations using the "caret" package in R.

# Installing Packages + Storing Packages in Libraries
install.packages("caret")
install.packages("jsonlite")
library(caret)
library(jsonlite)

# Loading User Reviews dataset, making it easier to call in future lines, and checking to see if any data has been incorrectly formatted
filepath_review_data <- "/Users/god/Desktop/God/3rdYearUni/DataScience/RStuff/Term1Assignment/ProvidedMaterial/yelp_academic_dataset_review.json"
review_data <- jsonlite:: stream_in(file(filepath_review_data))

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

# Part 2: Predicting User Reviews for those 10,000 observations - opting for RandomForest

# Installing Relevant Packages + Storing Packages in Libraries (repeating jsonlite)
install.packages("jsonlite")
install.packages("randomForest")
install.packages("dplyr")
install.packages("lubridate")
library(jsonlite)
library(randomForest)
library(dplyr)
library(lubridate)

# Loading datasets (except User Reviews dataset, as already done in Part 1)
business_data <- jsonlite::stream_in(file("/Users/god/Desktop/God/3rdYearUni/DataScience/RStuff/Term1Assignment/ProvidedMaterial/yelp_academic_dataset_business.json"))
users_data <- jsonlite::stream_in(file("/Users/god/Desktop/God/3rdYearUni/DataScience/RStuff/Term1Assignment/ProvidedMaterial/yelp_academic_dataset_user.json"))
checkins_data <- jsonlite::stream_in(file("/Users/god/Desktop/God/3rdYearUni/DataScience/RStuff/Term1Assignment/ProvidedMaterial/yelp_academic_dataset_checkin.json"))
tips_data <- jsonlite::stream_in(file("/Users/god/Desktop/God/3rdYearUni/DataScience/RStuff/Term1Assignment/ProvidedMaterial/yelp_academic_dataset_tip.json"))

# Merging datasets in preparation for model building
merged_data <- review_data %>%
  left_join(business_data, by = "business_id") %>%
  left_join(users_data, by = "user_id") %>%
  left_join(checkins_data, by = "business_id") %>%
  left_join(tips_data, by = c("user_id", "business_id"))

# Isolating variables that I think are relevant, and creating new ones. Also encoding a few categorical variables that I will use.
names(merged_data)
merged_data <- merged_data %>%
  mutate(
    # Directly use column names without 'merged_data$'
    review_length = nchar(ifelse(is.na(text.x), "", text.x)),
    tip_length = nchar(text.y),
    user_activity_years = as.numeric(interval(ymd(yelping_since), Sys.Date()) / years(1)),
    user_elite_status = ifelse(elite != "", 1, 0),
    checkin_count = n_distinct(date.y),
    compliment_total = rowSums(select(., starts_with("compliment_")), na.rm = TRUE),
    # Converting categorical variables to factors
    city = as.factor(city),
    state = as.factor(state),
    categories = as.factor(categories)
  ) 

merged_data <- merged_data %>%
  select(
    # Directly use column names without 'merged_data$'
    review_stars = stars.x, review_length, review_date = date.x,
    business_stars = stars.y, business_review_count = review_count.y, 
    is_open, categories, city, state, attributes, 
    average_stars, review_count = review_count.x, yelping_since, 
    fans, user_elite_status, checkin_count, 
    tip_length, compliment_count, user_activity_years, compliment_total
  )

# Splitting data into training and data sets in a consistent manner
set.seed(123)
train_indices <- sample(1:nrow(merged_data), size = 0.8 * nrow(merged_data))
training_dataset <- merged_data[train_indices, ]
test_dataset <- merged_data[-train_indices, ]

# Now training RandomForest model on the training dataset
randomforest_model <- randomForest(review_stars ~ ., data = training_dataset, ntree = 500)

# Making predictions on the test dataset, calculating Mean Square Error, analysing variable importance
predictions <- predict(randomforest_model, newdata = test_dataset)
mse_test <- mean((test_dataset$review_stars - predictions)^2)
print(paste("The MSE on the Test Dataset is: "), mse_test)
importancevalues <- importance(randomforest_model)
print(importance_values)






  




