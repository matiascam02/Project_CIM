#!/usr/bin/env Rscript

# ==============================================================================
# 02_model_development.R
# 
# This script develops predictive models for the Airbnb Berlin rental demand
# prediction project. It trains and evaluates different models to predict
# rental demand based on listing characteristics.
# ==============================================================================

# Load required libraries
library(tidyverse)      # For data manipulation
library(caret)          # For model training and evaluation
library(randomForest)   # For Random Forest models
library(xgboost)        # For Gradient Boosting models
library(glmnet)         # For regularized regression models
library(doParallel)     # For parallel processing

# Define file paths
processed_data_path <- "../data/processed/"
models_path <- "../models/"
results_path <- "../results/"

# Create directories if they don't exist
if (!dir.exists(models_path)) {
  dir.create(models_path, recursive = TRUE)
}
if (!dir.exists(results_path)) {
  dir.create(results_path, recursive = TRUE)
}

# Load the processed data
message("Loading processed data...")
train_data <- read.csv(paste0(processed_data_path, "train_berlin_clean.csv"))
test_data <- read.csv(paste0(processed_data_path, "test_berlin_clean.csv"))

# Print basic information about the datasets
message(paste("Loaded train dataset with", nrow(train_data), "rows and", ncol(train_data), "columns"))
message(paste("Loaded test dataset with", nrow(test_data), "rows and", ncol(test_data), "columns"))

# -------------------------------------------------------------------------------
# Data Preparation for Modeling
# -------------------------------------------------------------------------------

message("Preparing data for modeling...")

# Check for any lingering NA values
train_na_counts <- colSums(is.na(train_data))
message("Number of NA values in key train columns:")
print(train_na_counts[train_na_counts > 0])

# Simple imputation for remaining NAs to avoid modeling errors
# For numeric columns, use median; for categorical, use most frequent value
train_data <- train_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)),
         across(where(is.character), ~ifelse(is.na(.), "Unknown", .)))

# Handle categorical variables with many levels
# For neighbourhood, we'll use neighborhood_group instead (fewer levels)
# For other high-cardinality categorical variables, we'll use one-hot encoding or drop them

# Convert categorical variables to factors for modeling
# Only include categorical variables with a reasonable number of levels
cat_columns <- c("neighborhood_group", "property_type", "room_type", 
                "min_nights_category", "price_tier")

# Check the number of levels in each categorical column
for (col in cat_columns) {
  if (col %in% names(train_data)) {
    num_levels <- length(unique(train_data[[col]]))
    message(paste("Column", col, "has", num_levels, "unique values"))
  }
}

# Convert selected categorical columns to factors
train_data <- train_data %>%
  mutate(across(all_of(cat_columns), as.factor))

# Feature Selection: Select features that are likely to be important for demand prediction
# Remove identifier columns, high-cardinality categorical variables, and variables with high collinearity
features <- setdiff(names(train_data), 
                   c("listing_id", "listing_name", "host_id", "host_name", 
                     "city", "country", "country_code", "postal_code",
                     "first_review", "last_review", "host_since",
                     "demand_proxy", "demand_proxy_reviews", # These are our targets
                     "neighbourhood", # Too many levels for RandomForest
                     # Remove highly correlated features
                     "room_type_entire", "room_type_private", "room_type_shared")) 

# For this project, we'll use demand_proxy as our target variable
# This is a composite measure created during preprocessing
message("Using demand_proxy as target variable for model development")

# Create training and validation sets (80% train, 20% validation)
set.seed(42) # For reproducibility
train_index <- createDataPartition(train_data$demand_proxy, p = 0.8, list = FALSE)
train_set <- train_data[train_index, ]
valid_set <- train_data[-train_index, ]

message(paste("Training set size:", nrow(train_set), "rows"))
message(paste("Validation set size:", nrow(valid_set), "rows"))

# Create model formula
model_formula <- as.formula(paste("demand_proxy ~", paste(setdiff(features, "demand_proxy"), collapse = " + ")))

# -------------------------------------------------------------------------------
# Model Training - Random Forest
# -------------------------------------------------------------------------------

message("Training Random Forest model...")

# Setup parallel processing
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Train Random Forest model
# Using a smaller number of trees for initial development (can increase later)
start_time <- Sys.time()
rf_model <- randomForest(
  formula = model_formula,
  data = train_set,
  ntree = 100,
  mtry = floor(sqrt(length(setdiff(features, "demand_proxy")))),
  importance = TRUE,
  na.action = na.omit
)
end_time <- Sys.time()
message(paste("Random Forest training time:", difftime(end_time, start_time, units = "mins"), "minutes"))

# Stop parallel processing
stopCluster(cl)

# -------------------------------------------------------------------------------
# Model Evaluation
# -------------------------------------------------------------------------------

message("Evaluating model performance...")

# Make predictions on validation set
rf_predictions <- predict(rf_model, valid_set)

# Calculate evaluation metrics
rmse <- sqrt(mean((valid_set$demand_proxy - rf_predictions)^2, na.rm = TRUE))
mae <- mean(abs(valid_set$demand_proxy - rf_predictions), na.rm = TRUE)
r_squared <- 1 - sum((valid_set$demand_proxy - rf_predictions)^2, na.rm = TRUE) / 
             sum((valid_set$demand_proxy - mean(valid_set$demand_proxy, na.rm = TRUE))^2, na.rm = TRUE)

# Display evaluation metrics
message("Random Forest Model Evaluation:")
message(paste("RMSE:", round(rmse, 4)))
message(paste("MAE:", round(mae, 4)))
message(paste("R-squared:", round(r_squared, 4)))

# Analyze feature importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$feature <- rownames(importance_df)
importance_df <- importance_df %>%
  arrange(desc(`%IncMSE`)) %>%
  select(feature, `%IncMSE`, IncNodePurity)

message("Top 10 most important features:")
print(head(importance_df, 10))

# Create visualizations of model performance
p1 <- ggplot(data.frame(actual = valid_set$demand_proxy, predicted = rf_predictions), 
            aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Random Forest: Actual vs Predicted Values",
       x = "Actual Demand Proxy",
       y = "Predicted Demand Proxy") +
  theme_minimal()

p2 <- ggplot(importance_df[1:20,], aes(x = reorder(feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 20 Feature Importance",
       x = "Feature",
       y = "Importance (%IncMSE)") +
  theme_minimal()

# Save visualizations
ggsave(paste0(results_path, "rf_actual_vs_predicted.png"), p1, width = 10, height = 8)
ggsave(paste0(results_path, "rf_feature_importance.png"), p2, width = 12, height = 10)

# -------------------------------------------------------------------------------
# Save Model
# -------------------------------------------------------------------------------

message("Saving trained model...")
saveRDS(rf_model, paste0(models_path, "random_forest_model.rds"))

# Save model results for reporting
model_results <- data.frame(
  model = "Random Forest",
  rmse = rmse,
  mae = mae,
  r_squared = r_squared,
  training_time_mins = as.numeric(difftime(end_time, start_time, units = "mins")),
  num_features = length(setdiff(features, "demand_proxy")),
  timestamp = Sys.time()
)

# Save results
write.csv(model_results, paste0(results_path, "model_performance_metrics.csv"), row.names = FALSE)
write.csv(importance_df, paste0(results_path, "feature_importance.csv"), row.names = FALSE)

message("Model development complete. Results saved to ", results_path)
message("Trained model saved to ", models_path) 