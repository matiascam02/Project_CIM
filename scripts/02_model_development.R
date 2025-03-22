#!/usr/bin/env Rscript

# ==============================================================================
# 02_model_development.R
# 
# This script develops predictive models for the Airbnb Berlin rental demand
# prediction project. It trains and evaluates different models to predict
# rental demand based on listing characteristics.
#
# Improvements to prevent overfitting:
# - PCA for dimensionality reduction
# - Cross-validation for model evaluation
# - Hyperparameter tuning for optimal model complexity
# - Feature selection based on importance
# - Multiple models comparison
# ==============================================================================

# Load required libraries
library(tidyverse)      # For data manipulation
library(caret)          # For model training and evaluation
library(randomForest)   # For Random Forest models
library(xgboost)        # For Gradient Boosting models
library(glmnet)         # For regularized regression models
library(doParallel)     # For parallel processing
library(pROC)           # For ROC curve analysis
library(e1071)          # For SVM models

# Define file paths
processed_data_path <- "./data/processed/"
models_path <- "./models/"
results_path <- "./results/"

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
# Only include categorical variables with a reasonable number of levels that exist in the data
cat_columns <- c("neighborhood_group", "property_type", "room_type", 
                "host_response_time", "is_superhost", "instant_bookable")

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

# Create training and validation sets (70% train, 30% validation)
set.seed(42) # For reproducibility
train_index <- createDataPartition(train_data$demand_proxy, p = 0.7, list = FALSE)
train_set <- train_data[train_index, ]
valid_set <- train_data[-train_index, ]

message(paste("Training set size:", nrow(train_set), "rows"))
message(paste("Validation set size:", nrow(valid_set), "rows"))

# -------------------------------------------------------------------------------
# Feature Engineering and Dimensionality Reduction
# -------------------------------------------------------------------------------

message("Performing feature engineering and dimensionality reduction...")

# Separate numerical and categorical features
numeric_features <- names(train_set)[sapply(train_set, is.numeric)]
numeric_features <- setdiff(numeric_features, "demand_proxy")
categorical_features <- names(train_set)[sapply(train_set, is.factor)]

# Create dummy variables for categorical features (one-hot encoding)
# This step is necessary before applying PCA
message("Creating dummy variables for categorical features...")
dummy_model <- dummyVars(~., data = train_set[, c(categorical_features)])
train_dummy <- predict(dummy_model, newdata = train_set[, c(categorical_features)])
valid_dummy <- predict(dummy_model, newdata = valid_set[, c(categorical_features)])

# Combine numeric and dummy variables
train_combined <- cbind(train_set[, numeric_features], train_dummy)
valid_combined <- cbind(valid_set[, numeric_features], valid_dummy)

# Check for correlation among features
message("Analyzing feature correlations...")
correlation_matrix <- cor(train_combined)
high_corr_features <- findCorrelation(correlation_matrix, cutoff = 0.75)
message(paste("Found", length(high_corr_features), "highly correlated features"))

# Apply PCA for dimensionality reduction
message("Applying PCA for dimensionality reduction...")
# First, normalize the data (center and scale)
preproc <- preProcess(train_combined, method = c("center", "scale"))
train_normalized <- predict(preproc, train_combined)
valid_normalized <- predict(preproc, valid_combined)

# Perform PCA
pca_model <- prcomp(train_normalized)

# Analyze variance explained by principal components
variance_explained <- pca_model$sdev^2 / sum(pca_model$sdev^2)
cumulative_variance <- cumsum(variance_explained)

# Determine number of components to retain 90% of variance
n_components <- which(cumulative_variance >= 0.9)[1]
message(paste("Number of PCA components to retain 90% variance:", n_components))

# Create PCA-transformed datasets
train_pca <- predict(pca_model, train_normalized)[, 1:n_components]
valid_pca <- predict(pca_model, valid_normalized)[, 1:n_components]

# Add the target variable back
train_pca_df <- as.data.frame(train_pca)
train_pca_df$demand_proxy <- train_set$demand_proxy
valid_pca_df <- as.data.frame(valid_pca)
valid_pca_df$demand_proxy <- valid_set$demand_proxy

# -------------------------------------------------------------------------------
# Model Training with Cross-Validation
# -------------------------------------------------------------------------------

message("Setting up parallel processing for model training...")
# Setup parallel processing
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Define cross-validation settings
cv_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  savePredictions = "final",
  allowParallel = TRUE
)

# -------------------------------------------------------------------------------
# Model 1: Random Forest with Hyperparameter Tuning
# -------------------------------------------------------------------------------

message("Training Random Forest model with cross-validation and tuning...")
start_time <- Sys.time()

# Define hyperparameter grid for tuning
rf_grid <- expand.grid(
  mtry = c(floor(sqrt(n_components)), floor(n_components/3), floor(n_components/2))
)

# Train Random Forest on PCA-transformed data with cross-validation
rf_model_cv <- train(
  demand_proxy ~ .,
  data = train_pca_df,
  method = "rf",
  ntree = 500,
  importance = TRUE,
  trControl = cv_control,
  tuneGrid = rf_grid
)

end_time <- Sys.time()
message(paste("Random Forest training time:", difftime(end_time, start_time, units = "mins"), "minutes"))
message("Best Random Forest parameters:")
print(rf_model_cv$bestTune)

# -------------------------------------------------------------------------------
# Model 2: Gradient Boosting Machine (GBM)
# -------------------------------------------------------------------------------

message("Training Gradient Boosting model...")
start_time_gbm <- Sys.time()

# Define hyperparameter grid for GBM
gbm_grid <- expand.grid(
  n.trees = c(100, 500),
  interaction.depth = c(3, 5, 7),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = 10
)

# Train GBM with cross-validation
gbm_model_cv <- train(
  demand_proxy ~ .,
  data = train_pca_df,
  method = "gbm",
  trControl = cv_control,
  tuneGrid = gbm_grid,
  verbose = FALSE
)

end_time_gbm <- Sys.time()
message(paste("GBM training time:", difftime(end_time_gbm, start_time_gbm, units = "mins"), "minutes"))
message("Best GBM parameters:")
print(gbm_model_cv$bestTune)

# -------------------------------------------------------------------------------
# Model 3: Regularized Regression (Elastic Net)
# -------------------------------------------------------------------------------

message("Training Elastic Net model...")
start_time_glmnet <- Sys.time()

# Define hyperparameter grid for Elastic Net
glmnet_grid <- expand.grid(
  alpha = seq(0, 1, by = 0.2),  # 0 = ridge, 1 = lasso, in between = elastic net
  lambda = 10^seq(-4, -1, by = 0.5)
)

# Train Elastic Net with cross-validation
glmnet_model_cv <- train(
  demand_proxy ~ .,
  data = train_pca_df,
  method = "glmnet",
  trControl = cv_control,
  tuneGrid = glmnet_grid
)

end_time_glmnet <- Sys.time()
message(paste("Elastic Net training time:", difftime(end_time_glmnet, start_time_glmnet, units = "mins"), "minutes"))
message("Best Elastic Net parameters:")
print(glmnet_model_cv$bestTune)

# -------------------------------------------------------------------------------
# Stop parallel processing
# -------------------------------------------------------------------------------
stopCluster(cl)

# -------------------------------------------------------------------------------
# Model Evaluation
# -------------------------------------------------------------------------------

message("Evaluating model performance...")

# Create a function to calculate evaluation metrics
calculate_metrics <- function(actual, predicted, model_name) {
  rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  mae <- mean(abs(actual - predicted), na.rm = TRUE)
  r_squared <- 1 - sum((actual - predicted)^2, na.rm = TRUE) / 
               sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  
  data.frame(
    model = model_name,
    rmse = rmse,
    mae = mae,
    r_squared = r_squared
  )
}

# Make predictions on validation set using each model
rf_predictions <- predict(rf_model_cv, valid_pca_df)
gbm_predictions <- predict(gbm_model_cv, valid_pca_df)
glmnet_predictions <- predict(glmnet_model_cv, valid_pca_df)

# Calculate metrics for each model
rf_metrics <- calculate_metrics(valid_pca_df$demand_proxy, rf_predictions, "Random Forest (PCA)")
gbm_metrics <- calculate_metrics(valid_pca_df$demand_proxy, gbm_predictions, "Gradient Boosting (PCA)")
glmnet_metrics <- calculate_metrics(valid_pca_df$demand_proxy, glmnet_predictions, "Elastic Net (PCA)")

# Combine metrics
all_metrics <- rbind(rf_metrics, gbm_metrics, glmnet_metrics)
all_metrics$training_time_mins <- c(
  as.numeric(difftime(end_time, start_time, units = "mins")),
  as.numeric(difftime(end_time_gbm, start_time_gbm, units = "mins")),
  as.numeric(difftime(end_time_glmnet, start_time_glmnet, units = "mins"))
)
all_metrics$num_features_original <- length(features)
all_metrics$num_pca_components <- n_components
all_metrics$timestamp <- Sys.time()

# Display evaluation metrics
message("Model Evaluation Results:")
print(all_metrics)

# Determine the best model based on RMSE
best_model_idx <- which.min(all_metrics$rmse)
best_model_name <- all_metrics$model[best_model_idx]
message(paste("Best model based on RMSE:", best_model_name))

# -------------------------------------------------------------------------------
# Feature Importance Analysis
# -------------------------------------------------------------------------------

# For Random Forest model, analyze feature importance
if (best_model_name == "Random Forest (PCA)") {
  message("Analyzing feature importance for Random Forest model...")
  
  # Get variable importance from the Random Forest model
  rf_importance <- varImp(rf_model_cv)
  
  # Create a data frame for feature importance
  importance_df <- data.frame(
    feature = rownames(rf_importance$importance),
    importance = rf_importance$importance$Overall
  ) %>% arrange(desc(importance))
  
  message("Top 10 most important principal components:")
  print(head(importance_df, 10))
  
  # To interpret PCA components, we need to look at the loadings
  message("Analyzing PCA component loadings...")
  # Get top loadings for the most important components
  top_components <- importance_df$feature[1:5]  # Top 5 components
  
  # Extract the original feature loadings for these components
  loadings_df <- data.frame()
  
  for (comp in top_components) {
    comp_num <- as.numeric(gsub("PC", "", comp))
    
    # Get loadings for this component
    loadings <- pca_model$rotation[, comp_num]
    
    # Get top features with highest absolute loadings
    top_loadings <- sort(abs(loadings), decreasing = TRUE)[1:10]
    top_features <- names(top_loadings)
    
    # Create a data frame with the loadings
    comp_df <- data.frame(
      component = comp,
      feature = top_features,
      loading = loadings[top_features]
    )
    
    loadings_df <- rbind(loadings_df, comp_df)
  }
  
  message("Top feature loadings for important components:")
  print(loadings_df)
  
  # Save loadings for interpretation
  write.csv(loadings_df, paste0(results_path, "pca_component_loadings.csv"), row.names = FALSE)
} else if (best_model_name == "Gradient Boosting (PCA)") {
  message("Analyzing feature importance for Gradient Boosting model...")
  
  # Initialize importance_df to ensure it exists even if both methods fail
  importance_df <- data.frame(
    feature = colnames(valid_pca_df)[colnames(valid_pca_df) != "demand_proxy"],
    importance = 1  # Default equal importance if both methods fail
  )
  
  # Handle GBM importance differently to avoid the relative.influence issue
  # Access the variable importance directly from the caret model summary
  success <- FALSE
  
  # First try the caret varImp method
  tryCatch({
    # Try using varImp if it works
    gbm_importance <- varImp(gbm_model_cv)
    if (!is.null(gbm_importance) && !is.null(gbm_importance$importance)) {
      importance_df <- data.frame(
        feature = rownames(gbm_importance$importance),
        importance = gbm_importance$importance$Overall
      ) %>% arrange(desc(importance))
      success <- TRUE
      message("Using caret's varImp function for feature importance")
    }
  }, error = function(e) {
    message("caret's varImp failed: ", e$message)
  })
  
  # If the first method failed, try the permutation approach
  if (!success) {
    message("Standard varImp failed for GBM, using permutation importance method...")
    
    # Use the variable importance based on the permutation approach (model agnostic)
    # Create an importance table based on which variables increased error when permuted
    set.seed(123)
    var_imp <- data.frame(feature = colnames(valid_pca_df)[colnames(valid_pca_df) != "demand_proxy"])
    var_imp$importance <- 0
    
    # Get baseline prediction error
    baseline_pred <- predict(gbm_model_cv, valid_pca_df)
    baseline_error <- mean((valid_pca_df$demand_proxy - baseline_pred)^2)
    
    # For each variable, permute it and see how much error increases
    # For speed, only test the first 10 variables (we can increase this later)
    for (i in 1:min(10, nrow(var_imp))) {
      curr_var <- var_imp$feature[i]
      message("Testing importance of variable: ", curr_var)
      
      temp_data <- valid_pca_df
      # Permute the variable
      temp_data[[curr_var]] <- sample(temp_data[[curr_var]])
      # Get predictions with permuted data
      perm_pred <- predict(gbm_model_cv, temp_data)
      # Calculate error
      perm_error <- mean((valid_pca_df$demand_proxy - perm_pred)^2)
      # Store importance as increase in error
      var_imp$importance[i] <- perm_error - baseline_error
    }
    
    # Ensure we have positive values (some might be negative due to randomness)
    var_imp$importance <- pmax(0, var_imp$importance)
    
    # If all importances are 0, set them all to 1 (equal importance)
    if (sum(var_imp$importance) == 0) {
      var_imp$importance <- 1
    }
    
    # Normalize importance values
    if (max(var_imp$importance) > 0) {
      var_imp$importance <- var_imp$importance / max(var_imp$importance) * 100
    }
    
    # Sort by importance
    importance_df <- var_imp %>% arrange(desc(importance))
    message("Permutation-based importance calculated for ", min(10, nrow(var_imp)), " variables")
  }
  
  message("Top 10 most important principal components:")
  print(head(importance_df, 10))
} else {
  # For Elastic Net, get variable importance (coefficients)
  message("Analyzing feature importance for Elastic Net model...")
  glmnet_importance <- varImp(glmnet_model_cv)
  importance_df <- data.frame(
    feature = rownames(glmnet_importance$importance),
    importance = glmnet_importance$importance$Overall
  ) %>% arrange(desc(importance))
  
  message("Top 10 most important principal components:")
  print(head(importance_df, 10))
}

# -------------------------------------------------------------------------------
# Save Best Model and Results
# -------------------------------------------------------------------------------

message("Saving trained models...")

# Save the models
saveRDS(rf_model_cv, paste0(models_path, "random_forest_pca_model.rds"))
saveRDS(gbm_model_cv, paste0(models_path, "gbm_pca_model.rds"))
saveRDS(glmnet_model_cv, paste0(models_path, "glmnet_pca_model.rds"))

# Also save the PCA model and preprocessing steps for later use in predictions
saveRDS(pca_model, paste0(models_path, "pca_model.rds"))
saveRDS(preproc, paste0(models_path, "preproc_model.rds"))
saveRDS(dummy_model, paste0(models_path, "dummy_model.rds"))

# Also save the original Random Forest model for backward compatibility
# This model will be used by the existing simulation script
if (best_model_name == "Random Forest (PCA)") {
  saveRDS(rf_model_cv$finalModel, paste0(models_path, "random_forest_model.rds"))
  message("Saved Random Forest model for backward compatibility")
}

# Save model results for reporting
write.csv(all_metrics, paste0(results_path, "model_performance_metrics.csv"), row.names = FALSE)
write.csv(importance_df, paste0(results_path, "feature_importance.csv"), row.names = FALSE)

# -------------------------------------------------------------------------------
# Create Comparison Visualizations
# -------------------------------------------------------------------------------

message("Creating performance comparison visualizations...")

# Create a data frame with actual and predicted values for all models
predictions_df <- data.frame(
  actual = valid_pca_df$demand_proxy,
  random_forest = rf_predictions,
  gradient_boosting = gbm_predictions,
  elastic_net = glmnet_predictions
)

# Create a long format data frame for plotting
predictions_long <- predictions_df %>%
  pivot_longer(cols = c(random_forest, gradient_boosting, elastic_net),
               names_to = "model", values_to = "predicted")

# Create a scatter plot with actual vs predicted values for all models
p1 <- ggplot(predictions_long, aes(x = actual, y = predicted, color = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  facet_wrap(~model) +
  labs(title = "Model Comparison: Actual vs Predicted Values",
       x = "Actual Demand Proxy",
       y = "Predicted Demand Proxy") +
  theme_minimal()

# Create a bar plot comparing RMSE values
p2 <- ggplot(all_metrics, aes(x = reorder(model, -rmse), y = rmse, fill = model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison: RMSE",
       x = "Model",
       y = "RMSE (lower is better)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot comparing R-squared values
p3 <- ggplot(all_metrics, aes(x = reorder(model, r_squared), y = r_squared, fill = model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Comparison: R-squared",
       x = "Model",
       y = "R-squared (higher is better)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save visualizations
ggsave(paste0(results_path, "model_comparison_actual_vs_predicted.png"), p1, width = 12, height = 8)
ggsave(paste0(results_path, "model_comparison_rmse.png"), p2, width = 10, height = 6)
ggsave(paste0(results_path, "model_comparison_r_squared.png"), p3, width = 10, height = 6)

# Feature importance plot (for the best model)
if (nrow(importance_df) > 0) {
  p4 <- ggplot(importance_df[1:min(20, nrow(importance_df)),], 
               aes(x = reorder(feature, importance), y = importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Top Feature Importance -", best_model_name),
         x = "Feature",
         y = "Importance") +
    theme_minimal()
  
  ggsave(paste0(results_path, "best_model_feature_importance.png"), p4, width = 12, height = 10)
}

message("Model development complete. Results saved to ", results_path)
message("Trained models saved to ", models_path)
message("Improved models using PCA and cross-validation to reduce overfitting") 