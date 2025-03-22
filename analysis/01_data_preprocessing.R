#!/usr/bin/env Rscript

# ==============================================================================
# 01_data_preprocessing.R
# 
# This script performs data preprocessing for the Airbnb Berlin rental demand 
# prediction project. It cleans, transforms, and prepares the data for analysis.
# ==============================================================================

# Load required libraries
library(tidyverse)  # For data manipulation
library(lubridate)  # For date handling
library(janitor)    # For cleaning column names

# Define file paths - using existing files in the data directory
data_path <- "../data/raw/"
processed_data_path <- "../data/processed/"

# Create directories if they don't exist
if (!dir.exists(data_path)) {
  dir.create(data_path, recursive = TRUE)
  message("Created raw data directory")
}

if (!dir.exists(processed_data_path)) {
  dir.create(processed_data_path, recursive = TRUE)
  message("Created processed data directory")
}

# Check if train and test data exist
if (!file.exists(paste0(data_path, "train_airbnb_berlin.csv")) || 
    !file.exists(paste0(data_path, "test_airbnb_berlin.csv"))) {
  stop("Train or test data files not found in the data directory.")
}

# Load the data
message("Loading train and test data...")
train_data <- read.csv(paste0(data_path, "train_airbnb_berlin.csv"), stringsAsFactors = FALSE)
test_data <- read.csv(paste0(data_path, "test_airbnb_berlin.csv"), stringsAsFactors = FALSE)

# Exploring data dimensions
message(paste("Loaded train dataset with", nrow(train_data), "rows and", ncol(train_data), "columns"))
message(paste("Loaded test dataset with", nrow(test_data), "rows and", ncol(test_data), "columns"))

# Display original column names for reference
message("Original train dataset column names:")
print(colnames(train_data))

# -------------------------------------------------------------------------------
# Data Cleaning
# -------------------------------------------------------------------------------

message("Starting data cleaning process...")

# Clean column names (convert to lowercase)
# Note: Using clean_names() from janitor, which converts to snake_case
train_clean <- train_data %>%
  clean_names()

test_clean <- test_data %>%
  clean_names()

# Print column names after cleaning 
message("Train column names after cleaning (all lowercase):")
print(names(train_clean))

# Convert numeric columns that are stored as character
message("Converting character columns to appropriate types...")

# Get numeric columns based on attempts to convert
numeric_cols <- names(train_clean)[sapply(train_clean, function(x) !all(is.na(as.numeric(suppressWarnings(as.character(x))))))]
# Filter out obvious non-numeric columns
numeric_cols <- numeric_cols[!numeric_cols %in% c("id", "listing_url", "name", "host_id", "host_name", 
                                                 "neighbourhood", "property_type", "room_type", 
                                                 "host_response_time", "host_response_rate")]

# Print identified numeric columns
message("Converting these columns to numeric:")
print(numeric_cols)

# Create a function to convert columns to numeric
convert_to_numeric <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(as.character(df[[col]]))
    }
  }
  return(df)
}

# Apply conversion to both datasets
train_clean <- convert_to_numeric(train_clean, numeric_cols)
test_clean <- convert_to_numeric(test_clean, numeric_cols)

# Fix any naming inconsistencies
if ("accomodates" %in% names(train_clean)) {
  names(train_clean)[names(train_clean) == "accomodates"] <- "accommodates"
}
if ("accomodates" %in% names(test_clean)) {
  names(test_clean)[names(test_clean) == "accomodates"] <- "accommodates"
}

# -------------------------------------------------------------------------------
# Handling Missing Values 
# -------------------------------------------------------------------------------

message("Handling missing values...")

# Check for missing values
missing_values <- colSums(is.na(train_clean))
message("Columns with missing values in train data:")
print(missing_values[missing_values > 0])

# Handle missing values for numeric columns
numeric_cols <- names(train_clean)[sapply(train_clean, is.numeric)]
for (col in numeric_cols) {
  if (sum(is.na(train_clean[[col]])) > 0) {
    # Impute with median
    median_val <- median(train_clean[[col]], na.rm = TRUE)
    train_clean[[col]][is.na(train_clean[[col]])] <- median_val
    
    # Also apply to test data if the column exists
    if (col %in% names(test_clean)) {
      test_clean[[col]][is.na(test_clean[[col]])] <- median_val
    }
    
    message(paste("Imputed missing values in", col, "with median", median_val))
  }
}

# Handle missing values for categorical columns
cat_cols <- names(train_clean)[sapply(train_clean, is.character)]
for (col in cat_cols) {
  if (sum(is.na(train_clean[[col]])) > 0 || sum(train_clean[[col]] == "") > 0) {
    # Replace empty strings with NA
    train_clean[[col]][train_clean[[col]] == ""] <- NA
    
    # Find mode (most common value)
    mode_val <- names(sort(table(train_clean[[col]]), decreasing = TRUE))[1]
    train_clean[[col]][is.na(train_clean[[col]])] <- mode_val
    
    # Also apply to test data if the column exists
    if (col %in% names(test_clean)) {
      test_clean[[col]][test_clean[[col]] == ""] <- NA
      test_clean[[col]][is.na(test_clean[[col]])] <- mode_val
    }
    
    message(paste("Imputed missing values in", col, "with mode", mode_val))
  }
}

# -------------------------------------------------------------------------------
# Feature Engineering
# -------------------------------------------------------------------------------

message("Creating demand proxy and additional features...")

# Create a demand proxy based on available columns
if ("reviews" %in% names(train_clean)) {
  # Normalize reviews (higher = more demand)
  max_reviews <- max(train_clean$reviews, na.rm = TRUE)
  train_clean$review_score <- train_clean$reviews / max_reviews
  message("Created review_score from reviews column")
  
  # Create availability score
  # Using 4 quantiles (5 breaks) for consistency
  quantiles <- quantile(train_clean$reviews, probs = seq(0, 1, 0.25), na.rm = TRUE)
  train_clean$availability_score <- cut(
    train_clean$reviews, 
    breaks = c(-1, quantiles), 
    labels = seq(0, 0.75, 0.25)
  )
  train_clean$availability_score <- as.numeric(as.character(train_clean$availability_score))
  message("Created availability_score as a proxy from reviews column")
  
  # Combined demand score (average of review and availability scores)
  train_clean$demand_proxy <- (train_clean$review_score + train_clean$availability_score) / 2
  message("Created demand_proxy from review_score and availability_score")
  
} else if ("price" %in% names(train_clean)) {
  # Fallback to price-based proxy if reviews not available
  max_price <- max(train_clean$price, na.rm = TRUE)
  train_clean$demand_proxy <- train_clean$price / max_price
  message("Created demand_proxy from price (fallback method)")
} else {
  # Last resort - random values
  set.seed(123)
  train_clean$demand_proxy <- runif(nrow(train_clean))
  message("WARNING: Created random demand_proxy as last resort")
}

# Create additional features
message("Creating additional features...")

# Define Berlin city center coordinates (approximate, near Alexanderplatz)
berlin_center_lat <- 52.52
berlin_center_lng <- 13.405

# Create features for demand prediction
train_clean <- train_clean %>%
  mutate(
    # Location-based features
    distance_to_center_km = if("latitude" %in% names(.) && "longitude" %in% names(.)) {
      sqrt((latitude - berlin_center_lat)^2 + (longitude - berlin_center_lng)^2) * 111
    } else {
      NA
    },
    
    # Price features (if price exists)
    price_per_person = if("price" %in% names(.) && "accommodates" %in% names(.)) {
      ifelse(accommodates > 0, price / accommodates, price)
    } else {
      NA
    },
    
    # Host features
    is_superhost_numeric = if("is_superhost" %in% names(.)) {
      ifelse(is_superhost == "t", 1, 0)
    } else {
      NA
    }
  )

# Apply same transformations to test data where possible
test_clean <- test_clean %>%
  mutate(
    # Location-based features
    distance_to_center_km = if("latitude" %in% names(.) && "longitude" %in% names(.)) {
      sqrt((latitude - berlin_center_lat)^2 + (longitude - berlin_center_lng)^2) * 111
    } else {
      NA
    },
    
    # Host features
    is_superhost_numeric = if("is_superhost" %in% names(.)) {
      ifelse(is_superhost == "t", 1, 0)
    } else {
      NA
    }
  )

# -------------------------------------------------------------------------------
# Export processed data
# -------------------------------------------------------------------------------

message("Saving processed data...")

# Export cleaned data with consistent lowercase column naming
write.csv(train_clean, paste0(processed_data_path, "train_berlin_clean.csv"), row.names = FALSE)
write.csv(test_clean, paste0(processed_data_path, "test_berlin_clean.csv"), row.names = FALSE)

message("Data preprocessing complete. Files saved with standardized column names to:", processed_data_path) 