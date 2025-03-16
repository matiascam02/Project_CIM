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
data_path <- "../data/"
processed_data_path <- "../data/processed/"

# Create processed directory if it doesn't exist
if (!dir.exists(processed_data_path)) {
  dir.create(processed_data_path, recursive = TRUE)
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

# Basic exploration of column names
message("Train dataset column names:")
print(colnames(train_data))
message("Test dataset column names:")
print(colnames(test_data))

# -------------------------------------------------------------------------------
# Data Cleaning
# -------------------------------------------------------------------------------

message("Starting data cleaning process...")

# Clean column names (convert to snake_case)
train_clean <- train_data %>%
  clean_names()

test_clean <- test_data %>%
  clean_names()

# Print column names after cleaning for debugging
message("Train column names after cleaning:")
print(names(train_clean))
message("Test column names after cleaning:")
print(names(test_clean))

# Convert numeric columns that are stored as character
message("Converting character columns to appropriate types...")
train_clean <- train_clean %>%
  mutate(
    accomodates = as.numeric(accomodates),
    bathrooms = as.numeric(bathrooms),
    bedrooms = as.numeric(bedrooms),
    beds = as.numeric(beds),
    square_feet = as.numeric(square_feet),
    guests_included = as.numeric(guests_included),
    min_nights = as.numeric(min_nights),
    reviews = as.numeric(reviews),
    overall_rating = as.numeric(overall_rating),
    accuracy_rating = as.numeric(accuracy_rating),
    cleanliness_rating = as.numeric(cleanliness_rating),
    checkin_rating = as.numeric(checkin_rating),
    communication_rating = as.numeric(communication_rating),
    location_rating = as.numeric(location_rating),
    value_rating = as.numeric(value_rating),
    price = as.numeric(price)
  )

test_clean <- test_clean %>%
  mutate(
    accomodates = as.numeric(accomodates),
    bathrooms = as.numeric(bathrooms),
    bedrooms = as.numeric(bedrooms),
    beds = as.numeric(beds),
    square_feet = as.numeric(square_feet),
    guests_included = as.numeric(guests_included),
    min_nights = as.numeric(min_nights),
    reviews = as.numeric(reviews),
    overall_rating = as.numeric(overall_rating),
    accuracy_rating = as.numeric(accuracy_rating),
    cleanliness_rating = as.numeric(cleanliness_rating),
    checkin_rating = as.numeric(checkin_rating),
    communication_rating = as.numeric(communication_rating),
    location_rating = as.numeric(location_rating),
    value_rating = as.numeric(value_rating)
  )

# Add a dummy price column to test data for easier handling
# This will be used for feature engineering and then removed before final export
test_clean <- test_clean %>%
  mutate(price = NA)

# Display data structure for understanding
message("Structure of the train dataset after type conversion:")
str(train_clean, list.len = 15)

# Check for missing values
message("Checking for missing values in train data:")
missing_values <- colSums(is.na(train_clean))
print(missing_values[missing_values > 0])

# Handle missing values
train_clean <- train_clean %>%
  # Replace blank strings with NA
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  # Fill missing values as appropriate based on data exploration
  mutate(
    # Handle missing ratings with median values
    overall_rating = ifelse(is.na(overall_rating), 
                            median(overall_rating, na.rm = TRUE), 
                            overall_rating),
    accuracy_rating = ifelse(is.na(accuracy_rating), 
                             median(accuracy_rating, na.rm = TRUE), 
                             accuracy_rating),
    cleanliness_rating = ifelse(is.na(cleanliness_rating), 
                                median(cleanliness_rating, na.rm = TRUE), 
                                cleanliness_rating),
    checkin_rating = ifelse(is.na(checkin_rating), 
                            median(checkin_rating, na.rm = TRUE), 
                            checkin_rating),
    communication_rating = ifelse(is.na(communication_rating), 
                                 median(communication_rating, na.rm = TRUE), 
                                 communication_rating),
    location_rating = ifelse(is.na(location_rating), 
                            median(location_rating, na.rm = TRUE), 
                            location_rating),
    value_rating = ifelse(is.na(value_rating), 
                         median(value_rating, na.rm = TRUE), 
                         value_rating)
  )

test_clean <- test_clean %>%
  # Apply same transformations to test data
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  # Fill missing values with the same approach
  mutate(
    # Handle missing ratings with median values from train data
    overall_rating = ifelse(is.na(overall_rating), 
                            median(train_clean$overall_rating, na.rm = TRUE), 
                            overall_rating),
    accuracy_rating = ifelse(is.na(accuracy_rating), 
                             median(train_clean$accuracy_rating, na.rm = TRUE), 
                             accuracy_rating),
    cleanliness_rating = ifelse(is.na(cleanliness_rating), 
                                median(train_clean$cleanliness_rating, na.rm = TRUE), 
                                cleanliness_rating),
    checkin_rating = ifelse(is.na(checkin_rating), 
                            median(train_clean$checkin_rating, na.rm = TRUE), 
                            checkin_rating),
    communication_rating = ifelse(is.na(communication_rating), 
                                 median(train_clean$communication_rating, na.rm = TRUE), 
                                 communication_rating),
    location_rating = ifelse(is.na(location_rating), 
                            median(train_clean$location_rating, na.rm = TRUE), 
                            location_rating),
    value_rating = ifelse(is.na(value_rating), 
                         median(train_clean$value_rating, na.rm = TRUE), 
                         value_rating)
  )

# Convert date columns to proper date type
# First check if there are date columns
date_columns <- names(train_clean)[grepl("date|since|review", names(train_clean), ignore.case = TRUE)]
message("Potential date columns: ", paste(date_columns, collapse = ", "))

# Process date columns - handling potential errors
train_clean <- train_clean %>%
  mutate(
    host_since = as.Date(host_since, format = "%Y-%m-%d"),
    first_review = as.Date(first_review, format = "%Y-%m-%d"),
    last_review = as.Date(last_review, format = "%Y-%m-%d")
  )

test_clean <- test_clean %>%
  mutate(
    host_since = as.Date(host_since, format = "%Y-%m-%d"),
    first_review = as.Date(first_review, format = "%Y-%m-%d"),
    last_review = as.Date(last_review, format = "%Y-%m-%d")
  )

# Fix typo in column name (accomodates should be accommodates)
if("accomodates" %in% names(train_clean)) {
  names(train_clean)[names(train_clean) == "accomodates"] <- "accommodates"
}
if("accomodates" %in% names(test_clean)) {
  names(test_clean)[names(test_clean) == "accomodates"] <- "accommodates"
}

# Check for price column and perform basic data validation
message("Price statistics (train data):")
price_summary <- summary(train_clean$price)
print(price_summary)

# Remove extreme price outliers
price_99pct <- quantile(train_clean$price, 0.99, na.rm = TRUE)
train_clean <- train_clean %>%
  filter(price <= price_99pct | is.na(price))

message(paste("Removed", nrow(train_data) - nrow(train_clean), "price outliers above", price_99pct))

# -------------------------------------------------------------------------------
# Feature Engineering
# -------------------------------------------------------------------------------

message("Creating new features...")

# Define Berlin city center coordinates (approximate, near Alexanderplatz)
berlin_center_lat <- 52.52
berlin_center_lng <- 13.405

# Create features for demand prediction
train_clean <- train_clean %>%
  mutate(
    # Price-related features
    price_per_person = price / accommodates,
    price_per_bedroom = ifelse(bedrooms > 0, price / bedrooms, price),
    price_tier = cut(price, 
                     breaks = c(0, 25, 50, 75, 100, 150, 1000),
                     labels = c("budget", "economy", "standard", "comfort", "premium", "luxury"),
                     right = FALSE),
    
    # Location-based features
    distance_to_center_km = sqrt((latitude - berlin_center_lat)^2 + 
                                 (longitude - berlin_center_lng)^2) * 111, # 111km per degree
    
    # Host-related features
    host_days = as.numeric(difftime(Sys.Date(), host_since, units = "days")),
    host_years = host_days / 365,
    is_superhost = ifelse(is_superhost == "t", 1, 0),
    
    # Property features
    room_type_entire = ifelse(room_type == "Entire home/apt", 1, 0),
    room_type_private = ifelse(room_type == "Private room", 1, 0),
    room_type_shared = ifelse(room_type == "Shared room", 1, 0),
    
    # Review-based features
    has_reviews = ifelse(reviews > 0, 1, 0),
    days_since_last_review = ifelse(!is.na(last_review), 
                                    as.numeric(difftime(Sys.Date(), last_review, units = "days")),
                                    NA),
    review_period = ifelse(!is.na(first_review) & !is.na(last_review),
                          as.numeric(difftime(last_review, first_review, units = "days")),
                          NA),
    reviews_per_month = ifelse(review_period > 30, 
                              reviews / (review_period / 30),
                              reviews),
    
    # Rating features
    avg_rating = rowMeans(select(., matches("_rating$")), na.rm = TRUE),
    rating_range = apply(select(., matches("_rating$")), 1, 
                        function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    # Booking features
    guests_vs_capacity = guests_included / accommodates,
    min_nights_category = cut(min_nights, 
                             breaks = c(0, 1, 3, 7, 14, 1000),
                             labels = c("daily", "short_stay", "weekly", "biweekly", "monthly"),
                             right = FALSE),
    
    # Business features
    business_ready = ifelse(business_travel_ready == "t", 1, 0),
    instant_bookable = ifelse(instant_bookable == "t", 1, 0),
    
    # Create a combined demand proxy
    demand_proxy_reviews = reviews / max(reviews, na.rm = TRUE),
    demand_proxy = demand_proxy_reviews  # Will be enhanced later if more data available
  )

# Create features for test data that don't depend on price
test_clean <- test_clean %>%
  mutate(
    # Location-based features
    distance_to_center_km = sqrt((latitude - berlin_center_lat)^2 + 
                                 (longitude - berlin_center_lng)^2) * 111,
    
    # Host-related features
    host_days = as.numeric(difftime(Sys.Date(), host_since, units = "days")),
    host_years = host_days / 365,
    is_superhost = ifelse(is_superhost == "t", 1, 0),
    
    # Property features
    room_type_entire = ifelse(room_type == "Entire home/apt", 1, 0),
    room_type_private = ifelse(room_type == "Private room", 1, 0),
    room_type_shared = ifelse(room_type == "Shared room", 1, 0),
    
    # Review-based features
    has_reviews = ifelse(reviews > 0, 1, 0),
    days_since_last_review = ifelse(!is.na(last_review), 
                                    as.numeric(difftime(Sys.Date(), last_review, units = "days")),
                                    NA),
    review_period = ifelse(!is.na(first_review) & !is.na(last_review),
                          as.numeric(difftime(last_review, first_review, units = "days")),
                          NA),
    reviews_per_month = ifelse(review_period > 30, 
                              reviews / (review_period / 30),
                              reviews),
    
    # Rating features
    avg_rating = rowMeans(select(., matches("_rating$")), na.rm = TRUE),
    rating_range = apply(select(., matches("_rating$")), 1, 
                        function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)),
    
    # Booking features
    guests_vs_capacity = guests_included / accommodates,
    min_nights_category = cut(min_nights, 
                             breaks = c(0, 1, 3, 7, 14, 1000),
                             labels = c("daily", "short_stay", "weekly", "biweekly", "monthly"),
                             right = FALSE),
    
    # Business features
    business_ready = ifelse(business_travel_ready == "t", 1, 0),
    instant_bookable = ifelse(instant_bookable == "t", 1, 0),
    
    # Create a combined demand proxy based only on reviews
    demand_proxy_reviews = reviews / max(train_clean$reviews, na.rm = TRUE),
    demand_proxy = demand_proxy_reviews  # Will be updated later
  )

# Create neighborhood demand index based on average prices
neighborhood_demand <- train_clean %>%
  group_by(neighbourhood) %>%
  summarise(
    avg_neighborhood_price = mean(price, na.rm = TRUE),
    avg_neighborhood_reviews = mean(reviews, na.rm = TRUE),
    listing_count = n()
  )

# Add neighborhood demand index to the datasets
train_clean <- train_clean %>%
  left_join(neighborhood_demand, by = "neighbourhood") %>%
  mutate(
    price_to_neighborhood_ratio = price / avg_neighborhood_price,
    reviews_to_neighborhood_ratio = reviews / avg_neighborhood_reviews,
    # Enhance demand proxy with neighborhood info
    demand_proxy = (demand_proxy_reviews + (price_to_neighborhood_ratio / max(price_to_neighborhood_ratio, na.rm = TRUE))) / 2
  )

test_clean <- test_clean %>%
  left_join(neighborhood_demand, by = "neighbourhood") %>%
  mutate(
    # For test data, we will add price-related features during prediction
    reviews_to_neighborhood_ratio = reviews / avg_neighborhood_reviews,
    # We'll use only the reviews part of demand proxy since we don't have prices
    demand_proxy = demand_proxy_reviews
  )

# Remove the dummy price column from test data
test_clean <- test_clean %>%
  select(-price)

# -------------------------------------------------------------------------------
# Export processed data
# -------------------------------------------------------------------------------

message("Saving processed data...")

# Export cleaned data
write.csv(train_clean, paste0(processed_data_path, "train_berlin_clean.csv"), row.names = FALSE)
write.csv(test_clean, paste0(processed_data_path, "test_berlin_clean.csv"), row.names = FALSE)

# For combined dataset, fill in NAs for price-related columns in test data
test_with_price_cols <- test_clean %>%
  mutate(
    price = NA,
    price_per_person = NA,
    price_per_bedroom = NA,
    price_tier = NA,
    price_to_neighborhood_ratio = NA
  )

# Create a combined dataset for general analysis (with understanding that test has NAs for price)
combined_clean <- bind_rows(
  mutate(train_clean, dataset = "train"),
  mutate(test_with_price_cols, dataset = "test")
)

write.csv(combined_clean, paste0(processed_data_path, "airbnb_berlin_combined.csv"), row.names = FALSE)

message("Data preprocessing complete. Processed files saved to ", processed_data_path) 