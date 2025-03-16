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

# Define file paths
raw_data_path <- "../data/raw/"
processed_data_path <- "../data/processed/"

# Create processed directory if it doesn't exist
if (!dir.exists(processed_data_path)) {
  dir.create(processed_data_path, recursive = TRUE)
}

# Check if raw data exists
if (!file.exists(paste0(raw_data_path, "airbnb_berlin.csv"))) {
  stop("Raw data file not found. Please download the dataset and place it in the data/raw directory.")
}

# Load the data
message("Loading raw data...")
airbnb_data <- read.csv(paste0(raw_data_path, "airbnb_berlin.csv"))

# Exploring data dimensions
message(paste("Loaded dataset with", nrow(airbnb_data), "rows and", ncol(airbnb_data), "columns"))

# -------------------------------------------------------------------------------
# Data Cleaning
# -------------------------------------------------------------------------------

message("Starting data cleaning process...")

# Clean column names (convert to snake_case)
airbnb_clean <- airbnb_data %>%
  clean_names()

# Handle missing values
airbnb_clean <- airbnb_clean %>%
  # Replace blank strings with NA
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  # Fill missing values as appropriate (TBD based on actual data exploration)
  # For now, we're using placeholder code
  mutate(
    # Example: Replace missing overall_rating with median
    # overall_rating = ifelse(is.na(overall_rating), median(overall_rating, na.rm = TRUE), overall_rating)
  )

# Convert date columns to proper date type (if present)
airbnb_clean <- airbnb_clean %>%
  mutate(
    # Example: Parse date columns (column names to be adjusted based on actual data)
    # host_since = as.Date(host_since),
    # first_review = as.Date(first_review),
    # last_review = as.Date(last_review)
  )

# Remove outliers (to be refined based on actual data exploration)
# For now, this is placeholder code
airbnb_clean <- airbnb_clean %>%
  # Example: Remove extreme price outliers
  # filter(price <= quantile(price, 0.99, na.rm = TRUE))
  identity()  # Placeholder no-op

# -------------------------------------------------------------------------------
# Feature Engineering
# -------------------------------------------------------------------------------

message("Creating new features...")

airbnb_clean <- airbnb_clean %>%
  mutate(
    # Examples (to be adjusted based on actual data):
    
    # Pricing features
    # price_per_person = price / accommodates,
    
    # Time-based features
    # host_experience_days = as.numeric(Sys.Date() - host_since),
    # host_experience_years = host_experience_days / 365,
    
    # Review-based features
    # reviews_per_year = reviews / (as.numeric(Sys.Date() - first_review) / 365),
    
    # Location-based features (if lat/long available)
    # distance_to_center = sqrt((longitude - center_long)^2 + (latitude - center_lat)^2) * 111
    
    # Create demand proxy (placeholder - to be refined)
    # demand_proxy = reviews_per_year
  )

# -------------------------------------------------------------------------------
# Export processed data
# -------------------------------------------------------------------------------

message("Saving processed data...")

# Export cleaned data
write.csv(airbnb_clean, paste0(processed_data_path, "airbnb_berlin_clean.csv"), row.names = FALSE)

# Optionally create additional files for specific analyses
# E.g., a subset for geographic analysis
# airbnb_geo <- airbnb_clean %>%
#   select(id, latitude, longitude, neighborhood, price, demand_proxy)
# 
# write.csv(airbnb_geo, paste0(processed_data_path, "airbnb_berlin_geo.csv"), row.names = FALSE)

message("Data preprocessing complete. Processed files saved to ", processed_data_path) 