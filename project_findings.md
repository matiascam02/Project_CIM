# Airbnb Berlin Rental Demand Prediction - Project Findings

This document tracks the progress, findings, and decisions made throughout the project development.

## Project Setup
- Created project structure with directories for data, scripts, analysis, and figures
- Initialized Git repository for version control
- Set up R project environment

## Data Acquisition
- Need to download "Airbnb Berlin Price Prediction" dataset from Kaggle
- Will need to place raw data in the data/raw directory
- TODO: Document data loading and initial cleaning steps

## Initial Considerations
### Target Variable
- The dataset has "Price" as the target variable, but our focus is on predicting demand
- Options to consider:
  1. Use price as a proxy for demand (higher prices might indicate higher demand)
  2. Transform review frequency or booking rate into a demand indicator
  3. Create a demand index based on multiple variables

### Key Features to Explore
- Location features (neighborhood, proximity to attractions)
- Property characteristics (size, type, amenities)
- Host attributes (experience, rating)
- Temporal factors (seasonality, events)

### Modeling Approaches
- Regression models for numerical demand prediction
- Classification for demand categories (high/medium/low)
- Time series analysis for seasonal patterns

## Next Steps
- Download and inspect the raw dataset
- Perform initial data cleaning
- Conduct exploratory data analysis
- Define clear metrics for "demand" 