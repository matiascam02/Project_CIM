# Airbnb Rental Demand Prediction in Berlin

## Project Overview
This project aims to develop a machine learning model that predicts Airbnb rental demand across various districts in Berlin. We analyze factors such as location, price, amenities, proximity to tourist attractions, and local events to understand their influence on demand patterns.

## Dataset
The analysis uses the "Airbnb Berlin Price Prediction" dataset from Kaggle, which includes:
- Listing details (ID, name, property type, etc.)
- Host information (experience, response rate, etc.)
- Geographic data (neighborhood, location coordinates)
- Property attributes (rooms, beds, bathrooms, etc.)
- Rating metrics (overall, cleanliness, location, etc.)
- Pricing information

## Project Structure
```
Project_CIM/
├── data/
│   ├── raw/         # Original, immutable data
│   └── processed/   # Cleaned and transformed data
├── scripts/         # R scripts for data processing
├── analysis/        # R markdown files for analysis
├── figs/            # Generated figures and visualizations
└── README.md        # This file
```

## Methodology
1. **Exploratory Data Analysis (EDA)**: Examine price distributions, geographic patterns, and outliers
2. **Feature Engineering**: Create additional variables and handle missing data
3. **Predictive Modeling**: Implement and compare various machine learning algorithms
4. **Simulations**: Assess impact of external factors on demand

## Getting Started
1. Clone this repository
2. Download the dataset from Kaggle and place it in the `data/raw` directory
3. Run the analysis files in the `analysis` directory

## Dependencies
- R (>= 4.0.0)
- tidyverse
- caret
- randomForest
- gbm
- ggplot2
- sf (for spatial analysis) 