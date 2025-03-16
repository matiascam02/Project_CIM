# Airbnb Berlin Rental Demand Prediction - Project Findings

This document tracks the progress, findings, and decisions made throughout the project development.

## Project Setup
- Created project structure with directories for data, scripts, analysis, and figures
- Initialized Git repository for version control
- Set up R project environment

## Data Acquisition
- Located the train_airbnb_berlin.csv and test_airbnb_berlin.csv files in the data directory
- Train dataset contains 15,692 records with 39 columns
- Test dataset contains 7,842 records with 38 columns
- Price column present in both datasets (main target for prediction)

## Data Understanding and Cleaning
- Cleaned data column names to use snake_case
- Identified and handled missing values (several rating columns have ~3,000 missing values)
- Removed price outliers above €250 (99th percentile)
- Created combined dataset for comprehensive analysis

## Key Dataset Characteristics

### Price Distribution
- Prices range from €8 to €900, with median of €49 and mean of €60.34
- Price distribution is right-skewed, with most listings under €100
- Log transformation makes the price distribution more normal

### Property and Location Characteristics
- Data includes various Berlin neighborhoods, with highest concentration in central districts
- Most common property type is "Apartment" followed by "Private room"
- Most listings accommodate 2-4 people
- Strong correlation between price and number of accommodates, bedrooms, and bathrooms

### Ratings and Reviews
- Rating columns include Overall, Accuracy, Cleanliness, Check-in, Communication, Location, and Value
- Most ratings are high (4-5 stars), showing a positive skew
- Only about 19% of listings are missing ratings data
- Number of reviews varies widely across listings

## Target Variable Considerations
- While the dataset has "Price" as the target variable, our focus is on predicting demand
- After analysis, we've identified the following demand proxy options:
  1. Review counts (higher review count = higher demand)
  2. Availability metrics (where available, lower availability = higher demand)
  3. Normalized price for similar property types (higher relative price may indicate higher demand)

### Chosen Demand Proxy Approach
- Created a composite demand proxy based on normalized review counts
- Where available, included inverse of availability metrics (1 - availability/max_days)
- For listings without reviews or availability data, used normalized price as fallback

## Feature Engineering
- Created price-related features: price_per_person, price_per_bedroom, price_to_neighborhood_ratio
- Added categorical price tiers based on price ranges
- Generated neighborhood statistics including average price, review count
- Calculated relative metrics (listing's price vs. neighborhood average)
- Created host experience indicators (years as host)
- Added boolean flags for room types and other categorical variables
- Processed categorical variables to ensure they have manageable cardinality

## Model Development Results
- Successfully trained a Random Forest model for demand prediction
- Achieved excellent performance metrics:
  - RMSE: 0.0108
  - MAE: 0.0048
  - R²: 0.9809
- Key features driving demand prediction:
  1. Reviews (strong positive correlation)
  2. Average neighborhood price
  3. Price to neighborhood ratio
  4. Host experience (years active)
  5. Room type (entire homes preferred)
  6. Location (neighborhood)
  7. Superhost status

## Simulation Insights

### Location Impact
- Location has a significant impact on demand variability
- Central neighborhoods like Mitte and Friedrichshain show higher demand
- Outer neighborhoods show lower demand even with identical property characteristics
- Location is one of the strongest predictors of rental demand

### Price Impact
- Price shows a non-linear relationship with demand
- Each neighborhood has an optimal price point that maximizes demand
- Setting prices too high or too low can both negatively impact demand
- The demand-price curve typically shows a peak followed by decline

### Property and Room Type
- Room type has a stronger impact on demand than property type
- Entire homes/apartments generally have higher demand than private rooms
- Shared rooms show consistently lower demand across all property types
- Luxury property types (lofts, serviced apartments) show higher demand when offered as entire units

### Host Experience
- Both host experience (years) and superhost status positively impact demand
- Superhosts see higher demand across all experience levels
- The effect of experience on demand is stronger for non-superhosts
- New hosts benefit more from achieving superhost status than experienced hosts

### Price Optimization by Location
- Different neighborhoods have different optimal price points
- Central, high-demand areas can sustain higher prices while maintaining demand
- Price sensitivity varies by neighborhood
- Optimal prices are typically 10-20% below the neighborhood average

## Recommendations for Hosts
- **Location-Based Pricing**: Set prices according to neighborhood-specific optimal points
- **Room Type Selection**: Prioritize entire home/apartment listings when possible
- **Superhost Status**: Work toward achieving and maintaining superhost status
- **Price Testing**: Experiment with pricing around the optimal point for your specific property
- **Experience Building**: Focus on building positive reviews and host experience metrics

## Project Completion
- Model successfully trained and validated
- Simulations completed with actionable insights
- Analysis documented in R Markdown files
- Results visualized and saved to results directory
- Project findings summarized in comprehensive report

## Next Steps
- Potential improvements for future work:
  - Include external data sources (tourism statistics, events calendar)
  - Develop time-series models to account for seasonality
  - Create interactive dashboard for hosts to input property details and receive personalized recommendations
  - Expand analysis to other cities for comparative insights 