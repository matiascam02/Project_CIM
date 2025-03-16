# Airbnb Rental Demand Prediction in Berlin

## Project Overview
This project develops a machine learning model to predict Airbnb rental demand across Berlin's districts. By analyzing location, price, amenities, and host attributes, we provide insights that can help hosts optimize their listings for maximum occupancy.

## Key Findings
- **Location is Critical**: Central districts like Mitte and Friedrichshain-Kreuzberg show 20-40% higher demand
- **Price Sensitivity**: Lower-priced listings attract higher occupancy rates, with €20-50 range showing optimal demand
- **Room Type Matters**: Private rooms often outperform entire apartments in raw demand measures
- **Superhost Status**: Increases demand by approximately 12.5%
- **Essential Amenities**: WiFi is the most impactful amenity, boosting demand by ~15%

## Dataset
The analysis uses the "Airbnb Berlin Price Prediction" dataset, which includes:
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
├── scripts/         # R scripts for data processing and modeling
│   ├── install_packages.R         # Script to install required packages
│   ├── 01_data_preprocessing.R    # Data cleaning and feature engineering
│   ├── 02_model_development.R     # Random Forest model training
│   └── 03_demand_simulation.R     # Simulations for demand factors
├── analysis/        # R markdown files for analysis
│   ├── 01_exploratory_data_analysis.Rmd  # EDA with visualizations
│   ├── 02_modeling.Rmd                   # Model development and evaluation
│   └── 03_simulation.Rmd                 # Scenario simulations
├── models/          # Trained models
├── results/         # Model evaluation metrics and simulation results
├── figs/            # Generated figures and visualizations
└── project_findings.md  # Detailed documentation of findings
```

## Methodology
1. **Exploratory Data Analysis (EDA)**: We examined price distributions, geographic patterns, neighborhood effects, and correlations between features to understand the key factors influencing rental prices and demand.

2. **Feature Engineering**: We created several derived features including:
   - Normalized review counts and availability metrics
   - Price tiers and price-to-neighborhood ratios
   - Distance-based location metrics
   - Host experience indicators

3. **Predictive Modeling**: We developed a Random Forest model to predict rental demand, achieving:
   - R-squared: 0.98
   - RMSE: 0.01
   - MAE: 0.005

4. **Simulations**: We conducted extensive simulations to understand how different factors affect demand:
   - Location-based simulations
   - Price optimization
   - Room type comparisons
   - Superhost status impact
   - Amenity importance analysis

## Installation and Setup

### Prerequisites
- R (version 4.0.0 or higher)
- RStudio (recommended for R Markdown files)

### Setting Up the Environment
1. Clone this repository:
   ```
   git clone https://github.com/matiascam02/Project_CIM.git
   cd Project_CIM
   ```

2. Install required R packages:
   ```
   Rscript scripts/install_packages.R
   ```

### Running the Analysis
1. Data Preprocessing:
   ```
   Rscript scripts/01_data_preprocessing.R
   ```

2. Model Development:
   ```
   Rscript scripts/02_model_development.R
   ```

3. Demand Simulation:
   ```
   Rscript scripts/03_demand_simulation.R
   ```

4. For detailed analysis, open the R Markdown files in RStudio:
   - `analysis/01_exploratory_data_analysis.Rmd`
   - `analysis/02_modeling.Rmd`
   - `analysis/03_simulation.Rmd`

## Recommendations for Hosts
1. Focus on acquiring or converting properties in high-demand central districts
2. Price listings competitively (under €50) for maximum occupancy
3. Consider offering private rooms rather than entire apartments if occupancy rate is the primary goal
4. Invest effort in achieving and maintaining Superhost status
5. Ensure all properties have high-quality WiFi and essential amenities
6. Balance the trade-off between high demand (lower prices) and high margins (higher prices)

## Dependencies
- tidyverse (data manipulation and visualization)
- randomForest (for modeling)
- caret (for model evaluation)
- sf (for spatial analysis)
- knitr and kableExtra (for reporting)
- scales (for formatting)
- leaflet (for interactive maps)
- viridis (for enhanced visualizations)

## Contact
For questions or feedback, please contact:
- GitHub: [matiascam02](https://github.com/matiascam02) 