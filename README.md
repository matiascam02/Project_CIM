# Airbnb Rental Demand Prediction in Berlin

## Project Overview
This project develops a machine learning model to predict Airbnb rental demand across Berlin's districts. By analyzing location, price, amenities, and host attributes, we provide insights that can help hosts optimize their listings for maximum occupancy.

## Key Findings
- **Location is Critical**: Central districts like Mitte and Friedrichshain-Kreuzberg show 20-40% higher demand
- **Price Sensitivity**: Demand is highest at moderate price points (€40-60) and decreases as prices increase
- **Room Type Matters**: Private rooms often maintain high demand levels due to their affordability and practicality
- **Superhost Status**: Increases demand by approximately 15-25%, with greater impact for experienced hosts
- **Host Experience**: Each additional year of hosting experience progressively increases predicted demand
- **Price Optimization**: Different neighborhoods have distinct optimal price points for maximizing demand

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
│   └── simulations/ # Detailed simulation results and visualizations
├── figs/            # Generated figures and visualizations
└── README.md        # Project overview and instructions (this file)
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
   - Location-based simulations with neighborhood comparisons
   - Price-demand curve analysis showing optimal price points
   - Room type and property type impact analysis
   - Host experience and superhost status effects
   - Price optimization strategies by neighborhood

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

4. For detailed analysis, open the R Markdown files in RStudio and knit them:
   - `analysis/01_exploratory_data_analysis.Rmd`
   - `analysis/02_modeling.Rmd`
   - `analysis/03_simulation.Rmd`

5. View the rendered analysis with visualizations:
   - `analysis/03_simulation.html` (Full analysis with code and visualizations)

## Recommendations for Hosts
1. **Location Strategy**: Focus on properties in central districts like Mitte, Friedrichshain-Kreuzberg, and Prenzlauer Berg for maximum booking potential.

2. **Strategic Pricing**: Price listings around €40-60 for the best balance between demand and revenue, adjusting based on your specific neighborhood.

3. **Room Type Considerations**: Consider offering private rooms as they maintain good demand levels while requiring less investment than entire apartments.

4. **Superhost Investment**: Prioritize achieving and maintaining Superhost status, as it significantly increases predicted demand (15-25%).

5. **Experience Building**: Maintain consistent hosting activity, as increased experience on the platform progressively improves demand.

6. **Neighborhood-Specific Pricing**: Adjust pricing strategies based on your specific location - central neighborhoods can command higher prices while maintaining demand.

## Dependencies
- tidyverse (data manipulation and visualization)
- randomForest (for modeling)
- caret (for model evaluation)
- sf (for spatial analysis)
- knitr and kableExtra (for reporting)
- scales (for formatting)
- viridis (for enhanced visualizations)

## Contact
For questions or feedback, please contact:
- GitHub: [matiascam02](https://github.com/matiascam02) 