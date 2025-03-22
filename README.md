# Airbnb Demand Analysis and Simulation

![Airbnb Berlin](https://images.unsplash.com/photo-1560969184-10fe8719e047?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80)

## Overview

This project analyzes Airbnb listings data from Berlin to identify key factors that drive demand. It includes exploratory data analysis, model development with regularization techniques to prevent overfitting, and demand simulations to demonstrate the expected relationships between property features and demand.

## Key Features

- **Data Preprocessing**: Clean and prepare Airbnb data for analysis
- **Exploratory Data Analysis**: Visualize and understand data patterns
- **Model Development**: Build and evaluate predictive models
- **Demand Simulation**: Simulate the impact of property features on demand
- **Actionable Insights**: Provide recommendations for Airbnb hosts

## Project Structure

- `analysis/`: R Markdown files with detailed analysis
- `data/`: Raw and processed datasets
- `models/`: Trained prediction models
- `results/`: Output files, visualizations, and simulation results
- `scripts/`: R scripts for data processing, modeling, and simulation

For a complete explanation of the project structure, see [Project_Structure.md](Project_Structure.md).

## Documentation

- [Project Report](Project_Report.md): Comprehensive project documentation
- [Presentation](Presentation.md): Presentation slides for stakeholders
- [Simulation Explanation](results/simulation_explanation.md): Details about the simulation approach

## Key Findings

1. **Ratings Matter**: Higher overall, cleanliness, and communication ratings significantly increase demand
2. **Reviews Impact**: Properties with more reviews see higher demand (with diminishing returns)
3. **Price Sensitivity**: Higher prices generally reduce demand following a negative exponential curve
4. **Room Type Differences**: Room type has substantial impact on booking patterns
5. **Superhost Advantage**: Superhost status is associated with approximately 20% higher demand
6. **Property Type Variations**: Different property types attract varying levels of demand

## How to Run

### Prerequisites

Required R packages:
- tidyverse
- caret
- randomForest
- gbm
- glmnet
- rmarkdown
- knitr
- ggplot2
- gridExtra
- viridis
- kableExtra

### Installation & Execution

1. Clone this repository
2. Install required packages
3. Run scripts in numerical order (see [Project_Structure.md](Project_Structure.md) for details)

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Data provided by Inside Airbnb
- Research methodology inspired by AirDNA market research 