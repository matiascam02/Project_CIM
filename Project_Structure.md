# Project Structure

This document explains the organization of the Airbnb Demand Analysis project codebase.

## Directory Structure

```
Project_CIM/
├── analysis/
│   ├── 01_exploratory_data_analysis.Rmd  # EDA analysis and visualizations
│   ├── 02_model_development.Rmd          # Model development analysis
│   ├── 03_demand_simulation.Rmd          # Demand simulation analysis
│   └── results/                          # Analysis-specific results
│
├── data/
│   ├── raw/                              # Original dataset files
│   └── processed/                        # Cleaned and processed data
│
├── models/
│   ├── random_forest_model.rds           # Saved Random Forest model
│   ├── gradient_boosting_model.rds       # Saved Gradient Boosting model
│   └── elastic_net_model.rds             # Saved Elastic Net model
│
├── results/
│   ├── figures/                          # Generated plots and visualizations
│   ├── numeric_simulation_results.csv    # Numeric feature simulation results
│   ├── categorical_simulation_results.csv # Categorical feature simulation results
│   ├── demand_simulation_results.csv     # Combined simulation results
│   └── simulation_explanation.md         # Explanation of simulation approach
│
├── scripts/
│   ├── 01_data_preprocessing.R           # Data cleaning and preparation
│   ├── 02_model_development.R            # Model training and evaluation
│   └── 03_demand_simulation.R            # Demand simulation code
│
├── Presentation.md                       # Presentation slides in markdown
└── Project_Report.md                     # Comprehensive project report
```

## Workflow

The project follows this workflow:

1. **Data Preprocessing**: 
   - Run `scripts/01_data_preprocessing.R` to clean the raw data
   - Output: Processed data files in `data/processed/`

2. **Exploratory Data Analysis**:
   - View `analysis/01_exploratory_data_analysis.Rmd` for data insights
   - Output: Visualizations and insights in `results/figures/`

3. **Model Development**:
   - Run `scripts/02_model_development.R` to train prediction models
   - View `analysis/02_model_development.Rmd` for model analysis
   - Output: Trained models in `models/` directory

4. **Demand Simulation**:
   - Run `scripts/03_demand_simulation.R` to simulate feature effects
   - View `analysis/03_demand_simulation.Rmd` for analysis of simulations
   - Output: Simulation results in `results/` directory

5. **Documentation**:
   - `Project_Report.md`: Comprehensive project documentation
   - `Presentation.md`: Presentation slides for stakeholders

## How to Run the Project

### Prerequisites

The following R packages are required:
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

### Running the Analysis

1. Clone the repository
2. Install required packages: `install.packages(c("tidyverse", "caret", "randomForest", "gbm", "glmnet", "rmarkdown", "knitr", "ggplot2", "gridExtra", "viridis", "kableExtra"))`
3. Execute scripts in numerical order:
   ```R
   Rscript scripts/01_data_preprocessing.R
   Rscript scripts/02_model_development.R
   Rscript scripts/03_demand_simulation.R
   ```
4. Render the R Markdown files to HTML:
   ```R
   rmarkdown::render("analysis/01_exploratory_data_analysis.Rmd")
   rmarkdown::render("analysis/02_model_development.Rmd")
   rmarkdown::render("analysis/03_demand_simulation.Rmd")
   ```
5. Generate the presentation:
   ```R
   rmarkdown::render("Presentation.md", output_format = "powerpoint_presentation")
   ```

## Key Files Explained

- **01_data_preprocessing.R**: Handles data cleaning, outlier removal, feature engineering, and splitting data into training/testing sets.

- **02_model_development.R**: Implements model training with Random Forest, Gradient Boosting, and Elastic Net. Includes cross-validation and hyperparameter tuning.

- **03_demand_simulation.R**: Creates synthetic data to simulate the effect of changing property characteristics on demand.

- **Exploratory R Markdown files**: Provide detailed analysis with visualizations for data understanding, model evaluation, and simulation results.

## Contact

For questions or clarifications about this project, please contact: data-science-team@example.com 