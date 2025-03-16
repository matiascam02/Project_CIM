# Getting Started with the Airbnb Berlin Rental Demand Prediction Project

This guide will help you set up the project environment and download the necessary data to begin your analysis.

## 1. Prerequisites

Before getting started, make sure you have the following installed:

- R (version 4.0.0 or higher) - [Download R](https://cloud.r-project.org/)
- RStudio (recommended) - [Download RStudio](https://posit.co/download/rstudio-desktop/)
- Git - [Download Git](https://git-scm.com/downloads)

## 2. Clone the Repository

```bash
git clone <repository-url>
cd Project_CIM
```

## 3. Download the Dataset

The project uses the "Airbnb Berlin Price Prediction" dataset from Kaggle. Follow these steps to download it:

1. Create a Kaggle account if you don't already have one: [Kaggle Sign Up](https://www.kaggle.com/account/login?phase=startRegisterTab)
2. Go to the dataset page: [Airbnb Berlin Price Prediction](https://www.kaggle.com/datasets/thedevastator/airbnb-prices-in-european-cities/data?select=berlin_weekends.csv)
3. Click the "Download" button
4. Once downloaded, extract the files
5. Move the CSV files to the `data/raw/` directory in this project

Alternatively, you can use the Kaggle API to download the dataset:

```bash
# Install the Kaggle API client
pip install kaggle

# Configure your Kaggle API credentials
# (You need to download kaggle.json from your Kaggle account settings)
mkdir -p ~/.kaggle
cp path/to/kaggle.json ~/.kaggle/
chmod 600 ~/.kaggle/kaggle.json

# Download the dataset
kaggle datasets download thedevastator/airbnb-prices-in-european-cities -p data/raw/
unzip data/raw/airbnb-prices-in-european-cities.zip -d data/raw/
```

## 4. Install Required R Packages

Open R or RStudio and run the following commands to install the necessary packages:

```r
# Install required packages
packages <- c("tidyverse", "lubridate", "sf", "leaflet", "skimr", "corrplot", "GGally", 
              "knitr", "caret", "randomForest", "gbm", "e1071", "glmnet", "pROC", 
              "forecast", "MASS", "gridExtra", "janitor")

# Check which packages are not installed yet
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(new_packages)) install.packages(new_packages)

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))
```

## 5. Project Structure

The project is organized as follows:

- `data/raw/`: Contains the original, unmodified data files
- `data/processed/`: Will contain cleaned and preprocessed data
- `scripts/`: Contains R scripts for data processing
- `analysis/`: Contains R Markdown files for analysis
- `figs/`: Will contain generated figures and visualizations

## 6. Data Preprocessing

Run the preprocessing script to clean and prepare the data:

```r
source("scripts/01_data_preprocessing.R")
```

## 7. Exploratory Data Analysis

Open and run the EDA R Markdown file:

```
analysis/01_exploratory_data_analysis.Rmd
```

## 8. Model Development and Simulation

After completing the EDA, you can proceed to the modeling and simulation analyses:

```
analysis/02_modeling.Rmd
analysis/03_simulation.Rmd
```

## 9. Project Findings

The `project_findings.md` file contains a running log of discoveries, decisions, and progress throughout the project. Refer to this document for insights and update it as you make new findings.

## Troubleshooting

- **Missing packages**: If you encounter errors about missing packages, install them using `install.packages("package_name")`
- **Data loading issues**: Make sure the raw data files are in the correct location (data/raw/) with the expected filenames
- **Spatial analysis errors**: Some functions may require additional system dependencies for spatial analysis; refer to the sf package documentation for your operating system

## Contact

If you have any questions or run into issues, please open an issue in the GitHub repository or contact the project maintainers. 