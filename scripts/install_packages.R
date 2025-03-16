#!/usr/bin/env Rscript

# ==============================================================================
# install_packages.R
#
# This script installs all required R packages for the Airbnb Berlin rental 
# demand prediction project. It checks for existing packages and installs
# any that are missing.
# ==============================================================================

# List of required packages
required_packages <- c(
  # Core packages
  "tidyverse",    # For data manipulation and visualization
  "janitor",      # For data cleaning
  "lubridate",    # For date handling
  
  # Visualization
  "ggplot2",      # For creating plots
  "viridis",      # For color palettes
  "gridExtra",    # For arranging multiple plots
  "scales",       # For formatting plot scales
  "corrplot",     # For correlation plots
  "GGally",       # For pair plots
  
  # Spatial analysis
  "sf",           # For spatial data handling
  "leaflet",      # For interactive maps
  
  # Data exploration
  "skimr",        # For data summaries
  "knitr",        # For report generation
  "rmarkdown",    # For creating R Markdown documents
  
  # Modeling
  "caret",        # For model training and evaluation
  "randomForest", # For Random Forest models
  "xgboost",      # For Gradient Boosting models
  "glmnet",       # For regularized regression
  "doParallel",   # For parallel processing
  
  # Time series (if needed)
  "forecast",     # For time series forecasting
  "tseries"       # For time series analysis
)

# Function to check and install packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    message(paste("Installing package:", package))
    install.packages(package, dependencies = TRUE, repos = "https://cloud.r-project.org")
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      warning(paste("Failed to install package:", package))
      return(FALSE)
    }
  } else {
    message(paste("Package already installed:", package))
  }
  return(TRUE)
}

# Install packages
message("Checking and installing required packages...")
results <- sapply(required_packages, install_if_missing)

# Report results
if (all(results)) {
  message("All packages successfully installed or already available.")
} else {
  missing_packages <- required_packages[!results]
  warning(paste("Failed to install the following packages:", 
                paste(missing_packages, collapse = ", ")))
  
  # Special note for sf package which often requires system dependencies
  if ("sf" %in% missing_packages) {
    message("
    Note: The 'sf' package requires additional system dependencies:
    - On Ubuntu/Debian: sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev
    - On macOS with Homebrew: brew install gdal
    - On Windows: No additional steps needed, but installation may take longer.
    ")
  }
}

# Verify that key packages can be loaded
message("\nVerifying package loading:")
for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    detach(paste0("package:", pkg), character.only = TRUE, unload = TRUE)
    message(paste("Successfully loaded:", pkg))
  }, error = function(e) {
    warning(paste("Failed to load package:", pkg))
  })
}

message("\nPackage installation process complete.") 