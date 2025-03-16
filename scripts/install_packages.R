#!/usr/bin/env Rscript

# ==============================================================================
# install_packages.R
#
# This script installs all required packages for the Airbnb Berlin rental 
# demand prediction project.
# ==============================================================================

# Print message
cat("Installing required packages for the Airbnb Berlin Rental Demand Prediction project...\n")

# Required packages
packages <- c(
  # Core packages
  "tidyverse",   # For data manipulation and visualization
  "lubridate",   # For date manipulation
  "janitor",     # For data cleaning
  
  # Visualization
  "ggplot2",     # For visualization (included in tidyverse, but listed for clarity)
  "corrplot",    # For correlation plots
  "GGally",      # For ggpairs plots
  "gridExtra",   # For arranging multiple plots
  
  # Spatial analysis
  "sf",          # For spatial data
  "leaflet",     # For interactive maps
  
  # Data exploration and reporting
  "skimr",       # For data summarization
  "knitr",       # For R Markdown rendering
  "rmarkdown",   # For R Markdown
  
  # Modeling
  "caret",       # For model training and evaluation
  "randomForest", # For random forest models
  "gbm",         # For gradient boosting
  "glmnet",      # For regularized regression
  "e1071",       # For SVM
  "pROC",        # For ROC curves
  
  # Time series and simulation
  "forecast",    # For time series forecasting
  "MASS"         # For statistical functions
)

# Check which packages are already installed
installed_packages <- rownames(installed.packages())
packages_to_install <- packages[!packages %in% installed_packages]

# Install missing packages
if (length(packages_to_install) > 0) {
  cat("Installing", length(packages_to_install), "packages:\n")
  cat(paste(packages_to_install, collapse = ", "), "\n\n")
  
  for (pkg in packages_to_install) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
} else {
  cat("All required packages are already installed.\n")
}

# Verify installation
installed_packages <- rownames(installed.packages())
missing_packages <- packages[!packages %in% installed_packages]

if (length(missing_packages) > 0) {
  cat("\nWARNING: The following packages could not be installed:\n")
  cat(paste(missing_packages, collapse = ", "), "\n")
  cat("Please install them manually using install.packages() or troubleshoot any installation issues.\n")
} else {
  cat("\nSuccess! All required packages are now installed.\n")
  
  # Load all packages to verify they work
  cat("\nLoading packages to verify installation...\n")
  for (pkg in packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
      cat(pkg, "loaded successfully.\n")
    }, error = function(e) {
      cat("Error loading", pkg, ":", e$message, "\n")
    })
  }
  
  cat("\nSetup complete! You're ready to start analyzing Airbnb data in Berlin.\n")
}

# Check for system dependencies for spatial packages
if ("sf" %in% packages) {
  sf_status <- tryCatch({
    library(sf)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  if (!sf_status) {
    cat("\nNOTE: The 'sf' package requires additional system dependencies:\n")
    cat("- On Ubuntu/Debian: sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev\n")
    cat("- On CentOS/RHEL: sudo yum install udunits2-devel gdal-devel geos-devel proj-devel\n")
    cat("- On macOS (with Homebrew): brew install udunits gdal geos proj\n")
    cat("- On Windows: No additional steps required, dependencies are included in the binary package\n")
  }
} 