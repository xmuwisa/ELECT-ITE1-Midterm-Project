# check_setup.R
# This script checks the R installation and verifies the presence of required packages

# Display R version information
cat("R Version Information:\n")
print(R.version)
cat("\n")

# Display session information
cat("Session Information:\n")
print(sessionInfo())
cat("\n")

# List of required packages
required_packages <- c("tidyverse", "janitor", "lubridate", "skimr", "corrplot", "car", "rmarkdown", "knitr")

# Check for missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Report on package installation status
if(length(missing_packages) == 0) {
  cat("All required packages are installed.\n")
} else {
  cat("The following packages are missing:\n")
  print(missing_packages)
  cat("Please install the missing packages using install.packages().\n")
}
