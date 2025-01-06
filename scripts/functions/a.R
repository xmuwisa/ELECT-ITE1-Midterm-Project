# Function to display R version and session information
display_r_info <- function() {
  cat("R Version Information:\n")
  print(R.version)
  cat("\n")
  
  cat("Session Information:\n")
  print(sessionInfo())
  cat("\n")
}

# Function to check and install missing packages
check_packages <- function(required_packages) {
  # Check for missing packages
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  
  if (length(missing_packages) == 0) {
    cat("All required packages are installed.\n")
  } else {
    cat("The following packages are missing:\n")
    print(missing_packages)
  }
}

# Function to do both: display version/session and check/install packages
check_r_environment <- function(required_packages) {
  # Display R version and session information
  display_r_info()
  
  # Check and install required packages
  check_packages(required_packages)
  
  cat("\nAll tasks completed.\n")
}