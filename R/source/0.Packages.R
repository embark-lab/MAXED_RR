load_libraries <- function() {
  # List of packages to load
  libraries <- c("broom",
                 "brms",
                 "car",
                 "cgwtools", 
                 "cowplot", 
                 "dplyr", 
                 "effsize",
                 "embarktools", 
                 "emmeans",
                 "ggplot2", 
                 "grid", 
                 "gridExtra", 
                 "gridSVG", 
                 "haven",
                 "lme4", 
                 "lmerTest",
                 "nlme",
                 "patchwork", 
                 "psych",
                 "sjmisc", 
                 "stringr", 
                 "tidyr")
  

  # Loop through the list of packages
  for (lib in libraries) {
    # Check if the package is already installed
    if (!require(lib, character.only = TRUE, quietly = TRUE)) {
      # If not installed, install the package
      install.packages(lib, dependencies = TRUE)
      # Load the package after installation
      library(lib, character.only = TRUE)
    }
  }
  
  message("All packages loaded successfully!")
}

# Usage
load_libraries()
