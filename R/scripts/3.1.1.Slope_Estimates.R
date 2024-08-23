# load affect data
source("R/source/0.packages.R")
load("data/Affect/MAXED_Affect.RData")

tasks <- unique(Affect$task)
variables <- unique(Affect$variable)
participants <- unique(Affect$id)

Affect <- Affect |> 
  filter(condition == "Exercise", !is.na(value), !is.na(time))


# Predefine the results list for efficiency
results <- list()

# Loop through combinations
for (i in participants) {
  for (j in tasks) {
    for (k in variables) {
      subset <- Affect[Affect$id == i & Affect$task == j & Affect$variable == k,]
      if (nrow(subset) > 1) {
        model <- lm(value ~ time, data = subset)
        if (!is.null(model$coefficients[2])) { # Check if slope coefficient exists
          results[[length(results) + 1]] <- data.frame(id = i, task = j, variable = k, slope = model$coefficients["time"])
        }
      }
    }
  }
}

# Combine the results list into a data frame
affect_slope_estimates <- do.call(rbind, results)


# save affect slope estimates
save(affect_slope_estimates, file = "Data/Affect/Affect_Slope_Estimates.RData")


# Load BISS data
load("data/BISS/biss_data.RData")
tasks <- unique(BISS$task)
variables <- unique(BISS$variable)
participants <- unique(BISS$id)
# Correct filtering for NA values
BISS <- BISS %>%
  filter(condition== 'Exercise', !is.na(value), !is.na(time))

# Ensure 'value' and 'time' are numeric for regression
BISS$value <- as.numeric(as.character(BISS$value))
BISS$time <- as.numeric(as.character(BISS$time))

# Predefine the results list for efficiency
results <- list()

# Loop through combinations
for (i in participants) {
  for (j in tasks) {
    for (k in variables) {
      subset <- BISS[BISS$id == i & BISS$task == j & BISS$variable == k,]
      if (nrow(subset) > 1) {
        model <- lm(value ~ time, data = subset)
        if (!is.null(model$coefficients[2])) { # Check if slope coefficient exists
          results[[length(results) + 1]] <- data.frame(id = i, task = j, variable = k, slope = model$coefficients["time"])
        }
      }
    }
  }
}

# Combine the results list into a data frame
biss_slope_estimates <- do.call(rbind, results)

save(biss_slope_estimates, file = "Data/BISS/BISS_Slope_Estimates.RData")

