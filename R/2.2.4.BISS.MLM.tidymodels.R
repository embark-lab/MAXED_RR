library(tidyr)
library(dplyr)
library(lme4)
library(emmeans)
library(lmerTest)
library(cgwtools)
library(MuMIn)
library(broom.mixed)

load('results/biss_models.Rdata')
tasks <- unique(biss_models_summary$task)
variables <- unique(biss_models_summary$variable)

metrics <- list()
# Assuming 'affect_models' is a list of lists of model objects
for (var in variables) {
  metrics[[var]] <- list()
  for (task in tasks) {
    # Ensure the model for the task is available and correctly specified
    model <- biss_models[[var]][[task]]
    aic_value <- AIC(model)
    bic_value <- BIC(model)
    log_likelihood <- logLik(model)
    r_squared <- r.squaredGLMM(model)
    # save the metrics
    metrics[[paste(var, task, sep = "_")]] <- data.frame(
      Variable = var,
      Task = task,
      `R2 Marginal` = r_squared[1], 
      `R2 Conditional` = r_squared[2],
      AIC = aic_value, 
      BIC = bic_value,
      Log_Lik = as.numeric(log_likelihood))
  } 
}

# bind the metrics (now in a list with a sublist for each task) into a dataframe
biss_model_fit_indicies <- bind_rows(metrics)

# do the same for ED models
metrics_ed <- list()

for (var in variables) {
  metrics_ed[[var]] <- list()
  for (task in tasks) {
    # Ensure the model for the task is available and correctly specified
    model <- biss_models_ed[[var]][[task]]
    aic_value <- AIC(model)
    bic_value <- BIC(model)
    log_likelihood <- logLik(model)
    r_squared <- r.squaredGLMM(model)
    # save the metrics
    metrics_ed[[paste(var, task, sep = "_")]] <- data.frame(
      Variable = var,
      Task = task,
      `R2 Marginal` = r_squared[1], 
      `R2 Conditional` = r_squared[2],
      AIC = aic_value, 
      BIC = bic_value,
      Log_Lik = as.numeric(log_likelihood))
  } 
}

# bind the metrics (now in a list with a sublist for each task) into a dataframe
biss_model_fit_indicies_ed <- bind_rows(metrics_ed)

# combine the two dataframes -- make a column to indicate whether the model is ED or not
biss_model_fit_indicies$Model <- 'Overall'
biss_model_fit_indicies_ed$Model <- 'ED'

biss_model_fit_indicies <- rbind(biss_model_fit_indicies, biss_model_fit_indicies_ed)
# move 'Model' to the front
biss_model_fit_indices <- biss_model_fit_indicies %>%
  select(Model, everything())

# save model fit indices
save(biss_model_fit_indices, file = 'results/biss_model_fit_indices.Rdata')


