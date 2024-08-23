scripts <- list.files("R/scripts", full.names = TRUE)

# Run each script with error catching
for (script in scripts) {
  tryCatch(
    {
      source(script)
      message(paste("Successfully sourced:", script))
    },
    error = function(e) {
      message(paste("Error in script:", script))
      message("Error message:", e$message)
    }
  )
}
