# read all of the scripts in the scripts/folder
scripts <- list.files("R/scripts", full.names = TRUE)

# Run each script
for (script in scripts) {
  source(script)
}
