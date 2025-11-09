# This script sources all functions

## Source all functions:

# Lambda function to prevent persistent object in the environment
(\() {


  # Create a new environment for project functions
  p_funcs <- new.env()


  # Object list in new environment
  p_funcs$obj <- list()


  # Get the names of all R scripts in the directory
  all_scripts <- list.files(path = "Scripts/Functions/", pattern = "\\.R$",
                            full.names = T)


  # Get the name of the currently running script
  current_script <- ".source_all_functions.R"


  # Remove the currently running script from the list of scripts to be sourced
  scripts_to_source <- all_scripts[all_scripts != current_script]


  # Source each script in the 'scripts_to_source' vector using a for loop
  for (script in scripts_to_source) {
    sys.source(script, envir = p_funcs)
  }


  # Assign the p_funcs environment to the global environment
  assign("p_funcs", p_funcs, envir = .GlobalEnv)
})()
