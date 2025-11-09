# University of Chicago
# Development Impact Lab

# By: Alejandro Ortiz

# Date: 2025/11/09

#> Goal: Complete data task instructions.



# Environment setup and dependencies                                        ----


# Clean - R's environment
# .rs.restartR()
cat("\f")
graphics.off()
remove(list = ls())
gc(full = T)


# Print working directory
getwd()


# Set options
# options(java.parameters = "-Xmx8000m")
options(max.print = 200)


# Update and load packages
# update.packages(ask = F)

# Stata import
library(haven)

# FE regressions
library(modelsummary)
library(fixest)

# Plotting
library(patchwork)
library(scales)
library(maps)

# Core
library(tidyverse)
library(data.table)


# Get environment variables
cfg <- config::get()


# Source project functions
source("Scripts/Functions/.source_all_functions.R", local = environment())




# Import data sets                                                          ----

