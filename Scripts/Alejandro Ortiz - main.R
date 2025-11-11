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

school_bl     <- fread(str_glue("{cfg$raw_data}/schools.csv"))
student_bl    <- fread(str_glue("{cfg$raw_data}/student_baseline.csv"))

school_visits <- fread(str_glue("{cfg$raw_data}/school_visits_log.csv"))

student_followups <- fread(str_glue("{cfg$raw_data}/student_follow_ups.csv"))


# Helper functions                                                          ----


# Balancing test helper
bl_balance_helper <- function(dt, discrete_vars, continuous_vars) {

  # Copy for safety
  dt <- copy(dt)

  # Create results list
  results <- list()


  ## Formula for joint testing
  # Add factor around discrete vars
  factored_discrete_vars <- paste0("factor(", discrete_vars, ")")

  all_vars <- c(factored_discrete_vars, continuous_vars)

  balance_formula <- as.formula(
    paste("is_treated ~", paste(all_vars, collapse = " + "))
  )

  # Run the regression and check the overall model F-test
  model_full <- feols(balance_formula, data = dt, vcov = "HC1")


  # Wald test to confirm the F-test results
  obs_removed_vector <- model_full[["obs_selection"]][["obsRemoved"]]

  result_dt <- if (!is.null(obs_removed_vector)) {
    # If it is NOT null, subset dt using the vector
    sample_data <- dt[obs_removed_vector]
  } else {
    # If it IS null, return the entire original dt
    sample_data <- copy(dt)
  }


  # Restricted model
  model_restricted <- feols(
    is_treated ~ 1,
    data = sample_data
  )


  # Wald test
  wald_test_result <- lmtest::waldtest(
    model_full, model_restricted,
    vcov = function(x) sandwich::vcovHC(x, type = "HC1")
  )


  # T-tests

  # Initialize an empty list to store T test results
  t_results <- list()


  for (var in continuous_vars) {

    # Create the formula for the t-test
    test_formula <- as.formula(paste(var, "~ is_treated"))

    # Run the t-test on the full dataset
    test_result <- t.test(test_formula, data = dt)

    # Store the extracted results in the list
    t_results[[var]] <- data.table(
      variable = var,
      control_mean = test_result$estimate[1],
      treatment_mean = test_result$estimate[2],
      p.value = test_result$p.value
    )
  }


  # Combine all T-test results
  t_test_results <- rbindlist(t_results)


  # Chi-squared tests for discrete vars

  # List to store Chi-squared test results
  chi_results <- list()

  for (var in discrete_vars) {

    # Create the contingency table
    contingency_table <- table(dt$is_treated, dt[[var]])

    # Run the Chi-squared test
    test_result <- chisq.test(contingency_table)

    # Store the extracted results in the list
    chi_results[[var]] <- data.table(
      variable = var,
      chi_squared_statistic = test_result$statistic,
      p.value = test_result$p.value
    )
  }

  # Combine Chi-squared test results
  chi_squared_results <- rbindlist(chi_results)


  return(list(
    full_model = model_full,
    wald_test = wald_test_result,
    t_tests = t_test_results,
    chi_squared_tests = chi_squared_results
  ))

}


# Coefficient plotting helper
plot_coefs <- function(model_list_parent, list_name, include_missing = F) {

  # Set the target models based on the 'Missing' flag
  all_names <- names(model_list_parent[[list_name]])

  if (include_missing) {
    target_model_names <- all_names[str_detect(all_names, "Missing")]
  } else {
    target_model_names <- all_names[!str_detect(all_names, "Missing")]
  }

  # --- 1. Data Extraction ---
  all_estimates <- lapply(target_model_names, function(model_name) {

    model_obj <- model_list_parent[[list_name]][[model_name]]

    if (!"is_treated" %in% names(coef(model_obj))) {
      warning(paste("Model", model_name, "does not have 'is_treated' coefficient."))
      return(NULL)
    }


    # Extract coefficient and SE
    est <- coef(model_obj)["is_treated"]
    se <- se(model_obj)["is_treated"]


    # Calculate 95% CIs
    data.table(
      Model_Name = model_name,
      Estimate = est,
      CI_Lower = est - 1.96 * se,
      CI_Upper = est + 1.96 * se
    )
  })

  plot_data <- rbindlist(all_estimates)


  # --- 2. Data Shaping ---
  # Extract Time ("3y", "5y") and Category
  plot_data[, Time := str_sub(Model_Name, 1, 2)]


  # If 'Missing' is included, the Category needs to handle "Missing " at the start
  if (include_missing) {
    plot_data[, Category := str_sub(Model_Name, 4)]
    plot_data[, Category := str_replace(Category, "Missing ", "")]
  } else {
    plot_data[, Category := str_sub(Model_Name, 4)]
  }

  # Factorize Time for X-axis ordering and labeling
  plot_data[, Time := factor(Time,
                             levels = c("3y", "5y"),
                             labels = c("Endline", "2y Follow up"),
                             ordered = T)]

  # --- 3. Plotting ---

  # Define the width for dodging
  pd <- position_dodge(width = 0.2)

  ggplot(plot_data, aes(x = Time, y = Estimate, color = Category)) +

    # Add the dotted line connecting the time points
    geom_line(aes(group = Category),
              linetype = "dotted",
              linewidth = 0.6,
              alpha = 0.5,
              position = pd) +

    # Add points and error bars with dodging
    geom_point(size = 3, position = pd) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                  width = 0.1, linewidth = 0.8, position = pd) +

    # Add a dashed line at zero for reference
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +

    labs(
      y = "Estimated Treatment Effect Coefficient",
      x = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}



# Data cleaning and preparation                                             ----

# Summary of data
summary(school_bl)
summary(student_bl)


# Clean the school bl data
school_bl <- school_bl %>%

  # Create dummy vars
  mutate(
    is_treated   = treatment,
    is_urban     = ifelse(location == 1, yes = 1, no = 0),

    # Replace -99 for NAs
    across(
      all_of(c("av_teacher_age", "av_student_score", "n_latrines")),
      ~ na_if(., -99)
    )
  )


# Are there any schools with more female teachers than total teachers?
school_bl[n_teachers_fem > n_teachers, .N]
# No


# Clean the student baseline data
student_bl <- student_bl %>%

  # Replace -99 for NAs
  mutate(
    # Replace 9999 with NA
    yob = na_if(yob, 9999),

    # Create dummy var
    is_female = ifelse(sex == 2, yes = 1, no = 0),

    # Create approximate age variable
    age_bl = 2010 - yob
  )


# New summaries
summary(school_bl)
summary(student_bl)


# Take a closer look at student ages
student_bl[, age_bl] %>% hist
# The youngest student is 18 years old and the oldest is 25. These are not G6
# ages. G6 students should be around 13 years old. The spread of ages is also
# very strange 7 year difference for the same grade is unusual, and typically
# only something you see if adult education grades are included. From what I
# understand from the study description this is not the case.

# There isn't really a remedy for this, I can't assume that there was a fixed
# measurement error in the yob var, because I think that is too unlikely. And I
# can't drop students older than ~15 y/o because they are all older than 15.

student_bl[age_bl >= 24, .N] / student_bl[!is.na(age_bl), .N]
# Students 24 or older are 3% of the sample of students with non-missing age.

# I can drop students 24 or older and not bias the sample too much.
student_bl <- student_bl[age_bl < 24]


# For follow up data
# Summary of data
summary(school_visits)
summary(student_followups)


# Clean the school visits data


# Organize the data
setcolorder(school_visits, c("school_id", "year", "month", "day"))
setorder(school_visits)


# Create variable to indicate endline/followup
school_visits[, visit_n := .SD[, .I], by = school_id]


# Does every school have 2 visits?
school_visits[, .N, by = school_id][N != 2]
# Yes


# Create endline and endlne dummies
school_visits <- school_visits %>%

  # Create dummies
  mutate(
    is_endline   = ifelse(visit_n == 1, yes = 1, no = 0),
    is_followup  = ifelse(visit_n == 2, yes = 1, no = 0)
  )


# Check the distributon of follow up years in the student follow up data
student_followups[, .N, year]


# Clean the student follow up data
student_followups <- student_followups %>%

  mutate(

    # Replace -99 for NAs
    across(
      all_of(c("died", "married", "children", "pregnant")),
      ~ na_if(., -99)
    ),

    # Endline and followup dummies
    is_endline = ifelse(year == 3, yes = 1, no = 0),
    is_endline = ifelse(year == 5, yes = 1, no = 0),


    # Dummies for outcome attriton
    is_na_dropout = ifelse(is.na(dropout)  , yes = 1, no = 0),
    is_na_pregnant = ifelse(is.na(pregnant), yes = 1, no = 0),
    is_na_married = ifelse(is.na(married)  , yes = 1, no = 0)
  )


# New summaries
summary(school_visits)
summary(student_followups)




# Data merge                                                                ----

# Add student treatment variable to student baseline data
student_bl <- student_bl %>%


  # first we add the school id to the student baseline
  left_join(
    student_followups[, .(school_id, student_id)] %>% unique,
    by = "student_id"
  ) %>%

  # then we add the treatment variable from the school baseline
  left_join(
    school_bl[, .(school_id, is_treated)],
    by = "school_id"
  )


# Add info to stundent follow ups
student_followups <- student_followups %>%

  # Add treatment variable to student follow up data
  left_join(
    school_bl[, .(school_id, is_treated)],
    by = "school_id"
  ) %>%

  # Add baseline variables to student follow up data
  left_join(
    student_bl[, .(student_id, yob, age_bl, is_female)],
    by = "student_id"
  )



# Are treatment groups balanced for schools at baseline?                    ----


# Stratum variable makes me think that randomization could have been stratified
# but the description of the experiment doesn't mention this. I will first test
# without strata.
balance_schools <- bl_balance_helper(
  dt = school_bl,
  discrete_vars = c("is_urban", "female_head_teacher", "district"),
  continuous_vars = c("n_teachers", "n_teachers_fem", "n_students_fem",
                      "n_students_male", "n_schools_2km", "av_teacher_age",
                      "av_student_score", "n_latrines")
)


balance_schools$full_model %>% fitstat(type = "f")
# F-statistic is 1.005 and p-value is 0.44. We fail to reject the null that all
# coefficients are jointly equal to zero. Treatment and control schools appear
# balanced at baseline.

# This is a LPM which is Heteroskedastic and can have out-of-bounds predictions.
# This usually results in understated p-value for the F-stat. The fact that even
# under these circumstances the p-value is so high makes me more confident that
# the groups are balanced.


# Wald test to confirm the F-test results
balance_schools$wald_test
# 16% pval means that groups seem to be balanced at baseline.


# T-tests
balance_schools$t_tests
# All p-values are above 12%.Most p-values above 43%. number of girls and
# avg student score have the lowest possible p-values (13% and 19%) which
# is slightly concerning.


# Chi-squared tests
balance_schools$chi_squared_tests
# All variables have very high p-values (above 36%) indicating balance between
# treatment and control groups.




# Are treatment groups balanced for students at baseline?                   ----


balance_students <- bl_balance_helper(
  dt = student_bl,
  discrete_vars = c("is_female"),
  continuous_vars = c("yob")
)

# Run the regression and check the overall model F-test
balance_students$full_model %>% fitstat(type = "f")
# F-statistic is 1.3178 and p-value is 0.27. No evidence to reject the null that
# all coefficients are jointly equal to zero. Treatment and control students
# appear balanced at baseline.


# Wald test to confirm the F-test results
balance_students$wald_test
# 27% pval means that groups seem to be balanced at baseline.


# T-tests
balance_students$t_tests
# P-value is 20%. No evidence of imbalance in YoB.



# Chi-squared tests
balance_students$chi_squared_tests
# P-value is 45%. No evidence of imbalance


# Given that all tests indicate balance between treatment and control
# groups (at the 10% significance) at baseline I conclude that randomization
# was successful. Even is some p-values are a bit low there is no strong
# evidence of imbalance. at the school or student level.



# Effects of the intervention                                               ----


# list to store regression results
regs <- list()


# Regression to test if the intervetion affected school attendance
regs[["students"]][["3y Attendance"]] <- feols(
  dropout ~ is_treated,
  data = student_followups,
  subset = ~ is_endline == 1,
  cluster = ~ school_id
)

regs[["students"]][["5y Attendance"]] <- feols(
  dropout ~ is_treated,
  data = student_followups,
  subset = ~ is_endline == 0,
  cluster = ~ school_id
)


# School endline data
school_el <- student_followups[
  is_endline == 1,
  .(
    # Dropout rate
    dropout_rate = mean(dropout, na.rm = T),

    # Pregnancy rate
    pregnancy_rate = mean(pregnant[is_female == 1], na.rm = T),
    # Dic says this captures pregnancy since the start of treatment

    # Marriage rate
    marriage_rate = mean(married, na.rm = T),

    # Missing dropout rate
    missing_dropout_rate = mean(is_na_dropout, na.rm = T),

    # Missing pregnancy rate
    missing_pregnancy_rate = mean(is_na_pregnant, na.rm = T),

    # Missing marriage rate
    missing_marriage_rate = mean(is_na_married, na.rm = T),

    # The treatment is constant within a school, so take the first value
    is_treated = is_treated[1]
  ),
  by = school_id
]


# School follow up data
school_fu <- student_followups[
  is_endline == 0,
  .(
    # Dropout rate
    dropout_rate = mean(dropout, na.rm = T),

    # Pregnancy rate
    pregnancy_rate = mean(pregnant[is_female == 1], na.rm = T),
    # Dic says this captures pregnancy since the start of treatment

    # Marriage rate
    marriage_rate = mean(married, na.rm = T),

    # Missing dropout rate
    missing_dropout_rate = mean(is_na_dropout, na.rm = T),

    # Missing pregnancy rate
    missing_pregnancy_rate = mean(is_na_pregnant, na.rm = T),

    # Missing marriage rate
    missing_marriage_rate = mean(is_na_married, na.rm = T),

    # The treatment is constant within a school, so take the first value
    is_treated = is_treated[1]
  ),
  by = school_id
]


# Regression to test if the intervetion affected school-level outcomes
regs[["schools"]][["3y Dropout Rate"]] <- feols(
  dropout_rate ~ is_treated,
  data = school_el,
  cluster = ~ school_id
)

regs[["schools"]][["3y Pregnancy Rate"]] <- feols(
  pregnancy_rate ~ is_treated,
  data = school_el,
  cluster = ~ school_id
)


regs[["schools"]][["3y Marriage Rate"]] <- feols(
  marriage_rate ~ is_treated,
  data = school_el,
  cluster = ~ school_id
)

regs[["schools"]][["3y Missing Dropout Rate"]] <- feols(
  missing_dropout_rate ~ is_treated,
  data = school_el,
  cluster = ~ school_id
)

regs[["schools"]][["3y Missing Pregnancy Rate"]] <- feols(
  missing_pregnancy_rate ~ is_treated,
  data = school_el,
  cluster = ~ school_id
)

regs[["schools"]][["3y Missing Marriage Rate"]] <- feols(
  missing_marriage_rate ~ is_treated,
  data = school_el,
  cluster = ~ school_id
)


# 5 year regressions
regs[["schools"]][["5y Dropout Rate"]] <- feols(
  dropout_rate ~ is_treated,
  data = school_fu,
  cluster = ~ school_id
)

regs[["schools"]][["5y Pregnancy Rate"]] <- feols(
  pregnancy_rate ~ is_treated,
  data = school_fu,
  cluster = ~ school_id
)

regs[["schools"]][["5y Marriage Rate"]] <- feols(
  marriage_rate ~ is_treated,
  data = school_fu,
  vcov = ~ school_id
)


regs[["schools"]][["5y Missing Dropout Rate"]] <- feols(
  missing_dropout_rate ~ is_treated,
  data = school_fu,
  cluster = ~ school_id
)


regs[["schools"]][["5y Missing Pregnancy Rate"]] <- feols(
  missing_pregnancy_rate ~ is_treated,
  data = school_fu,
  cluster = ~ school_id
)


regs[["schools"]][["5y Missing Marriage Rate"]] <- feols(
  missing_marriage_rate ~ is_treated,
  data = school_fu,
  cluster = ~ school_id
)




# Effects on school evasion for girls and boys                              ----

# 3y school evasion of boys vs girls
regs[["students"]][["3y Dropout by gender"]] <- feols(
  dropout ~ is_treated * is_female,
  data = student_followups,
  subset = ~ is_endline == 1,
  cluster = ~ school_id
)


# 5y school evasion of boys vs girls
regs[["students"]][["5y Dropout by gender"]] <- feols(
  dropout ~ is_treated * is_female,
  data = student_followups,
  subset = ~ is_endline == 0,
  cluster = ~ school_id
)



# Plot of regression results                                                ----


# Normal outcomes
plot_coefs(
  model_list_parent = regs,
  list_name = "schools"
)


ggsave(
  filename = "Output/main_outcomes_plot.png",
  width = 9,
  height = 5
)


# This call uses the new function to plot the "Missing" variables
plot_coefs(
  model_list_parent = regs,
  list_name = "schools",
  include_missing = T
)


ggsave(
  filename = "Output/missing_outcomes_plot.png",
  width = 9,
  height = 5
)


# Save results                                                              ----

save(regs, balance_schools, balance_students,
     file = "Output/task_results.RData")
