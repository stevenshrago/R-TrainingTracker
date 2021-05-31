# Load packages -----------------------------------------------------------
library(pacman)

p_load(tidyverse)

source("functions/R-TrainingTrackerFunctions.R")


# Set 1RM for each lift ---------------------------------------------------

deadlift_1rm <- 185
squat_1rm <- 155
bench_1rm <- 130
press_1rm <- 77.5

# Calculate TM (training max) ---------------------------------------------

deadlift_TM <- training_max(deadlift_1rm)
squat_TM <- training_max(squat_1rm)
bench_TM <- training_max(bench_1rm)
press_TM <- training_max(press_1rm)


todays_plan <- build_plan(w3, press_TM) %>% 
  mutate(date = Sys.Date(),
         actual_reps = NA_integer_) %>%
  relocate(date) %>% 
  print()


