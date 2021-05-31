library(pacman)

p_load(tidyverse, rio, janitor)

strong_data_orig <- rio::import("data_in/strong.csv") %>% 
  clean_names() %>% 
  select(date, workout_name, exercise_name, set_order, weight, reps)

strong_data_master <- strong_data_orig %>% 
  filter(exercise_name == "Squat (Barbell)" | exercise_name == "Deadlift (Barbell)" | exercise_name == "Overhead Press (Barbell)" | exercise_name == "Bench Press (Barbell)") %>% 
  glimpse()

# Plot rep maxes over time, highlight best --------------------------------

rep_maxes <- strong_data_master %>% 
  mutate(onerm = `1rm_calc`(weight, reps),
         date = as.Date(date),
         exercise_name = factor(exercise_name, levels = c("Deadlift (Barbell)", "Squat (Barbell)", "Bench Press (Barbell)", "Overhead Press (Barbell)"))) %>% 
  group_by(date, exercise_name) %>% 
  summarise(rm = max(onerm))

best_rep_maxes <- rep_maxes %>% 
  group_by(exercise_name) %>% 
  summarise(rm = max(rm)) %>% 
  mutate(label = rm)

plot_rep_maxes <- rep_maxes %>% 
  left_join(best_rep_maxes, by = c("exercise_name", "rm"))


plot_rep_maxes %>% 
  ggplot(aes(date, rm)) +
  geom_line() +
  geom_label(aes(label = label)) +
  facet_wrap(~ exercise_name, ncol = 4)


# Plot volume per session over time ---------------------------------------

volume <- strong_data_master %>% 
  mutate(vol = weight * reps,
         date = as.Date(date),
         exercise_name = factor(exercise_name, levels = c("Deadlift (Barbell)", "Squat (Barbell)", "Bench Press (Barbell)", "Overhead Press (Barbell)"))) %>% 
  group_by(date, exercise_name) %>% 
  summarise(volume = sum(vol))

largest_volume <- volume %>% 
  group_by(exercise_name) %>% 
  summarise(volume = max(volume)) %>% 
  mutate(label = volume)

plot_volume <- volume %>% 
  left_join(largest_volume, by = c("exercise_name", "volume"))

plot_volume %>% 
  ggplot(aes(date, volume)) +
  geom_line() +
  geom_label(aes(label = label)) +
  facet_wrap(~ exercise_name, ncol = 4)
