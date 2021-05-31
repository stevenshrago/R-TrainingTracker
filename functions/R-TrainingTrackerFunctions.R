### Training tracker functions

# Generic round to a specific numnber function
round_any = function(x, accuracy, f = ceiling){
  f(x / accuracy) * accuracy
}


# Calculate the Training Max (TM) for 531 based on 90% of 1RM
training_max <- function(lift){
  round_any(lift*0.9 , 2.5)
}


# Build a weekly plan based on a lift and a weekly program template
build_plan <-  function(wk, lft) {
  
  lift <- str_extract(deparse(substitute(lft)), ".*(?=\\_)")
  
  main_lifts <- wk %>% 
    mutate(tm = lft, 
           weight = round_any(tm * perc, 2.5),
           lift = lift) %>%
    select(lift, weight, reps)
  
  wu_lifts <- wup %>% 
    mutate(st_weight = first(main_lifts$weight),
           weight = round_any(st_weight * perc, 2.5),
           lift = lift) %>% 
    select(lift, weight, reps = wreps)
  
  bind_rows(wu_lifts, main_lifts)
  
}


# Calculate projected 1RM based on weights and reps
`1rm_calc` <- function(wt, reps){
  floor(wt/(1.0278 - (0.0278 * reps)))
}


# Calculate number of reps required to beat current 1RM record based on weight and current best projected 1RM

rev_1rm_calc <- function(wt, rm) {
  floor(36.9712 - ((35.9712 * wt)/(rm + 2.5)))
}


# Set weekly percentage progressions 531BBB --------------------------------------

week1 <- c(0.7, 0.8, 0.9, 0.4, 0.4, 0.4, 0.4, 0.4) # 5s progression, BBB
week2 <- c(0.65, 0.75, 0.85, 0.5, 0.5, 0.5, 0.5, 0.5) # 5s progression, BBB
week3 <- c(0.75, 0.85, 0.95, 0.6, 0.6, 0.6, 0.6, 0.6) # 5s progression, BBB
reps <- c(5, 5, 5, 10, 10, 10, 10, 10)
wreps <- c(5, 3, 2, 1, 1)
warm_up <- c(.4, .6, 0.7,.85, .95)

w1 <- tibble(perc = week1, reps)
w2 <- tibble(perc = week2, reps)
w3 <- tibble(perc = week3, reps)

wup <- tibble(perc = warm_up, wreps)

rm(list = c("week1", "week2", "week3", "reps", "wreps", "warm_up"))