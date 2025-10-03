# MT5763_A1_<jn85>
# Thursday Sept 25, 2025

library(tidyverse)
set.seed(3)

# Problem A: 
# 1. 

# my interpretation of part 1 here is that I should just run the game a few
# times to explore the game, before truly analyzing the expected probabilities
# later in the process. These estimates are replaced with a more reliable 
# process later.

res <- rep(0, 10)

for (i in 1:10) {
  res[i] <- 
    (sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE))))
}

res

# This loop simulates playing the game 10 times, but does not return the same 10
# results each time. The last time I ran this code before submission: 
# We can see here that player 1 wins 3 times, player 2 wins four times, 
# and the players tie three times. 
# Early indications then of the probability are that player 2 has a slight 
# advantage in this game and that the player 1 win rate is approximately 
# 30 percent.
 
 # 2. 
 # function for a single game result output as an integer, 
 # either 0 for a draw/tie, 1 when player 1 wins or 2 when player 2 wins.
 
 single_game <- function(){
   x <- (sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE))))
   case_when(
     x > 0 ~ 1,
     x == 0 ~ 0,
     x < 0 ~ 2
   )
 }
 
 # function test
 
 single_game()
 
 # Here, a single game is played and the winner is output through the system: 
 # 0 for a draw/tie, 1 when player 1 wins or 2 when player 2 wins.
 
 
 # 3. 
 # function that plays the game multiple times and outputs the 
 # probability of each of the three outcomes.
 
 games <- function(N){ # N allows us to change the number of games played
   
   if (!is.numeric(N) || N <= 0 || N != floor(N)) {
     cat(sprintf("The N value proided is not an integer, try using a positive whole number. You provided %s.\n", N))
     return(invisible(NULL)) # Stop execution and return nothing
   }
   
   
   x <- vector(length = N)
   p1 <- rep(0, N)
   tie <- rep(0, N)
   p2 <- rep(0, N)
  
   
   for (i in 1:N) {
     x[i] <- (sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE))))
     
     p1[i] <- ifelse(x[i] > 0, 1, 0)
     p2[i] <- ifelse(x[i] < 0, 1, 0)
     tie[i] <- ifelse(x[i] == 0, 1, 0)

   }
   p1s <- (sum(p1)/N)*100
   p2s <- (sum(p2)/N)*100
   ties <- (sum(tie)/N)*100
   
   cat(sprintf(
     "In %s percent of games, player 1 won with the higher score.
     \nIn %s percent of games, player 2 won with the higher score.
     \nIn %s percent of games, the players tied with the same score.
     \nThese estimates were based on %s iterations of the game",
     p1s, p2s, ties, N
   ))
  }
 
 games(10)
 games(-2)
 games(4.7)
 
# 4. 
 
 games(10000)
 
 # from my analysis in section 3, and the line of code above which runs 10000
 # iterations of the game, I would prefer to be player 2 and have the two-six 
 # sided die. It appears that player 2 wins about 50 percent of the time as 
 # opposed to player one who seems to win about 41 percent of the time, with 
 # ties making up about 8 percent of games. 
 
 
# function for visualization of those results

 games_viz <- function(N){
   
   if (!is.numeric(N) || N <= 0 || N != floor(N)) {
     cat(sprintf("The N value proided is not an integer, try using a positive whole number. You provided %s.\n", N))
     return(invisible(NULL)) # Stop execution and return nothing
   }
   
   x <- vector(length = N)
   p1 <- rep(0, N)
   tie <- rep(0, N)
   p2 <- rep(0, N)
   res <- rep("", N)
   
   for (i in 1:N) {
     x[i] <- 
       (sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, 
                                                replace = TRUE))))
     
     p1[i] <- ifelse(x[i] > 0, 1, 0)
     p2[i] <- ifelse(x[i] < 0, 1, 0)
     tie[i] <- ifelse(x[i] == 0, 1, 0)
     
     res[i] <- ifelse(x[i] > 0, "Player 1 Win", 
                      ifelse(x[i] < 0, "Player 2 Win", "Tie"))
     
   }
   p1s <- (sum(p1)/N)
   p2s <- (sum(p2)/N)
   ties <- (sum(tie)/N)
   
   d <- data.frame(res)
   
   plot_data <- d |>
     count(res) |>
     group_by() |>
     mutate(prop = n / N,
            sd = sqrt(prop*(1 - prop) / N),
            lower = prop - 1.96 * sd,
            upper = prop + 1.96 * sd)
   
   ggplot(plot_data, aes(x = res, y = prop, fill = res)) +
     geom_col() +
     geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
     theme_bw() +
     labs(
       x = "Result",
       y = paste0("Proportion (out of ", N, ")"),
       title = "Bar graph with 95% Confidence Intervals") +
     guides(fill=guide_legend(title="Results"))
 }

games_viz(10000)

# From those two pieces of analysis, I would prefer to be player 2 in this game.


# 5. 

# to show the effect of changing the number of games played on the variability
# in the estimates:

games_est_mod <- function(N){ # N allows us to change the number of games played
  
  if (!is.numeric(N) || N <= 0 || N != floor(N)) {
    cat(sprintf("The N value proided is not an integer, try using a positive whole number. You provided %s.\n", N))
    return(invisible(NULL)) # Stop execution and return nothing
  }
  
  
  x <- vector(length = N)
  p1 <- rep(0, N)
  tie <- rep(0, N)
  p2 <- rep(0, N)
  
  
  for (i in 1:N) {
    x[i] <- (sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE))))
    
    p1[i] <- ifelse(x[i] > 0, 1, 0)
    p2[i] <- ifelse(x[i] < 0, 1, 0)
    tie[i] <- ifelse(x[i] == 0, 1, 0)
    
  }
  p1_prop <- (sum(p1)/N)
  p2_prop <- (sum(p2)/N)
  tie_prop <- (sum(tie)/N)
  
  p1_sd <- sqrt(p1_prop*(1-p1_prop)/N)
  p2_sd <- sqrt(p2_prop*(1-p2_prop)/N)
  tie_sd <- sqrt(tie_prop*(1-tie_prop)/N)
  
  d <- as.data.frame(matrix(
    data = c(p1_prop, p2_prop, tie_prop, p1_sd, p2_sd, tie_sd),
    nrow = 3, ncol = 2))
  
  d <- d |>
    mutate(prop = round(V1, 5),
           sd = round(V2, 5),
           id = c("p1", "p2", "tie")) |>
    select(-c(V1, V2)) |>
    select(c(id, prop, sd)) |>
    mutate(lower = round(prop - 1.96*sd, 5),
           upper = round(prop + 1.96*sd, 5),
           width = upper - lower)
  
  return(d)
  
}

games_est_mod(1000)
games_est_mod(10000)
games_est_mod(100000)

# These three outputs show the shrinking of confidence intervals that result 
# from changing the number of games run.

games_iterate <- function(N_start) {
  
  N <- N_start
  result <- games_est_mod(N)
  
  while (any(result$width > 0.0025)) { 
    # 0.0025 chosen since then the width of the 95% CI is less than half of the
    # second decimal point and will not allow for rounding to a different value
    # of that second decimal point
    
    N <- N + 2500
    result <- games_est_mod(N)
  }
  
  return(list(N_final = N, result = result))
}

games_iterate(600000)

# This function shows the number of games, within 2500, that it takes to get
# a 95% CI that shows certainty to two decimal places, and the estimates at that
# number of games.


# Problem B
# 1. Function for running the game n times, starting at square a.


board <- function(n, a){ 
  
  current = a
  outcomes <- rep(0, n)
  
  for (i in 1:n) {

    flip <- rbinom(1, 1, 0.5)
    
    potential = ifelse(flip == 1, current + 1, current - 1)
    
    potential = ifelse(potential == 13, 1, potential)
    potential = ifelse(potential == 0, 12, potential)
    
    red <- rep(0, current)
    blue <- rep(1, potential)
    
    full <- c(red, blue)
    
    pick <- sample(full, 1)
    
    current <- ifelse(pick == 1, potential, current)
    
    outcomes[i] <- current
  }
  print(outcomes)
}

board(30, 1) 


# 2. 

multi_board <- function(n, a, N){ 
  
  mat <- matrix(NA, nrow = n, ncol = N)
 
  
  for (j in 1:N){
    
    outcomes <- rep(0, n)
    current = a

    
    for (i in 1:n) {
      
      flip <- rbinom(1, 1, 0.5)
      
      potential = ifelse(flip == 1, current + 1, current - 1)
      
      potential = ifelse(potential == 13, 1, potential)
      potential = ifelse(potential == 0, 12, potential)
      
      red <- rep(0, current)
      blue <- rep(1, potential)
      
      full <- c(red, blue)
      
      pick <- sample(full, 1)
      
      current <- ifelse(pick == 1, potential, current)
      
      outcomes[i] <- current
      
    }
    mat[, j] <- outcomes
  }
  # print(mat)
  
  d <- as.data.frame(mat)
  
  # This code was my solution to my original understanding of the problem:
  # the frequency that every square was visited during the 30 moves.
  # I calculate this by finding the frequency that square 12 was reached in the
  # 30 moves. 
  # 
  # I now understand the problem to be the frequencies of the individual squares
  # over 30 moves, that solution is below this code block
  
  # prop_12s <- d |>
  #   summarise(across(everything(), ~ any(.x == 12))) |>
  #   select(where(~ .x)) |>
  #   ncol()
  # 
  # prop_12s <- tibble(prop = prop_12s / ncol(d))
  # 
  # ggplot(prop_12s, aes(x = "Proportion", y = prop)) +
  #   geom_col() +
  #   # geom_text(aes(label = round(prop, 3)), vjust = -0.5) +
  #   geom_text(aes(label = paste("Out of", N, "trials,", prop*N, 
  #                               "game(s) contained a 12", "(", prop_12s*100, "%).")), vjust = -2) +
  #   scale_y_continuous(limits = c(0, prop_12s$prop * 2)) +
  #   labs(x = "", y = "Proportion of games with a 12") +
  #   theme_bw()
  
  props <- d |>
    mutate(iter = row_number()) |>
    pivot_longer(-iter, names_to = "iteration", values_to = "square") |>
    group_by(iteration) |>
    count(square) |>
    mutate(prop = n / 30) |>
    group_by(square) |>
    summarise(mean_prop = mean(prop, na.rm = TRUE))
  
  ggplot(props, aes(x = factor(square), y = mean_prop)) +
    geom_col() +
    theme_bw() +
    labs(x = "Square", y = "Proportion of each square in 30 moves")
    
}

multi_board(30, 1, 1000)


# 3. 

points_board <- function(n, a, N){ 
  
  mat <- matrix(NA, nrow = n, ncol = N)
  
  
  for (j in 1:N){
    
    outcomes <- rep(0, n)
    current = a
    
    
    for (i in 1:n) {
      
      flip <- rbinom(1, 1, 0.5)
      
      potential = ifelse(flip == 1, current + 1, current - 1)
      
      potential = ifelse(potential == 13, 1, potential)
      potential = ifelse(potential == 0, 12, potential)
      
      red <- rep(0, current)
      blue <- rep(1, potential)
      
      full <- c(red, blue)
      
      pick <- sample(full, 1)
      
      current <- ifelse(pick == 1, potential, current)
      
      outcomes[i] <- current
    }
    
    mat[, j] <- outcomes
    
  }
  # print(mat)
  
  d <- as.data.frame(mat)
  
  sums <- d |>
    colSums()/n
  
  sums <- as.data.frame(sums)
  
  sums |>
    ggplot(aes(x = sums)) +
    geom_histogram(binwidth = 0.5) +
    # scale_x_continuous(minor_breaks = seq(0, max(sums$sums), 0.5)) +
    theme_bw() +
    labs(x = "Average Score per Move", y = "Frequency")
  
}

points_board(30, 1, 1000)


# 4 

points_board_mod <- function(a, n, Nsims){ 
  
  N = Nsims
  
  mat <- matrix(NA, nrow = n, ncol = N)
  
  
  for (j in 1:N){
    
    outcomes <- rep(0, n)
    current = a
    
    
    for (i in 1:n) {
      
      flip <- rbinom(1, 1, 0.5)
      
      potential = ifelse(flip == 1, current + 1, current - 1)
      
      potential = ifelse(potential == 13, 1, potential)
      potential = ifelse(potential == 0, 12, potential)
      
      red <- rep(0, current)
      blue <- rep(1, potential)
      
      full <- c(red, blue)
      
      pick <- sample(full, 1)
      
      current <- ifelse(pick == 1, potential, current)
      
      outcomes[i] <- current
    }
    
    mat[, j] <- outcomes
    
  }
  # print(mat)
  
  d <- as.data.frame(mat)
  
  sums <- d |>
    colSums()/n
  
  sums <- as.data.frame(sums)
  
  ret <- sums |>
    mutate(dat = sums) |>
    summarise(mean = round(mean(sums), 3),
              lower = round(mean - sqrt(var(sums)/N), 3),
              upper = round(mean + sqrt(var(sums)/N), 3),
              width = upper - lower,
              CI = paste0("(",lower, ", ", upper, ")")) |>
    select(-c(lower, upper, width))
  
  return(ret)
  
}

points_board_mod(6, 30, 1000)

# reports the mean number of points per move over 30 moves and the 95% CI of 
# that estimate. the paramter for that function adjusts the number of trials of
# 30-move games.


# 5. 

points_board_mod(6, 30, 1500)

# given that this output shows that the width of the 95% confidence interval 
# on the mean at 1500 simulations is on the order of 0.1, the threshold function
# for number 5 will be based on runs of 1500 simulations with a threshold value
# of 0.1 for a difference in the mean number of points per move.



# function that takes a threshold, representing a difference in means of the two
# runs, one starting at 1, one starting at 6. 

points_thresh <- function(n_start, threshold){
  
  diff <- 1
  n <- n_start
  
  while (diff > threshold){
    
    x = points_board_mod(1, n, 1500)
    ones <- x$mean
    
    y = points_board_mod(6, n, 1500)
    sixes <- y$mean
    
    diff = abs(sixes - ones)
    n = n + 10
    
  }
  return(n)
}

points_thresh(700, 0.1)


# This output shows that for runs of 1500 simulations, and a mean points per 
# move threshold of 0.1, it takes around 700 moves for the player who starts at
# square 1 and the player that starts at square 6 to have the same mean points
# per move at our threshold level.