# MT5763_A1_<jn85>
# Thursday Sept 25, 2025

library(tidyverse)
# library(erify)
set.seed(3)

# Problem A: 
# 1. 

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
     x > 0 ~ "1",
     x == 0 ~ "0",
     x < 0 ~ "2"
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

   check <- installr:::check.integer(N)
   # function from installr that checks if N is a positive integer
   
   if (check == "FALSE") {
     throw("The N value proided is not an integer, try using a positive whole number.")
   } # throws an error message if the check is failed, i.e. N is not a positive integer
   
   
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
 # games(-2)
 # games(4.7)
 
# 4. 
 
 games(10000)
 
 # from my analysis in section 3, and the line of code above which runs 10000
 # iterations of the game, I would prefer to be player 2 and have the two-six 
 # sided die. It appears that player 2 wins about 50 percent of the time as 
 # opposed to player one who seems to win about 41 percent of the time, with 
 # ties making up about 8 percent of games. 
 
 
# function for visualization of those results

 games_viz <- function(N){
   
   check <- installr:::check.integer(N)
   
   if (check == "FALSE") {
     throw("The N value proided is not an integer, try using a positive whole number.")
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
     mutate(res_num = case_when(
       res == "Player 1 Win" ~ 1,
       res == "Player 2 Win" ~ 1,
       res == "Tie" ~ 1
     )) |>
     group_by(res) |>
     summarise(n = n(),
               prop = sum(res_num)/N,
               sd = sqrt(prop*(1-prop)),
               upper = prop + (1.96 * (sd / sqrt(n))),
               lower = prop - (1.96 * (sd / sqrt(n)))) |>
     ungroup()
   
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


# 5. 

games_est <- function(N){ # N allows us to change the number of games played
  
  check <- installr:::check.integer(N)
  # function from installr that checks if N is a positive integer
  
  if (check == "FALSE") {
    throw("The N value proided is not an integer, try using a positive whole number.")
  } # throws an error message if the check is failed, i.e. N is not a positive integer
  
  
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
    mutate(prop = V1,
           sd = V2,
           id = c("p1", "p2", "tie")) |>
    select(-c(V1, V2)) |>
    select(c(id, prop, sd)) |>
    mutate(lower = prop - 1.96*sd,
           upper = prop + 1.96*sd)
  
  p <- d |>
    ggplot(aes(x = id, y = prop)) +
    geom_col() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    theme_bw()
  
  print(p)
  
  cat(paste0("The 95% confidence interval on the proportion of games where player 1 wins ranges from ", 
             round(d$lower[1], 2), " to ", round(d$upper[1], 2), "\n"))
  
  cat(paste0("The 95% confidence interval on the proportion of games where player 2 wins ranges from ", 
             round(d$lower[2], 2), " to ", round(d$upper[2], 2), "\n"))
  
  cat(paste0("The 95% confidence interval on the proportion of games where the players tie ranges from ", 
             round(d$lower[3], 2), " to ", round(d$upper[3], 2), "\n"))
  
}

games_est(10000)

# The output at 10,000 iterations shows that the 95% confidence intervals for 
# the empirical probabilities of each outcome are within two decimal places, 
# indicating relative certainty about those estimates at that level of 
# specificity.


# The following function was a different approach to this problem, iterating a
# certain number of games many times, to take a mean and variance instead of 
# a proportion and its variance. 

# games_est <- function(N, nit){ 
#   # N allows us to change the number of games played
#   # nit allows us to change the number of times that number of games is iterated
#   
#   check <- installr:::check.integer(N)
#   # function from installr that checks if N is a positive integer
#   
#   if (check == "FALSE") {
#     throw("The N value proided is not an integer, try using a positive whole number.")
#   } # throws an error message if the check is failed, i.e. N is not a positive integer
#   
#   x <- vector(length = N)
#   
#   p1 <- rep(0, N)
#   tie <- rep(0, N)
#   p2 <- rep(0, N)
#   
#   res_matrix <- matrix(NA, ncol = 3, nrow = nit)
#   
#   for (j in 1:nit) {
#     
#     res_vec <- rep(NA, N)
#     
#     for (i in 1:N) {
#       x[i] <- sample(1:12, 1) - sum(sample(1:6, 2, replace = TRUE))
#       res_vec[i] <- case_when(
#         x[i] > 0 ~ 1,
#         x[i] < 0 ~ 2,
#         TRUE ~ 0
#       )
#     }
#     
#     p1_sample <- sum(res_vec == 1)/N
#     p2_sample <- sum(res_vec == 2)/N
#     tie_sample <- sum(res_vec == 0)/N
#     
#     res_matrix[j, 1] <- p1_sample
#     res_matrix[j, 2] <- p2_sample
#     res_matrix[j, 3] <- tie_sample
#     
#   }
#   d <- as.data.frame(res_matrix)
#   
#   d <- d |>
#     mutate(P1_win_prop = V1,
#            P2_win_prop = V2,
#            tie_win_prop = V3) |>
#     select(-c(V1, V2, V3))
#   
#   p1_win <- d |>
#     summarise(mean = mean(P1_win_prop),
#               var = var(P1_win_prop))
#   p1_win
#   
#   d |>
#     ggplot(aes(x = P1_win_prop)) +
#     geom_histogram(binwidth = 0.005) +
#     theme_bw()
#   
#   d |>
#     ggplot(aes(x = P2_win_prop)) +
#     geom_histogram(binwidth = 0.005) +
#     theme_bw()
#   
#   d |>
#     ggplot(aes(x = tie_win_prop)) +
#     geom_histogram(binwidth = 0.005) +
#     theme_bw()
#   
# }
# 
# games_est(10000, 1000)
# 
# # To evaluate the variability in the estimates, here are several outputs with
# # varying numbers of iterations, showing their estimates.









# Problem B
# 1. Function for running the game n times, starting at square a.


board <- function(n, a){ 
  
  current = a
  outcomes <- rep(0, n)
  
  for (i in 1:n) {

    flip <- rbinom(1, 1, 0.5)
    potential = ifelse(flip == 1, current + 1, current - 1)
    
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
      
      red <- rep(0, current)
      blue <- rep(1, potential)
      
      full <- c(red, blue)
      
      pick <- sample(full, 1)
      
      current <- ifelse(pick == 1, potential, current)
      
      outcomes[i] <- current
      
      mat[,j] <- outcomes
    }
    
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
    labs(x = "Square", y = "Mean proportion across games with 30 moves")
    
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
      
      red <- rep(0, current)
      blue <- rep(1, potential)
      
      full <- c(red, blue)
      
      pick <- sample(full, 1)
      
      current <- ifelse(pick == 1, potential, current)
      
      outcomes[i] <- current
      
      mat[,j] <- outcomes
    }
    
  }
  # print(mat)
  
  d <- as.data.frame(mat)
  
  sums <- d |>
    colSums()/30
  
  sums <- as.data.frame(sums)
  
  sums |>
    ggplot(aes(x = sums)) +
    geom_histogram(binwidth = 0.5) +
    scale_x_continuous(minor_breaks = seq(0, max(sums), 0.5)) +
    theme_bw() +
    labs(x = "Average Score per Move", y = "Frequency")
  
}

points_board(30, 1, 1000)


# 4. 

avg_points_board <- function(N){ 
  
  a <- 6
  n <- 30
  
  mat <- matrix(NA, nrow = n, ncol = N)
  
  
  for (j in 1:N){
    
    outcomes <- rep(0, n)
    current = a
    
    
    for (i in 1:n) {
      
      flip <- rbinom(1, 1, 0.5)
      potential = ifelse(flip == 1, current + 1, current - 1)
      
      potential = ifelse(potential == 13, 1, potential)
      
      red <- rep(0, current)
      blue <- rep(1, potential)
      
      full <- c(red, blue)
      
      pick <- sample(full, 1)
      
      current <- ifelse(pick == 1, potential, current)
      
      outcomes[i] <- current
      
      mat[,j] <- outcomes
    }
    
  }
  # print(mat)
  
  d <- as.data.frame(mat)
  
  sums <- d |>
    colSums()/30
  
  sums_df <- as.data.frame(sums)
  
  summaries <- sums_df |>
    summarize(n = n(),
      avg = mean(sums),
      sd = sqrt(var(sums)),
      lower = avg - 1.96 * sd/sqrt(n),
      upper = avg + 1.96 * sd/sqrt(n))
  
  cat(paste0("After ", N, " trials, the mean number of points over 30 rounds is ", 
         round(summaries$avg, 3), ", and the 95% confidence interval is (", 
         round(summaries$lower, 3), ", ", round(summaries$upper, 3), ")"))
  
  sums_df |>
    ggplot(aes(x = sums)) +
    geom_histogram(binwidth = 0.4) +
    theme_bw() +
    labs(x = "Points per Move", y = "Frequency")


}

avg_points_board(1000)

#5. 

# What if we are thinking about this issue of B5 all wrong. 
# The only thing that affects the expectation of the number of points you earn 
# is the square you are on right now, and the number of turns after this. 
# The observations are indipendent
# so the answer would be, however many moves it takes 
# until the two players are on the same square

points_thresh <- function(thresh){ 
  
  a <- 1
  n <- thresh + 30
    
    outcomes <- rep(0, n)
    current = a
    
    
    for (i in 1:n) {
      
      flip <- rbinom(1, 1, 0.5)
      potential = ifelse(flip == 1, current + 1, current - 1)
      
      potential = ifelse(potential == 13, 1, potential)
      
      red <- rep(0, current)
      blue <- rep(1, potential)
      
      full <- c(red, blue)
      
      pick <- sample(full, 1)
      
      current <- ifelse(pick == 1, potential, current)
      
      outcomes[i] <- current
      
    }

    d <- as.data.frame(outcomes)
    
    d_slice <- d |>
      slice((thresh + 1):n())
    
    ppm <- sum(d_slice$outcomes)/nrow(d_slice)
    
    ppm_var <- 
  
  cat(paste0("After ", thresh, " moves are discarded, the mean number of points per round over the next ", 
             n-thresh, " moves is ", 
             round(ppm, 3)))
  
  # sums_df |>
  #   ggplot(aes(x = sums)) +
  #   geom_histogram(binwidth = 0.4) +
  #   theme_bw() +
  #   labs(x = "Points per Move", y = "Frequency")
  
  
}

points_thresh(1000)





