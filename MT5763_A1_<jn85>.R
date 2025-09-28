# MT5763_A1_<jn85>
# Thursday Sept 25, 2025

library(tidyverse)
library(erify)
set.seed(3)

# Problem A: 
# 1. 

sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE)))
sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE)))
sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE)))
sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE)))
sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE)))

# These five lines of code simulate playing the game five times. We can 
# see here that player 1 wins four times, and player 2 wins once. The players 
# do not tie during these five iterations. Early indications are that player 1
# has an advantage in this game.
 
 # 2. 
 # function for a single game
 
 single_game <- function(){
   x <- (sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, replace = TRUE))))
   case_when(
     x > 0 ~ "1",
     x == 0 ~ "0",
     x < 0 ~ "2"
   )
 }
 
 single_game()
 
 # Here, a single game is played and player 1 wins.
 
 
 # 3. 
 # function for multiple plays, parameter for number of games
 
 games <- function(N){

   check <- installr:::check.integer(N)
   
   if (check == "FALSE") {
     throw("The N value proided is not an integer, try using a positive whole number.")
   }
   
   x <- vector(length = N)
   p1 <- rep(0, N)
   tie <- rep(0, N)
   p2 <- rep(0, N)
   
   for (i in 1:N) {
     x[i] <- 
       (sample(seq(1, 12, 1), 1) -  sum((sample(seq(1, 6, 1), 2, 
                                                replace = TRUE))))
     
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
 
 
# function for visualization of results

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

# To evaluate the variability in the estimates, here are several outputs with
# varying numbers of iterations, showing their estimates.


games(100)
games(1000)
games(10000)
games(100000)
games(1000000)

# To be sure of the probabilities to two decimal places, it appears that 1x10^5
# iterations is sufficient. Iterations do not take long to simulate so
# I would recommend 1x10^6 to increase certainty.













# Problem B
# 1. 


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

# board(30, 1)

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
  
  prop_12s <- d |>
    summarise(across(everything(), ~ any(.x == 12))) |>
    select(where(~ .x)) |>
    ncol()
  
  prop_12s <- tibble(prop = prop_12s / ncol(d))
  
  ggplot(prop_12s, aes(x = "Proportion", y = prop)) +
    geom_col() +
    # geom_text(aes(label = round(prop, 3)), vjust = -0.5) +
    geom_text(aes(label = paste("Out of", N, "trials,", prop_12s$prop*N, 
                                "game(s) contained a 12", "(", prop_12s*100, "%).")), vjust = -2) +
    scale_y_continuous(limits = c(0, prop_12s$prop * 2)) +
    labs(x = "", y = "Proportion of games with a 12") +
    theme_bw()
    
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
    colSums()
  
  sums <- as.data.frame(sums)
  
  sums |>
    ggplot(aes(x = sums)) +
    geom_histogram(binwidth = 10) +
    theme_bw() +
    labs(x = "Score", y = "Frequency")
  
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
    colSums()
  
  sums_df <- as.data.frame(sums)
  
  summaries <- sums_df |>
    summarize(n = n(),
      avg = mean(sums),
      sd = sqrt(var(sums)),
      lower = avg - 1.96 * sd/sqrt(n),
      upper = avg + 1.96 * sd/sqrt(n))
  
  
  
  # sums_df |>
  #   ggplot(aes(x = sums)) +
  #   geom_boxplot() +
  #   theme_bw() +
  #   labs(x = "Score", y = "")
  
}

avg_points_board(1000)






















