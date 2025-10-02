
# Assignment2 - part A
# Due 12 noon on Thursday 2 Oct 2025
# submit this R file, edited to have your student number in the file name, and uploaded to moodle. 


# We'll be working with two datasets you'll find on moodle. 
# Download these onto a folder on your computer and save this R file in the same folder. 

# Each year the American business magazine Fortune compiles lists of the largest American companies by revenue. 
# These datasets contain details about the each company, name, location, sector, number of employees together 
# with corresponding financial data, revenue, profit, assets and information about the founders.

# We will use this datasets from 2024 as an example to explore the relationships between profit, revenue, sector and employees. 
# Understanding these relationships is important to eventually build appropriate statistical models to predict future 
# profits. Similar predictions could then be used, for example, to make investment decisions.


# The following criteria carry an equal amount of marks:

# Code - readable, logical, reproducible, tidy with appropriate use of comments and version control.
# Fulfilling the tasks in the assignment
# Data manipulation - efficient and accurate manipulation of the dataset. 
# Data visualisation - appropriate, well-annotated graphs and a description of the insights gained.


# Part 1a

# set the working directory to your folder with the datasets



# load in packages

library(tidyverse)

# read in the .csv files. 

d <- read_csv("Fortune24.csv")

# -------------------------------------------------------
# STEP1: DATA MANIPULATION

# Keep only the following columns and in this order: company, sector, state, market_cap, revenues, profits, assets, 
# employees, ceo_founder, ceo_woman.


d <- d |>
  select(c(company, sector, state, market_cap, revenues, profits, 
           assets, employees, ceo_founder, ceo_woman))

# Change the name of the columns to the following names (you will do the same for the 2023 data to have a consistent 
# set of variable names across both datasets):
# Company
# IndustrialSector
# State
# MarketCap
# Revenue
# Profits
# Assets
# Employees
# FounderCEO
# WomanCEO

# code that saves the data frame with names updated as described above under the object d 

d <- d |>
  mutate(Company = company,
         IndustrialSector = sector,
         State = state,
         MarketCap = market_cap,
         Revenue = revenues,
         Profits = profits,
         Assets = assets,
         Employees = employees,
         FounderCEO = ceo_founder,
         WomanCEO = ceo_woman) |>
  select(-c(company, sector, state, market_cap, revenues, profits, 
            assets, employees, ceo_founder, ceo_woman))

# Create a Date column, as a date object, where the date is 31st of March 2024.

d <- d |>
  mutate(date = "31-3-2024")

# Ensure MarketCap, Revenue, Profits, Assets and Employees are stored as numbers.
# hint: you can use parse_number function in the tidyverse

class(d$Employees)

# since employees is already numeric, I dont need to pass it to parse_number

d <- d |>
  mutate(MarketCap = parse_number(MarketCap), 
         Revenue = parse_number(Revenue), 
         Profits = parse_number(Profits), 
         Assets = parse_number(Assets))
  

# Change FounderCEO and FemaleCEO to be logical where yes corresponds to true.

d <- d |>
  mutate(FounderCEO = ifelse(FounderCEO == "no", 0, 1),
         WomanCEO = ifelse(WomanCEO == "no", 0, 1))

# Change Sector into a factor column

d <- d |>
  mutate(IndustrialSector = as.factor(IndustrialSector))

# Create a RevenueEmployee column that is Revenue divided by the number of employees.

d <- d |>
  mutate(RevenueEmployee = Revenue/Employees)

# Display the first few rows of the data

head(d, n = 5)

# -------------------------------------------------------
# STEP 2: DATA SUMMARIES

# Create a summary by 'Sector'
# Summarise the columns: Revenue, Profits, Assets
# Find the mean of each sector and sort the table in order of the mean Profits. 

summ_a <- d |>
  group_by(IndustrialSector) |>
  summarise(mean_rev = mean(Revenue),
            mean_prof = mean(Profits),
            mean_assets = mean(Assets)) |>
  arrange(desc(mean_prof))

print(summ_a)


# What is the difference in mean Revenue and Revenue per Employee
# between companies with and without women CEOs? 
# Summarise this for each Sector and then take an average across Sectors? 

summ_b <- d |>
  group_by(IndustrialSector, WomanCEO) |>
  summarise(mean_rev = mean(Revenue),
            mean_rev_per_emp = mean(RevenueEmployee))

avg_w_wceo <- summ_b |>
  group_by(WomanCEO) |>
  summarise(rev_summ = mean(mean_rev),
            rev_per_emp_summ = mean(mean_rev_per_emp))

print(avg_w_wceo)

# -------------------------------------------------------
# STEP 3: DATA VISUALISATION

# Create two plots that explore assocations between any of the variables in the dataset. 

# Ensure visualisations are labelled appropriately, font size should be large enough 
# to read easily, labels should be full words not codes or variable names, labels should 
# include units. Ensure that any colours used are accessible, any comparisons indicated 
# only by colour should work for everyone.

d |>
  ggplot(aes(x = log(Assets), y = log(Profits), color = RevenueEmployee)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme(legend.position = "bottom") +
  labs(x = "Log-Scaled Assets (in Millions of USD)", 
       y = "Log-Scaled Profits (in Millions of USD)",
       color = "Revenue per Employee")

# This plot shows a positive correlation between log-scaled assets and 
# log-scaled profits in millions of USD. The revenue per employee for those
# companies do not appear to show the same association.

d |>
  filter(IndustrialSector == c("Energy", "Technology", "Financials")) |>
  ggplot(aes(x = log(MarketCap), y = log(Revenue), color = IndustrialSector)) +
  geom_point() +
  theme_bw() +
  labs(x = "Log-Scaled Market Cap (in Millions of USD)", 
       y = "Log-Scaled Revenue (in Millions of USD)",
       color = "Industrial Sector") +
  theme(legend.position = "bottom")

# This plot shows that there is limited relation between the sectors of 
# these companies and their position on the revenue vs market cap space shown.
# The energy companies seem to be more concentrated near the low levels of both
# market cap and log-scaled revenues, with a notable outlier.



