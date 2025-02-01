library(nloptr)

# Load the dataset
wtp_data <- read.csv("wtp_data.csv", sep=";")

# Parameters for General section
capacity_general <- 14500 / 100 # Define the scaled capacity for the General section
min_demand_general <- 0.8 * capacity_general # Define minimum demand constraint (80% of capacity)

# Loop through all tiers for General section
tiers <- list(Tier1 = "WTP.Tier.1", Tier2 = "WTP.Tier.2", Tier3 = "WTP.Tier.3")
results <- list() # Initialize results list to store optimal solutions for each tier

for (tier_name in names(tiers)) {
  tier_column <- tiers[[tier_name]] # Get the column name for the current tier
  
  # Filter WTP data for General section and ensure numeric type
  general_data <- wtp_data[wtp_data$Fan.Type == "General", ]
  wtp_tier <- as.numeric(general_data[[tier_column]]) # Convert the WTP column to numeric
  
  # Objective function: Maximize revenue
  eval_f <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE) # Count how many people are willing to pay >= price
    demand <- min(demand, capacity_general) # Enforce maximum capacity constraint
    revenue <- price * demand # Calculate revenue
    return(-revenue) #  Return negative revenue for minimization
  }
  
  # Inequality constraints: Minimum and maximum demand
  eval_g_ineq <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE) # Calculate demand
    demand <- min(demand, capacity_general) # Enforce maximum capacity
    constraints <- c(
      min_demand_general - demand, # Ensure demand >= 80% capacity
      demand - capacity_general    # Ensure demand <= capacity
    )
    return(constraints)
  }
  
  # Optimization setup
  result <- nloptr(
    x0 = mean(wtp_tier, na.rm = TRUE), # Initial price guess
    eval_f = eval_f,
    lb = 0, # Lower bound for price
    ub = max(wtp_tier, na.rm = TRUE), # Upper bound for price
    eval_g_ineq = eval_g_ineq,
    opts = list(
      "algorithm" = "NLOPT_LN_COBYLA",
      "xtol_rel" = 1e-6,
      "maxeval" = 1000
    )
  )
  
  # Store results
  optimal_price <- result$solution # Optimal price
  optimal_revenue <- -result$objective # Optimal revenue
  optimal_demand <- sum(wtp_tier >= optimal_price, na.rm = TRUE) # Optimal demand
  
  results[[tier_name]] <- list(
    Optimal_Price = optimal_price,
    Optimal_Revenue = optimal_revenue,
    Optimal_Demand = optimal_demand
  )
}

# Print results for General section
for (tier_name in names(results)) {
  cat("Results for General Section -", tier_name, ":\n")
  cat("  Optimal Price: £", results[[tier_name]]$Optimal_Price, "\n")
  cat("  Optimal Revenue: £", results[[tier_name]]$Optimal_Revenue, "\n")
  cat("  Optimal Demand: ", results[[tier_name]]$Optimal_Demand, "\n\n")
}

# Parameters for Family section
capacity_family <- 2000 / 100 # Define scaled capacity for the Family section
min_demand_family <- 0.8 * capacity_family # Define minimum demand constraint (80% of capacity)

# Loop through all tiers for Family section
results_family <- list()

for (tier_name in names(tiers)) {
  tier_column <- tiers[[tier_name]]
  
  # Filter WTP data for Family section and ensure numeric type
  family_data <- wtp_data[wtp_data$Fan.Type == "Family", ]
  wtp_tier <- as.numeric(family_data[[tier_column]]) # Convert to numeric explicitly
  
  # Objective function: Maximize revenue
  eval_f <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE)
    demand <- min(demand, capacity_family) # Enforce maximum capacity
    revenue <- price * demand
    return(-revenue) # Negative for minimization
  }
  
  # Inequality constraints: Minimum and maximum demand
  eval_g_ineq <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE)
    demand <- min(demand, capacity_family) # Enforce maximum capacity
    constraints <- c(
      min_demand_family - demand, # Ensure demand >= 80% capacity
      demand - capacity_family    # Ensure demand <= capacity
    )
    return(constraints)
  }
  
  # Optimization setup
  result <- nloptr(
    x0 = mean(wtp_tier, na.rm = TRUE), # Initial price guess
    eval_f = eval_f,
    lb = 0, # Lower bound for price
    ub = max(wtp_tier, na.rm = TRUE), # Upper bound for price
    eval_g_ineq = eval_g_ineq,
    opts = list(
      "algorithm" = "NLOPT_LN_COBYLA",
      "xtol_rel" = 1e-6,
      "maxeval" = 1000
    )
  )
  
  # Store results
  optimal_price <- result$solution
  optimal_revenue <- -result$objective
  optimal_demand <- sum(wtp_tier >= optimal_price, na.rm = TRUE)
  
  results_family[[tier_name]] <- list(
    Optimal_Price = optimal_price,
    Optimal_Revenue = optimal_revenue,
    Optimal_Demand = optimal_demand
  )
}

# Print results for all tiers for Family section
for (tier_name in names(results_family)) {
  cat("Results for Family Section -", tier_name, ":\n")
  cat("  Optimal Price: £", results_family[[tier_name]]$Optimal_Price, "\n")
  cat("  Optimal Revenue: £", results_family[[tier_name]]$Optimal_Revenue, "\n")
  cat("  Optimal Demand: ", results_family[[tier_name]]$Optimal_Demand, "\n\n")
}

# Parameters for VIP section
capacity_VIP <- 750 / 100 # Define scaled capacity for the VIP section
min_demand_VIP <- 0.8 * capacity_VIP # Define minimum demand constraint (80% of capacity)

# Loop through all tiers for VIP section
results_VIP <- list()

for (tier_name in names(tiers)) {
  tier_column <- tiers[[tier_name]]
  
  # Filter WTP data for VIP section and ensure numeric type
  VIP_data <- wtp_data[wtp_data$Fan.Type == "VIP", ]
  wtp_tier <- as.numeric(VIP_data[[tier_column]]) # Convert to numeric explicitly
  
  # Objective function: Maximize revenue
  eval_f <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE)
    demand <- min(demand, capacity_VIP) # Enforce maximum capacity
    revenue <- price * demand
    return(-revenue) # Negative for minimization
  }
  
  # Inequality constraints: Minimum and maximum demand
  eval_g_ineq <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE)
    demand <- min(demand, capacity_VIP) # Enforce maximum capacity
    constraints <- c(
      min_demand_VIP - demand, # Ensure demand >= 80% capacity
      demand - capacity_VIP    # Ensure demand <= capacity
    )
    return(constraints)
  }
  
  # Optimization setup
  result <- nloptr(
    x0 = mean(wtp_tier, na.rm = TRUE), # Initial price guess
    eval_f = eval_f,
    lb = 0, # Lower bound for price
    ub = max(wtp_tier, na.rm = TRUE), # Upper bound for price
    eval_g_ineq = eval_g_ineq,
    opts = list(
      "algorithm" = "NLOPT_LN_COBYLA",
      "xtol_rel" = 1e-6,
      "maxeval" = 1000
    )
  )
  
  # Store results
  optimal_price <- result$solution
  optimal_revenue <- -result$objective
  optimal_demand <- sum(wtp_tier >= optimal_price, na.rm = TRUE)
  
  results_VIP[[tier_name]] <- list(
    Optimal_Price = optimal_price,
    Optimal_Revenue = optimal_revenue,
    Optimal_Demand = optimal_demand
  )
}

# Print results for all tiers for VIP section
for (tier_name in names(results_VIP)) {
  cat("Results for VIP Section -", tier_name, ":\n")
  cat("  Optimal Price: £", results_VIP[[tier_name]]$Optimal_Price, "\n")
  cat("  Optimal Revenue: £", results_VIP[[tier_name]]$Optimal_Revenue, "\n")
  cat("  Optimal Demand: ", results_VIP[[tier_name]]$Optimal_Demand, "\n\n")
}

# Parameters for Courtside section
capacity_Courtside <- 250 / 100 # Define scaled capacity for the Courtside section
min_demand_Courtside <- 0.8 * capacity_Courtside # Define minimum demand constraint (80% of capacity)

# Loop through all tiers for Courtside section
results_Courtside <- list()

for (tier_name in names(tiers)) {
  tier_column <- tiers[[tier_name]]
  
  # Filter WTP data for Courtside section and ensure numeric type
  Courtside_data <- wtp_data[wtp_data$Fan.Type == "Courtside", ]
  wtp_tier <- as.numeric(Courtside_data[[tier_column]]) # Convert to numeric explicitly
  
  # Objective function: Maximize revenue
  eval_f <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE)
    demand <- min(demand, capacity_Courtside) # Enforce maximum capacity
    revenue <- price * demand
    return(-revenue) # Negative for minimization
  }
  
  # Inequality constraints: Minimum and maximum demand
  eval_g_ineq <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE)
    demand <- min(demand, capacity_Courtside) # Enforce maximum capacity
    constraints <- c(
      min_demand_Courtside - demand, # Ensure demand >= 80% capacity
      demand - capacity_Courtside    # Ensure demand <= capacity
    )
    return(constraints)
  }
  
  # Optimization setup
  result <- nloptr(
    x0 = mean(wtp_tier, na.rm = TRUE), # Initial price guess
    eval_f = eval_f,
    lb = 0, # Lower bound for price
    ub = max(wtp_tier, na.rm = TRUE), # Upper bound for price
    eval_g_ineq = eval_g_ineq,
    opts = list(
      "algorithm" = "NLOPT_LN_COBYLA",
      "xtol_rel" = 1e-6,
      "maxeval" = 1000
    )
  )
  
  # Store results
  optimal_price <- result$solution
  optimal_revenue <- -result$objective
  optimal_demand <- sum(wtp_tier >= optimal_price, na.rm = TRUE)
  
  results_Courtside[[tier_name]] <- list(
    Optimal_Price = optimal_price,
    Optimal_Revenue = optimal_revenue,
    Optimal_Demand = optimal_demand
  )
}

# Print results for all tiers for Courtside section
for (tier_name in names(results_Courtside)) {
  cat("Results for Courtside Section -", tier_name, ":\n")
  cat("  Optimal Price: £", results_Courtside[[tier_name]]$Optimal_Price, "\n")
  cat("  Optimal Revenue: £", results_Courtside[[tier_name]]$Optimal_Revenue, "\n")
  cat("  Optimal Demand: ", results_Courtside[[tier_name]]$Optimal_Demand, "\n\n")
}

# Parameters for Ultras section
capacity_ultras <- 2000 / 100 # Define scaled capacity for the Ultras section
min_demand_ultras <- capacity_ultras # Ultras section requires full capacity

# Loop through all tiers for Ultras section
results_ultras <- list()

for (tier_name in names(tiers)) {
  tier_column <- tiers[[tier_name]]
  
  # Filter WTP data for Ultras section and ensure numeric type
  ultras_data <- wtp_data[wtp_data$Fan.Type == "Ultras", ]
  wtp_tier <- as.numeric(ultras_data[[tier_column]]) # Convert to numeric explicitly
  
  # Objective function: Maximize revenue
  eval_f <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE)
    demand <- min(demand, capacity_ultras) # Enforce maximum capacity
    revenue <- price * demand
    return(-revenue) # Negative for minimization
  }
  
  # Strictly enforce minimum demand constraint
  eval_g_ineq <- function(price) {
    demand <- sum(wtp_tier >= price, na.rm = TRUE)
    constraints <- c(
      capacity_ultras - demand, # Ensure demand = capacity
      demand - capacity_ultras  # Ensure demand = capacity
    )
    return(constraints)
  }
  
  # Optimization setup
  result <- nloptr(
    x0 = mean(wtp_tier, na.rm = TRUE), # Initial price guess
    eval_f = eval_f,
    lb = 0, # Lower bound for price
    ub = max(wtp_tier, na.rm = TRUE), # Upper bound for price
    eval_g_ineq = eval_g_ineq,
    opts = list(
      "algorithm" = "NLOPT_LN_COBYLA",
      "xtol_rel" = 1e-6,
      "maxeval" = 1000
    )
  )
  
  # Force demand to equal capacity
  optimal_price <- result$solution
  optimal_demand <- capacity_ultras # Enforce full capacity
  optimal_revenue <- optimal_price * optimal_demand
  
  results_ultras[[tier_name]] <- list(
    Optimal_Price = optimal_price,
    Optimal_Revenue = optimal_revenue,
    Optimal_Demand = optimal_demand
  )
}

# Print results for all tiers for Ultras section
for (tier_name in names(results_ultras)) {
  cat("Results for Ultras Section -", tier_name, ":\n")
  cat("  Optimal Price: £", results_ultras[[tier_name]]$Optimal_Price, "\n")
  cat("  Optimal Revenue: £", results_ultras[[tier_name]]$Optimal_Revenue, "\n")
  cat("  Optimal Demand: ", results_ultras[[tier_name]]$Optimal_Demand, "\n\n")
}  


library(ggplot2)

# Create a list of sections with their data and capacities
sections <- list(
  General = list(data = wtp_data[wtp_data$Fan.Type == "General", ], capacity = capacity_general),
  Family = list(data = wtp_data[wtp_data$Fan.Type == "Family", ], capacity = capacity_family),
  VIP = list(data = wtp_data[wtp_data$Fan.Type == "VIP", ], capacity = capacity_VIP),
  Courtside = list(data = wtp_data[wtp_data$Fan.Type == "Courtside", ], capacity = capacity_Courtside),
  Ultras = list(data = wtp_data[wtp_data$Fan.Type == "Ultras", ], capacity = capacity_ultras)
)

# Assuming `results_list` contains the optimal prices and revenues for each section and tier
results_list <- list(
  General = results,
  Family = results_family,
  VIP = results_VIP,
  Courtside = results_Courtside,
  Ultras = results_ultras
)


# Create a data frame to hold all revenue data for all sections and tiers
all_revenue_data <- data.frame()

# Loop through each section and tier to calculate revenue curves
for (section_name in names(sections)) {
  section_data <- sections[[section_name]]$data
  capacity <- sections[[section_name]]$capacity
  section_results <- results_list[[section_name]]
  
  for (tier_name in names(tiers)) {
    tier_column <- tiers[[tier_name]]
    optimal_price <- section_results[[tier_name]]$Optimal_Price
    
    # Generate a range of prices
    price_range <- seq(0, max(section_data[[tier_column]], na.rm = TRUE), length.out = 100)
    
    # Calculate revenue for each price in the range
    revenue <- sapply(price_range, function(price) {
      demand <- sum(section_data[[tier_column]] >= price, na.rm = TRUE)
      demand <- min(demand, capacity) # Enforce capacity constraint
      return(price * demand)
    })
    
    # Add to the data frame
    temp_data <- data.frame(
      Section = section_name,
      Tier = tier_name,
      Price = price_range,
      Revenue = revenue,
      Optimal_Price = optimal_price
    )
    all_revenue_data <- rbind(all_revenue_data, temp_data)
  }
}

# Plot all revenue curves in one plot with facets
ggplot(all_revenue_data, aes(x = Price, y = Revenue)) +
  geom_line(color = "blue", size = 1) +
  geom_vline(aes(xintercept = Optimal_Price), linetype = "dotted", color = "red", size = 0.8) +
  facet_wrap(~ Section + Tier, scales = "free", ncol = 3) +
  labs(
    title = "Revenue Curves for All Sections and Tiers",
    x = "Ticket Price (£)",
    y = "Revenue (£)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8, face = "bold"), # Panel titles
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5), # Main title
    axis.title.x = element_text(size=7),
    axis.title.y = element_text(size=7),
    axis.text = element_text(size = 6),      # Adjust axis tick text size
    panel.spacing = unit(0.2, "lines")
  )


# Initialize a data frame to store the total revenue for each tier
total_revenue_summary <- data.frame(Tier = character(), Total_Revenue = numeric(), Total_Season_Revenue = numeric(), stringsAsFactors = FALSE)

# Multipliers for the total season revenue calculation
season_games <- c(Tier1 = 4, Tier2 = 7, Tier3 = 6)

# Loop through each tier to calculate the total revenue and total season revenue
for (tier_name in names(tiers)) {
  total_revenue <- 0
  
  # Sum the optimal revenue across all sections for the current tier
  for (section_name in names(results_list)) {
    section_results <- results_list[[section_name]]
    total_revenue <- total_revenue + section_results[[tier_name]]$Optimal_Revenue
  }
  
  # Calculate total season revenue
  total_season_revenue <- total_revenue * season_games[[tier_name]]
  
  # Store the total revenue and total season revenue for the current tier
  total_revenue_summary <- rbind(total_revenue_summary, data.frame(
    Tier = tier_name,
    Total_Revenue = total_revenue,
    Total_Season_Revenue = total_season_revenue
  ))
}

# Print the total revenue summary
cat("Total Revenue and Total Season Revenue Across All Sections for Each Tier:\n")
print(total_revenue_summary)

# Calculate the sum of all total season revenue
sum_total_season_revenue <- sum(total_revenue_summary$Total_Season_Revenue)

cat("Sum of All Total Season Revenue: £", sum_total_season_revenue, "\n")


# Define last year's ticket prices for each section and tier
last_year_prices <- list(
  General = c(Tier3 = 30, Tier2 = 60, Tier1 = 75),
  Family = c(Tier3 = 25, Tier2 = 50, Tier1 = 65),
  VIP = c(Tier3 = 50, Tier2 = 100, Tier1 = 150),
  Courtside = c(Tier3 = 150, Tier2 = 300, Tier1 = 450),
  Ultras = c(Tier3 = 20, Tier2 = 40, Tier1 = 60)
)

# Data frame to store last year's total revenue per game and total season revenue
last_year_revenue_summary <- data.frame(Tier = character(), Total_Revenue_Last_Year = numeric(), Total_Season_Revenue_Last_Year = numeric(), stringsAsFactors = FALSE)


# Calculate last year's total revenue for a game and total season revenue for each tier
for (tier_name in names(tiers)) {
  tier_column <- tiers[[tier_name]]
  total_revenue <- 0
  
  # Sum the revenue across all sections for the current tier
  for (section_name in names(sections)) {
    section_data <- sections[[section_name]]$data
    capacity <- sections[[section_name]]$capacity
    
    # Get the correct price for the current section and tier
    price <- last_year_prices[[section_name]][[tier_name]]
    
    # Calculate revenue for this section and tier
    demand <- sum(section_data[[tier_column]] >= price, na.rm = TRUE)
    demand <- min(demand, capacity) # Enforce maximum capacity constraint
    total_revenue <- total_revenue + price * demand
  }
  
  # Calculate total season revenue
  total_season_revenue <- total_revenue * season_games[[tier_name]]
  
  # Store the total revenue for the current tier
  last_year_revenue_summary <- rbind(last_year_revenue_summary, data.frame(
    Tier = tier_name,
    Total_Revenue_Last_Year = total_revenue,
    Total_Season_Revenue_Last_Year = total_season_revenue
  ))
}

# Print last year's revenue summary with total season revenue
cat("Last Year's Revenue and Total Season Revenue for Each Tier:\n")
print(last_year_revenue_summary)

# Calculate the sum of all total season revenue
sum_total_season_revenue_lasteyear <- sum(last_year_revenue_summary$Total_Season_Revenue_Last_Year)

cat("Sum of All Total Season Revenue: £", sum_total_season_revenue_lasteyear, "\n")

print(paste("The difference in revenue compared to last season is (in %):",(204107-164950)/164950*100))

