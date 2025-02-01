### Assingment 2_CODE_Valentina Donado

#load required packages
library(nloptr)
library(mlogit)
library(matrixStats)
library(stargazer)
library(ggplot2)

# Read survey data
congestion_pricing = read.csv('CongestionPricing.csv')

# Get number of data observations
N=nrow(congestion_pricing)

# Print top few rows of data
head(congestion_pricing)


#Scenario 1: Maximise revenue with single congestion charge for peak and non-peak hours

# Compute maximum willingness to pay for each client across the two time slots
for (i in 1:N){
  congestion_pricing$max_WTP[i]=max(congestion_pricing[i,2:3])
}

# Visualize the first rows of data including the maxWTP for each client
congestion_pricing[1:10,]

# Calculate max WTP in data to use it as the upper bound for the price search
max_price=max(congestion_pricing$max_WTP)

# Defining empty array variables we will be introducing
demand_single_price=rep(NA,max_price)
revenue= rep(NA,max_price)
avg_speed = rep(NA,max_price)
emissions_per_car = rep(NA,max_price)
total_emissions = rep(NA,max_price)
demand_total = rep(NA,max_price)
price = c(1:max_price)


# Find how many people buy at each price level
for (p in 1:max_price){
  demand_single_price[p]=sum(congestion_pricing$max_WTP>=p)    #demand at each price level
  demand_total[p] = demand_single_price[p]*192/N               #converting demand to represent whole population (in thousands)
  revenue[p]=p*demand_total[p]                                 #total revenue of congestion charge
}

# Identifying the Best Price

revenue_single_price=max(revenue)
single_price_best=which(revenue == revenue_single_price)

print(paste("If a single price is to be charged across all time slots, the optimal price is:",single_price_best))
print(paste("With this single price strategy the max revenue made would be (£)",round(revenue_single_price),"thousand."))


# Calculating total level of emissions with this price in effect

#create an empty array
demand_nonpeak_single = rep(NA,N)
demand_peak_single = rep(NA,N)

# Calculating consumer surplus and classifying which drivers will enter the city
for (r in 1:N){
  surplus_nonpeak_single = congestion_pricing[r,3] - single_price_best
  surplus_peak_single = congestion_pricing[r,2] - single_price_best
  demand_nonpeak_single[r] = (surplus_nonpeak_single>surplus_peak_single)*(surplus_nonpeak_single>=0)
  demand_peak_single[r] = (surplus_peak_single>=surplus_nonpeak_single)*(surplus_peak_single>=0)


# Calculating demand of total population (in thousands)
  demand_nonpeak_single_total = sum(demand_nonpeak_single) * (192/N)
  demand_peak_single_total = sum(demand_peak_single) * (192/N)

# Calculating average speed and emissions at both time periods
  avg_speed_nonpeak = 30 - 0.0625*(demand_nonpeak_single_total)
  avg_speed_peak = 30 - 0.0625*(demand_peak_single_total)

  emissions_nonpeak = (ifelse(avg_speed_nonpeak<25,
                            617.5-16.7*avg_speed_nonpeak,
                            235.0-1.4*avg_speed_nonpeak))*demand_nonpeak_single_total

  emissions_peak = (ifelse(avg_speed_peak<25,
                         617.5-16.7*avg_speed_peak,
                         235.0-1.4*avg_speed_peak))*demand_peak_single_total
}

# Calculating total number of emissions
emissions_level = emissions_nonpeak + emissions_peak


print(paste("At", single_price_best,", the total level of emissions is",
            round(emissions_level),"thousand (g/km)"))



#Scenario 2: Maximize revenue with peak pricing strategy. With price for non-peak set at £7, calculate the price for peak period

# Price for non-peak
base_price = 7

# Calculate non_peak and peak maximun surplus
congestion_pricing$surplus_nonpeak = congestion_pricing$Nonpeak_WTP - base_price
congestion_pricing[1:10,]

surplus_peak = matrix(0,N,max_price) 

for (p in 1:max_price){
  for (i in 1:N){
    surplus_peak[i,p]=congestion_pricing[i,2]-p
  }
}
# Rename columns
colnames(surplus_peak)=paste0("p=",1:max_price)

surplus_peak[1:10,]

# Calculate demand for non_peak and peak time periods
# Create empty arrays with the correct dimensions
demand_nonpeak_1 = rep(0,max_price)
demand_peak_1 = rep (0,max_price)
revenue_1 = rep (0,max_price)
emissions_1 = rep (0,max_price)
demand_1 = rep (0,max_price)


# Comparing each client's surplus for each Peak price point p And for each of these price points, counting how many clients will buy NonPeak and how many clients will buy Peak
for (p in 1:max_price){
  surplus_nonpeak = congestion_pricing$surplus_nonpeak
  demand_nonpeak_1[p]=sum((surplus_nonpeak>surplus_peak[,p])*(surplus_nonpeak>=0)) * (192/N)
  demand_peak_1[p]=sum((surplus_peak[,p]>=surplus_nonpeak)*(surplus_peak[,p]>=0)) * (192/N)
  revenue_1[p]=base_price*demand_nonpeak_1[p]*1000+p*demand_peak_1[p]*1000


# Calculating level of emissions at each price point
  avg_speed_nonpeak_1 = 30 - 0.0625*demand_nonpeak_1[p]
  avg_speed_peak_1 = 30 - 0.0625*demand_peak_1[p]

  emissions_nonpeak_1 = (ifelse(avg_speed_nonpeak_1<25,
                            617.5-16.7*avg_speed_nonpeak_1,
                            235.0-1.4*avg_speed_nonpeak_1))*demand_nonpeak_1[p]

  emissions_peak_1 = (ifelse(avg_speed_peak_1<25,
                         617.5-16.7*avg_speed_peak_1,
                         235.0-1.4*avg_speed_peak_1))*demand_peak_1[p]

# Total level of emissions
  emissions_1[p] = emissions_nonpeak_1 + emissions_peak_1

  emissions_1[p]
}

# Calculating revenue
revenue_best_1 = max(revenue_1)
price_best_1 = which(revenue_1 == revenue_best_1)
emissions_best_1 = emissions_1[price_best_1]


print(paste("When non_peak period have a price of £7, the optimal price for peak period is £",price_best_1,"which gives us a revenue of: £",round(revenue_best_1)))
print(paste("The level of emission associated with this pricing strategy is:",round(emissions_best_1),"thousand (g/km)"))



#Scenario 3: Shifting objective to minimize emissions while maintaining a level of revenue per day

df_maxrev= data.frame(peak_price=c(1:max_price),
                      demand_nonpeak_1, demand_peak_1, 
                      revenue_1, emissions_1) 
df_maxrev

# Filter the dataframe based on the revenue constraint
filtered_df = subset(df_maxrev, revenue_1 > 1100000)
filtered_df

# Find the row with the minimum emissions_1
min_emissions_row = filtered_df[which.min(filtered_df$emissions_1), ]

# Extract the peak_price for the row
peak_price_min_emissions = min_emissions_row$peak_price
revenue_min_emissions = min_emissions_row$revenue_1
min_emissions = min_emissions_row$emissions_1
  
# Print the result
peak_price_min_emissions


print(paste("At price (£)", peak_price_min_emissions," we get the minimum emissions level of", round(min_emissions), "thousand (g/km), with the generated revenue of (£)", round(revenue_min_emissions)))
