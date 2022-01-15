library(readxl)


constants <- read.csv("input/constants.csv",
                               stringsAsFactors = F, header = T, comment.char = "")


variables <- read.csv("input/variables.csv",
                      stringsAsFactors = F, header = T, comment.char = "")

n <- 1
discount_factor <- 0

while (n < (constants$plant_lifetime+1)) {
     discount_factor <- discount_factor + (1/(1+constants$discount_rate))^n
     n <- n+1
}


operating_cost <- (constants$annual_electricity_generation * variables$operation_maintenance_cost
                    * variables$plant_capacity_factor * 10^6)

fuel_cost <- (constants$annual_electricity_generation * variables$fuel_cost
              * variables$plant_capacity_factor * 10^6)

total_cost <- operating_cost + fuel_cost


present_value_electricity <- constants$plant_lifetime * constants$annual_electricity_generation * discount_factor * 1000/24

interest <- constants$investment * constants$interest

yearly_payment <- constants$investment / constants$repayment_period


left <- constants$investment
n <- 1
totalintPV <- 0
intPV <- 0

while (n < (constants$repayment_period+1)) {
     interestVal <- left*constants$interest
     intPV <- interestVal / ((1+constants$discount_rate) ^ n)
     totalintPV <- totalintPV + intPV
     n <- n+1
}

LUEC <- 