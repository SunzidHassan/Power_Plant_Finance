###Libraries####


###Functions####

#discount factor function
discount_factor <- function(time, rate){
        n <- 1
        discount <- 1
        while(n < time+1){
                discount <- discount + (1/(1+rate)^n)
                n <- n+1
        }
        return(discount)
}

#discounted sum function
discounted_sum <- function(int_rate, disc_rate, time, starting_amount, step_change){
        left <- starting_amount
        n <- 1
        sum <- 0
        while(n < (time+1)){
                val <- left * int_rate
                sum <- ((val / ((1 + disc_rate) ^ n))
                        + sum)
                left <- left - step_change
                n <- n+1
        }
        return(sum)
}

# sum function
nom_sum <- function(int_rate, time, starting_amount, step_change){
        left <- starting_amount
        n <- 1
        sum <- 0
        while(n < (time+1)){
                val <- left * int_rate
                sum <- val + sum
                left <- left - step_change
                n <- n+1
        }
        return(sum)
}

#discount factored break even
BEY <- function(rate, yearlyP, fixedC){
        n <- 1
        income <- 0
        while(income < fixedC){
                income <- income + (yearlyP/((1+rate)^n))
                n <- n+1
        }
        return(n)
}

###Load data####
#Read constants and variables of power plant

constants <- read.csv("input/constants.csv",
                               stringsAsFactors = F, header = T, comment.char = "")

variables <- read.csv("input/variables.csv",
                      stringsAsFactors = F, header = T, comment.char = "")

###Common variables####

#yearly operation hour
yearly_operation_hour <- variables$daily_operation_hour * variables$yearly_operation_days


#plant lifetime discount factor
plant_lifetime_discount_factor <- discount_factor(constants$plant_lifetime,
                                                  variables$discount_rate)

##Income####

#unit profit
#value factor of MWh produced in lifetime
unit_profit <- variables$unit_electricity_price - (variables$operation_maintenance_cost + variables$fuel_cost)
#unit_profit <- (LUEC+4) - (variables$operation_maintenance_cost + variables$fuel_cost)
#if unit electricity price is less than LUEC+4, the calculation will not converge

#yearly profit
yearly_profit <- (unit_profit * constants$plant_capacity * variables$plant_capacity_factor 
                  * variables$daily_operation_hour * variables$yearly_operation_days)


#lifetime price factor
PV_lifetime_unit_electricity_price_factor <- (constants$plant_capacity * variables$plant_capacity_factor
                                    * yearly_operation_hour
                                    * plant_lifetime_discount_factor)

##Variable costs####

#lifetime MWh variable cost calculation
PV_lifetime_unit_operating_cost <- (constants$plant_capacity * variables$plant_capacity_factor
                   * variables$operation_maintenance_cost
                   * yearly_operation_hour
                   * plant_lifetime_discount_factor)

PV_lifetime_unit_fuel_cost <- (constants$plant_capacity * variables$plant_capacity_factor
              * variables$fuel_cost
              * yearly_operation_hour
              * plant_lifetime_discount_factor)


PV_lifetime_unit_variable_cost <- (PV_lifetime_unit_operating_cost
                                   + PV_lifetime_unit_fuel_cost)


##Fixed costs####

#present value of lifetime capital payment

yearly_capital_payment <- constants$investment / constants$repayment_period

capital_payment_discount_factor <- discount_factor(constants$repayment_period,
                                                   variables$discount_rate)

PV_lifetime_capital_payment <- yearly_capital_payment * capital_payment_discount_factor

# present value of lifetime interest payment

PV_lifetime_interest_payment <- discounted_sum(constants$interest_rate, variables$discount_rate,
                                               constants$repayment_period, constants$investment,
                                               yearly_capital_payment)

#present value of cost of capital

PV_lifetime_costs_of_capital <- PV_lifetime_capital_payment + PV_lifetime_interest_payment

#present value of all fixed costs
PV_fixed_cost <- PV_lifetime_costs_of_capital + constants$decommissioning_and_waste_cost


##Levelized Unit Energy Cost####

LUEC <- ((PV_fixed_cost + PV_lifetime_unit_variable_cost)
         / (PV_lifetime_unit_electricity_price_factor))

#the result is $51.16 (BDT 4.34/unit), whereas the official value is $56.73

##Break even analysis####
#
## nominal break even
nominal_fixed_cost <- (constants$investment 
                       + nom_sum(constants$interest_rate, constants$repayment_period, constants$investment, yearly_capital_payment)
                       +constants$decommissioning_and_waste_cost)
unit_profit <- variables$unit_electricity_price - (variables$operation_maintenance_cost + variables$fuel_cost)
nom_break_even_unit <- nominal_fixed_cost / unit_profit
nom_break_even_year <- nom_break_even_unit / (constants$plant_capacity * variables$plant_capacity_factor 
                                          * variables$daily_operation_hour * variables$yearly_operation_days)

## Present Value adjusted break even
break_even_year <- BEY(variables$discount_rate, yearly_profit, PV_fixed_cost)


##Net Present Value####

NPV <-  ((variables$unit_electricity_price * PV_lifetime_unit_electricity_price_factor)
         - (PV_lifetime_costs_of_capital + PV_lifetime_unit_variable_cost + constants$decommissioning_and_waste_cost))


##internal rate of return####
# for a given price of electricity, calculate the discount rate for which NPV is 0. This is the yearly % return
# compare this discount rate or IRR with project discount rate
IRR <- (NPV
        / (PV_lifetime_costs_of_capital
           + PV_lifetime_unit_variable_cost
           + constants$decommissioning_and_waste_cost))

##Weighted Average Cost of Capital: it's entirely debt financed, so the cost of debt is the fixed WACC####

##Export Data####
results <- cbind(break_even_year, NPV, IRR)

write.csv(results, "output/results.csv")

##Assumptions
# 1. Nominal interest rate includes considerations of future inflation rate, opportunity cost, deferred use discounting etc.
# 2. Payback via 28 Equal payment, with interest enacted on remaining amount

##Further thoughts:
#Evidence backed assumption of constant construction period
#Evidence backed assumption of above 92% PCF: taking variable optimistic, probable and pessimistic values
#Possible inclusions: exchange rate, overhead costs

##To-Do:
#Comparison with alternate power sources

#Further edit todo####
# PV_lifetime_costs_of_capital
# > capital payment: investment amount, repayment_period, discount_rate
# > interest payment: interest_rate, discount_rate, repayment_period, investment amount

# PV_lifetime_unit_variable_cost
# > operation cost: plant_capacity, plant_capacity_factor,
#                        operation_maintenance_cost, yearly_operation_hour, yearly_operation_days,
#                       plant_lifetime, discount_rate
# > fuel cost: fuel_cost

# constants$decommissioning_and_waste_cost

# PV_lifetime_unit_electricity_price_factor
# > plant_capacity, plant_capacity_factor, yearly_operation_hour, plant_lifetime_discount_factor

#investment_amount, repayment_period, discount_rate

# yearly_operation_hour <- function(daily_operation_hour, yearly_operation_days){
#         yearly_operation_hour = daily_operation_hour * yearly_operation_days
#         return(yearly_operation_hour)
# }

#rm()
