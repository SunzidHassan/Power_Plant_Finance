###Libraries####


###Functions####

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



#Read constants and variables of power plant

constants <- read.csv("input/constants.csv",
                               stringsAsFactors = F, header = T, comment.char = "")

variables <- read.csv("input/variables.csv",
                      stringsAsFactors = F, header = T, comment.char = "")

##common variables####

#yearly operation hour
yearly_operation_hour <- variables$yearly_operation_hour * variables$yearly_operation_days


#plant lifetime discount factor
plant_lifetime_discount_factor <- discount_factor(constants$plant_lifetime,
                                                  variables$discount_rate)

##income####
#value factor of MWh produced in lifetime
PV_lifetime_unit_electricity_price_factor <- (constants$plant_capacity * variables$plant_capacity_factor
                                    * yearly_operation_hour
                                    * plant_lifetime_discount_factor)

##variable costs####

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


##cost of capital####

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



LUEC <- ((PV_lifetime_costs_of_capital + PV_lifetime_unit_variable_cost + constants$decommissioning_and_waste_cost)
         / (PV_lifetime_unit_electricity_price_factor))

#the result is $51.16 (BDT 4.34/unit), whereas the official value is $56.73

#further analysis

#Break even

#Weighted Average Cost of Capital: it's entirely debt financed, so the cost of debt is the fixed WACC

#IF price of per unit electricity is greater than LUEC, then how long will it take to cover fixed costs?

#internal rate of return: return percentage against investment

#in the end: add rm()
