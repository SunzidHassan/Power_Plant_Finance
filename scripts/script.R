library(readxl)


constants -> read.csv("input/constants.csv",
                               stringsAsFactors = F, header = T, comment.char = "")

plant_capacity = 2400
investment = 11400000000
plant_lifetime = 60
interest = .04
repayment_period = 28
tax_rate = .25
discount_rate = .1
decommissioning_and_waste_cost = 1000000000
annual_electricity_generation = 21024000
discount_factor = 1+(1/1.1)^1+(1/1.1)^2+(1/1.1)^3+(1/1.1)^4+(1/1.1)^5+(1/1.1)^6
                    +(1/1.1)^7+(1/1.1)^8+(1/1.1)^9+(1/1.1)^10+(1/1.1)^11+(1/1.1)^12
                    +(1/1.1)^13+(1/1.1)^14+(1/1.1)^15+(1/1.1)^16+(1/1.1)^17+(1/1.1)^18
                    +(1/1.1)^19+(1/1.1)^20+(1/1.1)^21+(1/1.1)^22+(1/1.1)^23+(1/1.1)^24
                    +(1/1.1)^25+(1/1.1)^26+(1/1.1)^27+(1/1.1)^28+(1/1.1)^29+(1/1.1)^30
                    +(1/1.1)^31+(1/1.1)^32+(1/1.1)^33+(1/1.1)^34+(1/1.1)^35+(1/1.1)^36
                    +(1/1.1)^37+(1/1.1)^38+(1/1.1)^39+(1/1.1)^40+(1/1.1)^41+(1/1.1)^42
                    +(1/1.1)^43+(1/1.1)^44+(1/1.1)^45+(1/1.1)^46+(1/1.1)^47+(1/1.1)^48
                    +(1/1.1)^49+(1/1.1)^50+(1/1.1)^51+(1/1.1)^52+(1/1.1)^53+(1/1.1)^54
                    +(1/1.1)^55+(1/1.1)^56+(1/1.1)^57+(1/1.1)^58+(1/1.1)^59+(1/1.1)^60


construction_years
discount_rate
operation_maintenance_cost
fuel_cost
plant_capacity_factor
bd_inflation_rate
us_inflation rate
bd_us_exchange_rate

operating_cost = 