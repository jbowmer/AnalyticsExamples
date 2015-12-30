#Auto Fuel Efficiency Dataset

#Import Data:

setwd("/Users/Jake/Projects/AnalyticsExamples/AutoFuel")

vehicles = read.csv("vehicles.csv", stringsAsFactors = F)

head(vehicles)

#Exploratory analysis:
nrow(vehicles) #36872
ncol(vehicles) #83

#column names
names(vehicles)

#unique years represented:
unique(vehicles$year)
#number of unique years
length(unique(vehicles$year)) #33

min(vehicles$year) #1984
max(vehicles$year) #2016

#types of fuel:
table(vehicles$fuelType1)

#Transmission - automatic or manual?
vehicles$trany[vehicles$trany == ""] = NA

vehicles$trany2 = ifelse(substr(vehicles$trany, 1, 4) == "Auto", "Auto", "Manual")
vehicles$trany = as.factor(vehicles$trany)
table(vehicles$trany2)

#Does MPG change over time?
library(dplyr)
library(ggplot2)

#Split/Apply/Combine -> what has happened to the average miles per gallon over the years?
mpgByYear = group_by(vehicles, year)
mpgByYear = summarise(mpgByYear,
                      avgMPG = mean(comb08),
                      avgHighway = mean(highway08),
                      avgCity = mean(city08))

#now we pass to ggplot to see whats happening:
ggplot(mpgByYear, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("Milage over time")

#What if this huge increase in average MPG was due to new hybrid and electric cars?
#We reexamine with the focus on gas cars.

gasCars = filter(vehicles, fuelType1 == "Regular Gasoline" | fuelType1 == "Premium Gasoline" | fuelType1 == "Midgrade Gasoline")
head(gasCars)

mpgByYear_gas = group_by(gasCars, year)
mpgByYear_gas = summarise(mpgByYear_gas,
                      avgMPG = mean(comb08),
                      avgHighway = mean(highway08),
                      avgCity = mean(city08))

ggplot(mpgByYear_gas, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("Milage over time")

#Relationship between Large Engines and Efficiency:
gasCars$displ = as.numeric(gasCars$displ)
ggplot(gasCars, aes(displ, comb08)) + geom_point() 

avgCarSize = group_by(gasCars, year)
avgCarSize = summarise(avgCarSize, avgDispl = mean(displ))
ggplot(avgCarSize, aes(year, avgDispl)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average engine displacement (l)")

#Lets combine average displacement and average MPG into one dataframe. We can combine the dataframes using rbind or simply split apply combine again...

byYear = group_by(gasCars, year)
byYear = summarise(byYear, avgMPG = mean(comb08), avgDispl = mean(displ))

#Convert from wide to tall format to use the faceting operation in ggplot.
library(reshape2)
byYear = melt(byYear, id = "year")

#vertical wrap
ggplot(byYear, aes(year, value)) + geom_point() + geom_smooth() + facet_wrap(~variable, ncol = 1, scales = "free_y") + xlab("Year") + ylab("")
#horiontal wrap
ggplot(byYear, aes(year, value)) + geom_point() + geom_smooth() + facet_wrap(~variable, scales = "free_y") + xlab("Year") + ylab("")

#Proportion of manual vs automatic cars over time:

gasCars1 <- filter(gasCars, cylinders == "4")
ggplot(gasCars1, aes(factor(year), fill = factor(trany2))) + 
  geom_bar(position = "fill") + labs(x = "Year", y = "Proportion of cars", fill = "Transmission") + 
  scale_fill_brewer(palette = 'Pastel2')

#rcolorbrewer palettes: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3

#What about car makes over the years?

commonCars = filter(vehicles, make == "Ford" | make == "Nissan" | make == "Honda" | make == "Toyota")

avgMPG_commonMakes = group_by(commonCars, year, make)
avgMPG_commonMakes = summarise(avgMPG_commonMakes, avgMPG = mean(comb08))

ggplot(avgMPG_commonMakes, aes(year, avgMPG)) + geom_line() + facet_wrap(~make, nrow = 2) +scale_fill_brewer(palette = 'Pastel2')

