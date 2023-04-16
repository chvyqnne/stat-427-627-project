library(tidyverse)
data <- read_csv("gender1.csv")
data <- filter(data, Year > 1994)

tour1 <- read_csv("tourism1.csv")
tour1 <- pivot_longer(tour1, cols = 3:29, names_to = "Year", values_to = "values")
tour1 <- pivot_wider(tour1, names_from = "Indicator", values_from = "values")

tour2 <- read_csv("tourism2.csv")
tour2 <- pivot_longer(tour2, cols = 3:29, names_to = "Year", values_to = "Expenditures(USMillion)")
tour2 <- select(tour2, -Indicator)

tour <- full_join(tour1, tour2, by = c("Country", "Year"))
tour <- mutate(tour, Year = as.double(Year))
data <- full_join(data, tour, by = c("Country", "Year"))

write_csv(data, "data.csv")
