# First, we load necessary packages from the library
library(tidyverse)
library(dplyr)
library(ggplot2)

# Next, we import the data set named hsad (heatstroke-related ambulance dispatches)
hsad <- read_csv("C:/Users/Julia/OneDrive - UniversityCollegeRoosevelt/Desktop/Data Science Internship/Encounter/Encounter/hsad.csv")

# Then we import a second data set named temperature  
temp <- read_csv("C:/Users/Julia/OneDrive - UniversityCollegeRoosevelt/Desktop/Data Science Internship/Encounter/Encounter/temperature.csv")

# We merge the two data sets together by the columns 'Date' and 'Prefecturename'
merged <- left_join(temp, hsad, by = c("Date", "Prefecturename"))

# We filter out a column called 'Prefecture'. We rename the column 'Prefecturename' to 'Prefecture'
merged <- merged |> 
  select(-Prefecture) |> 
  rename("Prefecture" = Prefecturename)
  
# We can group the data by their Prefecture
merged <- merged |> 
  group_by(Prefecture)

# Now we can investigate the relationship between maximum temperature and hsad per day 
# in one perfecture (Hiroshima) in 2015
hiroshima_2015 <- merged |> 
  filter(Prefecture == "Hiroshima") |> 
  group_by(Date) |> 
  filter(Year == 2015)

# Now we can create a plot to showcase the relationship between Tempmax and HSAD 
ggplot(hiroshima_2015, aes(x = Tempmax, y = HSAD)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Temperature and heatstroke-related ambulance dispatches",
       x = "Maximal Temperature",
       y = "Heatstroke-related Ambulance Dispatches") +
  theme_minimal()

# We can make a second plot, showing temperture over time
ggplot(hiroshima_2015, aes(x = Month, y = Tempmax)) +
  geom_point() +
  labs(title = "Temperature Over Time in Hiroshima in 2015",
       x = "Date",
       y = "Maximal Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# We test if the data for temperature is normally distributed using Shapiro-Wilk normality test
shapiro.test(hiroshima_2015$Tempmax)
# We get that p = 0.1424; p > 0.05, so we can assume the data is normally distributed

# We can test if the data for HSAD is normally distributed using Shapiro-Wilk normality test
shapiro.test(hiroshima_2015$HSAD)
# We get that p = 5.952e-14; p < 0.05, so we can assume the data is not normally distributed

# We can proceed with a non-parametric test 
spearman <- cor.test(hiroshima_2015$Tempmax, hiroshima_2015$HSAD, method = "spearman")

# Spearman’s rho (ρ) = 0.815 indicates a strong positive correlation between Tempmax and HSAD. 
# As temperature increases, heatstroke-related ambulance dispatches also rise. 
# The p-value < 2.2e-16 confirms this relationship is highly statistically significant.

# We can perform a linear regression analysis to confirm the previous results
lr <- lm(HSAD ~ Tempmax, data = hiroshima_2015)
summary <- summary(lr)

# R-squared = 0.6193; this means that about 61.93% of the variability in HSAD is 
# explained by Tempmax. This indicates a moderate to strong relationship between 
# temperature and heat stroke-related ambulance dispatches.





