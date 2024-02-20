# install.packages("tidyverse")
# install.packages("corrr")
library(tidyverse)
library(corrr)

# load the data for the stringency index and assign it to the object containment
containment <- read_csv("https://github.com/ucrdatacenter/projects/raw/main/SCIBIOM303/2024h1/covid-containment-and-health-index.csv") |>
  # group the data by country code
  group_by(Code) |>
  # calculate the mean stringency index per country
  summarise(avg_containment = mean(containment_index, na.rm = TRUE))

# load the data for the number of COVID cases and assign it to the object covid
covid <- read_csv("https://github.com/ucrdatacenter/projects/raw/main/SCIBIOM303/2024h1/owid-covid-data.csv")

# create a new dataframe merged_data that contains both variables
merged_data <- covid |>
  # rename the column iso_code to Code
  rename(Code = iso_code) |>
  # group the data by country code
  group_by(Code) |>
  # calculate the mean number of new cases per million people per country
  summarise(avg_new_cases_per_million = mean(new_cases_per_million, na.rm = TRUE)) |>
  # join the data with the containment data
  inner_join(containment, by = "Code")

merged_data |>
  # define a plot with the containment index on the x-axis and the number of new cases per million on the y-axis
  ggplot(aes(x = avg_containment, y = avg_new_cases_per_million)) +
  # add points for each country
  geom_point() +
  # add a linear regression line
  geom_smooth(method="lm") +
  # add labels to the axes and a title
  labs(title = "COVID-19 Cases vs Containment Index: 185 countries over 2020-2022",
       y = "Average weekly new cases per million inhabitants",
       x = "Average containment index") +
  # customize the theme
  theme_minimal()

# calculate the correlation matrix
merged_data |>
  correlate()
