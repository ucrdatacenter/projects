# Define vectors
v1 <- c("A", "B", "C")
v2 <- 25
v3 <- 1:10

# Access elements of vectors
v1[1]
v3[2:5]
v3[-c(2, 4, 6)]

# Load tidyverse library
library(tidyverse)

# Define vectors within the tibble() function
tibble(
  name = c("Alice", "Bob", "Chris"),
  height = c(165, 180, 175)
)

# Define the vectors first, then combine them into a tibble
name <- c("Alice", "Bob", "Chris")
height <- c(165, 180, 175)
tibble(name, height)

# Define a vector
x <- c(1, 5, 6, 2, 1, 8)

# Calculate sum of x and assign it to a variable sum
sum(x)
sum <- sum(x)

# Calculate mean, length, and unique elements of the vector
mean(x) # return the mean; add the argument na.rm = TRUE if missing values should be excluded
length(x) # give the length of the vector (number of elements)
unique(x) # list the unique elements of the vector

# Read survey data from a CSV file
surveys <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/apprenticeship/1_intro/surveys.csv")

# Provide summary statistics of the data
summary(surveys)

# Inspect the structure of the data
str(surveys)

# Select certain columns from the data
select(surveys, plot_id, species_id, weight)

# Exclude certain columns from the data
select(surveys, -record_id, -species_id)

# Filter data based on a condition
filter(surveys, year == 1995)

# Filter data and rename variables
surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)

# Filter data, rename variables and pipe operations together
surveys_sml <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

# Print the filtered and selected data
surveys_sml

# Convert weight to kilograms
surveys %>%
  mutate(weight_kg = weight / 1000)

# Convert weight to both kilograms and pounds
surveys %>%
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2)

# Convert weight to kilograms and display the first few rows
surveys %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

# Exclude NA weights, convert to kilograms and display the first few rows
surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

# Group by sex and calculate mean weight
surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# Group by sex and species_id, calculate mean weight, and display the last few rows
surveys %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
  tail()

# Exclude NA weights, group by sex and species_id, and calculate mean weight
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight))

# Exclude NA weights, group by sex and species_id, calculate mean weight, and print the first 15 rows
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight)) %>%
  print(n = 15)

# Exclude NA weights, group by sex and species_id, calculate mean weight and minimum weight
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight))

# Exclude NA weights, group by sex and species_id, calculate mean weight and minimum weight, and arrange by minimum weight
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>%
  arrange(min_weight)

# Exclude NA weights, group by sex and species_id, calculate mean weight and minimum weight, and arrange by mean weight in descending order
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>%
  arrange(desc(mean_weight))

# Count the number of animals in each sex
surveys %>%
  count(sex)

# Group by sex and count the number of observations
surveys %>%
  group_by(sex) %>%
  summarize(count = n())

# Count the number of animals in each sex and sort the result
surveys %>%
  count(sex, sort = TRUE)

# Count the number of animals in each sex and species
surveys %>%
  count(sex, species)

# Count the number of animals in each sex and species and arrange by species and count in descending order
surveys %>%
  count(sex, species) %>%
  arrange(species, desc(n))

# Count the number of animals in each plot_type
surveys %>%
  count(plot_type)

# Exclude NA weights, group by species_id, and calculate mean, minimum, maximum, and count of hindfoot_length
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(species_id) %>%
  summarize(
    mean_hindfoot_length = mean(hindfoot_length),
    min_hindfoot_length = min(hindfoot_length),
    max_hindfoot_length = max(hindfoot_length),
    n = n()
  )

# Exclude NA weights, group by year, and select the heaviest animal measured in each year
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  select(year, genus, species, weight) %>%
  arrange(year)

# Filter out missing weights, hindfoot_length, and sex
surveys_complete <- surveys %>%
  filter(!is.na(weight),          # remove missing weight
         !is.na(hindfoot_length), # remove missing hindfoot_length
         !is.na(sex))             # remove missing sex

# Count the number of each species
species_counts <- surveys_complete %>%
  count(species_id) %>%
  filter(n >= 50)

# Keep only the most common species
surveys_complete <- surveys_complete %>%
  filter(species_id %in% species_counts$species_id)

# Drop all observations where at least one variable is missing
surveys %>%
  drop_na() %>%
  nrow()

# Drop all observations where at least one of the listed variables is missing
surveys %>%
  drop_na(weight, hindfoot_length) %>%
  nrow()
