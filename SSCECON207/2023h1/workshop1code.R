# load libraries
library(tidyverse)
library(readxl)

# load data
GDP <- read_csv("DP_LIVE_03062023161336958.csv")
CO2 <- read_excel("API_EN.ATM.CO2E.PC_DS2_en_excel_v2_5455036.xls", skip = 3)

# view data
summary(GDP)
summary(CO2)
View(GDP)
View(CO2)

# select which variables to use
CO2_renamed <- CO2 %>%
  select(`Country Code`, `2000`:`2019`) %>% # backticks `` for untidy column names
  rename(LOCATION = `Country Code`) # syntax: new name = old name

# filter for GDP per capita between 2000-2019
GDP_filtered <- GDP %>%
  filter(MEASURE == "USD_CAP" & TIME >= 2000 & TIME <= 2019) %>%
  select(-c(INDICATOR, SUBJECT, MEASURE, FREQUENCY, `Flag Codes`))

# create a new dataframe with only Dutch data
GDP_NL <- GDP_filtered %>%
  filter(LOCATION == "NLD")

# compare the following outcomes:

# calculate Dutch GDP growth
GDP_growth_NL <- GDP_NL %>%
  mutate(growth = (Value-lag(Value))/lag(Value)*100) # lag means the previous observation

# calculate average Dutch GDP
GDP_mean_NL <- GDP_NL %>%
  summarize(mean_GDP = mean(Value))

# growth rates and average GDP per country:

GDP_growth <- GDP_filtered %>%
  group_by(LOCATION) %>%
  mutate(growth = (Value-lag(Value))/lag(Value)*100)

GDP_mean <- GDP_filtered %>%
  group_by(LOCATION) %>%
  summarize(mean_GDP = mean(Value))

# pivot to long format
CO2_long <- CO2_renamed %>%
  pivot_longer(cols = -LOCATION, names_to = "TIME", values_to = "CO2") %>%
  mutate(TIME = as.numeric(TIME)) # recode year to be treated as a number

# pivot to wide format
GDP_wide <- GDP_filtered %>%
  pivot_wider(names_from = TIME, values_from = Value)

# combine datasets with bind_rows() -> in this case doesn't work
bind_rows(GDP_filtered, CO2_long) %>% View()

# use full_join instead
data <- full_join(GDP_filtered, CO2_long) %>%
  na.omit()

# create a new dataframe with only Dutch data
data_NL <- data %>%
  filter(LOCATION == "NLD")

# plot Dutch GDP over time with a line
ggplot(data_NL) +
  geom_line(aes(x = TIME, y = GDP))

# plot Dutch and Belgian GDP in the same line chart
data %>%
  filter(LOCATION %in% c("NLD", "BEL")) %>%
  ggplot() +
  geom_line(aes(x = TIME, y = GDP, color = LOCATION))

# make a scatterplot of GDP and CO2 emissions per country in 2019
data %>%
  filter(TIME == 2019) %>%
  ggplot(aes(x = GDP, y = CO2)) +
  # geom_point() +
  geom_smooth(method = "lm", color = "black") + # add linear trend
  geom_text(aes(label = LOCATION), size = 2) # add country labels to points

# save plot to project folder
ggsave("plot.jpg")
