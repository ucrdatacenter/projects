library(tidyverse)

# get data from Github (originally from OECD)
gdp <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SSCECON207/2023h1/EU2009data/gdp_oecd.csv") %>%
  filter(MEASURE == "USD_CAP")
unem <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SSCECON207/2023h1/EU2009data/unem_oecd.csv") %>%
  filter(FREQUENCY == "A", SUBJECT == "TOT") %>%
  mutate(TIME = as.numeric(TIME))
infl <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SSCECON207/2023h1/EU2009data/infl_oecd.csv") %>%
  filter(FREQUENCY == "A", MEASURE == "AGRWTH", SUBJECT == "TOT") %>%
  mutate(TIME = as.numeric(TIME))
irate <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SSCECON207/2023h1/EU2009data/i_oecd.csv") %>%
  filter(FREQUENCY == "A") %>%
  mutate(TIME = as.numeric(TIME))
budg <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SSCECON207/2023h1/EU2009data/budg_oecd.csv")
debt <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SSCECON207/2023h1/EU2009data/debt_oecd.csv")

# list of countries for bar charts
eu <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU",
        "GBR", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
        "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "OECDE", "EA19")

# average GDP per country
gdp %>%
  filter(TIME %in% 1999:2007, LOCATION %in% eu) %>%
  arrange(TIME) %>%
  group_by(LOCATION) %>%
  mutate(growth = (Value-lag(Value))/lag(Value)*100) %>%
  summarize(Value = mean(growth, na.rm = TRUE)) %>%
  ggplot(aes(LOCATION, Value)) +
  geom_col() +
  xlab(NULL) + ylab("Average GDP growth 2000-2007 (%)") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))

# Euro Area GDP
gdp %>%
  filter(TIME %in% 1999:2007, LOCATION == "EA19") %>%
  mutate(growth = (Value-lag(Value))/lag(Value)*100) %>%
  ggplot(aes(TIME, growth)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(2000, 2007)) +
  xlab("Year") + ylab("Euro Area GDP growth (%)") +
  theme_light()

# average unemployment rate per country
unem %>%
  filter(TIME %in% 2000:2007, LOCATION %in% eu) %>%
  group_by(LOCATION) %>%
  summarize(Value = mean(Value, na.rm = TRUE)) %>%
  ggplot(aes(LOCATION, Value)) +
  geom_col() +
  xlab(NULL) + ylab("Average unemployment rate 2000-2007 (%)") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))

# Euro Area unemployment rate
unem %>%
  filter(TIME %in% 2000:2007, LOCATION == "EA19") %>%
  ggplot(aes(TIME, Value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(2000, 2007)) +
  xlab("Year") + ylab("Euro Area unemployment rate (%)") +
  theme_light()

# average debt and budget deficit during crisis
bind_rows(budg, debt) %>%
  filter(TIME %in% 2008:2009, LOCATION %in% eu) %>%
  mutate(INDICATOR = ifelse(INDICATOR == "GGDEBT", "Government debt", "Budget deficit")) %>%
  ggplot(aes(LOCATION, Value)) +
  geom_col() +
  facet_grid(~INDICATOR~TIME, scales = "free_y") +
  xlab(NULL) + ylab("% of GDP") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# interest rate line chart
irate %>%
  filter(TIME %in% 2000:2019, LOCATION == "EA19") %>%
  ggplot(aes(TIME, Value)) +
  geom_point() +
  geom_line() +
  xlab(NULL) + ylab("Euro Area interest rate (%)") +
  theme_light()

# GDP & unemployment line charts
bind_rows(gdp, unem) %>%
  filter(LOCATION == "EA19") %>%
  arrange(TIME) %>%
  group_by(INDICATOR) %>%
  mutate(Value = ifelse(INDICATOR == "GDP",
                        (Value-lag(Value))/lag(Value)*100, Value),
         INDICATOR = ifelse(INDICATOR == "GDP",
                            "GPD growth", "Unemployment rate")) %>%
  filter(TIME %in% 2000:2019) %>%
  ggplot(aes(TIME, Value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~INDICATOR, ncol = 1, scales = "free_y") +
  xlab(NULL) + ylab("Euro Area (%)") +
  theme_bw()

# GDP & unemployment line chart per country
bind_rows(gdp, unem) %>%
  filter(LOCATION %in% c("DEU", "ESP", "GRC", "IRL", "PRT")) %>%
  arrange(TIME) %>%
  group_by(INDICATOR, LOCATION) %>%
  mutate(Value = ifelse(INDICATOR == "GDP",
                        (Value-lag(Value))/lag(Value)*100, Value),
         INDICATOR = ifelse(INDICATOR == "GDP",
                            "GPD growth", "Unemployment rate")) %>%
  filter(TIME %in% 2000:2019) %>%
  ggplot(aes(TIME, Value, color = LOCATION)) +
  geom_point() +
  geom_line() +
  facet_wrap(~INDICATOR, ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(2000, 2018)) +
  xlab(NULL) + ylab("%") +
  theme_bw()

# Phillips curves
bind_rows(infl, unem) %>%
  filter(LOCATION %in% c("GRC", "ESP", "IRL", "PRT"), TIME %in% 2000:2016) %>%
  select(LOCATION, INDICATOR, TIME, Value) %>%
  pivot_wider(names_from = INDICATOR, values_from = Value) %>%
  ggplot(aes(HUR, CPI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Unemployment rate") + ylab("Inflation rate") +
  facet_wrap(~LOCATION, scales = "free", dir = "v") +
  theme_bw()
