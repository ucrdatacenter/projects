# load tidyverse library
library(tidyverse)

# read data from the Data Center Github (originally downloaded form FRED)
d <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SSCECON207/2023h1/US2001data.csv") %>%
  # make sure all variables are treated as numbers
  mutate(across(-DATE, as.numeric))

# define custom plotting function for line charts
plot_ts <- function(variable, name) {
  d %>%
    rename("y" = variable) %>%
    drop_na(y) %>%
    filter(DATE >= as.Date("1993-01-01"),
           DATE <= as.Date("2003-01-01")) %>%
    ggplot(aes(DATE, y)) +
    geom_line() +
    xlab("") + ylab(name) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_light()
}

# investment plots
plot_ts("NASDAQCOM", "b0: NASDAQ index")
plot_ts("GPDI", "I: Gross personal\ndomestic investment")

# consumption plots
plot_ts("UMCSENT", "c0: Consumer sentiment")
plot_ts("PCE", "C: Private consumption")

# government spending plots
plot_ts("FYFSGDA188S", "Federal government\nsurplus/deficit\n(% of GDP)")

d %>%
  # define taxes and government spending as percentage of GDP
  mutate(t = FGRECPT/GDPC1*100, g = FGEXPND/GDPC1*100) %>%
  select(DATE, t, g) %>%
  setNames(c("DATE", "T: Federal government receipts", "G: Federal government expenditures")) %>%
  # transform T and G to long format
  pivot_longer(-DATE) %>%
  filter(DATE >= as.Date("1993-01-01"), DATE <= as.Date("2003-01-01")) %>%
  ggplot(aes(DATE, value, color = name)) +
  geom_line() +
  xlab("") + ylab("% of GDP") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_light() +
  theme(legend.position = "bottom", legend.title = element_blank())

# interest rate plot
plot_ts("FEDFUNDS", "i: Federal funds rate")

# GDP plot
plot_ts("GDPC1", "Real GDP")
