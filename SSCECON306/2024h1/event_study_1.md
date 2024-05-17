Event_study
================
2024-03-22

When selecting the data i started with the data i have available for
M&A. Because i needed a market variable i choose to look at the S&P500,
so from the Acquisitions data i selected only the variable in the
S&P500. Then when i knew what companies i need to look at i downloaded
the needed stock data for them. Also a note, the M&A are only until 2014
so the stock data was downloaded already for the needed time frame

``` r
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.3.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(ggplot2)
```

``` r
# The company tickers were manually written for the S&P500 companies present in the
# M&A data set for which stock prices are available

company_tickers <- read_delim("Data/Raw_data/company_tickers.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE,show_col_types = FALSE) |> 
  drop_na()


acquisitions <- read_csv("Data/Raw_data/Acquisitions.csv", show_col_types = FALSE) |> 
  select("Acquiring Company", "Deal announced on", "Acquired Company") |>
  rename(company_name = "Acquiring Company", date = "Deal announced on", acquired_company = "Acquired Company") |>
  mutate(date = dmy(date)) |>
  filter(year(date) >= 1995) |> #data before that is very little so just filtered it out
  inner_join(company_tickers, acquisitions, by = "company_name") 
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `date = dmy(date)`.
    ## Caused by warning:
    ## !  1 failed to parse.

``` r
# Because the stock prices data does not have a variable to identify the company by
# I decided to do a function that would take the name of the file (company ticker)
# and put it as the observation in the data file as the ticker


get_observation <- function(filename) {
  raw_file <- readr::read_csv(filename, show_col_types = FALSE)
  raw_file$ticker <- tools::file_path_sans_ext(basename(filename))
  return(raw_file)
}

# Specify the path for the files
files <- list.files(path = "Data/Raw_data/stock_prices", full.names = TRUE)

# Data frame to stock them
stock_prices <- data.frame()

# apply function
for (file in files) {
  stock_prices <- bind_rows(stock_prices, get_observation(file))
}
```

``` r
# clean stock price data and add company names
stock_prices <- stock_prices |>
  rename(date = Date) |>
  select("date", "ticker", "Adj Close") |>
  rename(adj_close = "Adj Close") |>
  inner_join(company_tickers, stock_prices, by = "ticker") 


## Now combine the stock prices and M&A data
clean_data <- full_join(acquisitions, stock_prices, by = c("date", "ticker", "company_name")) |>
  mutate(event = case_when(!is.na(acquired_company) ~ 1, TRUE ~ 0)) |> # creates dummy, if there was an acquisition = 1
  arrange(company_name, date) |> 
  mutate(year = year(date)) |> 
  group_by(company_name, year) |>
  mutate(ret = (adj_close / lag(adj_close) - 1)) |> # calculate daily returns, percentage change in the stock's price
  ungroup()


# get the data for the market returns, use SP500
market <- read_csv("Data/Raw_data/GSPC.csv", show_col_types = FALSE) |>
  select(Date, "Adj Close") |>
  rename(date = Date) |>
  rename(adj_close = "Adj Close") |>
  arrange(date) |> 
  mutate(market_return = (adj_close / lag(adj_close) - 1)) |> # calculate daily returns, percentage change in the stock's price
  select(-adj_close)


clean_data <- clean_data |>
  inner_join(market, by = "date")

# Save the data frame
write.csv(clean_data, file = "Data/clean_data.csv")
# This is the data file with acquisitions and stock prices before any manipulations
```

######################### Start the actual event study

``` r
# To not mess with the clead data assign to other dataframe

df <- read_csv("Data/clean_data.csv", show_col_types = FALSE) |>
  select(-1) |> 
  mutate(year = year(date)) 
```

    ## New names:
    ## • `` -> `...1`

``` r
# Create a data frame of event dates for each company
event_dates <- df |>
  filter(event == 1) |>
  select(company_name, event_date = date) |>
  mutate(year = year(event_date)) |>
  group_by(company_name, year) |>
  slice_min(order_by = event_date) |> # Only take first event of the year, this is
  ungroup() # To ensure that not to many events happen in a small time frame

# Join the original data frame with the event dates data frame
df <- df |>
  left_join(event_dates, by = c("company_name" = "company_name", "year" = "year"))
```

    ## Warning in left_join(df, event_dates, by = c(company_name = "company_name", : Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 20674 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# Create the period variable
df <- df |>
  drop_na(event_date) |> #for some years there is no event so we drop those observations
  group_by(company_name, year) |>
  mutate(row = ifelse(date == event_date, row_number(), NA)) |> # get the row number of when event happened
  mutate(row = ifelse(is.na(row), match(TRUE, date == event_date), row)) |> # make sure the row is equal to event row for whole year
  mutate(period = row_number() - row) |>  #make the period in relationship to the event day
  ungroup() 


# Create event window
df <- df |>
  mutate(event_window = ifelse(period >= -7 & period <= 7, 1, 0)) |>
  filter(period <= 7) |>  # Simply because we are not interested in later days
  group_by(company_name, year) |> 
  mutate(n = sum(event_window)) |> # Number of days in event window
  ungroup() |> 
  filter(n == 15) |>  # Select only event for which we have all days in event window
  drop_na(ret)
```

``` r
# Now we need to get the predicted returns based on the estimation window 
# For this, for each company and each event we need to run a regression of ret on market_return

# Filter the data to include only rows where period <= -8 because this is the estimation window
# Initially i tried to do this within the model but it didn't work
estimation_window <- df |>
  filter(period <= -8)

# Function for linear regression in estimation window
lin_reg <- function(estimation_window, key) {
  model <- lm(ret ~ market_return, data = estimation_window)
  broom::tidy(model) # Tidy data frame
}

# Run and store the model
df_with_lin_reg <- estimation_window |> 
  group_by(company_name, year) |> # Group by event
  group_modify(lin_reg) # Apply lin_reg function to each group

# To calculate the predicted values we need the intercept and slope
# So we get alpha and beta for each event
df_with_coefficients <- df_with_lin_reg |> 
  mutate(alpha = ifelse(term == "(Intercept)", estimate, NA),
         beta = ifelse(term == "market_return", estimate, NA)) |> 
  select(company_name, year, alpha, beta) |> 
  group_by(company_name, year) |> 
  summarize(alpha = first(alpha[!is.na(alpha)]),
            beta = first(beta[!is.na(beta)]))   
```

    ## `summarise()` has grouped output by 'company_name'. You can override using the
    ## `.groups` argument.

``` r
# Join the coefficients with the main dataset to have all necessary information in one place
df <- df |> 
  left_join(df_with_coefficients, by = c("company_name", "year"))
```

``` r
df <- df |> 
  group_by(company_name, year) |> 
  mutate(predicted_return = alpha + beta * market_return) |> 
  mutate(abnorm_ret = case_when(
    event_window == 1 ~ ret - predicted_return,
    TRUE ~ 0
  )) |> 
  mutate(cum_ret = sum(abnorm_ret, na.rm = TRUE)) |>
  mutate(std_dev = sd(abnorm_ret, na.rm = TRUE)) |> 
  ungroup()
```

################################ Visualizations

Basically each event is taken as one entity so for each event i
calculated the SD, then SE, and instead of the mean i take the
cumulative and then with those values I calculate the CI for each event

When plotting however, we need to look at the values in regard to the
period so i take the average over each period

``` r
# filter only needed data and calculate CI
filtered_data <- df |>
  filter(event_window == 1) |>
  mutate(dummy = as.numeric(period > 0 & event_window == 1)) |>
  group_by(company_name, year) |> 
  mutate(SE = std_dev / sqrt(n)) |>
  mutate(
    low_CI = cum_ret - 1.96 * SE,
    upp_CI = cum_ret + 1.96 * SE
  ) |> 
  ungroup() |> 
  drop_na(low_CI) |> # Otherwise can't calculate the mean per period
  group_by(period) |>
  mutate(mean_cum_ret = mean(cum_ret), na.rm = TRUE) |>
  mutate(mean_cum_ret = mean_cum_ret * 100, # Convert mean_cum_ret to percentage
         mean_up_CI = mean(upp_CI), na.rm = TRUE) |> 
  mutate(mean_up_CI = mean_up_CI * 100, # Convert mean_up_CI to percentage
         mean_low_CI = mean(low_CI), na.rm = TRUE) |> 
  mutate(mean_low_CI = mean_low_CI * 100) # Convert mean_low_CI to percentage
```

``` r
#Locally estimated scatter plot smoothing
ggplot(filtered_data, aes(x = period, y = mean_cum_ret)) +
  geom_line() +
  geom_vline(xintercept = 0) +
  labs(x = "Period", y = "Mean Cumulative Abnormal Return") +
  theme_minimal() +
  geom_smooth(aes(x = period, y = mean_cum_ret, group = dummy), method = "loess", formula = y ~ x)
```

![](event_study_1_files/figure-gfm/Plot%201-1.png)<!-- -->

``` r
# Plot again with CI
ggplot(filtered_data, aes(x = period, y = mean_cum_ret)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_low_CI, ymax = mean_up_CI), alpha = 0.2) +
  geom_vline(xintercept = 0, color = "blue") +
  labs(x = "Period", y = "Mean Cumulative Return") +
  theme_minimal()
```

![](event_study_1_files/figure-gfm/Plot%202-1.png)<!-- -->

``` r
# Another type of graph commonly used but the geom_segment does not work as intended. 
ggplot(filtered_data, aes(x = period, y = mean_cum_ret)) +
  geom_vline(xintercept = 0, color = "blue") +
  geom_point(color = "darkgreen", size = 3) + 
  geom_segment(aes(x = period, y = mean_low_CI, xend = period, yend = mean_up_CI), 
               linewidth = 0.5, lineend = "butt") + 
  labs(x = "Period", y = "Mean Cumulative Return") +
  theme_minimal()
```

![](event_study_1_files/figure-gfm/Plot%203-1.png)<!-- -->
