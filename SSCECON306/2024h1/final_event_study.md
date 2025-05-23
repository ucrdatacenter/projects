final_event_study
================
2024-04-08

When selecting the data i started with the data i have available for
M&A. Because i needed a market variable i choose to look at the S&P500,
so from the Acquisitions data i selected only the companies in the
S&P500. Then when i knew what companies i need to look at i downloaded
the needed stock data for them. Also a note, the M&A are only until 2014
so the stock data was downloaded already for the needed time frame

``` r
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.3.3

    ## Warning: package 'tidyr' was built under R version 4.3.3

    ## Warning: package 'purrr' was built under R version 4.3.3

    ## Warning: package 'dplyr' was built under R version 4.3.3

    ## Warning: package 'stringr' was built under R version 4.3.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.1
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


# Import the acquisitions data set

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
# and put it as the observation in the data file under the ticker variable

get_observation <- function(filename) {
  raw_file <- readr::read_csv(filename, show_col_types = FALSE)
  raw_file$ticker <- tools::file_path_sans_ext(basename(filename))
  return(raw_file)
}

# Specify the path for the files
files <- list.files(path = "Data/Raw_data/stock_prices", full.names = TRUE)

# Data frame to stock them
stock_prices <- data.frame()

# Apply function
for (file in files) {
  stock_prices <- bind_rows(stock_prices, get_observation(file))
}
```

``` r
# Clean stock price data and add company names
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
  mutate(ret = (adj_close / lag(adj_close))) |> # calculate daily returns change in the stock's price
  ungroup()


# get the data for the market returns, use SP500
market <- read_csv("data/raw_data/GSPC.csv", show_col_types = FALSE) |>
  select(Date, "Adj Close") |>
  rename(date = Date) |>
  rename(adj_close = "Adj Close") |>
  arrange(date) |> 
  mutate(market_return = (adj_close / lag(adj_close))) |> # calculate daily returns change in the stock's price
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
  slice_max(order_by = event_date) |> # Take the last event of the year to make sure there is enough data before the event happened
  ungroup()


# Join the original data frame with the event dates data frame
df <- df |>
  left_join(event_dates, by = c("company_name" = "company_name", "year" = "year"))
```

    ## Warning in left_join(df, event_dates, by = c(company_name = "company_name", : Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 40435 of `x` matches multiple rows in `y`.
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

# Filter the data to include only rows in the estimation window
# For this study the estimation window is 40 days before the event window.

estimation_window <- df |>
  filter(period >= -40 & period <= -8) |> 
  group_by(company_name, year) |> 
  mutate(
    min_window = min(period), # TO CHECK THAT THE ESTIMATION WINDOWS ARE THE SAME
    max_window = max(period)) |> 
  filter(min_window == -40) |> 
  ungroup()


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
# Use the formulas here (https://www.eventstudytools.com/expected-return-models) to calculate normal(predicted) and abnormal returns using the market model.
df <- df |> 
  group_by(company_name, year) |> 
  mutate(predicted_return = alpha + beta * market_return) |> 
  mutate(abnorm_ret = case_when(
    event_window == 1 ~ ret - predicted_return,
    TRUE ~ 0
  )) |> 
  ungroup()
```

``` r
# Filter the data only for the event window, calculate the cummulative abnormal returns and the standard deviation for each event
filtered_data <- df |>
  filter(event_window == 1) |> 
  group_by(company_name, year) |>
  mutate(cum_ret = sum(abnorm_ret, na.rm = TRUE)) |> # for both tests we just need the total cummulative return
  mutate(std_dev = sd(abnorm_ret, na.rm = TRUE)) |> 
  ungroup()


# For each event calculate the test statistic to see if it is statistically different from zero
test_per_company <- filtered_data |> 
  group_by(company_name, year) |> 
  mutate( test = 
    (cum_ret / n)/(std_dev / sqrt(n))
  ) |> 
  ungroup() |> 
  distinct(company_name, year, test) |>
  arrange(desc(test))
# Here we can see that only 22 events are statistically significant, meaning that only for these events the impact of the M&A is different from zero. 
# Out of the 22 significant events, 12 were higher than expected while 10 were lower than expected


# Now for all companies
# t-test for all companies
filtered_data |>
  distinct(company_name, year, cum_ret) |>
  t.test(cum_ret ~ 1, data = _)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  cum_ret
    ## t = 0.32897, df = 165, p-value = 0.7426
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.01046127  0.01464422
    ## sample estimates:
    ##   mean of x 
    ## 0.002091476

``` r
# These results are highly insignificant showing that the event had a statistically insignificant impact on the cumulative abnormal returns of all events. This can be explained by the fact that the stock market is highly unpredictable so the same event will impact a company's stock prices in completely different ways as it is dependent on so many other factors. 
```

``` r
# Visualize the general trends followed by the stocks on each day
filtered_data |>
  group_by(period) |>
  summarize(abnorm_ret = mean(abnorm_ret, na.rm = TRUE)) |>
  ggplot(aes(period, abnorm_ret)) +
  geom_line() +
  geom_vline(xintercept = 0) +
  labs(x = "Period", y = "Mean Abnormal Return") +
  theme_minimal()
```

![](final_event_study_files/figure-gfm/Visualization-1.png)<!-- -->

``` r
# Visualize how the stocks of all companies behave
# We can see that while some companies have increases in the returns, others have decreseases which in the end collapse to approximatelly zero which may be an explanation of why the effects over all companies are not significant. 
filtered_data |>
  mutate(group = paste(company_name, year)) |>
  ggplot(aes(period, abnorm_ret)) +
  geom_line(aes(group = group)) +
  geom_smooth(se = FALSE) +
  theme_minimal()
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 105 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 105 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](final_event_study_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# Make two subsets for the significant events

sig_comp_poz <- head(test_per_company, 12) #the companies with significantly higher returns than expected in the event window

sig_comp_neg <- tail(test_per_company, 10) #the companies with significantly lower returns than expected in the event window

sig_subset_poz <- filtered_data |> 
 semi_join(sig_comp_poz, by = c("company_name", "year"))

sig_subset_neg <- filtered_data |> 
  semi_join(sig_comp_neg, by = c("company_name", "year"))
```

``` r
# Visualize the general trends in the events that are statistically significant and higher than expected
sig_subset_poz |>
  group_by(period) |>
  summarize(abnorm_ret = mean(abnorm_ret, na.rm = TRUE)) |>
  ggplot(aes(period, abnorm_ret)) +
  geom_line() +
  geom_vline(xintercept = 0) +
  labs(x = "Period", y = "Mean Abnormal Return", title = "Significant Positive Events")
```

![](final_event_study_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#  Visualize the general trends in the events that are statistically significant and lower than expected
sig_subset_neg |>
  group_by(period) |>
  summarize(abnorm_ret = mean(abnorm_ret, na.rm = TRUE)) |>
  ggplot(aes(period, abnorm_ret)) +
  geom_line() +
  geom_vline(xintercept = 0) +
  labs(x = "Period", y = "Mean Abnormal Return", title = "Significant Negative Events")
```

![](final_event_study_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
