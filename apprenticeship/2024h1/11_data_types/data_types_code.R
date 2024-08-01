library(tidyverse)
library(lubridate) # for dates

# Data types --------------------------------------------------------------

# Create a tibble with logical column, coerce it to other types
vectors <- tibble(
 logical = c(TRUE, FALSE, NA),
 integer = as.integer(logical),
 numeric = as.numeric(logical),
 character_log = as.character(logical),
 character_num = as.character(numeric)
)

print(vectors)

# Logical is coerced to numeric
class(c(vectors$logical, vectors$numeric))

# Logical and numeric are coerced to character
class(c(vectors$logical, vectors$numeric, vectors$character_log, vectors$character_num))

# Logical is not numeric or character, just coercible to them
is.numeric(vectors$logical)
is.character(vectors$logical)

# Logical and factor is numbered differently when coerced to numeric
factors <- tibble(
 logical = c(TRUE, FALSE, NA),
 factor = as.factor(logical),
 numeric_log = as.numeric(logical),
 numeric_fac = as.numeric(factor)
)

print(factors)

# Define a character vector
char <- c("x", "x", "x")

# Convert character vector to factor with levels x and y
factor <- factor(char, levels = c("x", "y"))

# Only factor can be coerced to numeric, not character
as.numeric(char)
as.numeric(factor)

# Table counts all factor levels, even with no observations
table(char)
table(factor)

# Order character vector with levels x < y
ordered(char, levels = c("x", "y"))

# Create a vector of dates
dates <- c(ymd(20201001), dmy("31082022"), Sys.Date(), today())
dates

# Convert dates to datetime
as_datetime(dates)

# Convert dates to numeric
as.numeric(dates)

# Reconstruct dates from numeric representation as number of days since base date (01/01/1970)
ymd(19700101) + days(as.numeric(dates))

# Get current date and time
now()

# Convert decimal years to date
date_decimal(c(1990, 1990.1, 1990.5))

# Extract year, quarter, and week from dates
year(dates)
quarter(dates)
week(dates)

# Difference between time and difftime
dates + months(1)
dates + dmonths(1)

# Round down dates to nearest day
floor_date(dates + dmonths(1), unit = "day")

# Create a named vector
v <- c("a" = 1, "b" = 2, "c" = 3)

# Add an attribute 'other_attribute' to 'v'
attr(v, "other_attribute") <- "x"

# Display attributes of 'v'
attributes(v)

# Change names of 'v'
names(v) <- c("a1", "b1", "c1")
v <- setNames(v, c("a2", "b2", "c2"))

# Convert 'v' to a dataframe and add row names as a column
# Note: as_tibble() loses rownames
as.data.frame(v) |> rownames_to_column()

# Get dimensions of 'v'
# vectors always have NULL dimension
dim(v)

# Get dimensions of 'v' as a matrix
# matrices have two dimensions (rows x columns)
dim(as.matrix(v))

# NA: missing
# NaN: not a number
# NULL: special object of length 0
na <- c(NA, NA_integer_, NA_real_, NaN, NULL)

# NULL does not contribute to vector length/content
length(na)
# NA, NaN are all NA, but only NaN is NaN
is.na(na)
is.nan(na)
# object is NULL if it only contains NULL
is.null(na)

# NA and NaN create an observation
tibble(x = NA)
tibble(x = NaN)
# NULL creates tibble with 0 rows, 0 columns
tibble(x = NULL)

# Define list with elements of different types, lengths
l <- list(
  1:3,
  "a",
  c(TRUE, FALSE),
  x = tibble(y = c(2.3, 5.9), z = c(5, 6)),
  l1 = list(l2 = list(1, 2, 3), l3 = list(4, 5))
)

# Access elements of 'l' using different methods
l[1]
l[[1]]
l$x
l$x$y
l["l1"]
l[["l1"]]
l$l1
l$l1$l2
l$l1[["l2"]]
l$l1[[1]]

# convert list to a single vector (character, due to coercion)
unlist(l)

# Create a data frame and a tibble with the same contents
df <- data.frame(x = 1:3, y = letters[1:3])
tbl <- tibble(x = 1:3, y = letters[1:3])

# Compare attributes of 'df' and 'tbl'
attributes(df)
attributes(tbl)

# Get number of rows and columns of 'df'
nrow(df)
ncol(df)

# Get dimensions of 'df'
dim(df)

# Get names of 'df'
names(df)

# Create a complex tibble
d <- tibble("1 problematic variable name" = 1:3,
            y = letters[1:3],
            z = list(c(1:3), c(4:6), c(7:9)),
            tib = list(tibble(x1 = 1:2, y1 = letters[1:2]),
                       tibble(x1 = 3:4, y1 = letters[3:4]),
                       tibble(x1 = 5:6, y1 = letters[5:6])))

# Select a variable; result: tibble
select(d, y)
d[ , "y"]
d[ , 2]

# Pull a variable; result: vector
pull(d, y)
d$y
d[["y"]]
d[[2]]

# Use backquotes for problematic names
select(d, `1 problematic variable name`)
d[ , "1 problematic variable name"]
d[ , 1]

pull(d, `1 problematic variable name`)
d$`1 problematic variable name`
d[["1 problematic variable name"]]
d[[1]]

# Select all variables containing a single lowercase letter
select(d, matches("^[a-z]$"))
# Select which rows and columns to keep
d[2:3, c(1, 3)]

# Unnest 'z' and 'tib' from 'd'
d |> unnest(z)
d |> unnest(tib)
# Default unnest behavior: unnest all list columns (but length mismatch error)
# d |> unnest()

# Functions ---------------------------------------------------------------

# Access function definition
sd

# Defining a simple function: rescale a vector so that all elements are between 0 and 1
rescale01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Use function defined in global environment
rescale01(x = c(1, 4, 5, 8, 10))

# For short functions, use one line and omit the braces
rescale01 <- function(x) (x - min(x)) / (max(x) - min(x))

# Arguments with default values
rescale01 <- function(x = 1:10) {
  (x - min(x)) / (max(x) - min(x))
}

rescale01()
rescale01(1:10)
rescale01(x = 1:10)

# ... argument
commas <- function(...) paste(..., collapse = ", ")
commas(letters[1:10])

# Unexpected results in logical statements
names <- tibble(id = 1:3, name = LETTERS[1:3])

filter_name <- function(data, name) filter(data, name == name)
filter_name(names, name = "B")

filter_name <- function(data, n) filter(data, name == n)
filter_name(names, n = "B")

# Return values with and without return statement
rescale01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

rescale01 <- function(x) {
  result <- (x - min(x)) / (max(x) - min(x))
  result
}

rescale01 <- function(x) {
  result <- (x - min(x)) / (max(x) - min(x))
  return(result)
}

# access result only if assigned to an object
rescale01 <- function(x) {
  result <- (x - min(x)) / (max(x) - min(x))
}

# List returns
rescale01 <- function(x) {
  min <- min(x)
  max <- max(x)
  result <- (x - min) / (max - min)

  # returned list
  list(x = result, original_minimum = min, original_maximum = max)
}

# Access elements of list return
rescale01(1:10)$original_minimum
# better method for computation efficiency
res <- rescale01(1:10)
res$x

# Invisible returns: return object without displaying it
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")

  invisible(df)
}

tibble(x = rep(c(1, 2, NA), times = 4),
       y = letters[1:12]) |>
  show_missings() |>
  drop_na() |>
  show_missings()

## Mutate functions

# input: one vector; output: one vector (same length)
z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

first_upper <- function(x) {
  str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))
  x
}

tibble(x = 1:10,
       y = paste0("test", x)) |>
  mutate(z_score = z_score(x),
         y_upper = first_upper(y))

## Summarize functions

# input: one vector; output: one value
n_missing <- function(x) {
  sum(is.na(x))
}

commas <- function(x) {
  str_flatten(x, collapse = ", ", last = " and ")
}

tibble(x = rep(c(1, 2, NA), times = 4),
       y = letters[1:12]) |>
  summarize(x_missing = n_missing(x),
            y = commas(y))

tibble(x = rep(c(1, 2, NA), times = 4),
       y = letters[1:12],
       group = rep(1:2, each = 6)) |>
  group_by(group) |>
  summarize(x_missing = n_missing(x),
            y = commas(y))

## Dataframe functions

# input: dataframe; output: dataframe

# group_var and mean_var treated as variable names, not function arguments
grouped_mean <- function(df, group_var, mean_var) {
  df |>
    group_by(group_var) |>
    summarize(mean(mean_var))
}

# error: variables not found
tibble(x = 1:20,
       group = rep(1:2, each = 10)) |>
  grouped_mean(group_var = group, mean_var = x)


# embracing arguments means they take on the value defined in the function call
grouped_mean <- function(df, group_var, mean_var) {
  df |>
    group_by({{ group_var }}) |>
    summarize(mean({{ mean_var }}))
}

# works as expected
tibble(x = 1:20,
       group = rep(1:2, each = 10)) |>
  grouped_mean(group_var = group, mean_var = x)

# More examples
count_prop <- function(df, var, sort = FALSE) {
  df |>
    count({{ var }}, sort = sort) |>
    mutate(prop = n / sum(n))
}

diamonds |> count_prop(clarity)

unique_where <- function(df, condition, var) {
  df |>
    filter({{ condition }}) |>
    distinct({{ var }}) |>
    arrange({{ var }})
}

diamonds |> unique_where(cut == "Ideal", color)

## Plot functions

# Define the repetitive part of ggplot code
histogram <- function(df, var) {
  df |>
    ggplot(aes(x = {{ var }})) +
    geom_histogram()
}

histogram(diamonds, carat)

# Add more plot elements, data wrangling
diamonds |>
  filter(color == "G") |>
  histogram(carat) +
  facet_wrap(~cut)

# Add labels with rlang::englue
# variables in {{ }}, other arguments in { }
histogram <- function(df, var, binwidth) {
  label <- rlang::englue("A histogram of {{var}} with binwidth {binwidth}")

  df |>
    ggplot(aes(x = {{ var }})) +
    geom_histogram(binwidth = binwidth) +
    labs(title = label)
}

diamonds |> histogram(carat, 0.1)

## Conditional execution

# Use for early return/stopping/error messages

# Return normal mean if weights are not specified
weighted_mean <- function(x, w = NULL) {
  if (is.null(w)) {
    return(mean(x))
  }
  sum(w * x) / sum(w)
}

# Stop with custom error message
weighted_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length")
  }
  sum(w * x) / sum(w)
}

# Alternatively, use stopifnot for a generic error
weighted_mean <- function(x, w) {
  stopifnot(length(x) == length(w))
  sum(w * x) / sum(w)
}

# Use conditions for different behavior with different arguments
weighted_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(length(x) == length(w), is.logical(na.rm), length(na.rm) == 1)

  # Drop NA values from x and w if na.rm == TRUE
  if (na.rm) {
    na <- is.na(x) | is.na(w)
    x <- x[!na]
    w <- w[!na]
  }

  sum(w * x) / sum(w)
}

# Nested if else
if (this) {
  # do that
} else if (that) {
  # do something else
} else {
  # do a different thing
}

## Functions can find objects defined outside the function

# works if y is defined outside the function (but not advised)
f <- function(x) {
  x + y
}

y <- 5
f(2)

## Functions defined inside the function don't exist outside the function

f <- function(x) {
  res <- x + y
  res
}

y <- 5
f(2)

res


# Iteration, functionals --------------------------------------------------

# Most R functions are vectorized

# Other languages:
results <- NULL
for (i in 1:3) {
  results[i] <- paste("result", i)
}

# R: vectorized function
results <- paste("result", 1:3)

# Vectorized functions don't always give expected results
x <- rnorm(10)
mean(x, trim = c(0, 0.1, 0.2, 0.5))

# To get a list of means with different trim values, iterate explicitly

# One approach: for-loop
means <- NULL; i <- 1
for (t in c(0, 0.1, 0.2, 0.5)) {
  means[i] <- mean(x, trim = t)
  i <- i + 1
}

# Other approach: map()
means <- map(c(0, 0.1, 0.2, 0.5), ~mean(x, trim = .x))

# map() iterates over one vector, gives list
map(c(0, 0.1, 0.2, 0.5), ~mean(x, trim = .x))

# map_dbl/chr/df/etc. gives specified output type (if possible)
map_dbl(c(0, 0.1, 0.2, 0.5), ~mean(x, trim = .x))
map_chr(c(0, 0.1, 0.2, 0.5), ~mean(x, trim = .x))
map_df(c(0, 0.1, 0.2, 0.5), ~tibble(trim = .x,
                                    trimmed_mean = mean(x, trim = .x)))

# map2() family iterates over 2 vectors
n <- c(10, 50, 100, 500)
trims <- c(0, 0.1, 0.2, 0.5)
map2_df(n, trims, ~tibble(n = .x,
                          trim = .y,
                          trimmed_mean = mean(rnorm(.x), trim = .y)))

# pmap() family iterates over n vectors (parallel)
n <- c(10, 50, 100, 500)
means <- c(0, 1, 2, 5)
trims <- c(0, 0.1, 0.2, 0.5)
pmap_df(list(n, means, trims),
        ~tibble(n = ..1,
                mean = ..2,
                trim = ..3,
                trimmed_mean = mean(rnorm(..1, mean = ..2), trim = ..3)))

# Or use reference names with a named list
pmap_df(list(n = n, means = means, trims = trims),
        function (n, means, trims) tibble(
          n = n,
          mean = means,
          trim = trims,
          trimmed_mean = mean(rnorm(n, mean = means), trim = trims)))

# Iterate over a tibble rather than a list
params <- tribble(
  ~ n, ~ min, ~ max,
   1,     0,     1,
   2,    10,   100,
   3,   100,  1000
)
pmap(params, runif)


# Compare methods:

# Draws one random number with mean 0, recycles
tibble(true_mean = 0:5) |>
  mutate(sample = rnorm(1, mean = true_mean))

# Draws one random number per mean
tibble(true_mean = 0:5) |>
  mutate(sample = map_dbl(true_mean, ~rnorm(1, mean = .x)))

# Draws 5 random numbers per mean, creates list column
tibble(true_mean = 0:5) |>
  mutate(sample = map(true_mean, ~rnorm(5, mean = .x))) |>
  unnest()

# Without map(): error
tibble(true_mean = 0:5) |>
  mutate(sample = rnorm(5, mean = true_mean))

## Changing multiple variables at once with across()

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df |> summarize(
  n = n(),
  a = median(a),
  b = median(b),
  c = median(c),
  d = median(d)
)

# Get column medians for columns a to d
df |> summarize(
  n = n(),
  across(a:d, median)
)

# Get column medians for all columns
df |> summarize(
  n = n(),
  across(everything(), median)
)

# Other variable list specifications:

# starts_with("s") # equivalent to regex "^s.*"
# ends_with("s") # equivalent to regex ".*s$"
# contains("s") # equivalent to regex "s"
# matches("s") # equivalent to contains() but "s" can be regex
# where(is.numeric) # all numeric (or other type) variables
# ! negates selectors (e.g. !where(is.numeric))

# Explicit function arguments
df |> summarize(
  n = n(),
  across(everything(), ~mean(., na.rm = TRUE))
)

# More complex functions, overwrite existing variable
df |>
  mutate(across(everything(), ~ (. - min(.)) / (max(.) - min(.))))

# More complex functions, create new variable
df |>
  mutate(across(everything(), list(rescaled = ~ (. - min(.)) / (max(.) - min(.)))))

# Multiple functions
df |> summarize(
  n = n(),
  across(everything(),
         list(mean = ~mean(., na.rm = TRUE),
              median = median),
         .names = "{.fn}_{.col}")
)

## Filtering with if_any() and if_all()

df |> filter(if_any(a:d, ~ . > 0))
df |> filter(if_all(a:b, ~ . > 0))

# across() is equivalent to if_all but less intuitive
df |> filter(across(a:b, ~ . > 0))
