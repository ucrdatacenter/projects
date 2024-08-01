# Load necessary libraries
library(tidyverse)
library(stringr)
library(dslabs)

# Display the first few rows of reported_heights
head(reported_heights)

# Check the class of the height column
class(reported_heights$height)

# Convert the height column to numeric
x <- as.numeric(reported_heights$height)

# Count NA values in x
sum(is.na(x))

# Add a new column of numeric heights, display first problematic entries
reported_heights |>
  mutate(new_height = as.numeric(height)) |>
  filter(is.na(new_height)) |>
  head(n = 10)

# Identify problematic height entries
problems <- reported_heights |>
  mutate(inches = as.numeric(height)) |>
  filter(is.na(inches) | inches < 50 | inches > 84) |>
  pull(height)
length(problems)

# Detect pattern of feet and inches with apostrophe separator
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) |> head(n = 10) |> cat()

# Detect pattern of feet and inches with comma/period separator
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) |> head(n = 10) |> cat()

# Detect numbers in cm (too tall in inches)
ind <- which(between(as.numeric(problems)/2.54, 54, 81))
problems[ind] |> head(n = 10) |> cat()

# String with single quotes
s <- 'Hello!'

# Using quotation marks inside the string
s <- '10"'
s <- '5\'10"'
s <- "5'10\""

# Detect comma in numbers
str_detect(c("1", "10", "100", "1,000", "10,000"), ",")

# Filter heights that contain "cm"
str_subset(reported_heights$height, "cm")

# Create vectors of yes and no strings
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)

# Detect if any elements in 's' contain either "cm" or "inches"
str_detect(s, "cm") | str_detect(s, "inches")

# Combine into one str_detect function with regex
str_detect(s, "cm|inches")

# Create vectors of yes and no strings
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
# Detect if any elements in 's' contain a digit
str_detect(s, "\\d")

pattern <- "\\d"

# View matched patterns in 's'
str_view(s, pattern)

# View matched patterns in 's', also display strings with no match
str_view(s, pattern, match = NA)

# View matched patterns in 's'
str_view(s, "[56]", match = NA)

# Create vectors of yes and no strings
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
# Detect if any elements in 's' are within the range 4-7
str_detect(s, "[4-7]")

# Define a pattern of a single digit and create vectors of yes and no strings
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
# View matched patterns in 's', ignoring case
str_view(s, pattern, match = NA)

# Define a pattern of 1-2 digits and create vectors of yes and no strings
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
s <- c(yes, no)
# View matched patterns in 's', ignoring case
str_view(c(yes, no), pattern, match = NA)

# Define a pattern of one of 4-7, ', 1-2 digits (like 5'10)
pattern <- "^[4-7]'\\d{1,2}\"$"

# Create vectors of yes and no strings
yes <- c("5'7\"", "6'2\"", "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
# Detect if any elements in 'yes' match the pattern
str_detect(yes, pattern)
# Detect if any elements in 'no' match the pattern
str_detect(no, pattern)

# Check if two strings are identical
identical("Hi", "Hi ")

# Define a pattern like 5' 3"
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
# Subset 'problems' based on the pattern
str_subset(problems, pattern_2)

# Create vectors of yes and no strings
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
# Detect if any elements in 'yes' match the pattern "A1*B"
str_detect(yes, "A1*B")
# Detect if any elements in 'no' match the pattern "A1*B"
str_detect(no, "A1*B")

# Create a dataframe with various detection results
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# Define a pattern of anything but a letter followed by a digit
pattern <- "[^a-zA-Z]\\d"
# Create vectors of yes and no strings
yes <- c(".3", "+2", "-0","*4")
no <- c("A3", "B2", "C0", "E4")
# Detect if any elements in 'yes' match the pattern
str_detect(yes, pattern)
# Detect if any elements in 'no' match the pattern
str_detect(no, pattern)

# Define patterns without groups
pattern_without_groups <- "^[4-7],\\d*$"

# Define patterns with groups
pattern_with_groups <- "^([4-7]),(\\d*)$"

# Create vectors of yes and no strings
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
# Detect if any elements in 's' match the pattern without groups
str_detect(s, pattern_without_groups)
# Detect if any elements in 's' match the pattern with groups
str_detect(s, pattern_with_groups)

# Extract matched patterns in 's'
str_match(s, pattern_with_groups)

# Extract matched patterns in 's'
str_extract(s, pattern_with_groups)

# Define a pattern like 4'11"
pattern <- "^[4-7]'\\d{1,2}\"$"
# Sum up the detection results in 'problems'
sum(str_detect(problems, pattern))

# Subset 'problems' based on the pattern
str_subset(problems, "inches")

# Subset 'problems' based on the pattern
str_subset(problems, "''")

# Define a pattern like 6'1
pattern <- "^[4-7]'\\d{1,2}$"

# Replace "feet", "ft", "foot" with ' and remove all inches symbols in 'problems'
# Then detect if any elements match the pattern and sum the results
problems |>
  str_replace("feet|ft|foot", "'") |> # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") |> # remove all inches symbols
  str_detect(pattern) |>
  sum()

# Define a pattern and replace "feet", "ft", "foot" with ' and remove all inches symbols in 'problems'
# Then detect if any elements match the pattern with whitespace and sum the results
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems |>
  str_replace("feet|ft|foot", "'") |> # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") |> # remove all inches symbols
  str_detect(pattern) |>
  sum()

# Define a pattern and create vectors of yes and no strings
pattern_with_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
# Replace patterns in 's' using groups
str_replace(s, pattern_with_groups, "\\1'\\2")

# Define a pattern with groups like 4   10 or 4,  10
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# Subset 'problems' based on the pattern
str_subset(problems, pattern_with_groups) |> head()

# Subset 'problems' based on the pattern, replace ./,/ with '
str_subset(problems, pattern_with_groups) |>
  str_replace(pattern_with_groups, "\\1'\\2") |> head()

# Define a pattern with lookaheads and lookbehinds
pattern <- "(?=\\w{8,16})(?=^[a-z|A-Z].*)(?=.*\\d+.*)"

# Replace occurrences of "man" preceded by "super" in 's' with "girl"
s <- "Superman saved a man. The man thanked superman."
str_replace_all(s, "(?<=[Ss]uper)man", "girl")

# Create a dataframe 'tab' with a column 'x' containing certain strings
tab <- data.frame(x = c("5'10", "6' 1", "5 ' 9", "5'11\""))

# Separate the strings in 'x' into different columns based on the defined patterns
patterns <- c(feet = "\\d", "\\s*'\\s*", inches = "\\d{1,2}", ".*")
tab |> separate_wider_regex(x, patterns = patterns)

# Create a string 's' and check if it is identical to another string
s <- "Hi "
cat(s)
identical(s, "Hi")

# Remove leading and trailing whitespace from 's'
str_trim("5 ' 9 ")

# Convert 's' to lowercase
s <- c("Five feet eight inches")
str_to_lower(s)

# Define a function 'not_inches_or_cm' to filter out heights outside a certain range
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  is.na(inches) |
    !((inches >= smallest & inches <= tallest) |
        (inches/2.54 >= smallest & inches/2.54 <= tallest))
}

# Filter out heights that are not in inches or cm in 'reported_heights' and count them
problems <- reported_heights |>
  filter(not_inches_or_cm(height)) |>
  pull(height)
length(problems)

# Replace "feet", "ft", "foot" with ' and remove all inches symbols in 'problems'
# Then replace patterns in the modified strings and calculate the mean of the inverse of the detection results
converted <- problems |>
  str_replace("feet|foot|ft", "'") |>
  str_remove_all("inches|in|''|\"") |>
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
index <- str_detect(converted, "^[4-7]\\s*'\\s*\\d{1,2}$")
# Fraction of problems still unresolved
mean(!index)
# Select elements in 'converted' that remain unresolved
converted[!index]

# Load the 'english' library
library(english)

# Define a cleanup function to remove unnecessary characters and convert words to numbers
cleanup <- function(s){
  s <- str_remove_all(s, "inches|in|''|\"|cm|and") |>
    str_trim() |>
    str_to_lower()
  for (i in 0:11) {
    s <- str_replace_all(s, words(i), as.character(i))
  }
  return(s)
}

# Define a function to convert height formats
convert_format <- function(s){
  s |> str_replace("feet|foot|ft", "'") |>
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") |>
    str_replace("^([56])'?$", "\\1'0") |>
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")
}

# Define patterns for feet and inches
patterns <- c(feet = "[4-7](?=\\s*'\\s*)",
              "\\s*'\\s*",
              inches = "\\d+\\.?\\d*")

# Define the smallest and tallest acceptable heights
smallest <- 50
tallest <- 84

# Create a new dataframe 'new_heights' by converting and cleaning the heights in 'reported_heights'
new_heights <- reported_heights |>
  mutate(original = height,
         height = convert_format(cleanup(height))) |>
  separate_wider_regex(height, patterns = patterns,
                       too_few = "align_start",
                       cols_remove = FALSE) |>
  mutate(across(c(height, feet, inches), as.numeric)) |>
  mutate(guess = 12 * feet + inches) |>
  mutate(height = case_when(
    is.na(height) ~ as.numeric(NA),
    between(height, smallest, tallest) ~ height, #inches
    between(height/2.54, smallest, tallest) ~ height/2.54, #cm
    between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    TRUE ~ as.numeric(NA))) |>
  mutate(height = ifelse(is.na(height) &
                           inches <= 12 & between(guess, smallest, tallest),
                         guess, height)) |>
  select(-feet, -inches, -guess)

# Pull the original heights where the converted heights are NA
new_heights |> filter(is.na(height)) |> pull(original)

# Arrange 'new_heights' by height and display the first few elements
new_heights |> arrange(height) |> head(n = 6)
