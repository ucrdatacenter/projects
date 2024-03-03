# We will start with some basic operations in R. For further information, please refer to the R documentation.

# 1. R can be used as a basic calculator. Try the following operations. The output will be printed in the console (below).
# Please note that adding spaces between the numbers and the operators is not necessary, but it makes the code more readable.
# 1.1. Addition
1 + 1

# 1.2. Subtraction
1 - 1

# 1.3. Multiplication
2 * 3

# 1.4. Division
4 / 2

# 1.5. Exponentiation
2 ^ 3

# 2. R can also be used to assign values to variables. Try the following operations. These variables should be added to the environment pane (On the right)
# 2.1. Assign the value 1 to the variable x. Note that the arrow can be typed by pressing Alt + -, as well as by typing <- manually.
x <- 1

# 2.2. Assign the value 2 to the variable y
y <- 2

# 2.3. We can now use these variables in our calculations
x + y
x - y
x * y
x / y
x ^ y

# 3. R can also be used to assign values to vectors. Try the following operations. These vectors should be added to the environment pane (On the right)
# 3.1. Assign the values 1, 2, 3, 4, 5 to the vector x
x <- c(1, 2, 3, 4, 5)
# An alternative way to create this vector is to use the colon operator, which creates a sequence of consecutive numbers.
x <- 1:5

# 3.2. Assign the values 6, 7, 8, 9, 10 to the vector y
y <- c(6, 7, 8, 9, 10)
y <- 6:10

# 3.3. We can now use these vectors in our calculations. Note that these operations are performed element-wise.
x + y
x - y
x * y
x / y
x ^ y

# 3.4. Now we create a vector with a different length. Note that R will give a warning, but it will still perform the operation, reusing the values in the shorter vector.
z <- c(1, 2, 3)
x + z
x - z
x * z
x / z
x ^ z

# 3.5. We can also concatenate vectors together. Note that the order of the vectors and the order in the vectors matter.
c(x, y)
c(y, x)

# 3.6. Sometimes it is useful to find the sum of a vector. We can do this using the sum function.
sum(x)

# 3.7. We can also find the mean of a vector. We can do this using the mean function.
mean(x)

# 3.8. We can also find the length of a vector. We can do this using the length function. This is useful for finding the number of elements in a vector.
length(x)

# 3.9. We can also find the unique elements in a vector. We can do this using the unique function.
unique(x)

# 3.10. If you want to learn more about a function, you can add a question mark before the function name. This will open the documentation for the function (In the bottom right).
# At the bottom of the documentation, there are some examples of how to use the function.
?unique
?sum

# 4. R can also assign characters or strings to variables or vectors.
# 4.1. Assign the string "Hello" to the variable x
x <- "Hello"

# 4.2. Assign the string "World" to the variable y
y <- "World"

# 4.3. When we try adding these two words together, we get an error. This is because R does not know how to add strings together.
x + y

# 4.4. We can concatenate strings together using the paste function.
paste(x, y)

# 4.5. We can also concatenate strings together using the c function. This, however, creates a vector of characters instead of a single string.
c(x, y)

# 5. At UCR, we generally use the tidyverse package a lot to manipulate data. This package contains a lot of useful functions for data manipulation.
# 5.1. We can install the tidyverse package using the install.packages function. You should only need to run this once. If you rerun this notebook, you can comment out this line using a hashtag.
install.packages("tidyverse")

# 5.2. We can then load the tidyverse package using the library function.
library(tidyverse)

# 5.3. We can now use data structures from the tidyverse package. For example, we can create a tibble, which is a type of data frame.
tibble <- tibble(
  name = c("John", "Jane", "Joe"),
  age = c(20, 21, 22)
)

# 5.4. Alternatively, we can first create a vector for each column, and then use the tibble function to create the tibble.
name <- c("John", "Jane", "Joe")
age <- c(20, 21, 22)
tibble <- tibble(name, age)

# 5.5. We can then use the print function to print the tibble.
print(tibble)

# 5.6. We can also use the glimpse function to print the tibble in a different format.
glimpse(tibble)

# 5.7. We can also look at the structure of the tibble using the View function. This will open a new window with the tibble. This is useful for looking at large data sets.
View(tibble)

# 5.8. We can also use the summary function to get a summary of the tibble. This gives some statistics about each numeric column.
summary(tibble)

# 5.9. If we want to look at a specific column, we can use the $ operator. This will return a vector.
tibble$name

# 6. The pipe operator
# 6.1. The pipe operator is a very useful operator in R. It allows us to chain together multiple operations. This makes our code more readable. It can be typed by pressing Ctrl + Shift + M, as well as by typing %>% manually.
tibble %>% 
  View()

tibble %>% 
  glimpse()

# 7. The filter function
# 7.1. The filter function allows us to filter rows in a tibble. This is useful for selecting a subset of the data.
tibble %>% 
  filter(age > 20) %>% 
  View()

# 7.2. The inner workings of the filter function are as follows. The first argument is the tibble. The second argument is the condition. The condition is a logical vector. This means that it is a vector of TRUE and FALSE values. TRUE means that the row is kept, and FALSE means that the row is removed.
# This shows the logical vector that is actually used by the filter function. Rows with TRUE are kept and rows with FALSE are removed.
tibble$age > 20

# 7.3. It is also possible to filter on strings. This is done using the == operator.
tibble %>% 
  filter(name == "John") %>% 
  View()

# 8. The select function
# 8.1. The select function allows us to select columns in a tibble. This is useful for selecting a subset of the data.
tibble %>% 
  select(name) %>% 
  View()

# 9. The mutate function
# 9.1. The mutate function allows us to create new columns in a tibble. This is useful for creating new variables.
tibble %>% 
  mutate(age_squared = age ^ 2) %>% 
  View()

# 10. NA values
# 10.1. NA values are missing values. These are values that are not defined. We can create NA values using the NA function.
NA

# 10.2. We can also create NA values in a tibble.
tibble <- tibble(
  name = c("John", "Jane", "Joe"),
  age = c(20, NA, 22)
)

# 10.3. We can then use the print function to print the tibble.
print(tibble)

# 10.4. If we want to know what rows contain NA values, we can use the is.na function inside the filter.
# Note that this returns a vector of TRUE and FALSE values. TRUE means that the value is NA, and FALSE means that the value is not NA.
is.na(tibble$age)

# This means we can use is.na() inside the filter function to filter rows that contain NA values.
# Note that we define the column age in the is.na() function as we want R to check for NA values in the age column.
tibble %>% 
  filter(is.na(age)) %>% 
  View()

# Then using the "not" operator (!) we can filter rows that do not contain NA values.
tibble %>% 
  filter(!is.na(age)) %>% 
  View()

# 10.5. We can also use the drop_na() function to drop rows that contain NA values.
tibble %>% 
  drop_na(age) %>% 
  View()

# 10.6. A difference between the is.na() function and the drop_na() function is that in is.na() you must specify the column, but in drop_na() you do not need to specify the column (Do keep in mind that this will thus remove all rows that have an NA in any column).
tibble %>% 
  filter(is.na()) %>% 
  View()

tibble %>%
  drop_na() %>%
  View()

# 11. Loading external data
# Data link:
# https://www.carc.ox.ac.uk/XDB/ASP/searchOpen.asp?setResultCheckboxes=chkAlbum&chkAlbum=true&windowWidth=1535&windowHeight=689&search=%20{AND}%20%20[Provenance]%20GREECE%2C%20ATHENS%2C%20AGORA#aHeader

# 11.1. In order to load external data, we first need to get the working directory. This is the folder where R will look for the data. We can do this using the getwd function.
# Save your Beazley Archive data in the folder that is returned by this function. Call the file Beazley_Archive.csv.
getwd()

# 11.2. We can then use the read_csv function to read the data and save it as a variable. This will return a tibble.
data <- read_csv("Beazley_Archive.csv")

# Assignment 1: Based on the tibble we created in step 5, create a new column called age_in_20_years. 
# This should contain the age in 20 years. 
# Then, filter the tibble to only contain rows where the age in 20 years is greater than 40.
# Finally, print the tibble.



# Assignment 2: Using the Beazley Archive data, create two new data sets. Give them descriptive names.
# The first data set should only contain rows using an Mycenean fabric.
# The second data set should only contain rows using Cypriot fabrics (from the bronze age).
# Which data set has more rows?


