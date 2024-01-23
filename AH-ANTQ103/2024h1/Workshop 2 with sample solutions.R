# In this workshop, we will expand on some of the basic functions, use tidyverse more and introduce some archaeology specific functions
# It will follow the flow of a more typical data analysis workflow

# First, we will load the tidyverse package
library(tidyverse)

# We then load the data that we downloaded previous workshop

data <- read_csv("Beazley_Archive.csv")

# We start by looking at the data
# We can use the head() function to look at the first 6 rows of the data
head(data)

# We can also use the glimpse() function to look at the data
glimpse(data)

# We can also use the summary() function to look at the data, although this is less useful for this dataset due to the large amount of text data
summary(data)

# We can get the names of the columns in the data using the names() function
names(data)

# We can also get the number of rows and columns in the data using the dim() function
dim(data)
# Alternatively, we can also get the number of rows and columns in the data using the nrow() and ncol() functions
nrow(data)
ncol(data)

# After looking at the data, we have decided that we don't need the 9th and the 12th through the 29th columns, so we remove them.
# We can do this using the select() function

data_short <- data %>% 
  select(-9, -12:-29)

# We might want to check if there are multiple entries for the same object. Luckily there is a Vase Number column, so we can check if there are multiple entries for the same vase number.
# We can do this using the duplicated() function. Note that if there is a space in the column name, we need to use backticks around the column name.
# We can then use the filter() function to filter the data to only include the duplicated rows.

data_short %>% 
  filter(duplicated(`Vase Number`))
# Given that the tibble that this returns is empty, we can conclude that there are no rows with the same vase number.

# We can also check if there are any rows with missing data. We can do this using the is.na() function. This returns a logical vector, which we can then use to filter the data.

data_short %>% 
  filter(is.na(`Vase Number`))
# Given that the tibble that this returns is empty, we can conclude that there are no rows with missing data.

# Now that we know that there are no rows with missing data, and no rows with the same vase number, we can start looking at the data in more detail.
# We are interested in the decorations on the vases, so we will look at the Decoration column.

data_short %>% 
  select(Decoration)

# This is text data. An interesting question may be what the most common words are in the Decoration column. These would correspond with the most common decorations.
# We can do this using the unnest_tokens() function. This function takes a column of text data, and splits it into individual words. It then returns a tibble with the words in a column called word.
# The unnest_tokens() function is a part of the tidytext package, which we must download and load before we can use the function.

# install.packages("tidytext")
library(tidytext)

data_words <- data_short %>% 
  unnest_tokens(word, Decoration)

# We can now look at the data

data_words %>% 
  View()

# We can also look at the most common words. We can do this using the count() function. This function takes a tibble, and counts the number of times each value occurs in a column. It then returns a tibble with the values in a column called n.
# We can then use the arrange() function to sort the tibble by the n column. We can use the desc() function to sort the tibble in descending order.

data_words %>% 
  count(word) %>% 
  arrange(desc(n))

# We can already see that there are some NA values. We can remove these easily using the drop_na() function.

data_words %>% 
  drop_na(word) %>% 
  count(word) %>% 
  arrange(desc(n))

# There are still some words that are not interesting for our analysis. These are called stop words. We can remove these using the anti_join() function. This function takes two tibbles, and returns a tibble with the rows from the first tibble that are not in the second tibble.
# The stop words are in a tibble in the tidytext package

stop_words %>% 
  View()

# Alternatively we can define our own custom stop words.

tibble <- tibble(
  word = c("a", "and", "with", "an", "or", "the", "of", "to", "in", "for", "on", "at", "from", "by", "about", 
            "as", "into", "like", "through", "after", "over", "between", "out", "against", "during", "without", "before", "under",
            "around", "among"),
)

# We can then use the anti_join() function to remove the stop words from the data.

word_counts <- data_words %>% 
  drop_na(word) %>%
  anti_join(tibble, by = "word") %>%
  count(word) %>%
  arrange(desc(n)) %>% 
  print()

# Here we still have the letter "b". We may decide to remove all words that are only one letter long. We can do this using the filter() function.
# The nchar() function returns the number of characters in a string. We can use this to filter the data.

word_counts <- word_counts %>% 
  filter(nchar(word) > 1) %>% 
  print()

# Now we might want to create a plot of the most common words. We can do this using the ggplot() function. This function takes a tibble, and creates a plot.
# We will delve into ggplot in more detail in a later workshop, but for now we will use it to create a bar plot of the most common words.

ggplot(word_counts, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

# This obviously is not a very useful plot, as there are too many words. We can filter the data to only include the 20 most common words.
# We can do this using the top_n() function. This function takes a tibble, and a number. It then returns a tibble with the number of rows specified in the second argument.

word_counts_top_20 <- word_counts %>% 
  top_n(20, n) %>% 
  print()

# We can then use this tibble to create a plot.

ggplot(word_counts_top_20, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()


# For the homework, the separate function is essential. This function takes a tibble, a column name, a new column name and a separator. It then splits the column into two (or more) columns, and returns a tibble with the new columns.
# Here I give a silly example of how to use this function. It will be actually useful in the homework.
# If we take a look at the decoration column, we can see that some vases have an entry that is one word, then a colon and then more words. Imagine we want to isolate the first word and the words after the colon.
# We can do this using the separate() function, by setting the separator to ":".

data_short %>% 
  separate(Decoration, c("Decoration 1", "Decoration 2"), sep = ":") %>% 
  View()



# Homework assignments ----------------------------------------------------

# In the homework assignment, if you cannot get the plot to work, you may print the tibble with the same information instead. Do still leave your attempt at the plot code, so that I can see what you tried.
# It should, however, be straightforward to adapt the code from the lecture to create the plots.

# 1. Create a plot of the most common descriptions of the vases (As we did in the tutorial) in a new dataset. Use this dataset:
# https://www.carc.ox.ac.uk/XDB/ASP/searchOpen.asp?setResultCheckboxes=chkAlbum&chkAlbum=true&windowWidth=1535&windowHeight=689&search=%20%7BAND%7D%20%20%5BProvenance%5D%20GREECE%2C%20AEGINA#aHeader
# Reuse the code from the lecture to create the plot.

data2 <- read_csv("Beazley_Archive_2.csv")

data2_short <- data2 %>% 
  select(-9, -12:-29)

data2_words <- data2_short %>%
  unnest_tokens(word, Decoration)

word_counts2 <- data2_words %>% 
  drop_na(word) %>%
  anti_join(tibble, by = "word") %>%
  count(word) %>%
  arrange(desc(n)) %>% 
  print()

word_counts2 <- word_counts2 %>%
  filter(nchar(word) > 1) %>% 
  print()

word_counts2_top_20 <- word_counts2 %>% 
  top_n(20, n) %>% 
  print()

ggplot(word_counts2_top_20, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Not required for the assignment, but putting the old and new plot next to each other can be useful to compare them.
library(patchwork)

ggplot(word_counts_top_20, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Old plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  plot_layout(ncol = 2) +
  ggplot(word_counts2_top_20, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("New plot") +
  theme(plot.title = element_text(hjust = 0.5))

# Overlaying the plots can also be useful to compare them.

word_counts_top_20$dataset <- "Old"
word_counts2_top_20$dataset <- "New"

word_counts_combined <- rbind(word_counts_top_20, word_counts2_top_20)

ggplot(word_counts_combined, aes(x = reorder(word, n), y = n, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()

# This does not really make a lot of sense, as the new dataset is much larger than the old dataset. We can fix this by dividing the n column by the number of rows in the dataset.

word_counts_top_20$n <- word_counts_top_20$n / sum(word_counts_top_20$n)
word_counts2_top_20$n <- word_counts2_top_20$n / sum(word_counts2_top_20$n)

word_counts_combined <- rbind(word_counts_top_20, word_counts2_top_20)

ggplot(word_counts_combined, aes(x = reorder(word, n), y = n, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  # These extra lines are just to make the plot look nicer
  labs(x = "Word", y = "Count", title = "Most common words in two Beazley Archive datasets") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# 2.1. Create a plot of the most common countries of origin of the vases in a new dataset. Use the dataset from 1.1.
# You will need to split a column. 
# Hint: The separator you want to use is either a comma or a space. You can use this expression to do that "[,\\s]".
?separate

data2 <- read_csv("Beazley_Archive_2.csv")

data2 <- data2 %>% 
  filter(!is.na(Provenance))

# We now split the Provenance column into two columns. One for the country and one for the rest
data2 <- data2 %>% 
  separate(Provenance, into = c("Country", "Provenance"), sep = "[,\\s]", remove = FALSE) %>% 
  select(-Provenance) # We don't need the Provenance column anymore, so we remove it

# We then get the counts for each Country
data2_counts <- data2 %>% 
  count(Country) %>% 
  arrange(desc(n))

# We then create a plot, similar to the one we created in the tutorial.
data2_counts %>%
  ggplot(aes(x = reorder(Country, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # These extra lines are just to make the plot look nicer
  labs(x = "Country", y = "Count", title = "Country of origin of Beazley Archive 2") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# 2.2. Use the code you wrote for 1.1 to create a plot of the most common countries of origin of the vases in the original dataset. Adapting the code should be very straightforward.
# This plot will not be very useful, as there are only two countries in the dataset. You can still create the plot though.

data <- read_csv("Beazley_Archive.csv")

data <- data %>% 
  filter(!is.na(Provenance))

# We now split the Provenance column into two columns. One for the country and one for the rest
data <- data %>% 
  separate(Provenance, into = c("Country", "Provenance"), sep = "[,\\s]", remove = FALSE) %>% 
  select(-Provenance) # We don't need the Provenance column anymore, so we remove it

# We then get the counts for each Country
data_counts <- data %>% 
  count(Country) %>% 
  arrange(desc(n))

# We then create a plot, similar to the one we created in the tutorial.
data_counts %>%
  ggplot(aes(x = reorder(Country, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # These extra lines are just to make the plot look nicer
  labs(x = "Country", y = "Count", title = "Country of origin of Beazley Archive") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# 3.1. Create a plot of the most common shapes of the vases in a new dataset. Use the dataset from 1.1.

data2 <- read_csv("Beazley_Archive_2.csv")

# Splitting the shapes column into two columns. One for the shape and one for the rest
data2 <- data2 %>% 
  separate(`Shape Name`, into = c("Shape", "Shape2"), sep = "[,\\s]", remove = FALSE) %>% 
  select(-Shape2)

# Getting the counts
data2_counts <- data2 %>%
  count(Shape)

# Removing NA values and getting the top 10 shapes
top_shapes2 <- data2_counts %>% 
  filter(!is.na(Shape)) %>%
  top_n(10, n)

# Plotting the top 10 shapes
top_shapes2 %>% 
  ggplot(aes(x = reorder(Shape, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Shape", y = "Count", title = "Top 10 Shapes in Beazley Archive 2") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# 3.2. Create a plot of the most common shapes of the vases in the original dataset. Use the dataset from 2.2. Create one plot that includes the top 10 vases from Cyprus and one plot that includes the top 10 vases from Greece.

data <- read_csv("Beazley_Archive.csv")

# Splitting the shapes column into two columns. One for the shape and one for the rest
data_cyprus <- data %>% 
  separate(`Shape Name`, into = c("Shape", "Shape2"), sep = "[,\\s]", remove = FALSE) %>% 
  select(-Shape2) %>% 
  filter(Provenance == "CYPRUS")

data_greece <- data %>% 
  separate(`Shape Name`, into = c("Shape", "Shape2"), sep = "[,\\s]", remove = FALSE) %>% 
  select(-Shape2) %>% 
  filter(Provenance == "GREECE, ATHENS")

# Getting the counts
data_cyprus_counts <- data_cyprus %>%
  count(Shape)

data_greece_counts <- data_greece %>%
  count(Shape)

# Removing NA values and getting the top 10 shapes
top_shapes_cyprus <- data_cyprus_counts %>% 
  filter(!is.na(Shape)) %>%
  top_n(10, n)

top_shapes_greece <- data_greece_counts %>%
  filter(!is.na(Shape)) %>%
  top_n(10, n)

# Plotting the top 10 shapes
top_shapes_cyprus %>% 
  ggplot(aes(x = reorder(Shape, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Shape", y = "Count", title = "Top 10 Shapes in Cyprus") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

top_shapes_greece %>%
  ggplot(aes(x = reorder(Shape, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Shape", y = "Count", title = "Top 10 Shapes in Greece") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Not required, but you can also create a plot that includes the top 10 vases from Cyprus and Greece in the same plot.

# Converting to percentages
top_shapes_cyprus <- top_shapes_cyprus %>% 
  mutate(n = n / sum(data_cyprus_counts$n))

top_shapes_greece <- top_shapes_greece %>%
  mutate(n = n / sum(data_greece_counts$n))

top_shapes_cyprus %>%
  mutate(Provenance = "Cyprus") %>%
  bind_rows(top_shapes_greece %>% mutate(Provenance = "Greece")) %>%
  ggplot(aes(x = reorder(Shape, n), y = n, fill = Provenance)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(x = "Shape", y = "Count", title = "Top Shapes in Cyprus and Greece") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# 4.1. Create a plot of the most common collections (The location where the vases are kept) of the vases in a new dataset. Use the dataset from 1.1.

data2 <- read_csv("Beazley_Archive_2.csv")

# Splitting the Collection Record column into two columns. One for the main location and one for the rest. It is split by a comma
data2 <- data2 %>% 
  separate(`Collection Record`, into = c("Location", "Collection Record"), sep = ",", remove = FALSE) %>% 
  select(-`Collection Record`) # We don't need the Collection Record column anymore, so we remove it

# Getting the counts
data2_counts <- data2 %>%
  count(Location)

# Removing NA values and getting the top 10 locations
top_locations2 <- data2_counts %>% 
  filter(!is.na(Location)) %>%
  top_n(10, n)

# Plotting the top 10 locations
top_locations2 %>% 
  ggplot(aes(x = reorder(Location, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Location", y = "Count", title = "Top 10 Locations in Beazley Archive 2") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# This makes sense, as the data set was created by looking for the term "New York"

# 4.2. Create a plot of the most common collections of the vases in the original dataset. Use the dataset from 2.2.

data <- read_csv("Beazley_Archive.csv")

# Splitting the Collection Record column into two columns. One for the main location and one for the rest. It is split by a comma
data <- data %>% 
  separate(`Collection Record`, into = c("Location", "Collection Record"), sep = ",", remove = FALSE) %>% 
  select(-`Collection Record`)

# Getting the counts
data_counts <- data %>%
  count(Location)

# Removing NA values and getting the top 10 locations
top_locations <- data_counts %>% 
  filter(!is.na(Location)) %>%
  top_n(10, n)

# Plotting the top 10 locations
top_locations %>% 
  ggplot(aes(x = reorder(Location, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Location", y = "Count", title = "Top 10 Locations in Beazley Archive") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


