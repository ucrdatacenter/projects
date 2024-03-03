# In this workshop, we will expand on some of the basic functions, use tidyverse more and introduce some archaeology specific functions
# It will follow the flow of a more typical data analysis workflow
# We will work with data about lithics from the Jerimalai rockshelter in East-Timor
# This workshop has been inspired by the 'tidyverse for archaeologists' workshop by Professor Ben Marwick 

# Load the tidyverse package
library(tidyverse)

# This data is coming from an URL, and it is an excel file. We unfortunately cannot use tidyverse to read excel files, so we will use the rio package
# Install the rio package
# install.packages("rio")
library(rio)

# Load the data using import
data <- import("https://bit.ly/j_data_xlsx", setclass = "tbl_df")

# Let's have a look at the data
View(data)
summary(data)

nrow(data)
ncol(data)

names(data)

# The first few rows
head(data)

# We will select the Square, Spit and Weight columns
# Note that we can select multiple columns in one go using the comma

data %>% 
  select(Square, Spit, Weight)

# Alternatively, if we only want columns that have numbers, we can use the where function

data %>% 
  select(where(is.numeric))

# We now want to get into plotting our data
# For this we are only interested in the Square, Spit, Material, Weight and Thick columns

plotting_data <- data %>% 
  select(Square, Spit, Material, Weight, Thick)

plotting_data

# We can now create a histogram of the Weight column. This will show how many artefacts have a certain weight
# We can do this using the ggplot() function. This function takes a tibble, and creates a plot.
# ggplot works with layers. We can add these layers with a + sign.
# The base command is ggplot(). This creates a white canvas, and you can add information that is passed to the layers.

ggplot(plotting_data)

# As we want to make a histogram, we only need to specify the x axis

ggplot(plotting_data, aes(x = Weight))

# Now we need to add a layer that displays the data. These layers are known as geoms. There are many different geoms, but we want to create a column plot, so we will use the geom_histogram() function.

ggplot(plotting_data, aes(x = Weight)) +
  geom_histogram()

# There are some weights that are very large, but most are close to zero.
# If we change the x axis scale to a logaritmic scale, we can see the distribution better
# We do this by adding the scale_x_log10() layer

# A logarithm is a mathematical function that increases slowly at first, and then faster and faster. This is useful for data that has a large range, as it compresses the data and makes it easier to see the distribution.
# This specific logarithm is the base 10 logarithm, which means that 10 is raised to the power of the number on the x axis. For example, 10^2 = 100, so the number 2 would be at the position 100 on the x axis.

logs_tibble <- tibble(x = c(10^0, 10^1, 10^2, 10^3, 10^4), y = c("10^0", "10^1", "10^2", "10^3", "10^4"), z = log10(x))

ggplot(plotting_data, aes(x = Weight)) +
  geom_histogram() +
  scale_x_log10()

# We can also change the theme of the plot. There are many themes available, but we will use the theme_bw() theme

ggplot(plotting_data, aes(x = Weight)) +
  geom_histogram() +
  scale_x_log10() +
  theme_bw()

# We now only want to keep rows that have the "A" value for the Square column
# We can do this using the filter() function. Keep in mind that "A" is a string, so we need to use quotation marks and two equal signs

plotting_data %>% 
  filter(Square == "A")

# Now, from these rows we only want light objects, lets say with a Weight less than 10

plotting_data %>% 
  filter(Square == "A") %>% 
  filter(Weight < 10)

# And a thickness between 0.2 and 20
# We can either do this by adding two filters, or by using the between() function
# Note that there is an important difference between > and >=, and < and <=

plotting_data %>% 
  filter(Square == "A") %>% 
  filter(Weight < 10) %>% 
  filter(Thick >= 0.2) %>% 
  filter(Thick <= 20)

plotting_data %>%
  filter(Square == "A") %>% 
  filter(Weight < 10) %>% 
  filter(between(Thick, 0.2, 20))

# Alternatively, you can combine them in one filter function

plotting_data %>%
  filter(Square == "A" & Weight < 10 & between(Thick, 0.2, 20))

# We also don't want rows that have NA values in the Material column

plotting_data %>%
  filter(Square == "A") %>% 
  filter(Weight < 10) %>% 
  filter(between(Thick, 0.2, 20)) %>% 
  drop_na(Material)

# We now want to make a scatterplot of the Weight and Thick columns

new_plotting_data <- plotting_data %>%
  filter(Square == "A") %>% 
  filter(Weight < 10) %>% 
  filter(between(Thick, 0.2, 20)) %>% 
  drop_na(Material)

# Create the empty plot
ggplot(new_plotting_data, aes(x = Weight, y = Thick))

# Add the points
ggplot(new_plotting_data, aes(x = Weight, y = Thick)) +
  geom_point()

# It may again be useful to change the x axis to a logaritmic scale
ggplot(new_plotting_data, aes(x = Weight, y = Thick)) +
  geom_point() +
  scale_x_log10()

# We can also do the same for the y axis
ggplot(new_plotting_data, aes(x = Weight, y = Thick)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# Set the theme to bw
ggplot(new_plotting_data, aes(x = Weight, y = Thick)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# We can also change the axis labels
ggplot(new_plotting_data, aes(x = Weight, y = Thick)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  labs(x = "Weight (g)", y = "Thickness (mm)")

# We can also add a colour to the points based on the material the object is made of
# We do this by adding the colour = Material argument to the aes() function
ggplot(new_plotting_data, aes(x = Weight, y = Thick, colour = Material)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  labs(x = "Weight (g)", y = "Thickness (mm)")

# We can change the position of the legend using the theme() function
# We need to specify the x and y coordinates of the legend
# This uses a coordinate system between 0 and 1, so 0.5 is in the middle
ggplot(new_plotting_data, aes(x = Weight, y = Thick, colour = Material)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  labs(x = "Weight (g)", y = "Thickness (mm)") +
  theme(legend.position = c(0.85, 0.3))

# Our goal now is to make a boxplot of the plat_area column, grouped by the Material column
# Lets get the data first. We need to create the plat_area column first

plat_area_data <- data %>% 
  mutate(plat_area = Platwid * Platthic)

# Only keep materials that have more than 5 objects and remove NA values
plat_area_data <- plat_area_data %>% 
  group_by(Material) %>% 
  filter(n() > 5) %>% 
  drop_na(Material)

# Now create the plot using the geom_boxplot() function
ggplot(data = plat_area_data) +  
  aes(x = Material,  
      y = plat_area) +  
  geom_boxplot() +  
  scale_y_log10() +  
  labs(x = "Raw Material",  
       y =  "Platform Area") +  
  theme_classic(base_size = 14)

# We now want to create a plot that shows the number of objects per material
# First get the data

material_data <- plat_area_data %>% 
  group_by(Material) %>% 
  count()

# Now create the plot using the geom_col() function
ggplot(data = material_data) +  
  aes(x = Material,  
      y = n) +  
  geom_col() +  
  scale_y_log10() +  
  labs(x = "Raw Material",  
       y =  "Number of Objects") +  
  theme_classic(base_size = 14)

# We can change the order of the columns using the reorder() function
# We need to specify the column we want to reorder, and the column we want to order by
# In this case, we want to order the Material column by the n column
ggplot(data = material_data) +  
  aes(x = reorder(Material, n),  
      y = n) +  
  geom_col() +  
  scale_y_log10() +  
  labs(x = "Raw Material",  
       y =  "Number of Objects") +  
  theme_classic(base_size = 14)

# Now lets say we want to make a plot of the distributions of length of each object
# We can either manually create each plot by filtering for each material, or we can use the facet_wrap() function

ggplot(data = plat_area_data) +  
  aes(x = Length) +  
  geom_histogram() +  
  facet_wrap(~Material) +  
  labs(x = "Length (mm)",  
       y =  "Number of Objects") +  
  theme_classic(base_size = 14)

# Due to the amount of chert items in the data, this is not very useful, but we can set the y axis to be free for each plot
ggplot(data = plat_area_data) +  
  aes(x = Length) +  
  geom_histogram() +  
  facet_wrap(~Material, scales = "free_y") +  
  labs(x = "Length (mm)",  
       y =  "Number of Objects") +  
  theme_classic(base_size = 14)

