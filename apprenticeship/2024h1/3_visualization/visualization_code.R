# Packages
library(tidyverse)
library(patchwork) # multiple plots
library(GGally) # correlation scatterplot matrix
library(scatterplot3d) # 3-D scatterplots
library(ggalluvial) # static alluvial diagram
library(networkD3) # interactive alluvial diagram
library(plotly) # interactive plots

# Data --------------------------------------------------------------------

# Load the data from GitHub
chic <- read_csv("https://raw.githubusercontent.com/ucrdatacenter/projects/main/apprenticeship/3_visualization/chicago.csv")

# Create the base of a plot with date on the x-axis and temperature on the y-axis
ggplot(chic, aes(x = date, y = temp))

# Basic plots -------------------------------------------------------------

# Create a scatterplot
ggplot(chic, aes(x = date, y = temp)) +
  geom_point()

# Create a line plot
ggplot(chic, aes(x = date, y = temp)) +
  geom_line()

# Combine both points and lines in the plot
ggplot(chic, aes(x = date, y = temp)) +
  geom_point() +
  geom_line()

# Customize the appearance of points and lines
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick", shape = "diamond", size = 2) +
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  theme_light() # Apply a light theme

# Set the default theme for all following plots
theme_set(theme_light())

# Add axis labels with xlab() and ylab()
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  xlab("Year") +
  ylab("Temperature")

# Add axis labels with labs(), use math expressions
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = expression(paste("Temperature (", degree ~ F, ")")))

# Color the scatterplot points by season
ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature")

# Additional aesthetics and legends ---------------------------------------

chic |>
  # Convert season to a factor with seasons in the correct order
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) |>
  # Determine the color and shape of the points by season
  ggplot(aes(x = date, y = temp, color = season, shape = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature")

# Convert season to a factor in the original data
chic <- chic |>
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")))

# Histogram of temperatures
ggplot(chic, aes(temp)) +
  geom_histogram(fill = "grey", color = "red")

# Other geoms -------------------------------------------------------------

# Density plot of temperatures
ggplot(chic, aes(temp)) +
  geom_density(fill = "grey", alpha = 0.5)

# Density plot of temperatures per season
ggplot(chic, aes(temp, fill = season)) +
  geom_density(alpha = 0.3)

# Number of observations per month
ggplot(chic, aes(month)) +
  geom_bar()

# Number of observations per month, colors by year
ggplot(chic, aes(month, fill = factor(year))) +
  geom_bar()

# Number of observations per month, colors by year
ggplot(chic, aes(season, fill = factor(year))) +
  geom_bar(position = "dodge")

# Add a smooth curve to the scatterplot
ggplot(chic, aes(date, temp)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Year", y = "Temperature")

# Use a linear fit and remove confidence interval around the line
ggplot(chic, aes(date, temp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Temperature")

# Plot the mean temperature per season
chic |>
  # Calculate mean temperature per year and season
  group_by(year, season) |>
  summarize(temp = mean(temp)) |>
  ggplot(aes(year, temp)) +
  geom_text(aes(label = season)) +
  labs(x = NULL, y = "Mean temperature per season")

# geom_label is the same as geom_text but with a filled background
chic |>
  group_by(year, season) |>
  summarize(temp = mean(temp)) |>
  ggplot(aes(year, temp)) +
  # Fill label background per season
  geom_label(aes(label = season, fill = season)) +
  labs(x = NULL, y = "Mean temperature per season")

ggplot(chic, aes(date, temp)) +
  geom_point(color = "grey70", alpha = 0.5, size = 2) +
  # Highlight selected points: Christmas from each year
  geom_point(data = chic |> filter(yday %in% 358:360), color = "red", size = 2) +
  # Add a horizontal line at temp == 32
  geom_hline(yintercept = 32, color = "blue", linetype = "dashed", size = 1.5) +
  labs(x = "Year", y = "Temperature")

# Add a ribbon showing monthly range of temperatures
chic |>
  # Calculate lowest and highest temperature per month
  group_by(year, month) |>
  mutate(upper = max(temp),
         lower = min(temp)) |>
  ggplot() +
  # use those temperatures as upper and lower bound of a ribbon
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper), alpha = 0.2) +
  geom_point(aes(date, temp))

# Heatmap of average temperature per season
chic |>
  # Calculate average temperature per season
  group_by(year, season) |>
  summarize(temp = mean(temp)) |>
  ggplot() +
  # geom_tile with fill aesthetic creates heatmap
  geom_tile(aes(year, season, fill = temp)) +
  # Add text to display average temperatures
  geom_text(aes(year, season, label = round(temp, 1)), color = "white") +
  labs(x = NULL, y = NULL, fill = "Average\ntemperature")

# Scatterplot of temperature and ozone levels in Dec 2000 - evolution over time is unclear
chic |>
  filter(year == 2000, month == "Dec") |>
  ggplot(aes(temp, o3)) +
  geom_point() +
  labs(x = "Temperature", y = "Ozone", title = "December 2000")

# Define the coordinates of an arrow pointing to Dec 1 2000
arrow_data <- tibble(
  x = 20,
  y = 25,
  xend = chic |> filter(date == ymd(20001201)) |> pull(temp),
  yend = chic |> filter(date == ymd(20001201)) |> pull(o3)
)

# Path of temperature and ozone levels make it clear how values change over time
chic |>
  filter(year == 2000, month == "Dec") |>
  ggplot(aes(temp, o3)) +
  # Plot the path of temperature and ozone
  geom_path() +
  # Highlight Dec 31
  geom_point(data = filter(chic, date == ymd(20001231))) +
  # Add the previously defined arrow with geom_segment
  geom_segment(data = arrow_data, aes(x = x, y = y, xend = xend, yend = yend),
               # Do not use the global aesthetics from ggplot(aes())
               inherit.aes = FALSE, color = "red",
               # Specify that the segment is an arrow
               arrow = arrow()) +
  # Add a label to Dec 1 with coordinates defined within aes()
  geom_text(data = NULL, aes(x = 20, y = 26, label = "Dec 1, 2000"), color = "red") +
  labs(x = "Temperature", y = "Ozone", title = "December 2000")

# Scales ------------------------------------------------------------------

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature") +
  # Specify the title and legend labels of the color scale
  scale_color_discrete(
    name = "Seasons:",
    labels = c("Mar—May", "Jun—Aug", "Sep—Nov", "Dec—Feb")
  )

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature") +
  # Manually specify the colors per season
  scale_color_manual(values = c("darkblue", "green3", "pink", "gold"))


ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature") +
  # Use a predefined color palette from RColorBrewer
  scale_colour_brewer(type = "qual", palette = 2)

ggplot(chic, aes(x = date, y = temp, color = o3, shape = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature") +
  # Use a gradient color palette specifying the endpoints
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  # Manually specify the shapes per season
  scale_shape_manual(values = c(15, 16, 17, 18))

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature") +
  # Only plot observations with temp between 0-50
  scale_y_continuous(limits = c(0, 50))

# Coordinate systems ------------------------------------------------------

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature") +
  # Limit the y-axis between 0 and 50, but don't filter out points
  coord_cartesian(y = c(0, 50))

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature") +
  # Limit the y-axis between 0 and 50, and let points show beyond the plot panel up to the plot margins
  coord_cartesian(y = c(0, 50), clip = "off")

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature") +
  # Customize y-axis breaks to be 0, 10, 20, ... 90
  scale_y_continuous(breaks = seq(0, 90, 10), minor_breaks = NULL) +
  # Customize x-axis breaks to be every 6 months in the format of year-month
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%b", minor_breaks = NULL)

ggplot(chic, aes(temp)) +
  geom_histogram(fill = "grey", color = "red") +
  # Flip the x and y axes
  coord_flip()

ggplot(chic, aes(temp)) +
  geom_histogram(fill = "grey", color = "red") +
  # Reverse the x axis
  scale_x_reverse()

# Multiple plots with facets ----------------------------------------------

# Change the default theme to bw
theme_set(theme_bw())

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "orangered", alpha = .3) +
  labs(x = "Year", y = "Temperature") +
  # Create separate plots per year
  facet_wrap(~year)

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "orangered", alpha = .3) +
  labs(x = "Year", y = "Temperature") +
  # Create separate plots per year and allow different x-axes per plot
  facet_wrap(~year, scales = "free_x")

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "orangered", alpha = .3) +
  labs(x = "Year", y = "Temperature") +
  # Create separate plots per season, arrange all plots in one row, let all scales vary
  facet_wrap(~season, nrow = 1, scales = "free")

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "orangered", alpha = .3) +
  labs(x = "Year", y = "Temperature") +
  # Arrange plots vertically per year and horizontally per season with facet_grid
  facet_grid(year~season)

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "orangered", alpha = .3) +
  labs(x = "Year", y = "Temperature") +
  # Same but with facet_wrap
  facet_wrap(year~season, scales = "free")

# Multiple plots with patchwork -------------------------------------------

# Create and save plots of temp, ozone level and dewpoint over time

p1 <- ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature")

p2 <- ggplot(chic, aes(x = date, y = o3, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Ozone")

p3 <- ggplot(chic, aes(x = date, y = dewpoint, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Dewpoint")

# Combine temp and ozone horizontally
p1 + p2

# Combine temp and ozone vertically
p1 / p2

# Combine temp and ozone vertically with plot_layout, do not repeat legends
p1 + p2 + plot_layout(ncol = 1, guides = "collect")

# Arrange p1 and p2 horizontally on top, p1, p2 and p3 horizontally below
(p1 + p2) / (p1 + p2 + p3) + plot_layout(guides = "collect")

# Side-by-side plots of temperature, ozone and dewpoint with facet_wrap
chic |>
  # Convert data to long format with variable names to "name" and values to "value"
  pivot_longer(c(temp, o3, dewpoint)) |>
  ggplot(aes(date, value, color = season)) +
  geom_point() +
  # Facet by variable name, arrange in one column
  facet_wrap(~name, ncol = 1)

# Customizing elements via theme() ----------------------------------------

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (F)") +
  # Customize axis title fonts
  theme(axis.title = element_text(size = 15, color = "firebrick", face = "italic"),
        # Remove y-axis ticks
        axis.ticks.y = element_blank(),
        # Change minor grid to dashed lines
        panel.grid.minor = element_line(linetype = "dashed"))

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature", color = "Season") +
  # Move legend to above the plot
  theme(legend.position = "top",
        # Change legend background color
        legend.background = element_rect(fill = "grey90"),
        # Remove legend title
        legend.title = element_blank())

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature") +
  # Move legend to coordinates within the plot
  theme(legend.position = c(0.85, 0.2),
        # Add whitespace to the left side of the plot
        plot.margin = margin(l = 50))

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point(key_glyph = "vline") +
  labs(x = "Year", y = "Temperature") +
  # Customize the legend further with guide_legend()
  guides(color = guide_legend("Season:", title.hjust = 0.5, nrow = 2))

# Saving plots ------------------------------------------------------------

ggplot(chic, aes(x = date, y = temp)) +
  geom_point()

# Save last plot
ggsave("figures/plot1.png", scale = 1.5)

p <- ggplot(chic, aes(x = date, y = temp)) +
  geom_point()

# Save plot saved to the Environment
ggsave(p, "figures/plot2.png", height = 10, width = 15, units = "cm")

# Other plots: GGally -----------------------------------------------------

chic |>
  # Keep only 4 continuous variables
  select(temp, o3, dewpoint, pm10) |>
  # Create correlation matrix of the selected variables
  ggpairs()

chic |>
  # Keep 4 continuous variables and season as the grouping variable
  select(temp, o3, dewpoint, pm10, season) |>
  # Recreate the previous plot but group observations by season
  # Use only the first 4 columns for the plots (exclude season)
  ggpairs(columns = 1:4, ggplot2::aes(color = season))

# Other plots: pie chart --------------------------------------------------

chic |>
  # Get the number of observations per month
  count(month) |>
  # Specify y as the counts and fill as the categorical variable
  ggplot(aes(x = "", y = n, fill = month)) +
  # Create bars with white borders
  geom_bar(stat = "identity", width = 1, color = "white") +
  # Change coordinate system to polar coordinates instead of Cartesian
  coord_polar("y", start = 0) +
  # Remove background theme elements
  theme_void()

# Other plots: 3-D scatterplot --------------------------------------------

# Create a 3-D scatterplot of temperature, dewpoint and ozone levels
scatterplot3d(x = chic$temp,
              y = chic$dewpoint,
              z = chic$o3)

# Other plots: alluvial/Sankey diagram ------------------------------------

chic |>
  # Redefine temperature as a categorical variable: above mean temperature is high, below is low
  mutate(temp = ifelse(temp > mean(temp), "High temp", "Low temp")) |>
  # Get number of observations per year, season, temp category
  count(year, season, temp) |>
  # axis1, axis2, axis3 are the categorical grouping variables, y is the number of observations per group
  ggplot(aes(axis1 = year, axis2 = season, axis3 = temp, y = n)) +
  # Create flows, with colors per year
  geom_alluvium(aes(fill = factor(year))) +
  # Add rectangles for the categories of each variable
  geom_stratum() +
  # Label the rectangles
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  # Remove background plot elements and legend
  theme_void() +
  theme(legend.position = "none")

# Create a tibble of links with 3 variables: source, target, number of observations
links <- chic |>
  # Redefine temp to hig/low categorical
  mutate(temp = ifelse(temp > mean(temp), "High temp", "Low temp")) |>
  # Number of observations per season-temperature combination
  count(season, temp)

# Create a tibble of nodes by listing the unique categories in the links tibble
nodes <- tibble(name = unique(c(links$season, links$temp)))

# Add numerical identifiers of the nodes to the links tibble by using the row index in the nodes tibble
# Subtract 1 to start the count at 0
links$IDseason <- match(links$season, nodes$name)-1
links$IDtemp <- match(links$temp, nodes$name)-1

# Create an interactive Sankey diagram from the links and nodes tibbles, the numerical category IDs, the observation counts per link and the variable name of the nodes tibble
sankeyNetwork(Links = links, Nodes = nodes, Source = "IDseason",
              Target = "IDtemp", Value = "n", NodeID = "name")

# Other plots: interactive ggplot with plotly -----------------------------

# Define a simple plot and assign to an object
p <- ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature")

# Display the plot as an interactive plot
ggplotly(p)

