# Install the libraries we will need today:
install.packages('ggplot2')
install.packages('plotly') 
install.packages('palmerpenguins')

# Import the libraries
library(ggplot2)
library(palmerpenguins)
library(plotly)
library(dplyr)
library(tidyr)

# Read the palmerpenguins data into 'data' variable
data <- penguins

################################################################################
###################### Exploring the dataset ###################################
################################################################################

# Get the structure of data, number of rows and number of columns
head(data)
str(data)
nrow(data)
ncol(data)

#How many NA values are in total in the dataset?
NA_count <- sum(is.na(data))
NA_count

# Remove rows containing NA values. How many rows are left now?

data <- na.omit(data)
nrow(data)

# What islands do these penguins live on?

lists <- unique(data$island)
lists

# What is the mean body mass of male penguins?

mean(data$body_mass_g[data$sex == "male"])

# Compute maximum flipper length for each of the penguin species

max(data$flipper_length_mm[data$species == "Adelie"])
max(data$flipper_length_mm[data$species == "Chinstrap"])
max(data$flipper_length_mm[data$species == "Gentoo"])

# Compute how many penguins lived on each of the islands in each year
data %>%
  group_by(island, year) %>%
  summarise(n = n())

# (HW) Is the minimal bill length of Adelie species higher than Gentoo?

min(data$bill_length_mm[data$species == "Adelie"])
min(data$bill_length_mm[data$species == "Gentoo"])

# (HW) On which island do more female than male penguins live?

data %>%
  group_by (island,sex) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = sex, values_from = n) %>%
  filter (female > male)


################################################################################
############################## Visualization ###################################
################################################################################

################################ Base R ########################################

# Adjust the following code, so that it has proper labels of x-axis, y-axis and title
boxplot(data$body_mass_g ~ data$species,
        xlab = 'Species',
        ylab = 'Body mass (g)',
        main = 'Body mass of penguins based on species')

# Adjust the following code, so that:
# Color of the points is based on species, and add the color legend
# Size of the points is 1
# Shape of the points is based on sex

plot(x = data$bill_depth_mm,
     y = data$bill_length_mm,
     col = as.numeric(as.factor(data$species)),  
     pch = as.numeric(as.factor(data$sex)),      
     cex = 1)

legend("topright",
       legend = levels(as.factor(data$species)),
       col = 1:length(levels(as.factor(data$species))), 
       pch = 16)


# Adjust the following code, so that the barplot is horizontal (years will be on y axis)
data.barplot <- data %>%
  group_by(year) %>%
  summarise(n = n()) 

barplot(data.barplot$n,
        xlab = 'Number of penguins',
        ylab = 'Year',
        names.arg= data.barplot$year)

# Create a barplot, where you plot the number of male and female penguins in the study
data.barplot <- data %>%
  group_by(sex) %>%
  summarise(n = n())

barplot(data.barplot$n,
        xlab = 'Sex',
        ylab = 'Number of penquins',
        names.arg= data.barplot$sex)

# Plot the histogram of flipper length of penguins living on Biscoe island in year 2008.

data.histogram <- data %>%
  filter(island == "Biscoe",
         year == "2008")


hist(data.histogram$flipper_length_mm,
     xlab = 'Flipper length (mm)',
     main = 'Histogram of flipper length of penguins living on Biscoe island in year 2008')

# Create a box plot of bill length [y] based on sex [x], and reorder the x-axis labels in a way male is first

data$sex <- factor(data$sex, levels = c("male", "female"))

boxplot(bill_length_mm ~ sex,
        data = data,
        xlab = "Sex",
        ylab = "Bill length (mm)")

# Create a scatterplot of flipper length based on body mass

plot(x = data$body_mass_g,
     y = data$flipper_length_mm,
     xlab = 'Body mass (g)',
     ylab = 'Flipper length (mm)')

# color the points based on the island

plot(x = data$body_mass_g,
     y = data$flipper_length_mm,
     xlab = 'Body mass (g)',
     ylab = 'Flipper length (mm)',
     col = data$island)

# set the shape of points based on the species

plot(x = data$body_mass_g,
     y = data$flipper_length_mm,
     xlab = 'Body mass (g)',
     ylab = 'Flipper length (mm)',
     col = data$island,
     pch = c(0,2,8)[as.integer(data$species)])
# label the axes appropriately

plot(x = data$body_mass_g,
     y = data$flipper_length_mm,
     xlab = 'Body mass (g)',
     ylab = 'Flipper length (mm)',
     main = 'Scatterplot of flipper length based on body mass',
     col = data$island,
     pch = c(0,2,8)[as.integer(data$species)])


################################# ggplot2 ######################################

# Adjust the following code, so that the bar plot is horizontal
t <- data %>%
  group_by(island) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = island, y = n))+
  geom_bar(stat = "identity")

t + coord_flip()

# Adjust the previous code, so that the bar plots are arranged in an descending way
data$island <- factor(data$island, levels = c("Torgersen", "Dream", "Biscoe"))
data %>%
  group_by(island) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = island, y = n))+
  geom_bar(stat = "identity") +
  coord_flip()

# Adjust the following code, so that the boxplot fill is based on species

data %>%
  ggplot(aes(x = sex, y = bill_length_mm, fill = species)) +
  geom_boxplot()

# Adjust the previous code, so that faceting on island will be on x-axis

data %>%
  ggplot(aes(x = sex, y = bill_length_mm, fill = species))+
  geom_boxplot() +
  facet_wrap(~island)

# Adjust the following code: add individual points using the geom_jitter() on the plot
# Color the individual points based on species

data %>%
  ggplot(aes(x = sex, y = bill_length_mm))+
  geom_boxplot() +
  geom_jitter(alpha = 0.5, aes(color = species))

# Plot the histogram of body mass of penguins living on Biscoe island in 2008

data.histogram <- data %%
  filter((island == "Biscoe") & (year == "2008"))

data.histogram %>%
  ggplot(aes(x = body_mass_g)) +
  geom_histogram(binwidth = 100)



# Create a scatterplot, where you will plot the dependency of bill length on bill depth and:
#   - color the points by species,
#   - set the shape based on island,
#   - create facets based on sex,
#   - set the theme to theme_minimal()
#   - label the axes appropriately

# Create a stacked barplot, where:
#   - on the x-axis will be species
#   - on y axis counts for each of the respective categories (number of penguins),
#   - fill will be based on island,
#   - set theme to theme_classic()

# Plot a violin plot of Adelie species body mass based on island and overlay it with a boxplot.
#   - Change the color based on island. 
#   - How does parameter trim = FALSE in the geom_violin() function change the output? 

# Create a scatterplot, where you will display the flipper length and body mass of Adelie species which have bill length >= 36,
#   - color the points based on the island, and use a dark palette from RColorBrewer package. 
#   - Use theme_minimal()
#   - change the points size to 2
#   - change opacity to 0.5 

# Adjust the previous graph as if you were to publish it in a scientific journal :-)

################################################################################

# Read into 'df' variable the 'stats-results.csv' file

# plot a volcano plot for the Ubi6-Ctrl contrast
# add a variable 'significant' specifying if the protein is upregulated, downregulated or not-changed
## upregulated: logFC > 1 & p-value < 0.05
## downregulated: logFC < -1 & p-value < 0.05
## not-changed: all other cases

# color the points based on the 'significant' variable
# let's suppose we are specifically interested in following proteins: ARF4, BRAP.1, PSMD12, USP5, TNIP1, VKORC1
## label these proteins in the volcano plot

# create the volcano plot in interactive version using plotly or ggplotly
# use the 'df'; color the points based on 'significant' variable
# display gene names on hover 
# save the interactive volcano plot as an .html file