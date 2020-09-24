# Lab 3B 
# Alison Fowler 

# Data manipulation using dplyr and tidyr 
# Data carpentry instructions: 
# https://datacarpentry.org/R-ecology-lesson/03-dplyr.html#Split-apply-combine_data_analysis_and_the_summarize()_function 

library(tidyverse)

# The tidyverse package tries to address 3 common issues that arise when doing data analysis with some of the functions 
# that come with R:

# 1. The results from a base R function sometimes depend on the type of data.
# 2. Using R expressions in a non standard way, which can be confusing for new learners.
# 3. Hidden arguments, having default operations that new learners are not aware of.

# download data file from online 
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")

surveys <- read_csv("data_raw/portal_data_joined.csv")
# notice this is not read.csv(); read_csv() is in the readr package 

# inspect the data 
str(surveys)

# differences between a datafram and a tibble: 
# 1. In addition to displaying the data type of each column under its name, it only prints the first few rows of data 
# and only as many columns as fit on one screen.
# 2. Columns of class character are never converted into factors.

# preview the data 
View(surveys)

# select certain columns 
select(surveys, plot_id, species_id, weight)

# select all columns except certain ones 
select(surveys, -record_id, -species_id)

# choose rows based on specific criterion 
filter(surveys, year == 1995)

# PIPES 
# What if you want to select and filter at the same time? There are three ways to do this: 
# use intermediate steps, nested functions, or pipes.

# Intermediate steps: 
surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)

# Nest functions: 
surveys_sml <- select(filter(surveys, weight < 5), species_id, sex, weight)

# can be difficult to read... 

# Pipes: let you take the output of one function and send it directly to the next, 
# which is useful when you need to do many things to the same dataset.

surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

# assign it to a new object 
surveys_sml <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

surveys_sml

# Challenge: 
# Using pipes, subset the surveys data to include animals collected before 1995 and 
# retain only the columns year, sex, and weight. 

surveys_sub <- surveys %>% 
  filter(year > 1995) %>% 
  select(year, sex, weight)

surveys_sub

# MUTATE 
# create new columns based on the values in existing columns, 
# for example to do unit conversions, or to find the ratio of values in two columns

# create a new column of weight in kg: 

kg_added <- surveys %>% 
  mutate(weight_kg = weight/1000)

kg_and_lb_added <- surveys %>% 
  mutate(weight_kg = weight/1000, 
         weight_lb = weight_kg * 2.2)

# if we wanted to just look at the head of these new mutated data 

surveys %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

# if we wanted to remove NAs in weight column: 

surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

# Challenge: 
# Create a new data frame from the surveys data that meets the following criteria: 
# contains only the species_id column and a new column called hindfoot_cm containing the 
# hindfoot_length values converted to centimeters. In this hindfoot_cm column, there are no NAs 
# and all values are less than 3. 

# (I'm assuming the hindfoot lengths are currently in mm?)

new_HFL <- surveys %>% 
  mutate(hindfoot_cm = hindfoot_length / 10) %>% 
  filter(!is.na(hindfoot_cm)) %>% 
  filter(hindfoot_cm < 3) %>% 
  select(species_id, hindfoot_cm)

new_HFL

# Split-apply-combine data analysis and the summarize() function

# split the data into groups, apply some analysis to each group, and then combine the results. 
# dplyr makes this very easy through the use of the group_by() function.

# The group_by() function is often used together with summarize(), which collapses each group 
# into a single-row summary of that group. group_by() takes as arguments the column names that contain the
# categorical variables for which you want to calcuate the summary statistics. 

# to compute the mean weight by sex: 

surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>% 
  tail()

# We can see that the sex column contains NA values because some animals had escaped before their 
# sex and body weights could be determined. The resulting mean_weight column does not contain NA 
# but NaN (which refers to “Not a Number”) because mean() was called on a vector of NA values while
# at the same time setting na.rm = TRUE. 
# To avoid this, we can remove the missing values for weight before we attempt to calculate the 
# summary statistics on weight. Because the missing values are removed first, we can omit na.rm = TRUE 
# when computing the mean:

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight))

# specify number of rows to display with print(n=)

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight)) %>%
  print(n = 15)

# Once the data are grouped, you can also summarize multiple variables at the same time (and not necessarily the same
# variable). For instance, we could add a column indicating the minimum weight for each species for each sex: 

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight))

# We can sort on min_weight to put the lighter species first 

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>%
  arrange(min_weight)

# and we can sort in descending order... 

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>%
  arrange(desc(mean_weight)) 

# Counting

# We often want to know the number of observations found for each factor or combination of factors. 
# For this task, dplyr provides count(). 

# if we wanted to count the number of rows of data for each sex: 

surveys %>% 
  count(sex)

# which is equivalent to: 

surveys %>% 
  group_by(sex) %>% 
  summarize(count = n())

# there is a sort argument for count() 

surveys %>% 
  count(sex, sort = T)

# multiple factors: specify the order 

surveys %>%
  count(sex, species)

surveys %>%
  count(species, sex)

# arrange species in alphabetical order and count in descending order 

surveys %>%
  count(sex, species) %>%
  arrange(species, desc(n))

# Challenge: 
# 1. How many animals were caught in each plot_type surveyed? 

surveys %>% 
  count(plot_type)

# 2. Use group_by() and summarize() to find the mean, min, and max hindfoot length for each species (using species_id). 
# Also add the number of observations (hint: see ?n).

spp_HFL <- surveys %>% 
  group_by(species_id) %>% 
  summarize(mean_length = mean(hindfoot_length),
            min_length = min(hindfoot_length),
            max_length = max(hindfoot_length))

View(spp_HFL)

# Oh, it can't calculate means with NAs; let's remove...  

spp_HFL <- surveys %>% 
  filter(!is.na(hindfoot_length)) %>%
  group_by(species_id) %>% 
  summarize(mean_length = mean(hindfoot_length),
            min_length = min(hindfoot_length),
            max_length = max(hindfoot_length),
            n = n())

View(spp_HFL)

# 3. What is the heaviest animal measured each year? Return the columns year, genus, species_id, and weight 

spp_weights <- surveys %>% 
  filter(!is.na(weight)) %>%
  group_by(year) %>% 
  filter(weight == max(weight)) %>% # filter, not summarize! 
  select(year, species_id, genus, weight) %>% 
  arrange(year)

View(spp_weights)

# Reshaping with gather and spread 

# spread() takes three principal arguments: 
# 1. the data
# 2. the key column variable whose values will become new column names
# 3. the value column whose values will fill the new column variables 

# further arguments include fill which, if set, fills in missing values with the value provided 

# use spread() to transform surveys to find the mean weight of each genus in each plot over the entire period. 

surveys_gw <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(plot_id, genus) %>% 
  summarize(mean_weight = mean(weight))

View(surveys_gw)
str(surveys_gw)

# observations for each plot are spread across multiple rows, 196 obs of three variabes. 

surveys_spread <- surveys_gw %>% 
  spread(key = genus, value = mean_weight)

View(surveys_spread)
str(surveys_spread)

surveys_gw %>%
  spread(genus, mean_weight, fill = 0) %>%
  head()


# gather() can be used to do the opposite; takes in four principle arguments: 
# 1. the data
# 2. the key column variable we wish to create from column names.
# 3. the values column variable we wish to create and fill with values associated with the key.
# 4. the names of the columns we use to fill the key variable (or to drop).

surveys_gather <- surveys_spread %>%
  gather(key = "genus", value = "mean_weight", -plot_id)

View(surveys_gather)
str(surveys_gather)


# you can also specify what columns to include - but for some reason when I tried to change the range of genera, it got messed up
surveys_spread %>%
  gather(key = "genus", value = "mean_weight", Baiomys:Spermophilus) %>% 
  View()

# Challenge: 

# 1. Spread the surveys data frame with year as columns, plot_id as rows, and the number of genera per plot as the values. 
# You will need to summarize before reshaping, and use the function n_distinct() to get the number of unique genera within a 
# particular chunk of data. It’s a powerful function! See ?n_distinct for more.

genera_per_plot <- surveys %>% 
  group_by(plot_id, year) %>% 
  summarize(genera = n_distinct(genus)) %>% 
  # I guess it knows to do distinct per plot_id and not year because it is listed first? 
  View()

# now lets add spread 

genera_per_plot <- surveys %>% 
  group_by(plot_id, year) %>% 
  summarize(genera = n_distinct(genus)) %>%
  spread(year, genera)

View(genera_per_plot)

# 2. Now take that data frame and gather() it again, so each row is a unique plot_id by year combination. 

gathered_genera <- genera_per_plot %>% 
  gather("year", "genera", -plot_id) 
# why does gather need columns to be in quotes? 

View(gathered_genera)

# 3. The surveys data set has two measurement columns: hindfoot_length and weight. This makes it difficult to do things 
# like look at the relationship between mean values of each measurement per year in different plot types. Let’s walk through 
# a common solution for this type of problem. 

# First, use gather() to create a dataset where we have a key column called measurement 
# and a value column that takes on the value of either hindfoot_length or weight. 
# Hint: You’ll need to specify which columns are being gathered.

surveys_long <- surveys %>% 
  gather ("measurement", "value", hindfoot_length, weight)

View(surveys_long)

# this doubled the amount of rows. It added hindfootlength to one and then weight to another for each obs.

# 4. With this new data set, calculate the average of each measurement in each year for each different plot_type. Then spread() them 
# into a data set with a column for hindfoot_length and weight. Hint: You only need to specify the key and value columns for spread()

measurements <- surveys_long %>% 
  group_by(year, measurement, plot_type) %>% 
  summarize(mean_value = mean(value, na.rm=T)) %>% 
  spread(measurement, mean_value)

View(measurements)

# Exporting Data 

# first, remove Nas 

surveys_complete <- surveys %>% 
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))

# remove rare species
# first make a subset of the most common species (50 or more observations)

species_counts <- surveys_complete %>%
  count(species_id) %>% 
  filter(n >= 50)
    
# then subset the complete data set with those species names 
surveys_complete <- surveys_complete %>% 
  filter(species_id %in% species_counts$species_id)

# double check we have the right dimensions (30,463 x 13)
dim(surveys_complete)

# write new csv file 
write_csv(surveys_complete, path = "generated_data/surveys_complete.csv")


