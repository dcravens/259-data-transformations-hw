#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: N/A

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse) 
ds <- read_csv("data_raw/rolling_stone_500.csv")
  
### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER
glimpse(ds)

# Convert "Year" to numeric
ds <- ds %>%
  mutate(Year = as.numeric(Year))

# Check data set
glimpse(ds)
typeof(ds$Year)

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER
ds <- ds %>%
  rename_with(tolower)

# Check the updated column names
colnames(ds)

### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER
# Create a new variable for the decade
ds <- ds %>%
  mutate(decade = floor(year / 10) * 10)

# View the first few rows to confirm
head(ds)

### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER
# Sort dataset by rank
ds <- ds %>%
  arrange(rank)

# View the first few rows to confirm
head(ds)

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER
# Create 'top10' tibble with only artists and songs for the top 10 ranked songs
top10 <- ds %>%
  filter(rank <= 10) %>%
  select(artist, song)

# View the result
top10

### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER
# Create summary tibble with earliest, most recent, and average release year
ds_sum <- ds %>%
  summarize(
    earliest_year = min(year, na.rm = TRUE),
    most_recent_year = max(year, na.rm = TRUE),
    average_year = mean(year, na.rm = TRUE)
  )

# View the summary
ds_sum

### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

#ANSWER
library(dplyr)

# Find the songs from the earliest, most recent, and closest-to-average years
ds_filtered <- ds %>%
  filter(year %in% c(ds_sum$earliest_year, ds_sum$most_recent_year, round(ds_sum$average_year))) %>%
  arrange(year)

# View the result
ds_filtered

### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER
library(dplyr)

# Fix the incorrect year for "Brass in Pocket"
ds <- ds %>%
  mutate(year = ifelse(song == "Brass in Pocket", 1979, year))

# Recalculate decade
ds <- ds %>%
  mutate(decade = floor(year / 10) * 10)

# Recalculate summary statistics
ds_sum <- ds %>%
  summarize(
    earliest_year = min(year, na.rm = TRUE),
    most_recent_year = max(year, na.rm = TRUE),
    average_year = mean(year, na.rm = TRUE)
  )

# Find the correct oldest, most recent, and average-ist songs
ds_filtered <- ds %>%
  filter(year %in% c(ds_sum$earliest_year, ds_sum$most_recent_year, round(ds_sum$average_year))) %>%
  arrange(year)

# View the results
ds_sum
ds_filtered

### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER
# Summarize average rank and count of songs by decade
ds %>%
  filter(!is.na(decade)) %>%  # Remove NA values from decade
  group_by(decade) %>%        # Group by decade
  summarize(
    avg_rank = mean(rank, na.rm = TRUE),  # Calculate average rank
    song_count = n()                      # Count number of songs
  )

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

#ANSWER

# Count the number of songs by decade and find the decade with the most songs
ds %>%
  filter(!is.na(decade)) %>%  # Remove rows with NA in 'decade'
  count(decade) %>%           # Count songs per decade
  slice_max(n = 1, order_by = n)  # Select the decade with the most songs
  