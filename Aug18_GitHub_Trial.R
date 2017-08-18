## My script from data carpentry UC Merced
## Sabah Ul-Hasan sul-hasan@ucmerced.edu

## Download data
download.file("https://ndownloader.figshare.com/files/2292169",
              "data/portal_data_joined.csv")

## read data into R
surveys <- read.csv("data/portal_data_joined.csv")

## see if the data looks good
head(surveys)
tail(surveys)
str(surveys)
summary(surveys) # statistics of the data
## the $ operator for isolating columns
tail(survey$weight)
str(survey$weight)
summary(survey$weight)

## explore the data
dim(surveys)
nrow(surveys)
ncol(surveys)
names(surveys)

# plot (basic plotting) 
plot(surveys$year,surveys$weight)
plot(surveys$hindfoot_length, surveys$weight)

## explore data with historgrams
summary(surveys$month)
hist(surveys$month, col="blue")

## explore factor variable
summary(surveys$taxa)
levels(surveys$taxa)
nlevels(surveys$taxa)
hist(survey$taxa) ## error

class(surveys$taxa) # factor
table(surveys$taxa) # turns into a table
class(table(surveys$taxa)) # not it's a table
barplot(table(surveys$taxa))

## subset in base R ----
## [rows, columns]

## return all columns for genus Ammodramus
surveys[surveys$genus == 'Ammodramus', ]

## return a few columns
surveys[surveys$genus == 'Ammodramus', c('record_id', 'month', 'weight') ]

## How many observations are there in January and February?
summary(surveys$month == '1') #2455
summary(surveys$month == '2') #2758
Jan_and_Feb <- 2455 + 2758
Jan_and_Feb #5213

# Alternatives
nrow(surveys[surveys$month==1 | surveys$month==2, ])
table(surveys$month==1 | surveys$month==2)
length(which(surveys$month <3 ))

###############################################
###############################################

# Aug 18, 2017 (using tidyverse)
JL_Data <- surveys

install.packages("tidyverse")
library(tidyverse)

# Select columns from the data frame
select(JL_Data, plot_id, species_id, weight)
# Similar to what we did Aug 17 (return a few columns)
# This approach is more organized than the Aug 17 approach

# Use filter to select rows where the year is 1995
filter(JL_Data, year == 1995)

# Pipe = %>%
# Purpose of pipe function: https://goo.gl/aiPwMz
JL_Data_sml <- JL_Data %>%
  filter(year == 1995) %>%
  select(year, plot_id, species_id, weight) 

JL_Data_sml

JL_Data %>%
  filter(!is.na(weight)) %>% # remove NAs
  mutate(weight_kg = weight / 1000,
         weight_kg2 = weight_kg * 2) %>%
  tail # just to duoble check 

# Exercise A
# Create a new data frame from the surveys data that
# Contains only the species_id column and
# a new column called hindfood_half containing values
# half the hindfoot_length values (no NAs and less than 30)

Ex_A <- JL_Data %>%
  select(species_id, hindfoot_length) %>% # don't need this
  mutate(hindfoot_half = hindfoot_length/2) %>%
  filter((!is.na(hindfoot_half))&hindfoot_half<30) %>%
  # previously had filter(hindfoot_half<30) %>%, shorten with &
  select(species_id, hindfoot_half)
# 31,436 

# Alternatively
Ex_A <- JL_Data %>%
  mutate(hindfoot_half=0.5*hindfoot_length) %>%
  select(species_id,hindfoot_half) %>%
  filter(!is.na(hindfoot_half),hindfoot_half<30)

# Exercise B
Sex_Data <- JL_Data %>%
  filter(!is.na(weight),
         sex == "F" | sex == "M") %>%
  group_by(sex, species_id) %>% # group_by vs. select?
  # select gives whole columns, group_by gives values within column 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) 

# Tally counts the total number of observations for the variable
Sex_Data <- JL_Data %>%
  group_by(sex) %>%
  tally

# Exercise C
# (1) How many individuals were caught in each plot_type surveyed?
Individuals <- JL_Data %>%
  group_by(plot_type) %>%
  tally
Individuals # Answer in console

# (2) Use group_by() and summarize() to find the mean, min, and max
# hindfoot length for each species (using species_id)
Length <- JL_Data %>%
  select(hindfoot_length, species_id) %>%
  filter(!is.na(hindfoot_length)) %>%
  group_by(species_id) %>%
  summarize(mean_length = mean(hindfoot_length),
          min_length = min(hindfoot_length),
          max_length = max(hindfoot_length))
str(Length) # Answer in console

# (3) What was the heaviest animal measured in each year? 
# Return the columns year, genus, species_id, and weight. 
Weight <- JL_Data %>%
  select(year, genus, species_id, weight) %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  arrange(year)

# or

Weight <- JL_Data %>%
  select(year, genus, species_id, weight) %>%
  group_by(year) %>%
  top_n(1, weight)

plot(Weight$species_id, Weight$weight)

# (4) How can you get the same result using group_by 
# by sex instead of year and species? 
# Hint: see ?n
Weight <- JL_Data %>%
  group_by(sex) %>%
  summarize(n())

# Export your data
Complete_Data <- JL_Data %>%
  filter(species_id != "", # remove missing species_id
         !is.na(weight),
         !is.na(hindfoot_length),
         sex !="")

Species_Counts <- Complete_Data %>% # pipe for identifying common sp.
  group_by(species_id) %>%
  tally %>%
  filter(n >= 30)

Common_Species <- Complete_Data %>% # pipe for keeping common sp.
  filter(species_id %in% Species_Counts$species_id)

write.csv(Common_Species, file = "")

  