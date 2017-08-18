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
