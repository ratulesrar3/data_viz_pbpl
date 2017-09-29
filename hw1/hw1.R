# DataViz hw1, Ratul Esrar
# Installing relevant packages
install.packages('readr')
install.packages('tidyr')
install.packages('haven')
install.packages('stringr')
install.packages('ggplot2')

library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Set working directory
setwd('/Users/ratulesrar/Desktop/data_viz_pbpl/hw1/')

# Load the data
sas_file <- 'accident.sas7bdat'
csv_file <- 'accident.csv'
acc2014 <- read_sas(sas_file)
acc2015 <- read_csv(csv_file)

class(acc2014)
class(acc2015)

# Convert to NA
acc2014 <- mutate(acc2014, TWAY_ID2 = na_if(TWAY_ID2, ""))

# Check dims
dim(acc2014)
dim(acc2015)

# Check cols
names(acc2015)[!names(acc2015) %in% names(acc2014)]
names(acc2014)[!names(acc2014) %in% names(acc2015)]
# acc2014 has a col named "ROAD_FNC" that is missing from acc2015
# acc2015 has cols named "RUR_URB", "FUNC_SYS", and "RD_OWNER" which are missing from acc2014

# Combine the Data
acc <- bind_rows(acc2014, acc2015)
count(acc, RUR_URB) # there are 30056 NAs because the 2014 data did not have RUR_URB as a var

# Load fips data
fips <- read_csv('fips.csv', skip = 1)
head(fips)
glimpse(fips)
