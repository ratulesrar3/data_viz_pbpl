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

# Load the data
sas_file <- 'accident.sas7bdat'
csv_file <- 'accident.csv'
acc2014 <- read_sas(sas_file)
acc2015 <- read_csv(csv_file)

class(acc2014)
class(acc2015)
