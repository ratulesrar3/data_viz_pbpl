library(tidyverse)
library(readxl)
library(reshape2)
library(zoo)
library(ggrepel)
library(directlabels)
library(jsonlite)

setwd("/Users/ratulesrar/Desktop/data_viz_pbpl/hw2/")

# Import pricestats data from MIT BPP
pricestats <- read_csv('pricestats_bpp_arg_usa.csv')
pricestats_usa <- pricestats %>% 
  filter(country == 'USA') %>%
  mutate(date = as.Date(date, '%d%b%Y'))

pricestats_usa %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=indexPS, color="Pricestats")) +
  geom_line(aes(y=indexCPI, color="BLS")) +
  scale_colour_discrete(guide=FALSE) +
  labs(title='Are federally reported inflation rates in the US underestimated?',
     subtitle='Comparing Pricestats and BLS CPI from 2008-2015',
     caption='Source: MIT Billion Prices Project, OECD & BLS', 
     x='Year', y='Inflation Index') 

oecd_cpi <- read_csv('us_cpi_oecd.csv')
oecd_cpi <- oecd_cpi %>%
  filter(MEASURE == 'IXOB') %>%
  mutate(qtr = as.yearqtr(Time, format = 'Q%q-%Y'))

oecd_cpi <- oecd_cpi %>%
  mutate(label=if_else(qtr==max(qtr), as.character(Subject), NA_character_)) %>%
  mutate(subject=if_else(Subject=="Consumer prices - food", "Food", Subject)) %>%
  mutate(subject=if_else(Subject=="Consumer prices - energy", "Energy", subject)) %>%
  mutate(subject=if_else(Subject=="Consumer prices - all items non-food, non-energy", "Non-food, non-energy", subject)) %>%
  mutate(subject=if_else(Subject=="Consumer prices - services less housing", "Non-housing", subject)) %>%
  mutate(subject=if_else(Subject=="Consumer prices - housing", "Housing", subject)) %>%
  mutate(subject=if_else(Subject=="Consumer prices - all items", "All", subject))

oecd_cpi %>%
  ggplot(aes(x=qtr, y=Value, color=Subject)) +
  geom_line() +
  geom_dl(aes(label=subject), method=list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_discrete(guide=FALSE) +
  scale_x_continuous(limits=c(2008, 2019)) +
  labs(title='What is going on with energy prices?',
       subtitle='Comparing Pricestats and OECD CPI from 2008-2015',
       caption='Source: OECD & BLS', 
       x='Year', y='Index Value')

oecd_cpi %>%
  filter(subject != "Energy") %>%
  ggplot(aes(x=qtr, y=Value, color=Subject)) +
  geom_line() +
  geom_dl(aes(label=subject), method=list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_discrete(guide=FALSE) +
  scale_x_continuous(limits=c(2008, 2019)) +
  labs(title='Price Indexes (Not including Energy)',
       subtitle='Comparing Pricestats and OECD CPI from 2008-2015',
       caption='Source: OECD & BLS', 
       x='Year', y='Index Value')

oecd_to_merge <- oecd_cpi %>%
  mutate(index=Value) %>%
  filter(subject!="All") %>%
  select(qtr, subject, index)

ps_to_merge <- pricestats_usa %>%
  mutate(qtr=as.yearqtr(date, format='%Y-%m-%d'),
         pricestats=indexPS) %>%
  select(date, pricestats)
 
cpi_to_merge <- pricestats_usa %>%
  mutate(qtr=as.yearqtr(date, format='%Y-%m-%d'),
         bls_agg=indexCPI) %>%
  select(date, bls_agg)

food_to_merge <- oecd_to_merge %>%
  filter(subject == 'Food') %>%
  mutate(date = as.Date(qtr)) %>%
  mutate(time = as.yearmon(date), bls_food=index) %>%
  select(date, bls_food)

energy_to_merge <- oecd_to_merge %>%
  filter(subject == 'Energy') %>%
  mutate(date = as.Date(qtr)) %>%
  mutate(time = as.yearmon(date), bls_energy=index) %>%
  select(date, bls_energy)

energy_to_merge <- oecd_to_merge %>%
  filter(subject == 'Energy') %>%
  mutate(date = as.Date(qtr)) %>%
  mutate(time = as.yearmon(date), bls_energy=index) %>%
  select(date, bls_energy)

housing_to_merge <- oecd_to_merge %>%
  filter(subject == 'Housing') %>%
  mutate(date = as.Date(qtr)) %>%
  mutate(time = as.yearmon(date), bls_housing=index) %>%
  select(date, bls_housing)

df <- left_join(ps_to_merge, food_to_merge, by='date') 
df <- left_join(df, energy_to_merge, by='date')
df <- left_join(df, housing_to_merge, by='date')
df <- left_join(df, cpi_to_merge, by='date')
df <- df %>%
  drop_na()

write_csv(df, "quarterly_inflation.csv")
json <- toJSON(df, pretty=TRUE)
write(json, file="quarterly_inflation.json")


# df <- left_join(ps_to_merge, oecd_to_merge, by='qtr') %>%
#   select(date, subject, index)
# df1 <- rbind(df, ps_to_merge)
# df2 <- rbind(df1, cpi_to_merge)
# write_csv(df2, "cpi_data.csv")
# json <- toJSON(df, pretty=TRUE)
# write(json, file="yearly_inflation.json")

# df <- read_csv("cpi_data.csv") %>%
#   mutate(date = as.Date(date)) %>%
#   mutate(time = as.yearmon(date)) %>%
#   group_by(time, subject) %>%
#   summarize(avg = mean(index)) %>%
#   mutate(date = as.Date(time))
# 
# df <- read_csv("monthly_cpi.csv") %>%
#   mutate(index = avg) %>%
#   select(date, subject, index)
# 
# write_csv(df, "monthly_cpi.csv")

# df <- read_csv("monthly_cpi.csv") %>%
#   mutate(date = format(as.Date(df$date, "%Y-%m-%d"), "%Y")) %>%
#   group_by(date, subject) %>%
#   summarize(index = mean(index))
# 
# write_csv(df, "yearly_cpi.csv")

df %>%
  filter(subject != "Energy") %>%
  ggplot(aes(x=date, y=index, color=subject)) +
  geom_line() +
  geom_dl(aes(label=subject), method=list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_discrete(guide=FALSE) +
  labs(title='Price Indexes (Not including Energy)',
       subtitle='Comparing Pricestats and OECD CPI from 2008-2015',
       caption='Source: OECD & BLS', 
       x='Year', y='Index Value')

