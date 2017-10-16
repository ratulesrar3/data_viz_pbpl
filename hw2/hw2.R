# HW2 Intro to ggplot2, Ratul Esrar
library(tidyverse)
library(reshape2)
library(zoo)
library(ggrepel)

# Set working directory
setwd('/Users/ratulesrar/Desktop/data_viz_pbpl/hw2/')

# Import pricestats data from MIT BPP
pricestats <- read_csv('pricestats_bpp_arg_usa.csv')
pricestats_usa <- pricestats %>% 
  filter(country == 'USA') %>%
  mutate(date = as.Date(date, '%d%b%Y')) %>%
  mutate(qtr = as.yearqtr(date)) %>%
  group_by(qtr) %>%
  summarize(avg_quarterly_cpi = mean(indexPS))

ggplot(pricestats_usa) +
  geom_line(aes(x=qtr, y=avg_quarterly_cpi)) +
  ggtitle('Quarterly CPI in the US from 08Q3-15Q3 (Pricestats Database)') +
  labs(x='Year', y='CPI')

# Import OECD CPI data
oecd_cpi <- read_csv('us_cpi_oecd.csv')
oecd_cpi <- oecd_cpi %>%
  filter(Subject == 'Consumer prices - all items', MEASURE == 'IXOB') %>%
  mutate(qtr = as.yearqtr(Time, format = 'Q%q-%Y'))
  
ggplot(oecd_cpi) +
  geom_line(aes(x=qtr, y=Value)) +
  ggtitle('Quarterly CPI in the US from 08Q3-15Q3 (OECD Database)') +
  labs(x='Year', y='CPI')

agg_cpi <- bind_rows(pricestats_usa, oecd_cpi)
agg_cpi <- agg_cpi %>% mutate(cpi = ifelse(is.na(avg_quarterly_cpi), Value, avg_quarterly_cpi)) %>%
  mutate(source = ifelse(is.na(Measure), 'Pricestats', 'OECD')) %>%
  mutate(qtr = as.yearqtr(qtr)) %>%
  select(qtr, cpi, source)

ggplot(agg_cpi, aes(x=qtr, y=cpi, color=source)) +
  geom_line() +
  labs(title='Consumer Price Index (CPI) in the United States',
       subtitle='Comparing Pricestats and OECD CPI from 2008Q3 - 2015Q3',
       caption='Source: MIT Billion Prices Project, OECD', 
       x='Year', y='CPI')

agg_cpi %>% 
  mutate(label=if_else(qtr==max(qtr), as.character(source), NA_character_)) %>%
  ggplot(aes(x=qtr, y=cpi, color=source)) +
  geom_line() +
  scale_colour_discrete(guide=FALSE) +
  labs(title='Are federally reported inflation rates in the US underestimated?',
       subtitle='Comparing Pricestats and OECD CPI from 2008Q3 - 2015Q3',
       caption='Source: MIT Billion Prices Project, OECD', 
       x='Year', y='CPI') +
  geom_label_repel(aes(label=label), na.rm=TRUE)

# Import Quarterly Income Data from Bureau of Economic Analysis
# https://www.bea.gov/iTable/index_regional.cfm
income <- read_csv('personal_income_quarterly_bystate_2007-2017.csv', skip = 4)
drop_cols <- c('2008:Q3', '2015:Q3')
drop_rows <- c('New England', 'Mideast', 'Great Lakes', 'Plains', 'Southeast', 
               'Southwest', 'Rocky Mountain', 'Far West', 'United States')
personal_income <- income %>% 
  filter(LineCode == 1, !GeoName %in% drop_rows) %>%
  select(-c(LineCode, Description)) %>%
  select(GeoName, '2008:Q3', '2015:Q3') %>%
  mutate(t1 = as.integer(personal_income[['2008:Q3']]), t2 = as.integer(personal_income[['2015:Q3']])) %>%
  mutate(pct_change = 100*(t2-t1)/t1)

ggplot(personal_income, aes(x=GeoName, y=pct_change)) + 
  geom_point(stat='identity', color='blue') +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_smooth(method='lm', aes(group=1), color='black', linetype='dashed', se=FALSE) +
  labs(title='Percent Change in Personal Income by State',
       subtitle='Percent Change calculated using 2015Q3 and 2008Q3 data',
       caption='Source: Bureau of Economic Analysis (US Dept of Commerce)', 
       x='State', y='Percent Change')

per_income <- melt(personal_income, id.var='GeoName')
per_income <- per_income %>%
  mutate(date = as.Date(as.yearqtr(variable, format='%Y:Q%q')), state = GeoName, income = as.numeric(value)) %>%
  select(state, date, income)

ggplot(per_income, aes(x=state, y=income, fill=as.factor(date))) + 
  geom_bar(stat='identity') + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_y_log10() +
  labs(title='Increase in Personal Income by State',
       subtitle='Increase calculated using 2015Q3 and 2008Q3 data',
       caption='Source: Bureau of Economic Analysis (US Dept of Commerce)', 
       x='State', y='Log(Personal Income in Millions)') +
  scale_fill_discrete(name='Year',
                      labels=c('2015', '2008'))
