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

# Import OECD CPI data
oecd_cpi <- read_csv('us_cpi_oecd.csv')
oecd_cpi <- oecd_cpi %>%
  filter(Subject == 'Consumer prices - all items', MEASURE == 'IXOB') %>%
  mutate(qtr = as.yearqtr(Time, format = 'Q%q-%Y'))

# Merge the pricestats and oecd data
binded_cpi <- bind_rows(pricestats_usa, oecd_cpi)
agg_cpi <- binded_cpi %>% mutate(cpi = ifelse(is.na(avg_quarterly_cpi), Value, avg_quarterly_cpi)) %>%
  mutate(source = ifelse(is.na(Measure), 'Pricestats', 'OECD')) %>%
  mutate(qtr = as.yearqtr(qtr)) %>%
  select(qtr, cpi, source)

merged_cpi <- left_join(pricestats_usa, oecd_cpi, by='qtr')
merged_cpi <- merged_cpi %>%
  mutate(cpi_oecd=Value, cpi_ps=avg_quarterly_cpi, qtr=as.yearqtr(qtr)) %>%
  mutate(difference=cpi_ps-cpi_oecd) %>%
  select(qtr, cpi_oecd, cpi_ps, difference)

# Plot Graph 1
agg_cpi %>% 
  mutate(label=if_else(qtr==max(qtr), as.character(source), NA_character_)) %>%
  ggplot(aes(x=qtr, y=cpi, color=source)) +
  geom_line() +
  scale_colour_discrete(guide=FALSE) +
  labs(title='Are federally reported inflation rates in the US underestimated?',
       subtitle='Comparing Pricestats and OECD CPI from 2008Q3 - 2015Q3',
       caption='Source: MIT Billion Prices Project, OECD & BLS', 
       x='Year', y='CPI') +
  geom_label_repel(aes(label=label), na.rm=TRUE)

# Plot Graph 2
# I'd like to eventually add a recession thingy to this
merged_cpi %>%
  ggplot(aes(x=qtr, y=difference)) +
  geom_point(alpha = 0.65, color='#800000') +
  geom_smooth(se=FALSE, show.legend=TRUE, span=1.5, 
              size=0.5, alpha=0.5, aes(color='Trendline (Pricestats - BLS)')) +
  labs(title='The BLS underestimates CPI during recessions',
       subtitle='Comparing the difference between Pricestats and OECD CPI from 2008Q3 - 2015Q3',
       caption='Source: MIT Billion Prices Project, OECD & BLS', 
       x='Year', y='Difference in CPI') +
  scale_colour_manual(name='Legend', values='#14bde8') +
  theme(legend.position = c(0.83, 0.1))


