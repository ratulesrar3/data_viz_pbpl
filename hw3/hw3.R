# HW3 Code, Ratul Esrar
library(tidyverse)
library(reshape2)
library(zoo)
library(ggrepel)
library(readxl)

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

# Import Quarterly Income Data from Bureau of Economic Analysis
income <- read_csv('personal_income_quarterly_bystate_2007-2017.csv', skip = 4)
quarter_cols <- c('2008:Q3', '2015:Q3')
drop_rows <- c('New England', 'Mideast', 'Great Lakes', 'Plains', 'Southeast', 
               'Southwest', 'Rocky Mountain', 'Far West', 'United States')
state_personal_income <- income %>%
  filter(LineCode==1, !GeoName %in% drop_rows) %>%
  select(GeoName, quarter_cols) 
state_personal_income <- state_personal_income %>%
  mutate(state=as.character(GeoName),
         t1=as.integer(state_personal_income[['2008:Q3']]), 
         t2=as.integer(state_personal_income[['2015:Q3']]),
         pct_change=100*(t2-t1)/t1) %>%
  select(state, t1, t2, pct_change) %>%
  mutate(state=factor(state, levels=unique(state)))

# Plot Graph 3
state_personal_income %>%
  ggplot(aes(x=state, y=pct_change)) + 
  theme(axis.text.x=element_text(angle=90, hjust=1),
        legend.position=c(0,0)) +
  geom_point(color='#000000', alpha=0.65, stat='identity', size=0.75) +
  geom_hline(yintercept=median(state_personal_income$pct_change), 
             aes(linetype='Median Percent Change'),
             size=0.5, alpha=0.65, linetype='dashed') +
  geom_hline(yintercept=9.8, aes(linetype='Percent Change in CPI'),
             size=0.5, alpha=0.85, linetype='dashed') +
  labs(title='Changes in Personal Income Outpaced Change in Inflation',
     subtitle='Percent Change calculated using 2015Q3 and 2008Q3 data',
     caption='Source: Bureau of Economic Analysis (US Dept of Commerce)', 
     x='State', y='Percent Change in Personal Income') + 
  annotate("text", 'South Dakota', 25.5, label='Median Income Change') +
  annotate("text", 'South Dakota', 14, label='Percent Change in CPI') +
  scale_y_continuous(labels = function(x){paste0(x, '%')})

# Import income distribution data
income_dist <- read_xlsx('income_dist.xlsx')

income_dist %>%
  ggplot() +
  geom_line(aes(x=year, y=avg_income, color=percentile)) +
  facet_wrap(~percentile) +
  guides(color=FALSE) +
  scale_y_continuous(labels = function(x){paste0(x, '$')}) +
  labs(title='Income Distribution Has Not Kept Pace with CPI',
       subtitle='Buying power for families in the 10th and 50th Percentiles continues to diminish',
       caption='Source: Urban Institute', 
       x='Year', y='Family Income (2016 Dollars)')
  

