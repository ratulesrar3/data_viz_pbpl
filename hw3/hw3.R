# HW3 Code, Ratul Esrar
library(tidyverse)
library(reshape2)
library(zoo)
library(ggrepel)
library(readxl)
library(rgdal)
library(maptools)
library(stringr)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()



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
  
theme_data_viz <- function () { 
  theme_bw(base_size=14, base_family='Gill Sans') %+replace% 
      theme(axis.text = element_text(size = rel(0.8)), 
            axis.ticks = element_line(colour = "black"), 
            legend.key = element_rect(colour = "grey80"), 
            panel.background = element_rect(fill = "white", colour = NA), 
            panel.border = element_rect(fill = NA, colour = "grey50"), 
            panel.grid.major = element_line(colour = "grey90", size = 0.2), 
            panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
            strip.background = element_rect(fill = "grey80", colour = "grey50", 
                                            size = 0.2))
}

income <- read_csv('personal_income_quarterly_bystate_2007-2017.csv', skip = 4)
quarter_cols <- c('2010:Q1', '2010:Q2', '2010:Q3', '2010:Q4',
                  '2011:Q1', '2011:Q2', '2011:Q3', '2011:Q4',
                  '2012:Q1', '2012:Q2', '2012:Q3', '2012:Q4',
                  '2013:Q1', '2013:Q2', '2013:Q3', '2013:Q4',
                  '2014:Q1', '2014:Q2', '2014:Q3', '2014:Q4',
                  '2015:Q1', '2015:Q2', '2015:Q3')
drop_rows <- c('New England', 'Mideast', 'Great Lakes', 'Plains', 'Southeast', 
               'Southwest', 'Rocky Mountain', 'Far West')
capita_income <- income %>%
  filter(LineCode==3, !GeoName %in% drop_rows) %>%
  select(GeoName, quarter_cols) 
state_capita_income <- capita_income %>%
  mutate(state=as.character(GeoName)) %>%
  mutate(state=factor(state, levels=unique(state))) %>%
  select(state, quarter_cols)


readOGR(dsn="director", layer="filename")

class(ward_map)
ward_map@data
ward_map@data$WARD_ID

# Load median income data
median_income <- read_csv("county_incomes.csv") %>%
  mutate(county_fips = county_id) %>%
  select(county, state, income, state_id, county_fips)

median_income %>%
  group_by(state) %>%
  ggplot(aes(x = state, y = income)) +
    geom_boxplot(fill = "darkorange", alpha = 0.5) + 
    coord_flip() + scale_x_discrete(limits = rev(levels(income))) +
    labs(title = "Median household income by state",
         subtitle = "Source: 2011-2015 American Community Survey") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(x = "State", 
         y = "Median household income",
         fill = "Median household income") +
    theme_bw()
  
states <- readOGR(dsn="cb_2016_us_state_500k", 
                  layer = "cb_2016_us_state_500k", 
                  encoding = "UTF-8", verbose = FALSE)
states_df <- fortify(states, region="GEOID")

state_incomes <- median_income %>% 
  group_by(state, state_id) %>% 
  summarize(state_median=median(income)) %>%
  mutate(id = state_id)

income_df <- full_join(states_df, state_incomes, by='id')
income_df %>%
  ggplot() +
  geom_polygon(data=income_df,
               aes(x=long, y=lat, group=group, fill = state_median),
               color='black', size=0.25)

county <- readOGR(dsn="UScounties", 
                  layer = "UScounties", 
                  encoding = "UTF-8", verbose = FALSE)
county_df <- fortify(county, region="CNTY_FIPS")

county_df <- county_df %>%
  mutate(id = as.numeric(id))

county_poverty <- read_csv("county_poverty.csv") %>%
  mutate(county_fips = as.numeric(str_sub(as.character(county_id), 2, -1))) %>%
  select(state, county_fips, state_county_name, poverty_percent)
  
poverty_df <- merge(county_poverty, county_df, by.x='county_fips', by.y='id')

poverty_df %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=poverty_percent),
               color='white', size=0.25) +
  guides(fill=guide_legend(title="% in Poverty")) +
  #scale_fill_gradient2(limits = c(0, 100000)) +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(-124.85, -66.88), ylim = c(24.4, 49.38),
            orientation = c(90, 0, -98.35)) +
  labs(title="The Northeast United States have higher median Incomes",
       caption="Source: 2011-2015 American Community Survey")

