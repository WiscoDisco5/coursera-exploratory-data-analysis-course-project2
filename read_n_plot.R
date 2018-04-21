library(tidyverse)

## download data if it isnt in wd
if (!all(file.exists('Source_Classification_Code.rds', 'summarySCC_PM25.rds'))) {
  
  download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip',
                destfile = 'NEI_data.zip')
  unzip(zipfile = 'NEI_data.zip')
  file.remove('NEI_data.zip')

}

## read in data if doesnt exist in environment (assumes no meddling)
if (!exists('nei')) {

  nei <- read_rds('summarySCC_PM25.rds')
  scc <- read_rds('Source_Classification_Code.rds')
  
  scc <- scc %>% as_tibble %>% mutate_if(is.factor, as.character)
  
  nei <- nei %>% as_tibble %>% left_join(scc, by = "SCC")
  
  rm(scc)

}

## plot 1

nei %>% group_by(year) %>% 
  summarise(total_emissions =  sum(Emissions)) %>%
  plot(type='b')

## plot 2

nei %>% filter(fips == "24510") %>%
  group_by(year) %>% 
  summarise(total_emissions =  sum(Emissions)) %>%
  plot(type='b')

## plot 3

nei %>% filter(fips == "24510") %>%
  group_by(year, type) %>% 
  summarise(total_emissions =  sum(Emissions)) %>%
  ggplot(aes(year, total_emissions, color = type, group = type)) +
  geom_line() +
  geom_point()

## plot 4

nei %>% 
  filter(grepl('Combustion', SCC.Level.One)) %>%
  filter(grepl(' Coal', SCC.Level.Three) | grepl(' Coal', SCC.Level.Four) ) %>%
  group_by(year) %>%
  summarise(total_emissions = sum(Emissions)) %>%
  plot(type = 'b')
  
## plot 5

nei %>% 
  filter(fips %in% c("24510", "06037")) %>%
  filter(grepl('Vehicle', SCC.Level.Two)) %>%
  group_by(year, fips) %>%
  summarise(total_emissions = sum(Emissions)) %>%
  mutate(fips_name = if_else(fips == '06037', 'Los Angeles County', 'Baltimore County')) %>%
  ggplot(aes(year, total_emissions, color = fips_name, group = fips_name)) +
  geom_point() +
  geom_line()

