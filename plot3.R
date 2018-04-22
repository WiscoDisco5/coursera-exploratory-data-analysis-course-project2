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

## plot 3: grouped sums by type and year in baltimore

nei %>% filter(fips == "24510") %>%
  group_by(year, type) %>% 
  summarise(total_emissions =  sum(Emissions)/1000) %>%
  ggplot(aes(year, total_emissions, color = type, group = type)) +
  geom_line() +
  geom_point() +
  labs(x = 'Year',
       y = 'Total Emissions (kilotons)',
       title = 'Total PM2.5 Emissions over Time by Type in Baltimore City',
       color = 'Type') +
  scale_x_continuous(breaks = 1999:2008)

dev.copy(png, "plot3.png", width = 480, height =480)
dev.off()