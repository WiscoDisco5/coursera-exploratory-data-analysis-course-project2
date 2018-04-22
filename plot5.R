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

## plot 5: grouped sums by year for motor vehicles in baltimore

nei %>% 
  filter(fips == "24510") %>%
  filter(grepl('Vehicle', SCC.Level.Two)) %>%
  group_by(year, fips) %>%
  summarise(total_emissions = sum(Emissions)) %>%
  ggplot(aes(year, total_emissions)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1999:2008) +
  labs(x = 'Year',
       y = 'Total Emissions (tons)',
       color = 'Location',
       title = 'Total PM2.5 Emissions from Motor Vehicles over Time in Baltimore City') +
  scale_y_continuous(limits = c(0,500))

dev.copy(png, "plot5.png", width = 480, height =480)
dev.off()

