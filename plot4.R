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

## plot 4: grouped sums for coal combustion by year

nei %>% 
  filter(grepl('Combustion', SCC.Level.One)) %>%
  filter(grepl(' Coal', SCC.Level.Three) | grepl(' Coal', SCC.Level.Four) ) %>%
  group_by(year) %>%
  summarise(total_emissions = sum(Emissions)/1000) %>%
  plot(type = 'b',
       ylim = c(0,max(.$total_emissions)),
       ylab = 'Total Emissions (kilotons)',
       xlab = 'Year',
       main = 'Total PM2.5 Emissions from Coal Combustion over Time')

axis(1, 1999:2008)

dev.copy(png, "plot4.png", width = 480, height =480)
dev.off()