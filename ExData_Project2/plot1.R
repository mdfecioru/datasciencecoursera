# Author: Mihai Fecioru [21 Dec 2015]

library(plyr)

# Generates a plot showing total emissions per year (1999 to 2008)
total_emissions_per_year <- function() {
    
    NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
    
    # Generate sum for emissions for each year
    m <- ddply(NEI, .(year), summarize, Emissions=sum(Emissions))    
    png(filename = "plot1.png", width = 480, height = 480, units = "px")    
    plot(m$year, m$Emissions, type="l", ylab = "Emissions", xlab = "Year",
         main="Total emissions trend from 1999 to 2008")
    dev.off()
    m
}
