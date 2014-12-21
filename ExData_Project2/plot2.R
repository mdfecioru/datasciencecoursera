# Author: Mihai Fecioru [21 Dec 2015]

library(plyr)

# Generates a plot showing total emissions in Baltimore per year (1999 to 2008)
total_emissions_per_year_Baltimore <- function() {
    
    NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
    
    # Select only the rows that correspond to Baltimore.
    NEI_Baltimore <- NEI[NEI$fips == "24510", ]
    
    # Generate sum for emissions for each year
    m <- ddply(NEI_Baltimore, .(year), summarize, Emissions=sum(Emissions))    
    png(filename = "plot2.png", width = 480, height = 480, units = "px")    
    plot(m$year, m$Emissions, type="l", ylab = "Emissions", xlab = "Year",
         main="Total emissions trend in Baltimore from 1999 to 2008")    
    dev.off()
    m
}
