# Author: Mihai Fecioru [21 Dec 2015]

library(plyr)
library(ggplot2)

# Generates a plot showing total emissions per year (1999 to 2008) bucketized
# by type, for Baltimore city only.
total_emissions_per_year_and_type_Baltimore <- function() {
    
    NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
    
    # Select only the rows that correspond to Baltimore.
    NEI_Baltimore <- NEI[NEI$fips == "24510", ]
        
    # Generate sum for emissions for each year
    m <- ddply(NEI_Baltimore, .(year, type), summarize, Emissions=sum(Emissions))    
    p <- qplot(year, Emissions, data=m, facets=.~type, geom=c("line"),
          ylab = "Emissions", xlab = "Year",
          main="Total emissions trend from 1999 to 2008 for Baltimore (per type)")
    p <- p + theme(title = element_text(size=8)) +
             theme(axis.title.x = element_text(size=6)) +
             theme(axis.title.y = element_text(size=6)) +
             theme(axis.text.x = element_text(size=6)) +
             theme(axis.text.y = element_text(size=6))
    ggsave(filename = "plot3.png", width = 14, height = 8, units = "cm")
    m
}
