# Author: Mihai Fecioru [21 Dec 2015]

library(plyr)

# Generates a plot showing total emissions per year (1999 to 2008) but only
# for coal-related sources.
total_emissions_per_year_coal <- function() {
    
    # Read the source types
    SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
    # Select only the rows that contain Coal (search for "Coal" word in 
    # "EI.Sector" column).
    a<-SCC[grepl("Coal", SCC$EI.Sector), ]
    # Get the list of SCC vaules that corespond to carbon sources.
    sel<-as.vector(a$SCC)
    
    NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
    
    # Select only the rows that correspond to coal sources.
    NEI_Coal <- NEI[NEI$SCC %in% sel, ]
    
    # Generate sum for emissions for each year
    m <- ddply(NEI_Coal, .(year), summarize, Emissions=sum(Emissions))    
    png(filename = "plot4.png", width = 480, height = 480, units = "px")    
    plot(m$year, m$Emissions, type="l", ylab = "Emissions", xlab = "Year",
         main="Total emissions trend from coal sources from 1999 to 2008")    
    dev.off()
    m
}
