# Author: Mihai Fecioru [21 Dec 2015]

library(plyr)
library(ggplot2)

# Generates a plot showing total emissions per year (1999 to 2008) but only
# for motor-vechicle-related sources and comparing Los Angeles to Baltimore.
total_emissions_per_year_LAB <- function() {
    
    # Read the source types
    SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
    # Select only the rows that contain Vehicles (search for "Vehicles" word in 
    # "EI.Sector" column).
    a<-SCC[grepl("Vehicles", SCC$EI.Sector), ]
    # Get the list of SCC vaules that corespond to vehicles sources.
    sel<-as.vector(a$SCC)
    
    NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
    
    # Select only the rows that correspond to vechicle sources.
    NEI_Vechicle <- NEI[NEI$SCC %in% sel, ]
    # Select only the rows that correspond to Baltimore.    
    NEI_Vechicle_LAB <- NEI_Vechicle[NEI_Vechicle$fips %in% c("24510", "06037"), ]
    # Convert "fips" to factor and re-label fips values wo pretty names.        
    NEI_Vechicle_LAB$fips <- factor(NEI_Vechicle_LAB$fips)
    NEI_Vechicle_LAB$fips <- revalue(NEI_Vechicle_LAB$fips, c("06037"="Los Angeles","24510"="Baltimore"))
    
    # Generate sum for emissions for each year
    m <- ddply(NEI_Vechicle_LAB, .(year, fips), summarize, Emissions=sum(Emissions))    
    p <- qplot(year, Emissions, data=m, facets=.~fips, geom=c("line"),
          ylab = "Emissions", xlab = "Year",
          main="Total emissions trend from vechicles from 1999 to 2008 (Los Angeles vs Baltimore)")
    p <- p + theme(title = element_text(size=8)) +
             theme(axis.title.x = element_text(size=6)) +
             theme(axis.title.y = element_text(size=6)) +
             theme(axis.text.x = element_text(size=6)) +
             theme(axis.text.y = element_text(size=6))
    ggsave(filename = "plot6.png", width = 14, height = 8, units = "cm")
    m
}
