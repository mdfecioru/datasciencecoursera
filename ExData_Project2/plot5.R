# Author: Mihai Fecioru [21 Dec 2015]

library(plyr)

# Generates a plot showing total emissions per year (1999 to 2008) but only
# for motor-vechicle-related sources and for Baltimore only.
total_emissions_per_year_motor_Baltimore<- function() {
    
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
    NEI_Vechicle_Baltimore <- NEI_Vechicle[NEI_Vechicle$fips == "24510", ]
    
    # Generate sum for emissions for each year
    m <- ddply(NEI_Vechicle_Baltimore, .(year), summarize, Emissions=sum(Emissions))    
    png(filename = "plot5.png", width = 600, height = 480, units = "px")    
    plot(m$year, m$Emissions, type="l", ylab = "Emissions", xlab = "Year",
         main="Total emissions trend from vechicle sources in Baltimore from 1999 to 2008")    
    dev.off()
    m
}
