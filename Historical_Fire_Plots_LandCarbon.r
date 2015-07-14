## The R code contained in this script makes graphs of fire count and area burned for subregions within the state of Alaska, based on data extracted from the Alaska Fire Service Historical Fires shapefile. 
## There are three versions, the first is for making plots individually, the second for putting the fire count and km burned graphs into a single figure, and the third is for putting all five ecoregions 
## and both variables (fire count and area burned) into a single ten-panel plot. 
##
## By Angie Floyd
####################################################################################################################################################################################################
# This is a script to plot number of fires per year and km burned as separate stand-alone graphs.

annual <- read.csv("/workspace/Shared/Users/afloyd8/Landcarbon/Annual_AB_fireCount_1950-2009.csv")
d <- data.frame(annual)
png <- png("/workspace/Shared/Users/afloyd8/Landcarbon/Annual_FireCount_1950-2009.png", width=8, height=6, units="in", res=300)
par(mar=c(8.5,6,4,1.5))
p <- plot(fireCount~Year,d, ylab="Number of Fires", cex.lab=1.2, axes=FALSE, main="Number of Historical Fires by Year") 
abline(h=seq(0,175,25), lty=3, col="lightgrey")
axis(2, at=seq(0, 175, by=25), las=2, cex=1.2)
axis(1, at=seq(1950,2010, by=10), cex=1.2)
box()
dev.off()

png <- png("/workspace/Shared/Users/afloyd8/Landcarbon/Annual_AB_1950-2009.png", width=8, height=6.5, units="in", res=300)
par(mar=c(8.5,6,4,1.5))
p <- plot(kmBurned~Year,d, ylab=NA, axes=FALSE, cex.lab=1.2, main="Area Burned by Year") 
abline(h=seq(0, 25000, by=5000), lty=3, col="lightgrey")
axis(2, at=seq(0, 25000, by=5000), las=2, cex=1.2)
axis(1, at=seq(1950,2010, by=10), cex=1.2) 
title(ylab="Square Kilometers Burned", cex.lab=1.2, line=4)
box()
dev.off()

#################################################################################################################################################
# This piece of code will make the plots two panels within one figure. 

annual <- read.csv("/workspace/Shared/Users/afloyd8/Landcarbon/Annual_AB_fireCount_1950-2009.csv")
png <- png("/workspace/Shared/Users/afloyd8/Landcarbon/Annual_FireStats_1950-2009.png", width=8, height=10, units="in", res=300) # To make the graphs less squished, make the height a larger number. 
d <- data.frame(annual)
par(mfrow=c(2,1))
par(mar=c(4,6,4,1))
p <- plot(fireCount~Year,d, ylab="Number of Fires", cex.lab=1.2, axes=FALSE, main="Number of Historical Fires by Year") 
abline(h=seq(0,175,25), lty=3, col="lightgrey")
axis(2, at=seq(0, 175, by=25), las=2, cex=1.2)
axis(1, at=seq(1950,2010, by=10), cex=1.2)
box()
p <- plot(kmBurned~Year,d, ylab=NA, axes=FALSE, cex.lab=1.2, main="Area Burned by Year") 
abline(h=seq(0, 25000, by=5000), lty=3, col="lightgrey")
axis(2, at=seq(0, 25000, by=5000), las=2, cex=1.2)
axis(1, at=seq(1950,2010, by=10), cex=1.2) 
title(ylab="Square Kilometers Burned", cex.lab=1.2, line=4)
box()
dev.off()

######################################################################################################################################################
# The following blocks of code will plot ten graphs in one figure. There are five ecoregions and each has a plot for fire count and as well as km burned. 
# In this figure, the code is referencing decadal data rather than annual data. 

arctic <- read.csv("/workspace/Shared/Users/afloyd8/Landcarbon/ArcticLCC_FireStats.csv")
count <- arctic[,3] # The csv file has 4 columns - "Decade", "kmBurned", "fireCount", and "LCC". This line is indexing out the third column "fireCount". 
ab <- arctic[,2] # Indexing the second column "kmBurned"
decades <- arctic[,1] # Indexing the first column "Decade"
png <- png("/workspace/Shared/Users/afloyd8/Landcarbon/Fig3.3.png", width=8, height=11, units="in", res=300) # Setting up the initial ouput png file. 
par(mfrow=c(5,2)) # This command sets the layout, which we would like to be 5 rows long and 2 columns wide
par(mar=c(4,6,4,1)) # Setting margins
b <- barplot(count, ylab="Number of Fires", xlab=NA, ylim=c(0,55), axes=FALSE, main="Arctic Fire Count per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 55, by=10), las=2, cex.axis=1)
box()
b <- barplot(ab, ylab=NA, xlab=NA, ylim=c(0,1500), axes=FALSE, main="Arctic Area Burned per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 1500, by=300), las=2, cex.axis=1)
title(ylab="Square km Burned", cex.lab=1, line=3.8)
box()

northPacific <- read.csv("/workspace/Shared/Users/afloyd8/Landcarbon/NorthPacificLCC_FireStats.csv")
count <- northPacific[,3]
ab <- northPacific[,2]
decades <- northPacific[,1]
b <- barplot(count, ylab="Number of Fires", xlab=NA, ylim=c(0,18), axes=FALSE, main="North Pacific Fire Count per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 18, by=3), las=2, cex.axis=1)
box()
b <- barplot(ab, ylab=NA, xlab=NA, ylim=c(0,55), axes=FALSE, main="North Pacific Area Burned per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 50, by=10), las=2, cex.axis=1)
title(ylab="Square km Burned", cex.lab=1, line=3.8)
box()

interiorNorth <- read.csv("/workspace/Shared/Users/afloyd8/Landcarbon/NorthWestInteriorForestNorthLCC_FireStats.csv")
count <- interiorNorth[,3]
ab <- interiorNorth[,2]
decades <- interiorNorth[,1]
b <- barplot(count, ylab="Number of Fires", xlab=NA, ylim=c(0,700), axes=FALSE, main="NW Interior Forest North Fire Count per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 700, by=100), las=2, cex.axis=1)
box()
b <- barplot(ab, ylab=NA, xlab=NA, ylim=c(0,80000), axes=FALSE, main="NW Interior Forest North Area Burned per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 80000, by=20000), las=2, cex.axis=1)
title(ylab="Square km Burned", cex.lab=1, line=3.8)
box()

interiorSouth <- read.csv("/workspace/Shared/Users/afloyd8/Landcarbon/NorthWestInteriorForestSouthLCC_FireStats.csv")
count <- interiorSouth[,3]
ab <- interiorSouth[,2]
decades <- interiorSouth[,1]
b <- barplot(count, ylab="Number of Fires", xlab=NA, ylim=c(0,60), axes=FALSE, main="NW Interior Forest South Fire Count per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 60, by=10), las=2, cex.axis=1)
box()
b <- barplot(ab, ylab=NA, xlab=NA, ylim=c(0,1500), axes=FALSE, main="NW Interior Forest South Area Burned per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 1500, by=300), las=2, cex.axis=1)
title(ylab="Square km Burned", cex.lab=1, line=3.8)
box()

western <- read.csv("/workspace/Shared/Users/afloyd8/Landcarbon/WesternAlaskaLCC_FireStats.csv")
count <- western[,3]
ab <- western[,2]
decades <- western[,1]
b <- barplot(count, ylab="Number of Fires", xlab=NA, ylim=c(0,120), axes=FALSE, main="Western Alaska Fire Count per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 120, by=20), las=2, cex.axis=1)
box()
b <- barplot(ab, ylab=NA, xlab=NA, ylim=c(0,15000), axes=FALSE, main="Western Alaska Area Burned per Decade")
axis(1, at=c(0.7,1.9,3.1,4.3,5.5,6.7), labels=decades, cex.axis=1, cex.lab=1)
axis(2, at=seq(0, 15000, by=3000), las=2, cex.axis=1)
title(ylab="Square km Burned", cex.lab=1, line=3.8)
box()

dev.off()















