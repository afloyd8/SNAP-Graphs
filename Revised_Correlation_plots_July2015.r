###########  This is a script to plot correlation values into an image and add a legend. ############
########## Written by Angie Floyd ############

mainDir <- "Y:/Tech_Projects/Jane_publication/project_data/Correlation_plots/Revised_Plots_July2015" 
# This is the path to the folder containing the input CSV files. Assuming you keep all the files in this folder, this will not need to be altered. 
data <- "Y:/Tech_Projects/Jane_publication/project_data/Correlation_plots/Revised_Plots_July2015/0026SiteLevelTemp_PrecipCorr.csv" 
# Full path and filename to the CSV file. This will need to be changed when you remake the plots with different input CSV files. 
tree <- read.csv(data) 
# Here we are opening the CSV file of the tree ring correlation data. 
rownames(tree) 
# This line and the following line are looking at the rownames of the data, which are years. 
rownames(tree) = tree[,1] 
tree <- tree[,-1] 
# The first column of the CSV file is the years, but we don't actually want to plot the years, we just want those as labels on the output plot. So, we are going to tell R to ignore the first column using the [,-1] syntax. 
m <- as.matrix(tree) 
# The image() function in the following line requires a matrix as input so the data has to be converted to a matrix form. 


###################### Making the variables for the legend and axis labels: ######################
years <- 1959:2009 
# This is the length of years in each spreadsheet. This changes from file to file and must be updated every time the script is executed.
years2 <- (years-min(years))/(length(years)-1)
# We want the years to be converted to coordinate points from (0,1) so they can be placed on the x-axis. 
l <- seq(years[1],tail(years,1), by=10)
# We are assigning the years to a sequence and saying to start with the first year, end with the last year, and label every tenth year. 
ind <- match(l,years)
# Now we are matching the years with the coordinates we assigned to them for placement on the x-axis. 
colors <- colorRampPalette(c("navyblue","dodgerblue","white","orange","darkred"))(21)
# Color scheme used in the plot, indicating that we would like 21 colors on a gradient between the 5 colors given. 
brks <- seq(-1.05,1.05,length.out=length(colors)+1)
# The break points we want to have on the legend, there must always be one more break than color. 
zlim <- c(-1.05,1.05) 
# The extend the legend will cover, related to the break points, but defining where the labels will start and end. 
binwidth <- diff(brks)[1]
# Distinguishing the correct spacing of bins. 
x <- seq(zlim[1]+binwidth/2, zlim[2]-binwidth/2, by=binwidth)
# Making a sequence from 1-2 and adding half the binwidth to define colors and breakpoints, which will put the labels in the middle of the color bin. 
z <- matrix(x, nrow = 1, ncol = length(x))
# Creating a single row matrix of the sequence x. 


###################### Plotting: #####################
png(paste0(mainDir,"/Site0026.png"), height=4500, width=5200, res=250) 
# Create an empty output PNG to save the final plot into. A new name will need to be assigned for every new plot made with this script. Can alter the resolution here, but it can affect the look of the overall plot and
# it may be necessary to go back and fix other things such as label size and distance from the axes. 
layout(matrix(c(1,2,nrow=2)),height=c(24,1))
# Divides plotting region into two - one for plot and one for legend. Set to be two rows, which is one plot on top of another. The height argument is how much larger the first plot is in relation to the second. 
par(mar=c(11,14,5,10)+0.1, mgp=c(4,1.3,0))
# This sets the margins of the plot larger, so when we add in the legend it will not overlay the title of the x-axis. The default setting is c(5,4,4,2)+0.1. 
# These numbers start on the bottom and move clockwise around the sides of the plot. 
# The second argument of mgp moves the axis labels farther from the tick marks.
image(m, col=colors, axes=FALSE, breaks=brks, zlim=zlim) 
# This is the plotting call where we plot our matrix, m, with the color scheme defined above. The x and y axes are devoid of numbers or tick marks, but we assign them the proper labels. 
axis(2, at=seq(0,1, length=length(colnames(m))), labels=colnames(m), las=1, cex.axis=3.3, tck=-0.01) 
# This line adds in a custom y-axis which is labeled using the column names from the original csv file. This is for the y-axis so it has the number 2 at the beginning. To refer to the x-axis we would use the number 1. 
# The call "las=1" sets the orientation of the axis tick labels horizontal rather than vertical, which is the default. The call to cex.axis changes the size of the axis labels.  
axis(1, at=years2, labels=FALSE, tck=-0.005)
# This is the first of two calls to create the x-axis. This makes minor ticks that are half the size of the major x-axis ticks. In the second axis call we plot the labels. 
par(mgp=c(4,3.0,0))
# Changing the location of the labels in relation to the x axis. The middle number is the distance of the labels from the axis. Have to change because the distance is different than in the first call to mgp, which controls the y-axis. 
axis(1, at=years2[ind], labels=l, cex.axis=3.7, tck=-0.01)
# This adds in a custom x-axis using the years and labeling the tick marks as such. This contains the labels for the x-axis.  
abline(h=0.5) 
# Adds a black line across the plot between the temp and precip variables
title(ylab="Monthly Climatic Variable", cex.lab=4.0, line=10.8) 
# Labeling the y-axis. Cex.lab is setting the size of the text and line=4.25 is how far from the plot we want the axis title to be, which relates to the plot margins. 
# In the initial call to mar on line 48, we set the margin to be 14 for this side of the plot. The larger the number in line=, the farther away from the plot the title will be.
title(xlab="Last Year of Interval", cex.lab=4.0, line=6.9) 
# Same as previous line except now for the x-axis. 
box() 
# Adds a box around the plot
title(main="Moving Intervals: Correlation Values", cex.main=4.0) 
# Adding a title 


####################### Creating the legend: ##########################
op <- par(mar=c(4.5,7,0,2)+0.1)
# Margins for legend
image(x,1,t(z),axes=F,xlab="", ylab="",col=colors)
# Plotting the legend, the 1 makes it a single row. Z is x again, except it has been put in a matrix and transposed so it will plot horizontally. 
box()
par(mgp=c(4,2.5,0))
# Here is another call to mgp, this time it is controlling how close the labels on the legend are to the label axis.
# Normally this could be embedded in the par command, such as on line 103, but that would move the entire legend and we just want to control the spacing of the legend labels in relation to the axis. So, we add this call
# directly before ploting the legend axis.  
axis(1, at=seq(-1,1,by=0.1), labels=round(seq(-1,1,by=0.1),2), cex.axis=3.3)
# Adding the legend axis. 
par(op) 
# Sets the parameters back to the orginial. 
dev.off() 
# Writes the png to the file previously created and closes it. 
