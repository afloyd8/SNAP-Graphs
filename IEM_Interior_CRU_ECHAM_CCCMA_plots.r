### This is a script to create two time series plots over Interior Alaska using IEM versions of CRU and the models ECHAM5 and CCCMA (A1B scenario) for the full time series of both CRU and the projected data (1901-2100). 
### Customers would also like the data exported to .csv format.  

### The plots are:

### 1. Mean annual temp for CRU, CCCMA, ECHAM5
###    Mean temp averaged across March, April, May, June, and July for CRU, CCCMA, ECHAM5

### 2. Total annual precipitation for CRU, CCCMA, ECHAM5
###    Total precipitation averaged across March, April, May, June, and July for CRU, CCCMA, ECHAM5

library (raster)
library(rgdal)
library (shapefiles)
library(parallel)

### load shapefile of AOI
aoi <- shapefile("/workspace/Shared/Users/afloyd8/Amy/ALF_CAVMbuf.shp")

### load temperature data paths
cru.tas <- "/Data/Base_Data/ALFRESCO_formatted/ALFRESCO_Master_Dataset/ALFRESCO_Model_Input_Datasets/AK_CAN_Inputs/Climate/cru_TS31/historical/tas"
echam.tas <- "/Data/Base_Data/ALFRESCO_formatted/ALFRESCO_Master_Dataset/ALFRESCO_Model_Input_Datasets/AK_CAN_Inputs/Climate/mpi_echam5/sresa1b/tas"
cccma.tas <- "/Data/Base_Data/ALFRESCO_formatted/ALFRESCO_Master_Dataset/ALFRESCO_Model_Input_Datasets/AK_CAN_Inputs/Climate/cccma_cgcm3_1/sresa1b/tas"

### load precipitation data paths
cru.pr <- "/Data/Base_Data/ALFRESCO_formatted/ALFRESCO_Master_Dataset/ALFRESCO_Model_Input_Datasets/AK_CAN_Inputs/Climate/cru_TS31/historical/pr"
echam.pr <- "/Data/Base_Data/ALFRESCO_formatted/ALFRESCO_Master_Dataset/ALFRESCO_Model_Input_Datasets/AK_CAN_Inputs/Climate/mpi_echam5/sresa1b/pr"
cccma.pr <- "/Data/Base_Data/ALFRESCO_formatted/ALFRESCO_Master_Dataset/ALFRESCO_Model_Input_Datasets/AK_CAN_Inputs/Climate/cccma_cgcm3_1/sresa1b/pr"

### CRU
yrs <- 1901:2009
historical.f <- function(i) {
	pat <- gsub("expression","",paste(bquote(expression(.(yrs[i]),".tif$")),collapse="")) # Orders chronologically
	# CRU tas
	cru.tasFiles <- list.files(cru.tas, pattern=pat, full=T, recur=F) 
	s <- stack(cru.tasFiles)
	cru.tasClip <- extract(s, aoi)
	cru.tasMean <- round(colMeans(cru.tasClip[[1]], na.rm=TRUE),1) # Averages each monthly raster layer into a single value
	cru.tasMean <- colMeans(data.frame(cru.tasMean)) # Averages the 12 monthly averages into a single value
	# CRU pr
	cru.prFiles <- list.files(cru.pr, pattern=pat, full=T, recur=F)
	s <- stack(cru.prFiles)
	cru.prClip <- extract(s, aoi)
	cru.prMean <- round(colMeans(cru.prClip[[1]], na.rm=TRUE),0)
	cru.prMean <- colMeans(data.frame(cru.prMean))
	### 5 Month Averages
	pat <- gsub("expression","",paste(bquote(expression(.(yrs[i]),".tif$")),collapse="")) # Orders chronologically
	# CRU tas
	cru.tasFiles <- list.files(cru.tas, pattern=pat, full=T, recur=F)[3:7] # The first iteration of this command was for all the monhts of the year, this second iteration is for only 5 months
	s <- stack(cru.tasFiles)
	cru.tasClip <- extract(s, aoi)
	cru.tasMean_5month <- round(colMeans(cru.tasClip[[1]], na.rm=TRUE),1)
	cru.tasMean_5month <- colMeans(data.frame(cru.tasMean_5month))
	# CRU pr
	cru.prFiles <- list.files(cru.pr, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(cru.prFiles)
	cru.prClip <- extract(s, aoi)
	cru.prMean_5month <- round(colMeans(cru.prClip[[1]], na.rm=TRUE),0)
	cru.prMean_5month <- colMeans(data.frame(cru.prMean_5month))
	
	m1 <- round(data.frame(Year=yrs[i], CRU_Mean_Ann_Temp=cru.tasMean, CRU_Total_Ann_Pr=cru.prMean, CRU_Mean_5month_Temp=cru.tasMean_5month, CRU_Total_5month_Pr=cru.prMean_5month),1)
	print(yrs[i])
	m1
}

m1 <- mclapply(1:length(yrs), historical.f, mc.cores=16)
m1 <- do.call(rbind, m1)

### Projected Annual Averages
yrs <- 2001:2100
projected.f <- function(i) {
	pat <- gsub("expression","",paste(bquote(expression(.(yrs[i]),".tif$")),collapse="")) # Orders chronologically
	# ECHAM5 tas
	echam.tasFiles <- list.files(echam.tas, pattern=pat, full=T, recur=F)
	s <- stack(echam.tasFiles)
	echam.tasClip <- extract(s, aoi)
	echam.tasMean <- round(colMeans(echam.tasClip[[1]], na.rm=TRUE),1)
	echam.tasMean <- colMeans(data.frame(echam.tasMean))
	# ECHAM5 pr
	echam.prFiles <- list.files(echam.pr, pattern=pat, full=TRUE, recur=F)
	s <- stack(echam.prFiles)
	echam.prClip <- extract(s, aoi)
	echam.prMean <- round(colMeans(echam.prClip[[1]], na.rm=TRUE))
	echam.prMean <- colMeans(data.frame(echam.prMean))
	# CCCMA tas
	cccma.tasFiles <- list.files(cccma.tas, pattern=pat, full=TRUE, recur=F)
	s <- stack(cccma.tasFiles)
	cccma.tasClip <- extract(s, aoi)
	cccma.tasMean <- round(colMeans(cccma.tasClip[[1]], na.rm=TRUE),1)
	cccma.tasMean <- colMeans(data.frame(cccma.tasMean))
	# CCCMA pr
	cccma.prFiles <- list.files(cccma.pr, pattern=pat, full=TRUE, recur=F)
	s <- stack(cccma.prFiles)
	cccma.prClip <- extract(s, aoi)
	cccma.prMean <- round(colMeans(cccma.prClip[[1]], na.rm=TRUE))
	cccma.prMean <- colMeans(data.frame(cccma.prMean))
	#### 5 Month Averages
	pat <- gsub("expression","",paste(bquote(expression(.(yrs[i]),".tif$")),collapse="")) # Orders chronologically
	# ECHAM5 tas
	echam.tasFiles <- list.files(echam.tas, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(echam.tasFiles)
	echam.tasClip <- extract(s, aoi)
	echam.tasMean_5month <- round(colMeans(echam.tasClip[[1]], na.rm=TRUE),1)
	echam.tasMean_5month <- colMeans(data.frame(echam.tasMean_5month))
	# ECHAM5 pr
	echam.prFiles <- list.files(echam.pr, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(echam.prFiles)
	echam.prClip <- extract(s, aoi)
	echam.prMean_5month <- round(colMeans(echam.prClip[[1]], na.rm=TRUE),0)
	echam.prMean_5month <- colMeans(data.frame(echam.prMean_5month))
	# CCCMA tas
	cccma.tasFiles <- list.files(cccma.tas, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(cccma.tasFiles)
	cccma.tasClip <- extract(s, aoi)
	cccma.tasMean_5month <- round(colMeans(cccma.tasClip[[1]], na.rm=TRUE),1)
	cccma.tasMean_5month <- colMeans(data.frame(cccma.tasMean_5month))
	# CCCMA pr
	cccma.prFiles <- list.files(cccma.pr, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(cccma.prFiles)
	cccma.prClip <- extract(s, aoi)
	cccma.prMean_5month <- round(colMeans(cccma.prClip[[1]], na.rm=TRUE),0)
	cccma.prMean_5month <- colMeans(data.frame(cccma.prMean_5month))
	m2 <- round(data.frame(Year=yrs[i], ECHAM_Mean_Ann_Temp=echam.tasMean, ECHAM_Total_Ann_Pr=echam.prMean, ECHAM_Mean_5month_Temp=echam.tasMean_5month, 
	ECHAM_Total_5month_Pr=echam.prMean_5month, CCCMA_Mean_Ann_Temp=cccma.tasMean, CCCMA_Total_Ann_Pr=cccma.prMean, CCCMA_Mean_5month_Temp=cccma.tasMean_5month, CCCMA_Total_5month_Pr=cccma.prMean_5month),1)
	print(yrs[i])
	m2
}

m2 <- mclapply(1:length(yrs), projected.f, mc.cores=16)
m2 <- do.call(rbind, m2)
m <- merge(m1, m2, all=TRUE)

write.csv(m, file="/workspace/Shared/Users/afloyd8/Amy/CAVMbuf_tas_pr.csv", row.names=FALSE)

###### Temperature Plots
# Combined
png("/workspace/Shared/Users/afloyd8/Amy/CAVMbuf_Temp.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Mean_Ann_Temp, type="l", col="gray40", lwd=1.5, main= "CAVM Buffered Region in Alaska Mean Temperature: 1901-2100", axes=FALSE, xlab="Year", ylab="Mean Temperature (C)", ylim=c(-10,8))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(-10,8,2), labels=c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(-10,8,2), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Mean_Ann_Temp, type="l", col="red", cex=0.3, lwd=1, lty=5)
lines(m$Year, m$CCCMA_Mean_Ann_Temp, type="l", col="dodgerblue", lwd=1.5, lty=3)
lines(m$Year, m$CRU_Mean_5month_Temp, type="o", col="black", cex=0.4, lwd=1.5, pch=15)
lines(m$Year, m$ECHAM_Mean_5month_Temp, type="o", col="firebrick", cex=0.5, lwd=1, lty=5, pch=16)
lines(m$Year, m$CCCMA_Mean_5month_Temp, type="o", col="royalblue", cex=0.6, lwd=1.5, lty=3, pch=18)
lines(m$Year, m$CRU_Mean_Ann_Temp, type="l", col="gray40", lwd=1.5) # Plotting the CRU data again so it is on top of the grid lines.
legend("topleft", inset=c(0.02,0.05), col=c("gray40", "red", "dodgerblue", "black", "firebrick", "royalblue"), lwd=1.5, cex=0.8, lty=rep(c(1,5,3)), 
legend=c("CRU Annual", "ECHAM5 Annual", "CCCMA Annual", "CRU March-July", "ECHAM5 March-July", "CCCMA March-July"), pch=c(NA,NA,NA,15,16,18), ncol=2, seg.len=3) 
box()
dev.off()

# Annual Temp
png("/workspace/Shared/Users/afloyd8/Amy/CAVMbuf_Annual_Temp.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Mean_Ann_Temp, type="l", col="gray40", lwd=1.5, main= "CAVM Buffered Region in Alaska Annual Mean Temperature: 1901-2100", axes=FALSE, xlab="Year", ylab="Mean Temperature (C)", ylim=c(-10,8))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(-10,8,2), labels=c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(-10,8,2), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Mean_Ann_Temp, type="l", col="red", cex=0.3, lwd=1, lty=5)
lines(m$Year, m$CCCMA_Mean_Ann_Temp, type="l", col="dodgerblue", lwd=1.5, lty=3)
lines(m$Year, m$CRU_Mean_Ann_Temp, type="l", col="gray40", lwd=1.5)
legend("topleft", inset=c(0.02,0.05), col=c("gray40", "red", "dodgerblue"), lwd=1.5, cex=0.8, lty=c(1,5,3), legend=c("CRU", "ECHAM5", "CCCMA"), seg.len=3) 
box()
dev.off()

# 5 month Temp
png("/workspace/Shared/Users/afloyd8/Amy/CAVMbuf_March-July_Temp.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Mean_5month_Temp, type="o", col="black", lwd=1.5, cex=0.4, pch=15, main= "CAVM Buffered Region in Alaska March-July Mean Temperature: 1901-2100", axes=FALSE, xlab="Year", ylab="Mean Temperature (C)", ylim=c(-10,8))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(-10,8,2), labels=c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(-10,8,2), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Mean_5month_Temp, type="o", col="firebrick", cex=0.5, lwd=1, lty=5, pch=16)
lines(m$Year, m$CCCMA_Mean_5month_Temp, type="o", col="royalblue", cex=0.6, lwd=1.5, lty=3, pch=18)
lines(m$Year, m$CRU_Mean_5month_Temp, type="o", col="black", cex=0.4, lwd=1.5, pch=15)
legend("topleft", inset=c(0.02,0.05), col=c("black", "firebrick", "royalblue"), lwd=1.5, cex=0.8, lty=c(1,5,3), legend=c("CRU", "ECHAM5", "CCCMA"), pch=c(15,16,18), seg.len=3)
box()
dev.off()

####### Precipitation Plots
# Combined
png("/workspace/Shared/Users/afloyd8/Amy/CAVMbuf_Total_Pr.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Total_Ann_Pr, type="l", col="gray40", lwd=1.5, main= "CAVM Buffered Region in Alaska Mean Total Precipitation: 1901-2100", axes=FALSE, xlab="Year", ylab="Total Precipitation (mm)", ylim=c(10,70))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(10,70,10), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(10,70,10), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Total_Ann_Pr, type="l", col="red", cex=0.3, lwd=1, lty=5)
lines(m$Year, m$CCCMA_Total_Ann_Pr, type="l", col="dodgerblue", lwd=1.5, lty=3)
lines(m$Year, m$CRU_Total_5month_Pr, type="o", col="black", cex=0.4, lwd=1.5, pch=15)
lines(m$Year, m$ECHAM_Total_5month_Pr, type="o", col="firebrick", cex=0.5, lwd=1, lty=5, pch=16)
lines(m$Year, m$CCCMA_Total_5month_Pr, type="o", col="royalblue", cex=0.6, lwd=1.5, lty=3, pch=18)
lines(m$Year, m$CRU_Total_Ann_Pr, type="l", col="gray40", lwd=1.5)
legend("topleft", inset=c(0.02,0.05), col=c("gray40", "red", "dodgerblue", "black", "firebrick", "royalblue"), lwd=1.5, cex=0.8, lty=rep(c(1,5,3)), 
legend=c("CRU Annual", "ECHAM5 Annual", "CCCMA Annual", "CRU March-July", "ECHAM5 March-July", "CCCMA March-July"), pch=c(NA,NA,NA,15,16,18), ncol=2, seg.len=3) 
box()
dev.off()

# Annual Total Pr
png("/workspace/Shared/Users/afloyd8/Amy/CAVMbuf_Total_Annual_Pr.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Total_Ann_Pr, type="l", col="gray40", lwd=1.5, main= "CAVM Buffered Region in Alaska Mean Annual Total Precipitation: 1901-2100", axes=FALSE, xlab="Year", ylab="Total Precipitation (mm)", ylim=c(10,70))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(10,70,10), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(10,70,10), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Total_Ann_Pr, type="l", col="red", cex=0.3, lwd=1, lty=5)
lines(m$Year, m$CCCMA_Total_Ann_Pr, type="l", col="dodgerblue", lwd=1.5, lty=3)
lines(m$Year, m$CRU_Total_Ann_Pr, type="l", col="gray40", lwd=1.5) # Plotting the CRU data again so it is on top of the grid lines.
legend("topleft", inset=c(0.02,0.05), col=c("gray40", "red", "dodgerblue"), lwd=1.5, cex=0.8, lty=c(1,5,3), legend=c("CRU", "ECHAM5", "CCCMA"), seg.len=3) 
box()
dev.off()

# 5 Month total Pr
png("/workspace/Shared/Users/afloyd8/Amy/CAVMbuf_Total_5month_Pr.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Total_5month_Pr, type="l", col="black", lwd=1.5, cex=0.4, main= "CAVM Buffered Region in Alaska Mean March-July Total Precipitation: 1901-2100", axes=FALSE, xlab="Year", ylab="Total Precipitation (mm)", ylim=c(10,70))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(10,70,10), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(10,70,10), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$CRU_Total_5month_Pr, type="o", col="black", cex=0.4, lwd=1.5, pch=15)
lines(m$Year, m$ECHAM_Total_5month_Pr, type="o", col="firebrick", cex=0.5, lwd=1, lty=5, pch=16)
lines(m$Year, m$CCCMA_Total_5month_Pr, type="o", col="royalblue", cex=0.6, lwd=1.5, lty=3, pch=18)
legend("topleft", inset=c(0.02,0.05), col=c("black", "firebrick", "royalblue"), lwd=1.5, cex=0.8, lty=c(1,5,3), legend=c("CRU", "ECHAM5", "CCCMA"), pch=c(15,16,18), seg.len=3)
box()
dev.off()

###############################################################################################################################################################################################################
############################## This is a duplication of the first script except using a second shapefile called "CAVMonly" ####################################################################################
###############################################################################################################################################################################################################

aoi <- shapefile("/workspace/Shared/Users/afloyd8/Amy/ALF_CAVMonly_extent.shp")

### CRU Annual Average
yrs <- 1901:2009
historical.f <- function(i) {
	pat <- gsub("expression","",paste(bquote(expression(.(yrs[i]),".tif$")),collapse="")) # Orders chronologically
	# CRU tas
	cru.tasFiles <- list.files(cru.tas, pattern=pat, full=T, recur=F)
	s <- stack(cru.tasFiles)
	cru.tasClip <- extract(s, aoi)
	cru.tasMean <- round(colMeans(cru.tasClip[[1]], na.rm=TRUE),1) # Averages each monthly raster layer into a single value
	cru.tasMean <- colMeans(data.frame(cru.tasMean)) # Averages the 12 monthly averages into a single value
	# CRU pr
	cru.prFiles <- list.files(cru.pr, pattern=pat, full=T, recur=F)
	s <- stack(cru.prFiles)
	cru.prClip <- extract(s, aoi)
	cru.prMean <- round(colMeans(cru.prClip[[1]], na.rm=TRUE),0)
	cru.prMean <- colMeans(data.frame(cru.prMean))
	### 5 Month Averages
	pat <- gsub("expression","",paste(bquote(expression(.(yrs[i]),".tif$")),collapse="")) # Orders chronologically
	# CRU tas
	cru.tasFiles <- list.files(cru.tas, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(cru.tasFiles)
	cru.tasClip <- extract(s, aoi)
	cru.tasMean_5month <- round(colMeans(cru.tasClip[[1]], na.rm=TRUE),1)
	cru.tasMean_5month <- colMeans(data.frame(cru.tasMean_5month))
	# CRU pr
	cru.prFiles <- list.files(cru.pr, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(cru.prFiles)
	cru.prClip <- extract(s, aoi)
	cru.prMean_5month <- round(colMeans(cru.prClip[[1]], na.rm=TRUE),0)
	cru.prMean_5month <- colMeans(data.frame(cru.prMean_5month))
	
	m1 <- round(data.frame(Year=yrs[i], CRU_Mean_Ann_Temp=cru.tasMean, CRU_Total_Ann_Pr=cru.prMean, CRU_Mean_5month_Temp=cru.tasMean_5month, CRU_Total_5month_Pr=cru.prMean_5month),1)
	print(yrs[i])
	m1
}

m1 <- mclapply(1:length(yrs), historical.f, mc.cores=16)
m1 <- do.call(rbind, m1)

### Projected Annual Averages
yrs <- 2001:2100
projected.f <- function(i) {
	pat <- gsub("expression","",paste(bquote(expression(.(yrs[i]),".tif$")),collapse="")) # Orders chronologically
	# ECHAM5 tas
	echam.tasFiles <- list.files(echam.tas, pattern=pat, full=T, recur=F)
	s <- stack(echam.tasFiles)
	echam.tasClip <- extract(s, aoi)
	echam.tasMean <- round(colMeans(echam.tasClip[[1]], na.rm=TRUE),1)
	echam.tasMean <- colMeans(data.frame(echam.tasMean))
	# ECHAM5 pr
	echam.prFiles <- list.files(echam.pr, pattern=pat, full=TRUE, recur=F)
	s <- stack(echam.prFiles)
	echam.prClip <- extract(s, aoi)
	echam.prMean <- round(colMeans(echam.prClip[[1]], na.rm=TRUE))
	echam.prMean <- colMeans(data.frame(echam.prMean))
	# CCCMA tas
	cccma.tasFiles <- list.files(cccma.tas, pattern=pat, full=TRUE, recur=F)
	s <- stack(cccma.tasFiles)
	cccma.tasClip <- extract(s, aoi)
	cccma.tasMean <- round(colMeans(cccma.tasClip[[1]], na.rm=TRUE),1)
	cccma.tasMean <- colMeans(data.frame(cccma.tasMean))
	# CCCMA pr
	cccma.prFiles <- list.files(cccma.pr, pattern=pat, full=TRUE, recur=F)
	s <- stack(cccma.prFiles)
	cccma.prClip <- extract(s, aoi)
	cccma.prMean <- round(colMeans(cccma.prClip[[1]], na.rm=TRUE))
	cccma.prMean <- colMeans(data.frame(cccma.prMean))
	#### 5 Month Averages
	pat <- gsub("expression","",paste(bquote(expression(.(yrs[i]),".tif$")),collapse="")) # Orders chronologically
	# ECHAM5 tas
	echam.tasFiles <- list.files(echam.tas, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(echam.tasFiles)
	echam.tasClip <- extract(s, aoi)
	echam.tasMean_5month <- round(colMeans(echam.tasClip[[1]], na.rm=TRUE),1)
	echam.tasMean_5month <- colMeans(data.frame(echam.tasMean_5month))
	# ECHAM5 pr
	echam.prFiles <- list.files(echam.pr, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(echam.prFiles)
	echam.prClip <- extract(s, aoi)
	echam.prMean_5month <- round(colMeans(echam.prClip[[1]], na.rm=TRUE),0)
	echam.prMean_5month <- colMeans(data.frame(echam.prMean_5month))
	# CCCMA tas
	cccma.tasFiles <- list.files(cccma.tas, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(cccma.tasFiles)
	cccma.tasClip <- extract(s, aoi)
	cccma.tasMean_5month <- round(colMeans(cccma.tasClip[[1]], na.rm=TRUE),1)
	cccma.tasMean_5month <- colMeans(data.frame(cccma.tasMean_5month))
	# CCCMA pr
	cccma.prFiles <- list.files(cccma.pr, pattern=pat, full=T, recur=F)[3:7]
	s <- stack(cccma.prFiles)
	cccma.prClip <- extract(s, aoi)
	cccma.prMean_5month <- round(colMeans(cccma.prClip[[1]], na.rm=TRUE),0)
	cccma.prMean_5month <- colMeans(data.frame(cccma.prMean_5month))
	m2 <- round(data.frame(Year=yrs[i], ECHAM_Mean_Ann_Temp=echam.tasMean, ECHAM_Total_Ann_Pr=echam.prMean, ECHAM_Mean_5month_Temp=echam.tasMean_5month, 
	ECHAM_Total_5month_Pr=echam.prMean_5month, CCCMA_Mean_Ann_Temp=cccma.tasMean, CCCMA_Total_Ann_Pr=cccma.prMean, CCCMA_Mean_5month_Temp=cccma.tasMean_5month, CCCMA_Total_5month_Pr=cccma.prMean_5month),1)
	print(yrs[i])
	m2
}

m2 <- mclapply(1:length(yrs), projected.f, mc.cores=16)
m2 <- do.call(rbind, m2)
m <- merge(m1, m2, all=TRUE)

write.csv(m, file="/workspace/Shared/Users/afloyd8/Amy/CAVMonly_tas_pr.csv", row.names=FALSE)

###### Temperature Plots
# Combined
png("/workspace/Shared/Users/afloyd8/Amy/CAVMonly_Temp.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Mean_Ann_Temp, type="l", col="gray40", lwd=1.5, main= "CAVM Region in Alaska Mean Temperature: 1901-2100", axes=FALSE, xlab="Year", ylab="Mean Temperature (C)", ylim=c(-10,8))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(-10,8,2), labels=c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(-10,12,2), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Mean_Ann_Temp, type="l", col="red", cex=0.3, lwd=1, lty=5)
lines(m$Year, m$CCCMA_Mean_Ann_Temp, type="l", col="dodgerblue", lwd=1.5, lty=3)
lines(m$Year, m$CRU_Mean_5month_Temp, type="o", col="black", cex=0.4, lwd=1.5, pch=15)
lines(m$Year, m$ECHAM_Mean_5month_Temp, type="o", col="firebrick", cex=0.5, lwd=1, lty=5, pch=16)
lines(m$Year, m$CCCMA_Mean_5month_Temp, type="o", col="royalblue", cex=0.6, lwd=1.5, lty=3, pch=18)
lines(m$Year, m$CRU_Mean_Ann_Temp, type="l", col="gray40", lwd=1.5) # Plotting the CRU data again so it is on top of the grid lines.
legend("topleft", inset=c(0.02,0.05), col=c("gray40", "red", "dodgerblue", "black", "firebrick", "royalblue"), lwd=1.5, cex=0.8, lty=rep(c(1,5,3)), 
legend=c("CRU Annual", "ECHAM5 Annual", "CCCMA Annual", "CRU March-July", "ECHAM5 March-July", "CCCMA March-July"), pch=c(NA,NA,NA,15,16,18), ncol=2, seg.len=3) 
box()
dev.off()

# Annual Temp
png("/workspace/Shared/Users/afloyd8/Amy/CAVMonly_Annual_Temp.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Mean_Ann_Temp, type="l", col="gray40", lwd=1.5, main= "CAVM Region in Alaska Annual Mean Temperature: 1901-2100", axes=FALSE, xlab="Year", ylab="Mean Temperature (C)", ylim=c(-10,8))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(-10,8,2), labels=c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(-10,12,2), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Mean_Ann_Temp, type="l", col="red", cex=0.3, lwd=1, lty=5)
lines(m$Year, m$CCCMA_Mean_Ann_Temp, type="l", col="dodgerblue", lwd=1.5, lty=3)
lines(m$Year, m$CRU_Mean_Ann_Temp, type="l", col="gray40", lwd=1.5)
legend("topleft", inset=c(0.02,0.05), col=c("gray40", "red", "dodgerblue"), lwd=1.5, cex=0.8, lty=c(1,5,3), legend=c("CRU", "ECHAM5", "CCCMA"), seg.len=3) 
box()
dev.off()

# 5 month Temp
png("/workspace/Shared/Users/afloyd8/Amy/CAVMonly_March-July_Temp.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Mean_5month_Temp, type="o", col="black", lwd=1.5, cex=0.4, pch=15, main= "CAVM Region in Alaska March-July Mean Temperature: 1901-2100", axes=FALSE, xlab="Year", ylab="Mean Temperature (C)", ylim=c(-10,8))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(-10,8,2), labels=c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(-10,12,2), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Mean_5month_Temp, type="o", col="firebrick", cex=0.5, lwd=1, lty=5, pch=16)
lines(m$Year, m$CCCMA_Mean_5month_Temp, type="o", col="royalblue", cex=0.6, lwd=1.5, lty=3, pch=18)
lines(m$Year, m$CRU_Mean_5month_Temp, type="o", col="black", cex=0.4, lwd=1.5, pch=15)
legend("topleft", inset=c(0.02,0.05), col=c("black", "firebrick", "royalblue"), lwd=1.5, cex=0.8, lty=c(1,5,3), legend=c("CRU", "ECHAM5", "CCCMA"), pch=c(15,16,18), seg.len=3)
box()
dev.off()

####### Precipitation Plots
# Combined
png("/workspace/Shared/Users/afloyd8/Amy/CAVMonly_Total_Pr.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Total_Ann_Pr, type="l", col="gray40", lwd=1.5, main= "CAVM Region in Alaska Mean Total Precipitation: 1901-2100", axes=FALSE, xlab="Year", ylab="Total Precipitation (mm)", ylim=c(10,70))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(10,70,10), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(10,70,10), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Total_Ann_Pr, type="l", col="red", cex=0.3, lwd=1, lty=5)
lines(m$Year, m$CCCMA_Total_Ann_Pr, type="l", col="dodgerblue", lwd=1.5, lty=3)
lines(m$Year, m$CRU_Total_5month_Pr, type="o", col="black", cex=0.4, lwd=1.5, pch=15)
lines(m$Year, m$ECHAM_Total_5month_Pr, type="o", col="firebrick", cex=0.5, lwd=1, lty=5, pch=16)
lines(m$Year, m$CCCMA_Total_5month_Pr, type="o", col="royalblue", cex=0.6, lwd=1.5, lty=3, pch=18)
lines(m$Year, m$CRU_Total_Ann_Pr, type="l", col="gray40", lwd=1.5)
legend("topleft", inset=c(0.02,0.05), col=c("gray40", "red", "dodgerblue", "black", "firebrick", "royalblue"), lwd=1.5, cex=0.8, lty=rep(c(1,5,3)), 
legend=c("CRU Annual", "ECHAM5 Annual", "CCCMA Annual", "CRU March-July", "ECHAM5 March-July", "CCCMA March-July"), pch=c(NA,NA,NA,15,16,18), ncol=2, seg.len=3) 
box()
dev.off()

# Annual Total Pr
png("/workspace/Shared/Users/afloyd8/Amy/CAVMonly_Total_Annual_Pr.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Total_Ann_Pr, type="l", col="gray40", lwd=1.5, main= "CAVM Region in Alaska Mean Annual Total Precipitation: 1901-2100", axes=FALSE, xlab="Year", ylab="Total Precipitation (mm)", ylim=c(10,70))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(10,70,10), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(10,70,10), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$ECHAM_Total_Ann_Pr, type="l", col="red", cex=0.3, lwd=1, lty=5)
lines(m$Year, m$CCCMA_Total_Ann_Pr, type="l", col="dodgerblue", lwd=1.5, lty=3)
lines(m$Year, m$CRU_Total_Ann_Pr, type="l", col="gray40", lwd=1.5) # Plotting the CRU data again so it is on top of the grid lines.
legend("topleft", inset=c(0.02,0.05), col=c("gray40", "red", "dodgerblue"), lwd=1.5, cex=0.8, lty=c(1,5,3), legend=c("CRU", "ECHAM5", "CCCMA"), seg.len=3) 
box()
dev.off()

# 5 Month total Pr
png("/workspace/Shared/Users/afloyd8/Amy/CAVMonly_Total_5month_Pr.png", width=12, height=6, units="in", res=300)
plot(m$Year, m$CRU_Total_5month_Pr, type="l", col="black", lwd=1.5, cex=0.4, main= "CAVM Region in Alaska Mean March-July Total Precipitation: 1901-2100", axes=FALSE, xlab="Year", ylab="Total Precipitation (mm)", ylim=c(10,70))
axis(1, at=seq(1900,2100,20))
axis(2, at=seq(10,70,10), las=2) # las=2 reorients the y-axis labels so they are perpendicular to the axis. 
abline(h=seq(10,70,10), v=seq(1900,2100,20), lty=3, col="lightgrey")
lines(m$Year, m$CRU_Total_5month_Pr, type="o", col="black", cex=0.4, lwd=1.5, pch=15)
lines(m$Year, m$ECHAM_Total_5month_Pr, type="o", col="firebrick", cex=0.5, lwd=1, lty=5, pch=16)
lines(m$Year, m$CCCMA_Total_5month_Pr, type="o", col="royalblue", cex=0.6, lwd=1.5, lty=3, pch=18)
legend("topleft", inset=c(0.02,0.05), col=c("black", "firebrick", "royalblue"), lwd=1.5, cex=0.8, lty=c(1,5,3), legend=c("CRU", "ECHAM5", "CCCMA"), pch=c(15,16,18), seg.len=3)
box()
dev.off()

