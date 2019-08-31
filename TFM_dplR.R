# ====================================================================
#
#      SCRIPT TFM - Eva Ventura Orteu
#      This script describes basic features of dplR when working with 
#      a new shrub-ring data set.
#      .   Reading, describing and plotting Ring-Width Data
#      .   Detendring
#      .   Descriptive statistics
#      .   Build and plot a mean-value chronology
#      .   Build and plot a chronology from the detrended ring widths
#
# ====================================================================


### set working directory
setwd("C:/Users/ona_v/OneDrive/Documentos/TFM_R_dades")

### check working directory
getwd()

### load necessary packages
library("dplR")


####-----tree-ring data-----####

### read rwl-files
shrub <- read.rwl("Shrub.rwl", format = "tucson")

### reading data
nrow(shrub)              #58 years
ncol(shrub)              #60 individuals
colnames(shrub)          #individual IDs
head(time(shrub),n = 10) #the first 10 years

### Describing Ring-Width Data
rwl.report(shrub)                #absent rings (zeros) and so on
rwi.stats(shrub, prewhiten=TRUE) #rbar and EPS
shrub.stats <- summary(shrub)    # descriptive statistics
foo <- interseries.cor(shrub,n=NULL,prewhiten=TRUE,biweight=TRUE,
                method = c("pearson"))
r   <- mean(foo[,1])
MS  <- sens2(shrub[1:50])

## Plotting Ring-Width Data
spag.plot(shrub) # spaghetti plot
seg.plot(shrub)  # segment plot: length of individual tree-ring series

# Chronology plot with raw data (mean in red)
plot(shrub[,1]~rownames(shrub), type= "l", ylim = c(0,0.65), xlim = c(1961, 2018),
     xlab = "years", ylab = "ring width (mm)", col = "gray70", las = 1)
for(i in 2:20) {
  lines(shrub[,i]~rownames(shrub), col = "gray70")
}
lines(rowMeans(shrub, na.rm = TRUE)~rownames(shrub), col = "red", lwd = 2)

rm(i)

## for calculation over a common overlap period
# determined by series:
SRWcommon <- common.interval(shrub, type = "series")
# or by years: 
SRWcommon <- common.interval(shrub, type = "years")


### Detrending:
SRWdetr <- detrend(shrub, method = "Spline", nyrs = 30, make.plot = TRUE) # non-interactive - using one method for all series

### Describing RWI
rwl.report(SRWdetr)
rwi.stats (SRWdetr, prewhiten=TRUE)
gini.coef (SRWdetr)
foo <- interseries.cor(SRWdetr,n=NULL,prewhiten=TRUE,biweight=TRUE,
                       method = c("pearson"))
r   <- mean(foo[,1])
MS  <- sens1(SRWdetr[, 1])

## Build a master chronology (over the longest period)
SRWsite <- chron(SRWdetr, prefix = "AVG", biweight = TRUE, prewhiten = FALSE)

## plot the master chronology with sample depth
crn.plot(SRWsite)
crn.plot(SRWsite, add.spline = FALSE, nyrs = 15,
         crn.line.col = "red", crn.lwd = 2,
         samp.depth.col = "grey",
         xlab = "Years", ylab = "RWI", las = 1)

# Gr?fic d'autocorrelaci?
boxplot(shrub.stats$ar1, ylab = expression(phi[1]),col = "lightblue")
stripchart(shrub.stats$ar1, vertical = TRUE,  
           method = "jitter", jitter = 0.02,add = TRUE, pch = 20, col = 'darkblue',cex=1.25)
ar1Quant <- quantile(shrub.stats$ar1,probs = c(0.25,0.5,0.75))
abline(h=ar1Quant,lty="dashed",col="grey")
mtext(text = names(ar1Quant),side = 4,at = ar1Quant,las=2)

