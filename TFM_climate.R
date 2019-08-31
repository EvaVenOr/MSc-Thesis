# ====================================================================
#
#      SCRIPT TFM - Eva Ventura Orteu
#      This script correlates shrub ring width with monthly climate 
#      data of zone chronologies and add significance with asterisks.
#      .  Correlation
#      .  Creation of new data set with "r" and "P" values
#      .  Plotting by zones with diferent factors and adding pvalues
#
# ====================================================================


### set working directory
setwd("C:/Users/ona_v/OneDrive/Documentos/TFM_R_dades")

### check working directory
getwd()

### load necessary packages
library("tidyverse")  
library("Hmisc")
library("ggpubr")
library("readxl")
library("dplR")

####-----climate-growth relationships----####

### RAW DATA 
data <- read_excel("Temp_Prec_1913-2018.xls", sheet = 7, col_names = TRUE, col_types = NULL, na = "", skip = 0)

# Subsetting
start <- 1968
end   <- 2018

data <- data[data$Anys >= start,]
data <- data[data$Anys <= end,]

CM  <- data [data$Zona %in% c("CM"),]
CV  <- data [data$Zona %in% c("CV"),]
Pal <- data [data$Zona %in% c("Pal"),]
Tra <- data [data$Zona %in% c("Tra"),]

clima_CM  <- as.matrix(CM[5:28])
clima_CV  <- as.matrix(CV[5:28])
clima_Pal <- as.matrix(Pal[5:28])
clima_Tra <- as.matrix(Tra[5:28])

# Correlation
corr_CM  <- rcorr(CM$Gruix, clima_CM, type = c("pearson"))
corr_CV  <- rcorr(CV$Gruix, clima_CV, type = c("pearson"))
corr_Pal <- rcorr(Pal$Gruix, clima_Pal, type = c("pearson"))
corr_Tra <- rcorr(Tra$Gruix, clima_Tra, type = c("pearson"))


## Creating data set
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
months <- factor(months, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
df_corr_CM <- NULL
df_corr_CM <- data.frame(c(months, months))
df_corr_CM["r"] <- data.frame(corr_CM$r[1,2:25])

# CM
df_corr_CM_temp <- data.frame(months)
df_corr_CM_temp["z"] <- data.frame("CM")
df_corr_CM_temp["t"] <- data.frame("Temp")
df_corr_CM_temp["r"] <- data.frame(corr_CM$r[1,2:13])
df_corr_CM_temp["P"] <- data.frame(corr_CM$P[1,2:13])

df_corr_CM_prec <- data.frame(months)
df_corr_CM_prec["z"] <- data.frame("CM")
df_corr_CM_prec["t"] <- data.frame("Prec")
df_corr_CM_prec["r"] <- data.frame(corr_CM$r[1,14:25])
df_corr_CM_prec["P"] <- data.frame(corr_CM$P[1,14:25])

# CV
df_corr_CV_temp <- data.frame(months)
df_corr_CV_temp["z"] <- data.frame("CV")
df_corr_CV_temp["t"] <- data.frame("Temp")
df_corr_CV_temp["r"] <- data.frame(corr_CV$r[1,2:13])
df_corr_CV_temp["P"] <- data.frame(corr_CV$P[1,2:13])

df_corr_CV_prec <- data.frame(months)
df_corr_CV_prec["z"] <- data.frame("CV")
df_corr_CV_prec["t"] <- data.frame("Prec")
df_corr_CV_prec["r"] <- data.frame(corr_CV$r[1,14:25])
df_corr_CV_prec["P"] <- data.frame(corr_CV$P[1,14:25])

# Pal
df_corr_Pal_temp <- data.frame(months)
df_corr_Pal_temp["z"] <- data.frame("Pal")
df_corr_Pal_temp["t"] <- data.frame("Temp")
df_corr_Pal_temp["r"] <- data.frame(corr_Pal$r[1,2:13])
df_corr_Pal_temp["P"] <- data.frame(corr_Pal$P[1,2:13])

df_corr_Pal_prec <- data.frame(months)
df_corr_Pal_prec["z"] <- data.frame("Pal")
df_corr_Pal_prec["t"] <- data.frame("Prec")
df_corr_Pal_prec["r"] <- data.frame(corr_Pal$r[1,14:25])
df_corr_Pal_prec["P"] <- data.frame(corr_Pal$P[1,14:25])

# Tra
df_corr_Tra_temp <- data.frame(months)
df_corr_Tra_temp["z"] <- data.frame("Tra")
df_corr_Tra_temp["t"] <- data.frame("Temp")
df_corr_Tra_temp["r"] <- data.frame(corr_Tra$r[1,2:13])
df_corr_Tra_temp["P"] <- data.frame(corr_Tra$P[1,2:13])

df_corr_Tra_prec <- data.frame(months)
df_corr_Tra_prec["z"] <- data.frame("Tra")
df_corr_Tra_prec["t"] <- data.frame("Prec")
df_corr_Tra_prec["r"] <- data.frame(corr_Tra$r[1,14:25])
df_corr_Tra_prec["P"] <- data.frame(corr_Tra$P[1,14:25])

## Plotting
df_corr <- NULL
df_corr <- df_corr_CM_temp
df_corr <- rbind(df_corr, df_corr_CM_prec)
df_corr <- rbind(df_corr, df_corr_CV_temp)
df_corr <- rbind(df_corr, df_corr_CV_prec)
df_corr <- rbind(df_corr, df_corr_Pal_temp)
df_corr <- rbind(df_corr, df_corr_Pal_prec)
df_corr <- rbind(df_corr, df_corr_Tra_temp)
df_corr <- rbind(df_corr, df_corr_Tra_prec)

f<-function(x){
  ifelse(x <= 0.05, "*", ifelse(x <= 0.01, "**", ifelse(x <= 0.001, "***", "")))
}

df_corr$Pvalue=f(df_corr$P)

plot_raw <- ggplot (data=df_corr, aes(x=months, y=r, fill=t)) + geom_bar(stat="identity", position=position_dodge()) + facet_wrap( ~ z)
plot_raw + labs(x = "Months", y = "Correlation coefficient", fill = "Climate data") + geom_text(aes(label=Pvalue), size=10, position=position_dodge(width=0.9), vjust=+0.5) 


### RWI DATA 
data <- read_excel("Temp_Prec_1913-2018.xls", sheet = 7, col_names = TRUE, col_types = NULL, na = "", skip = 0)
#shrub_mean <- read.rwl("Betula_mean.rwl", format = "tucson")
#SRWdetr_m <- detrend(shrub_mean, method = "Spline", nyrs = 30, make.plot = TRUE) # non-interactive - using one method for all series
data_RWI <- read_excel("data_RWI.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

CM  <- data [data$Zona %in% c("CM"),]
CV  <- data [data$Zona %in% c("CV"),]
Pal <- data [data$Zona %in% c("Pal"),]
Tra <- data [data$Zona %in% c("Tra"),]

clima_CM  <- as.matrix(CM[5:28])
clima_CV  <- as.matrix(CV[5:28])
clima_Pal <- as.matrix(Pal[5:28])
clima_Tra <- as.matrix(Tra[5:28])

## Subset
CM_d  <- data_RWI [data_RWI$Zona %in% c("CM"),]
CV_d  <- data_RWI [data_RWI$Zona %in% c("CV"),]
Pal_d <- data_RWI [data_RWI$Zona %in% c("Pal"),]
Tra_d <- data_RWI [data_RWI$Zona %in% c("Tra"),]

## Correlacionem
corr_CM_d  <- rcorr(CM_d$RWI, clima_CM, type = c("pearson"))
corr_CV_d  <- rcorr(CV_d$RWI, clima_CV, type = c("pearson"))
corr_Pal_d <- rcorr(Pal_d$RWI, clima_Pal, type = c("pearson"))
corr_Tra_d <- rcorr(Tra_d$RWI, clima_Tra, type = c("pearson"))

## Plot
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
months <- factor(months, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
df_corr_CM_d <- NULL
df_corr_CM_d <- data.frame(c(months, months))
df_corr_CM_d["r"] <- data.frame(corr_CM_d$r[1,2:25])

# CM
df_corr_CM_temp_d <- data.frame(months)
df_corr_CM_temp_d["z"] <- data.frame("CM")
df_corr_CM_temp_d["t"] <- data.frame("Temp")
df_corr_CM_temp_d["r"] <- data.frame(corr_CM_d$r[1,2:13])
df_corr_CM_temp_d["P"] <- data.frame(corr_CM_d$P[1,2:13])

df_corr_CM_prec_d <- data.frame(months)
df_corr_CM_prec_d["z"] <- data.frame("CM")
df_corr_CM_prec_d["t"] <- data.frame("Prec")
df_corr_CM_prec_d["r"] <- data.frame(corr_CM_d$r[1,14:25])
df_corr_CM_prec_d["P"] <- data.frame(corr_CM_d$r[1,14:25])

# CV
df_corr_CV_temp_d <- data.frame(months)
df_corr_CV_temp_d["z"] <- data.frame("CV")
df_corr_CV_temp_d["t"] <- data.frame("Temp")
df_corr_CV_temp_d["r"] <- data.frame(corr_CV_d$r[1,2:13])
df_corr_CV_temp_d["P"] <- data.frame(corr_CV_d$P[1,2:13])


df_corr_CV_prec_d <- data.frame(months)
df_corr_CV_prec_d["z"] <- data.frame("CV")
df_corr_CV_prec_d["t"] <- data.frame("Prec")
df_corr_CV_prec_d["r"] <- data.frame(corr_CV_d$r[1,14:25])
df_corr_CV_prec_d["P"] <- data.frame(corr_CV_d$r[1,14:25])

# Pal
df_corr_Pal_temp_d <- data.frame(months)
df_corr_Pal_temp_d["z"] <- data.frame("Pal")
df_corr_Pal_temp_d["t"] <- data.frame("Temp")
df_corr_Pal_temp_d["r"] <- data.frame(corr_Pal_d$r[1,2:13])
df_corr_Pal_temp_d["P"] <- data.frame(corr_Pal_d$r[1,2:13])

df_corr_Pal_prec_d <- data.frame(months)
df_corr_Pal_prec_d["z"] <- data.frame("Pal")
df_corr_Pal_prec_d["t"] <- data.frame("Prec")
df_corr_Pal_prec_d["r"] <- data.frame(corr_Pal_d$r[1,14:25])
df_corr_Pal_prec_d["P"] <- data.frame(corr_Pal_d$r[1,14:25])

# Tra
df_corr_Tra_temp_d <- data.frame(months)
df_corr_Tra_temp_d["z"] <- data.frame("Tra")
df_corr_Tra_temp_d["t"] <- data.frame("Temp")
df_corr_Tra_temp_d["r"] <- data.frame(corr_Tra_d$r[1,2:13])
df_corr_Tra_temp_d["P"] <- data.frame(corr_Tra_d$r[1,2:13])

df_corr_Tra_prec_d <- data.frame(months)
df_corr_Tra_prec_d["z"] <- data.frame("Tra")
df_corr_Tra_prec_d["t"] <- data.frame("Prec")
df_corr_Tra_prec_d["r"] <- data.frame(corr_Tra_d$r[1,14:25])
df_corr_Tra_prec_d["P"] <- data.frame(corr_Tra_d$r[1,14:25])

## Plot
df_corr_d <- NULL
df_corr_d <- df_corr_CM_temp_d
df_corr_d <- rbind(df_corr_d, df_corr_CM_prec_d)
df_corr_d <- rbind(df_corr_d, df_corr_CV_temp_d)
df_corr_d <- rbind(df_corr_d, df_corr_CV_prec_d)
df_corr_d <- rbind(df_corr_d, df_corr_Pal_temp_d)
df_corr_d <- rbind(df_corr_d, df_corr_Pal_prec_d)
df_corr_d <- rbind(df_corr_d, df_corr_Tra_temp_d)
df_corr_d <- rbind(df_corr_d, df_corr_Tra_prec_d)

f<-function(x){
  ifelse(x <= 0.05, "*", ifelse(x <= 0.01, "**", ifelse(x <= 0.001, "***", "")))
}

df_corr_d$Pvalue <- f(df_corr_d$P)

plot_RWI <- ggplot (data=df_corr_d, aes(x=months, y=r, fill=t)) + geom_bar(stat="identity", position=position_dodge()) + facet_wrap( ~ z)
plot_RWI + labs(x = "Months", y = "Correlation coefficient", fill = "Climate data") + geom_text(aes(label=Pvalue), size=10, position=position_dodge(width=0.9), vjust=+0.5)

 
