
# ==========================================================
#
#      SCRIPT TFM - Eva Ventura Orteu
#      This script describes growth rates with shrub-ring 
#      data set.
#      .   Plotting cumulative ring width
#      .   Plotting and analizing growth rate
#      .   Mixed model for mean ring width
#
# ==========================================================


### set working directory
setwd("C:/Users/ona_v/OneDrive/Documentos/TFM_R_dades")

### check working directory
getwd()

### load necessary packages
library("readxl")  # Read excel
library("ggplot2") # Plot
library("emmeans") # Multiple comparisons
library("nlme")    # Model
library("car")     # Normalitat i homocedasticitat


##---Cumulative ring width---##

# - Plotting by zone - #
zona_cumu <- read_excel("data_cum.xlsx", sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
zona_cumu$Zona <- factor(zona_cumu$Zona, levels = c("Pal", "Tra", "CV", "CM")) #arranging zones
start <- 1961
end   <- 2018
zona_cumu  <- zona_cumu[zona_cumu$Year >= start,]
zona_cumu  <- zona_cumu[zona_cumu$Year <= end,]
View(zona_cumu)

p <- ggplot (zona_cumu, aes(x = Year, y = Gruix, colour = Zona)) + geom_line() + geom_point() 
p + labs(x = "Year", y = "Cumulative ring width", color = "Zone")
# + geom_smooth(method="lm", size=1)

# Subsetting
selected <- c("Pal")
Pal <- zona_cumu [zona_cumu$Zona %in% selected,]

selected <- c("Tra")
Tra <- zona_cumu [zona_cumu$Zona %in% selected,]

selected <- c("CV")
CV <- zona_cumu [zona_cumu$Zona %in% selected,]

selected <- c("CM")
CM <- zona_cumu [zona_cumu$Zona %in% selected,]

#Slope Pal
start <- 1961
end   <- 1986
Pal  <- Pal[Pal$Year >= start,]
Pal  <- Pal[Pal$Year <= end,]
plotPal <- ggplot (Pal, aes(x = Year, y = Gruix, colour = Zona)) + geom_line() + geom_point() 
plotPal + labs(x = "Year", y = "Cumulative ring width", color = "Zone")
abline(mod <- lm(Pal$Gruix ~ Pal$Year))
coef(mod)
coef(mod)[2]

#Slope Tra
start <- 1961
end   <- 1986
Tra  <- Tra[Tra$Year >= start,]
Tra  <- Tra[Tra$Year <= end,]
plotTra <- ggplot (Tra, aes(x = Year, y = Gruix, colour = Zona)) + geom_line() + geom_point() 
plotTra + labs(x = "Year", y = "Cumulative ring width", color = "Zone")
abline(mod <- lm(Tra$Gruix ~ Tra$Year))
coef(mod)
coef(mod)[2]

#Slope CV
start <- 1961
end   <- 1986
CV  <- CV[CV$Year >= start,]
CV  <- CV[CV$Year <= end,]
plotCV <- ggplot (CV, aes(x = Year, y = Gruix, colour = Zona)) + geom_line() + geom_point() 
plotCV + labs(x = "Year", y = "Cumulative ring width", color = "Zone")
abline(mod <- lm(CV$Gruix ~ CV$Year))
coef(mod)
coef(mod)[2]

#Slope CM
start <- 1961
end   <- 1986
CM  <- CM[CM$Year >= start,]
CM  <- CM[CM$Year <= end,]
plotCM <- ggplot (CM, aes(x = Year, y = Gruix, colour = Zona)) + geom_line() + geom_point() 
plotCM + labs(x = "Year", y = "Cumulative ring width", color = "Zone")
abline(mod <- lm(CM$Gruix ~ CM$Year))
coef(mod)
coef(mod)[2]


# - Plotting by zone/transect - #
ZT_cumu <- read_excel("data_cum.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
start <- 1961
end   <- 2018
ZT_cumu  <- ZT_cumu[ZT_cumu$Year > start,]
ZT_cumu  <- ZT_cumu[ZT_cumu$Year <= end,]
View(ZT_cumu)
ZT_cumu$Zona <- factor(ZT_cumu$Zona, levels = c("Pal", "Tra", "CV", "CM")) #arranging zones

# Plot divided by transect
ggplot (ZT_cumu, aes(x = Year, y = Gruix, colour = Zona)) + geom_line() + geom_point() + facet_wrap( ~ Transecte) + labs(x = "Year", y = "Cumulative ring width", color = "Zone")
# Plot divided by zones 
ggplot (ZT_cumu, aes(x = Year, y = Gruix, colour = Transecte)) + geom_line() + geom_point() + facet_wrap( ~ Zona) + labs(x = "Year", y = "Cumulative ring width", color = "Zone") + geom_smooth(method="lm", size=1)



##---Growth rate---##

# - Plotting by zone - #
zona_mean <- read_excel("data_mean.xlsx", sheet = 8, col_names = TRUE, col_types = NULL, na = "", skip = 0)
zona_mean$Zona <- factor(zona_mean$Zona, levels = c("Pal", "Tra", "CV", "CM")) #arranging zones
start <- 1986
end   <- 2018
zona_mean  <- zona_mean[zona_mean$Year >= start,]
zona_mean  <- zona_mean[zona_mean$Year <= end,]
View(zona_mean)

# lm
p <- ggplot (zona_mean, aes(x = Year, y = Gruix, colour = Zona)) + geom_line() + geom_point() + geom_smooth(method="lm", size=1)
p + labs(x = "Year", y = "Ring width", color = "Zone")

#Spline
SplineSmooth <- function(formula, data, weights, span = 0.5, ...) {
  pred <- smooth.spline(data$x, data$y, df = length(data$y)*span,...)$y
  model <- list(x = data$x, pred = pred)
  class(model) <- "my_smooth"
  model
}
predictdf.my_smooth <- function(model, xseq, se, level) {
  data.frame(x = model$x, y = model$pred)
}

ggplot(zona_mean, aes(x = Year, y = Gruix, colour = Zona)) +
  geom_point() + geom_line() +
  geom_smooth(method = "SplineSmooth", method.args = list(span = 0.1)) + labs(x = "Year", y = "Ring width", color = "Zone")



# - Plotting by zone/transect - #
ZT_mean <- read_excel("data_mean.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
ZT_mean$Zona <- factor(ZT_mean$Zona, levels = c("Pal", "Tra", "CV", "CM")) #arranging zones

plot <- ggplot (ZT_mean, aes(x = Transecte, y = Gruix, outlier.shape=16, outlier.size=2, notch=FALSE, colour = Zona)) + 
  geom_boxplot(
    
    # custom outliers
    outlier.colour="grey",
    outlier.fill="grey",
    outlier.size=2
    
  )
plot + labs(x = "Transect", y = "Ring width (mm)", colour = "Zone")


##--- Statistics ---##

#- ANOVA -#

# Subsetting
attach(ZT_mean)

selected <- c("M1")
M1 <- ZT_mean [Transecte %in% selected,]

selected <- c("M2")
M2 <- ZT_mean [Transecte %in% selected,]

selected <- c("M3")
M3 <- ZT_mean [Transecte %in% selected,]

selected <- c("O1")
O1 <- ZT_mean [Transecte %in% selected,]

selected <- c("O2")
O2 <- ZT_mean [Transecte %in% selected,]

selected <- c("O3")
O3 <- ZT_mean [Transecte %in% selected,]


#ANOVA and multiple comparisons
modM1 <- lm(Gruix~Zona, data = M1) 
summary(modM1)
anova(modM1)
pairs(emmeans(modM1, ~Zona))

modM2 <- lm(Gruix~Zona, data = M2) 
summary(modM2)
anova(modM2)
pairs(emmeans(modM2, ~Zona))

modM3 <- lm(Gruix~Zona, data = M3) 
summary(modM3)
anova(modM3)
pairs(emmeans(modM3, ~Zona))

modO1 <- lm(Gruix~Zona, data = O1) 
summary(modO1)
anova(modO1)
pairs(emmeans(modO1, ~Zona))

modO2 <- lm(Gruix~Zona, data = O2) 
summary(modO2)
anova(modO2)
pairs(emmeans(modO2, ~Zona))

modO3 <- lm(Gruix~Zona, data = O3) 
summary(modO3)
anova(modO3)
pairs(emmeans(modO3, ~Zona))


#- MIXED MODEL -#
means <- aggregate(Gruix ~  Transecte*Zona, ZT_mean, mean)
View(means)

# Model
model <- lme (Gruix ~ Zona, random = ~ 1 | Transecte/Zona, method="REML", na.action = na.omit, data = means)
summary(model)
anova(model)
pairs(emmeans(model, ~Zona))

# Normality and homoscedasticity
res <- resid (model) 
qqPlot (resid (model))
fit <- fitted(model)
plot(res ~ fit)
abline(h=0)





