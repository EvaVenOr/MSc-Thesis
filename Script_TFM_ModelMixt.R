# ==========================================================
#
#      SCRIPT TFM - Eva Ventura Orteu
#      .   Mixed Models
#
# ==========================================================

### set working directory
setwd("C:/Users/ona_v/OneDrive/Documentos/TFM_R_dades")

### check working directory
getwd()

### load necessary packages
library("ggplot2") # Gràfics
library("readxl")  # Llegir excel
library("nlme")    # Model
library("car")     # Normalitat i homocedasticitat


ZT_mean <- read_excel("data_mean.xlsx", sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
View(ZT_mean)
ZT_mean$Edat.cambial <- factor(ZT_mean$Edat.cambial)
ZT_mean$Transecte    <- factor(ZT_mean$Transecte)
ZT_mean$Zona         <- factor(ZT_mean$Zona, levels = c("Pal", "Tra", "CV", "CM")) # Ordenem les zones per a que "Palsa" quedi la primera i quna fem l'ANOVA ens la compar amb aquesta
ZT_mean$Mostra       <- factor(ZT_mean$Mostra)

#- MIXED MODEL -#

# Plotting
boxplot (Gruix ~ Transecte)
boxplot (Gruix ~ Zona)
interaction.plot (Transecte, Zona, Gruix, col= 1:4)

# Model
#model <- lme (Gruix ~ Edat.cambial+Zona*Year, random = ~ 1 | Transecte/Mostra, method="REML", na.action = na.omit, data = ZT_mean)
#summary(model)
#anova(model)

model <- lme (Gruix ~ Edat.cambial+Zona, random = ~ 1 | Transecte/Mostra, method="REML", na.action = na.omit, data = ZT_mean)
summary(model)
anova(model)


# Mirem normalitat i homocedasticitat:
res <- resid (model) 
qqPlot (resid (model))
fit <- fitted(model)
plot(res ~ fit)
abline(h=0)
plot(res~Year)

#Surt heteroneitat. Mirem d'on prové
model <- lme (Gruix ~ Zona, random = ~ 1 | Transecte/Mostra, method="REML", na.action = na.omit, data = ZT_mean)
summary(model)
anova(model)
res <- resid (model) 
qqPlot (resid (model))
fit <- fitted(model)     #Heterogeneitat
plot(res ~ fit)
abline(h=0)
plot(res~Zona)

model <- lme (Gruix ~ Year, random = ~ 1 | Transecte/Mostra, method="REML", na.action = na.omit, data = ZT_mean)
summary(model)
anova(model)
res <- resid (model) 
qqPlot (resid (model))
fit <- fitted(model)     #Heterogeneitat
plot(res ~ fit)
abline(h=0)
plot(res~Year)


#VarIdent
Gruix.mod <- log(ZT_mean$Gruix)
lmc <-lmeControl(niterEM=10000, nsMaxIter=10000) 
ctrl <- lmeControl(opt='optim') 
model2 <- lme (Gruix ~ Edat.cambial+Zona*Year, random = ~ 1 | Transecte/Mostra, data = ZT_mean, method="REML", na.action = na.omit, control = lmc)

op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
plot(model2, which = c(1), col = 1, add.smooth = FALSE, caption = "")
plot(Transecte, resid(model2, type = "normalized"), xlab = "Factor", ylab = "Normalized Residuals")
par(op)


