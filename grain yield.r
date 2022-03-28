library(readxl)
library(tidyverse)
library(pbkrtest)
dry.grainyield <- read_excel("grain yield2.xlsx")


dry.grainyield$crop<-as.factor(dry.grainyield$crop)
dry.grainyield$nitrogen<-as.factor(dry.grainyield$nitrogen)
dry.grainyield$year<-as.factor(dry.grainyield$year)
dry.grainyield$location<-as.factor(dry.grainyield$location)
summary(dry.grainyield)
attach(dry.grainyield)
library(agricolae)
library(emmeans)
library(multcomp)
library(multcompView)
# Subseting with crop
einkorn <- subset(dry.grainyield, crop=='Einkorn')
library(lme4)

options(contrasts = c("contr.treatment", "contr.poly"))
library(lmerTest)
fit <- lmer(`yield (kg/ha)` ~ location*nitrogen + (1 | year), data = einkorn)
anova(fit)
ranova(fit)
#fit2 <- lmer(`yield (kg/ha)` ~ location+nitrogen + (1 | year), data = einkorn)

#einkorn.cld<-cld(emmeans(fit2, ~location), Letters=LETTERS, reverse=TRUE) 

#einkorn.cld
emmer <- subset(dry.grainyield, crop=='Emmer')
fit3 <- lmer(`yield (kg/ha)` ~ location*nitrogen + (1 | year), data = emmer)
anova(fit3)
ranova(fit3)
fit4 <- lmer(`yield (kg/ha)` ~ location+nitrogen + (1 | year), data = emmer)
emmer.cld<-cld(emmeans(fit4, ~location), Letters=LETTERS, reverse=TRUE) 

emmer.cld

spelt <- subset(dry.grainyield, crop=='Spelt')
fit5 <- lmer(`yield (kg/ha)` ~ location*nitrogen + (1 | year), data = spelt)
anova(fit5)
ranova(fit5)
fit6 <- lmer(`yield (kg/ha)` ~ location+nitrogen + (1 | year), data = spelt)
spelt.cld<-cld(emmeans(fit6, ~location), Letters=LETTERS, reverse=TRUE) 
spelt.cld

#crop
SAREC <- subset(dry.grainyield, location=='SAREC')
fit9 <- lm(`yield (kg/ha)` ~ crop, data = SAREC)
anova(fit9)
crop.cld<-cld(emmeans(fit9, ~crop), Letters=LETTERS, reverse=TRUE) 
crop.cld
#PREC
ShREC <- subset(dry.grainyield, location=='ShREC')
fit10 <- lmer(`yield (kg/ha)` ~ crop + (1 | year), data = ShREC)
anova(fit10)
ranova(fit10)
crop.cld1<-cld(emmeans(fit10, ~crop), Letters=LETTERS, reverse=TRUE) 
crop.cld1


