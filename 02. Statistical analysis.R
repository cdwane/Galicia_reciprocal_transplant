## Load libraries----
{
library(respirometry)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)
library(mgcv)
  library(car)
  library(DHARMa)
  library(performance)
  library(glmmTMB)
  library(lme4)
  library(nlme)
  library(lmerTest)
  library(sjPlot)
  library(broom)
  library(emmeans)
  library(multcomp)
  library(multcompView)
  
}
# Set correct conrasts for type 3 ANOVA
options(contrasts = c("contr.sum", "contr.poly"))


# Load required defaults used for plotting

{
col.labs <- c("Mid-shore","Upper shore")
names(col.labs) <- c("lower","upper")

row.labs <- c("Acute (1 Day)", "Chronic (4 Day)")
names(row.labs) <- c("D1", "D4")

Population.labs <- c("Upper shore Crab","Mid-shore Crab", "Mid-shore Wave")
names(Population.labs) <- c("uRB","mRB", "mSU")

Population.colors <- c(
  "mRB" = "#CC4678FF",
  "mSU" = "#FCA636FF",
  "uRB" = "#6A00A8FF"
)
}


## Stats for heartrate using mixed model-----

### Diagnostics----

#How many measurements did we get at each site at each timepoint?

table(longdata$Population,longdata$day)

hist(longdata$heartrate,breaks=20) # follows gausisan distribtuion

hist(longdata$temperature,breaks=20) # follows approximately gaussian distribution

### Model simplification process for mixed model----

# Create full model including interaction with temperature

# Note: "day" refers to whether the recording was collected on day 1 (acute exposure) or day 4 (chronic exposure). 
# "Group" refers to which batch the animal was part of, as described in the manuscript
# "Population" refers to population (upper RB, mid RB, mid SU), not Ecotype (RB vs. SU, which are Crab and Wave in the manuscript)


model=lmer(heartrate ~  temperature * Population * Height * day * Size + (1 | ID) + (1 | Site) + (1 | Group), data=longdata)

model2=lmer(heartrate ~  temperature + Population * Height * day * Size + (1 | ID) + (1 | Site) + (1 | Group), data=longdata)

anova(model,model2) # Suggests interactions with temperature should be removed (as AIC of model 2 is 20 lower)

# Next, test if Size (in mm) should be included in the final model

longdata2=subset(longdata,longdata$Size>=0) #Remove individual with missing size data (to compare models with and without size)

model=lmer(heartrate ~  temperature + Population * Height * day * Size + (1 | ID) + (1 | Site) + (1 | Group), data=longdata2)

model2=lmer(heartrate ~ Size + temperature + Population * Height * day + (1 | ID) + (1 | Site) + (1 | Group), data=longdata2)

model3=lmer(heartrate ~ temperature + Population * Height * day + (1 | ID) + (1 | Site) + (1 | Group), data=longdata2)


anova(model,model2,model3) # Lower AIC value in model 3 seems to support removing size completely.
# Size is a dubious one to include as it a) is a trait that inherently varies across Populations, b) was controlled for in the experimental design (as size ranges were restricted), and c) size is a poor proxy of weight


# Now, check which random effects can be removed

model=lmer(heartrate~temperature+Population*Height*day+(1|ID)+(1|Site)+(1|Group), data=longdata)
model2=lmer(heartrate~temperature+Population*Height*day+(1|ID)+(1|Group), data=longdata)
model3=lmer(heartrate~temperature+Population*Height*day+(1|ID)+(1|Site), data=longdata)

anova(model,model2,model3) # supports retaining site and group as random effects in the final model


### Run Final selected model-----

model=lmer(heartrate~temperature+Population*Height*day+(1|ID)+(1|Site)+(1|Group), data=longdata)

# diagnostics on final model using Performance and DHARMa packages
check_model(model)
plot(simulateResiduals(model)) # Diagnostics all look very good

Anova(model,type=3)

# Generate posthoc comparison
model_means=emmeans(model, list(pairwise ~ day*Population|Height))

model_means_cld <- cld(object = model_means,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)

# Create table for the ANOVA test
tab_df(tidy(Anova(model, type=3)), title = "Type III ANOVA Table", show.rownames = FALSE, show.type=FALSE,digits=3)

summary(model)

# Create table for the post-hocs
tab_df(model_means_cld, title = "Pairwise contrasts", show.rownames = FALSE, show.type=FALSE,digits=3)


### Plots----


#Remove high SD individuals

longdata$Population <- factor(
  longdata$Population,
  levels = c("uRB","mRB", "mSU")
)


ggplot(longdata,
       aes(x=temperature,
           y=heartrate, shape=day,linetype=day))+
  geom_point(aes(colour=Population),size=2,alpha=1)+
  geom_smooth(method='lm', aes(colour=Population), se=FALSE, fullrange=TRUE)+
  coord_cartesian( xlim = c(15, 40), ylim = c(0, 140))+
  scale_colour_manual(values = Population.colors,labels = Population.labs)+
  #scale_color_viridis(discrete=TRUE, begin=0.2,end=0.8,option="plasma",direction=-1,breaks = c("uRB","mRB","mSU"),labels = Population.labs)+
  scale_fill_viridis(discrete=TRUE, begin=0.2,end=0.8,option="plasma",direction=-1,breaks = c("uRB","mRB","mSU"),labels = Population.labs)+
  ylab("Heartrate (BPM)")+  
  xlab("Temperature (°C)") +
  #theme_mine()+
  facet_grid (Height~Population, labeller = labeller(Height = col.labs,Population=Population.labs))+
  theme_bw()


## Stats for heartrates using Q10s-----

### Diagnostics ----

# Only using Q10 values in caes where the differences betwen T1 and T2 temperatures was >4 degrees

# Diagnostics - samples sizes for Q10 are quite small
table(widedata$Q10yes,widedata$Population,widedata$Height,widedata$day)


hist(widedata$tempdiff,breaks=20)
summary(subset(widedata,widedata$Q10yes=="yes")$tempdiff) # median difference in temp between the first and second 
summary(subset(widedata,widedata$Q10yes=="yes")$temperature_T1) # median first temperature used 
summary(subset(widedata,widedata$Q10yes=="yes")$temperature_T2) # median second temperature used 

### Basic model----



model=lm((Q10)~Population*Height*day, data=widedata)

check_model(model)
plot(simulateResiduals(model)) # Fine according to DHARMa
Anova(model,type=3)


### Model simplification----

{
model=glmmTMB(Q10 ~ Population*Height*day+ (1|Size) +(1|tempdiff) +(1|Site)+(1|ID), data=widedata, family=gaussian(link=identity))
model2=glmmTMB(Q10 ~ Population*Height*day+ (1|Size) +(1|Site)+(1|ID), data=widedata, family=gaussian(link=identity))
model3=glmmTMB(Q10 ~ Population*Height*day+(1|tempdiff) +(1|Site)+(1|ID), data=widedata, family=gaussian(link=identity))
model4=glmmTMB(Q10 ~ Population*Height*day+(1|Site)+(1|ID), data=widedata, family=gaussian(link=identity))
model5=glmmTMB(Q10 ~ Population*Height*day+(1|ID), data=widedata, family=gaussian(link=identity))
model6=glmmTMB(Q10 ~ Population*Height*day, data=widedata, family=gaussian(link=identity))
}

anova(model,model2,model3,model4,model5,model6) 

anova(model,model6) # adding any random effects does not improve model



### Final model----

model=lm((Q10)~Population*Height*day, data=widedata)

Anova(model, type=3)


# Overall, no evidence of differences in Q10 between Populations, sampling points or otherwise

### Plots----

widedata$Population <- factor(
  widedata$Population,
  levels = c( "uRB","mRB", "mSU")
)


ggplot(widedata,
       aes(x=Population,
           y=Q10))+
  geom_boxplot(aes(colour=Population),size=0.5,alpha=1)+
  #scale_y_continuous(limits = c(0.4,1,2), expand = c(0, 0)) +
  scale_color_viridis(discrete=TRUE, begin=0.2,end=0.8,option="plasma",direction=1,labels=Population.labs)+
  scale_fill_viridis(discrete=TRUE, begin=0.2,end=0.8,option="plasma",direction=1)+
  ylab("Q10")+  
  xlab("Temperature (°C)") +
  #theme_mine()+
  scale_x_discrete(label = NULL)+
  facet_grid (Height~day, labeller = labeller(Height = col.labs,day=row.labs))+
  theme_bw()+
  theme(legend.position = "bottom")

### Power analysis of Q10 data----

# Load required libraries

library(pwr)
library(effectsize)

# Set Q10 model

model=lm((Q10)~Population+Height+day+Population:Height+Population:day+Population:Height:day, data=widedata)

anova(model)

# Calculate eta squared

eta_squared(model, partial = TRUE)

# eta_squared for Population:Height:day = 0.03

# Calculate Cohen’s f 

eta <- 0.03

f <- sqrt(eta/(1-eta))

# a posteri analysis to determine statistical power

pwr.f2.test(
  u = 2,       # numerator df for Population:Height:day
  v = 80,      # residual df
  f2 = f^2,
  sig.level = 0.05
)

# statistical power is 0.27


