
## Load required libraries----

{library(dplyr)
  library(respirometry)
library(ggplot2)
library(ggpubr)
library(viridis)
library(tidyverse)
library(readxl)
  library(car)
  library(DHARMa)
  library(performance)
  library(glmmTMB)
  library(lme4)
  library(nlme)
  library(lmerTest)
  library(sjPlot)
  library(broom)
  library(mgcv)
  library(marginaleffects)
  library(emmeans)
  library(multcomp)
  library(multcompView)
  
  
  # Load required defaults
  
  {
    col.labs <- c("Mid-shore","Upper shore")
    names(col.labs) <- c("Mid","upper")
    
    row.labs <- c("Acute (1 Day)", "Chronic (4 Day)")
    names(row.labs) <- c("D1", "D4")
    
    population.labs <- c("Mid-shore Crab", "Mid-shore Wave", "Mid-shore Wave", "Upper shore Crab")
    names(population.labs) <- c("mRB", "mSU", "SU", "uRB")
}
  
   #Helper function to create error bars and means on plots
  data_summary=function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sd = sd(x[[col]], na.rm=TRUE),
        se = sd(x[[col]]) / sqrt(length(x[[col]])), na.rm=TRUE)
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    return(data_sum)
  }
  
  theme_mine <- function(base_size = 16) {
    # Starts with theme_grey and then modify some parts
    theme_bw(base_size = base_size) %+replace%
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.ticks =  element_line(colour = "black"), 
        axis.title.x= element_text(size=14),
        axis.title.y= element_text(size=14,angle=90),
        panel.background = element_blank(), 
        panel.border =element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(2.0, "lines"), 
        plot.background = element_blank(), 
        plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines"),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1)
      )
  } #function for custom theme
  
  
}


## Load data from ramping experiment-----
rampingdata <- read.csv("rampingdata.csv")

rampingdata$snail_id=as.factor(rampingdata$snail_id)
rampingdata$population=as.factor(rampingdata$population)

# Remove low-confidence individuals
rampingdata=subset(rampingdata,rampingdata$conf!="Discard")



# Create version of temperature column that uses "rounded" temperatures (used as part of the plotting later on, not the statistical test)

rampingdata$Tpoint=round(rampingdata$temp, digits = 0)  

rampingdata$Tpoint=as.factor(rampingdata$Tpoint)

rampingdata$Tpoint[rampingdata$Tpoint==35]<-34



# Create wide- format version of data and calculate Q10 values

widerampingdata<- pivot_wider(data = rampingdata, 
                       id_cols = c("batch","population","size","snail_id"),
                       names_from = "Tpoint",
                       values_from = c("heart_rate","temp"))


# Calculate Q10 values between 20 and 30 degrees
widerampingdata$Q1010=q10(,widerampingdata$heart_rate_20,widerampingdata$heart_rate_30,widerampingdata$temp_20,widerampingdata$temp_30)



## Generalized Additive Model-----

### Initial step: Determine whether populations have different thermal response curves-----

# Full model:  each population has its own nonlinear temperature response curve

full_model <- gam(
  heart_rate ~ population +
    size +
    s(temp, by = population) +
    s(snail_id, bs = "re"),
  data = rampingdata,
  method = "ML"
)

# Reduced model: Pulations differ in average HR but all share the same response curve
reduced_model <- gam(
  heart_rate ~ population +
    size +
    s(temp) +
    s(snail_id, bs = "re"),
  data = rampingdata,
  method = "ML"
)

# Compare models
anova(full_model,reduced_model, test = "Chisq")

AIC(full_model, reduced_model)

# Model with seperate curves for each population has reduced AIC


plot(full_model, select = 3, shade = TRUE)
abline(h = 0, lty = 'dashed')




### Test whether thermal curves are better explained by population, ecotype, or shore height ----


# First, add grouping variables for shore height (Mid or Upper), or for Ecotype (Crab (RB) vs Wave (SU))

#Add "Height" column
rampingdata <- rampingdata %>%
  mutate(height = case_when(
    grepl("SU|mRB", population) ~ "Mid",
    grepl("uRB", population) ~ "Upper",
    TRUE ~ NA_character_
  ))

#Add "ecotype" column
rampingdata <- rampingdata %>%
  mutate(ecotype = case_when(
    grepl("uRB|mRB", population) ~ "Crab",
    grepl("SU", population) ~ "Wave",
    TRUE ~ NA_character_
  ))

# Convert grouping variables to factors

rampingdata$height = as.factor(rampingdata$height)
rampingdata$ecotype = as.factor(rampingdata$ecotype)
rampingdata$population = as.factor(rampingdata$population)

### Compare three alternative GAM models----

# Model A:
# each population has its own thermal curve

gam_population<- gam(heart_rate ~ population + size + s(temp, by = population) + s(snail_id, bs = "re"), data = rampingdata, method = "ML")

# Model B:
# thermal response curves differ only by ecotype identity

gam_ecotype <- gam(heart_rate ~ population + size + s(temp, by = ecotype) + s(snail_id, bs = "re"), data = rampingdata, method = "ML")

# Model C:
# thermal response curves differ only by shore height

gam_height <- gam(heart_rate ~ population + size + s(temp, by = height) + s(snail_id, bs = "re"), data = rampingdata, method = "ML")

AIC(
  gam_population,
  gam_ecotype,
  gam_height
)

summary(gam_population)



emm_smooths <- emmeans(gam_population, pairwise ~ population)

emm_smooths

emm_smooths_cld <- cld(object = emm_smooths,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)


tab_df(emm_smooths_cld)

### Create plot for publication-------


population.colors <- c(
  "mRB" = "#CC4678FF",
  "SU" = "#FCA636FF",
  "uRB" = "#6A00A8FF"
)


rampsummary=data_summary(data=rampingdata, varname="heart_rate", groupnames=c("population","Tpoint"))
tempsummary=data_summary(data=rampingdata, varname="temp", groupnames=c("population","Tpoint"))
rampsummary$temp=tempsummary$mean
rampsummary$Tpoint=as.character(rampsummary$Tpoint)
rampsummary$Tpoint=as.numeric(rampsummary$Tpoint)

ggplot(data=rampingdata,aes(y=heart_rate,x=temp, group=population, shape=population))+
  #geom_point(data=rampingdata, aes(y=heart_rate,x=temp,   ,colour=population, alpha=0.01))+
  geom_linerange(data = rampsummary,linetype="solid", aes(temp     , mean, ymin = mean - se, ymax = mean + se,colour=population),size=1)+
  geom_smooth(data=rampingdata, aes(y=heart_rate, x=temp, fill=population), method="gam",se=TRUE, colour="black", linewidth=1, alpha=0.1)+
  #geom_line(colour="black")+
  #scale_y_continuous(limits = c(0,140), expand = c(0, 0)) +
  scale_colour_manual(values = population.colors,labels = population.labs)+
  scale_fill_manual(values = population.colors,labels = population.labs)+
  #scale_color_viridis(discrete=TRUE, begin=0.2,end=0.8,option="plasma",direction=-1)+
  #scale_fill_viridis(discrete=TRUE, begin=0.2,end=0.8,option="plasma",direction=-1)+
  ylab("Heartrate (BPM)")+  
  xlab("Temperature (°C)")+
  theme_mine()


## Stats for Q10 comparison of ramping data ------

#Basic model


model=lm((Q1010)~population, data=widerampingdata)


boxplot((Q1010)~population, data=widerampingdata)

# Model diagnostics
plot(model)
check_model(model)
plot(simulateResiduals(model)) # Fine according to DHARMa
Anova(model,type=3)

# See if adding covariate effect of size inproves the model

model=lmer((Q1010)~population+(1|size), data=widerampingdata)
model2=lm((Q1010)~population, data=widerampingdata)

anova(model,model2)

#final model
model=lm((Q1010)~population, data=widerampingdata)
Anova(model,type=3)


model_means=emmeans(model, list(pairwise ~ population))

model_means_cld <- cld(object = model_means,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)

# Sample sizes
table(widerampingdata$population)

# Set order of factor levels for plotting
widerampingdata$population <- factor(
  widerampingdata$population,
  levels = c( "uRB","mRB", "SU")
)

### Create Q10 plot for publication----

ggplot(widerampingdata,
       aes(x=population,
           y=Q1010))+
  geom_boxplot(aes(colour=population),size=0.5,alpha=1)+
  geom_text(data = model_means_cld, aes(label=.group,x=population, y=emmean+0.8),size=4 )+
  #scale_y_continuous(limits = c(0,110), expand = c(0, 0)) +
  scale_color_viridis(discrete=TRUE, begin=0.2,end=0.8,option="plasma",direction=1,labels=population.labs)+
  scale_fill_viridis(discrete=TRUE, begin=0.2,end=0.8,option="plasma",direction=1)+
  ylab("Q10")+  
  xlab(NULL) +
  #theme_mine()+
  scale_x_discrete(label = NULL)+
  theme_bw()+
  theme(legend.position = "bottom")


### Power analysis of Q10 data------ 

# Load libraries
library(effectsize)
library(pwr)


model=lm((Q1010)~population, data=widerampingdata)

anova(model)

# Calculate Eta squared
eta_squared(model)
# Eta squared is 0.10

# Calculate Cohen’s f 

f <- sqrt(eta_squared(model)$Eta2 / (1 - eta_squared(model)$Eta2))

# Determine statistical power

pwr.anova.test(
  k = 3,
  f = f,
  n = mean(c(18,18,16)),
  sig.level = 0.05
)

# Resultant estimate of statistical power is 0.5610367

