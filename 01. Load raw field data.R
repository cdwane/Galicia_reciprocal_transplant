
## Load required libraries----

{library(readxl)
library(respirometry)
library(tidyr)
library(dplyr)
library(tidyverse)
library(scales)
library(lubridate)
}


## Load field heart rate data in long format----
longdata <- read_csv("field_hr_data.csv")

{
  longdata$day=as.factor(longdata$day)
  longdata$timepoint=as.factor(longdata$timepoint)
  longdata$Site=as.factor(longdata$Site)
  longdata$Height=as.factor(longdata$Height)
  longdata$Population=as.factor(longdata$Population)
  longdata$Group=as.factor(longdata$Group)
  longdata=subset(longdata,longdata$confidence!="Discard") # Remove low confidence readings
  
}
  
# Create wide format version of the dataframe to look at Q10 responses

widedata<- pivot_wider(data = longdata, 
                       id_cols = c("ID","day"),
                       names_from = "timepoint",
                       values_from = c("heartrate","temperature"))

# Add missing metadata to the wide format dataframe
metadata <- longdata[, c("Group", "Height", "Site", "Population", "Size", "ID")]
colnames(metadata)<-c("Group","Height","Site","Population","Size","ID")
metadata <- metadata[!duplicated(metadata$ID), ]


widedata=merge(metadata,widedata,by="ID")

# calculate temperature differential between the two temperature readings for each individual, each day
widedata$tempdiff=widedata$temperature_T2-widedata$temperature_T1

#Calculate Q10 ONLY if difference between temperatures is >4
{widedata$Q10=ifelse(widedata$tempdiff > 4, 
                     (widedata$heartrate_T2 / widedata$heartrate_T1)^(10 / (widedata$temperature_T2 - widedata$temperature_T1)), 
                     NA)
  widedata$Q10yes=ifelse(widedata$tempdiff > 4, 
                         "yes", 
                         NA)
}


## Load and format field temperature data----
fieldtemps <- read.csv("field_temperature_data.csv")
{fieldtemps$time = as.character(fieldtemps$time)
  fieldtemps$time = as.POSIXct(fieldtemps$time, tz = "", "%d/%m/%Y %H:%M")
  fieldtemps$height = as.factor(fieldtemps$height)
  fieldtemps$logger = as.factor(fieldtemps$logger)
}

### Create helper function to summaries temperature data across the different loggers at each shore height-----

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

tempsummary <- data_summary(data=fieldtemps, varname="temp", groupnames=c("time","height"))


tempsummary$time = as.POSIXct(tempsummary$time, tz = "", "%d/%m/%Y %H:%M")

tempsummary <- tempsummary %>%
  group_by(height) %>%
  slice(seq(1, n(), by = 6))



cols=c(
  "lower" = "#0B63FF",
  "upper" = "#FF0B0B"
)

### Plot field temperature data-----
fieldtempsplot=ggplot(tempsummary, aes(y = mean, x = time)) +
  geom_line(aes(y = mean, x = time,colour=height), size=1, data = tempsummary, alpha = 1) +
  scale_colour_manual(values = cols)+
  scale_linetype_manual(values = c("lower" = "solid","upper" = "twodash"))+
  theme_linedraw() +
  
  theme(legend.position = "none") +
  ylab("Temperature (C)") +
  xlab(NULL) +
  scale_x_datetime(
    breaks = date_breaks("24 hour"),
    limits = as.POSIXct(c('2022-7-12 00:00', '2022-7-20 19:00')),
    labels = date_format("%b %d
%H:%M")
  )+
  theme_bw()+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_line(linewidth = 1),
  )

fieldtempsplot




