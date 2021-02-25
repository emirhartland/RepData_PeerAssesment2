Reproducible Research Course Project 2
=============

**February 23, 2021**

**Emirrio Renaldie Hartland**

U.S. National Oceanic and Atmospheric Administration’s (NOAA) Storm Database - Health and Economic Impacts
---------------

### Synopsis
This is the second peer assessment project of the Reproducible Research course at Coursera.

#### Introduction
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

#### Questions

1. Across the United States, which types of events (as indicated in the \color{red}{\verb|EVTYPE|}EVTYPE variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

#### Summary
Based on the data analysis, tornado is considered as the most harmful weather event in the United States, with respect to population health (by measuring the fatalities and injuries variables). On the other hand, flood has the greatest economic consequences across the United States, as measured by the property damages and the crop damages.

### Data Processing

#### Data
The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site:

* [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) [47Mb]

There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.

* National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
* National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

#### Loading the data
```{r, echo = TRUE}
library(data.table)
library(ggplot2)

data <- read.csv("repdata_data_StormData.csv", header = TRUE, sep=",")
```

#### Examining the data
```{r, echo = TRUE}
dim(data)
str(data)
```

#### Subsetting the data
From a list of variables in **data**, these are the columns of interest:

Population health variables:

* FATALITIES: approx. number of deaths
* INJURIES: approx. number of injuries

Economic variables:

* PROPDMG: approx. property damags
* PROPDMGEXP: the units for property damage value
* CROPDMG: approx. crop damages
* CROPDMGEXP: the units for crop damage value

Events - target variable:

* EVTYPE: weather event (Tornados, Wind, Snow, Flood, etc..)

Subset variables of interest from the original data set:
```{r, echo = TRUE}
vars <- c('EVTYPE', 'FATALITIES', 'INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')
data <- data[, vars]
summary(data)
```

Subset variables of interest where fatalities, injuries, or damages occured:
```{r, echo = TRUE}
data <- as.data.table(data)
data <- data[(EVTYPE != "?" & (INJURIES > 0 | FATALITIES > 0 | PROPDMG > 0 | CROPDMG > 0)), 
        c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
```

The units of property damage and crop damage need to be converted by the following rules:

* K or k: thousand dollars (10^3)
* M or m: million dollars (10^6)
* B or b: billion dollars (10^9)

Transform the PROPDMGEXP and CROPDMGEXP variables:
```{r, echo = TRUE}
cols <- c("PROPDMGEXP", "CROPDMGEXP")
data[,  (cols) := c(lapply(.SD, toupper)), .SDcols = cols]

trans.PROPDMG <-  c("\"\"" = 10^0, 
                 "-" = 10^0, "+" = 10^0, "0" = 10^0, "1" = 10^1, "2" = 10^2, "3" = 10^3,
                 "4" = 10^4, "5" = 10^5, "6" = 10^6, "7" = 10^7, "8" = 10^8, "9" = 10^9, 
                 "H" = 10^2, "K" = 10^3, "M" = 10^6, "B" = 10^9)
trans.CROPDMG <-  c("\"\"" = 10^0, "?" = 10^0, "0" = 10^0, "K" = 10^3, "M" = 10^6, "B" = 10^9)

data[, PROPDMGEXP := trans.PROPDMG[as.character(data[,PROPDMGEXP])]]
data[is.na(PROPDMGEXP), PROPDMGEXP := 10^0 ]

data[, CROPDMGEXP := trans.CROPDMG[as.character(data[,CROPDMGEXP])] ]
data[is.na(CROPDMGEXP), CROPDMGEXP := 10^0 ]
```

Combining the coefficient and exponent part of property damage and crop damage:
```{r, echo = TRUE}
data <- data[, .(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, PROPCOST = PROPDMG * PROPDMGEXP, CROPDMG, CROPDMGEXP, CROPCOST = CROPDMG * CROPDMGEXP)]
```

### Data Analysis

#### Estimating the total population health variables (fatalities and injuries)
```{r, echo = TRUE}
pop_health <- data[, .(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES), TOTAL_HEALTH = sum(FATALITIES) + sum(INJURIES)), by = .(EVTYPE)]

pop_health <- pop_health[order(-TOTAL_HEALTH), ]

pop_health <- pop_health[1:10, ]

head(pop_health, 10)
```

#### Estimating the total economic variables (propert and crop damage)
```{r, echo = TRUE}
economic <- data[, .(PROPCOST = sum(PROPCOST), CROPCOST = sum(CROPCOST), TOTAL_ECONOMIC = sum(PROPCOST) + sum(CROPCOST)), by = .(EVTYPE)]

economic <- economic[order(-TOTAL_ECONOMIC), ]

economic <- economic[1:10, ]

head(economic, 10)
```

### Results

#### Accross the United States, tornado is the most harmful event with respect to population health

```{r, echo = TRUE}
ggplot(pop_health, aes(x = reorder(EVTYPE, -TOTAL_HEALTH), y = TOTAL_HEALTH/1e3)) + 
  geom_bar(stat = "identity", aes(fill = TOTAL_HEALTH), position = "dodge") + 
  ylab("Total Injuries and Fatalities (K)") + 
  xlab("Event Type") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Top 10 US Weather Events Most Harmful to Population Health") +
  theme(plot.title = element_text(hjust = 0.5))
```

#### Accross the United States, flood has the greatest economic consequences

```{r, echo = TRUE}
ggplot(economic, aes(x = reorder(EVTYPE, -TOTAL_ECONOMIC), y = TOTAL_ECONOMIC/1e9)) + 
  geom_bar(stat = "identity", aes(fill = TOTAL_ECONOMIC), position = "dodge") + 
  ylab("Total Property and Crop Damages (B)") + 
  xlab("Event Type") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Top 10 US Weather Events Having the Greatest Economic Consequences") +
  theme(plot.title = element_text(hjust = 0.5))
```