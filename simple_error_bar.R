# Simple Error Bar v.1.0
# Simple error bar for common data visualization for life science

# Forewords
## This is a "cookbook" or "recipe" for visualizing the uncertainties in data,
## represented by the error bar. We will delve into into most used and common
## data visualization in life science and the type of error bar they used.
## --fabifuu, 2021

# Library used
library(dplyr)
library(ggplot2)
library(tidyr)

# Data source
## Simple experimental data with ToothGrowth data set
## Note that this data is already TIDY
##    len: length of tooth in mm
##    supp: type of supplement (VC: pure Vitamin C, OJ: orange juice)
##    dose: dose in mg/day
tg <- ToothGrowth
tg

## Check data type
### This is the "long" way to check each data in a column.
### Extract column, then check the class of data in each column
len <- tg[,1]
supp <- tg[,2]
dose <- tg[,3]
class(len) 
class(supp) 
class(dose)

### For shorter way, use str()
str(tg)

# Descriptive Summary using dplyr

## Grouping data by type of supplement
### However, this is not suitable for our analysis since we have different dosage
tg_supp <- group_by(tg, supp) %>%
   summarise(
      mean = mean(len),
      count = n(),
      median = median(len),
      min = min(len),
      '25%'= quantile(len, probs = 0.25),
      '50%' = quantile(len, probs = 0.5),
      '75%' = quantile(len, probs = 0.75),
      max = max(len),
      std = sd(len)
   )
tg_supp

## Grouping data by type of supplement AND dose
### This is what we want. Please note the use of DOUBLE pipe operator.
### (See "Tidy and Descriptive" for more information)
tg_desc <- 
   tg %>%
   group_by(dose, supp) %>%
   summarise(
      mean = mean(len),
      count = n(),
      median = median(len),
      min = min(len),
      '25%'= quantile(len, probs = 0.25),
      '50%' = quantile(len, probs = 0.5),
      '75%' = quantile(len, probs = 0.75),
      max = max(len),
      std = sd(len), #standard deviation
      sem = sd(len)/sqrt(n()), #standard error of mean
      ci = 1.96*sd(len)/sqrt(n()) #confidence interval 95%
   )
tg_desc

## Convert "dose" into factor variable
### Even though dose is number, we would like treat it like a condition group
### (and indeed, it is an experimental condition).
tg_desc$dose = as.factor(tg_desc$dose)
tg_desc

# Bar Plot with Double Error Bar
### Standard deviation: +/- std in y-axis in the aes of geom_errorbar
ggplot(
   data = tg_desc,
   aes(
      x = dose,
      y = mean,
      fill = supp)) +
   geom_bar(
      stat = "identity",
      color = "black",
      position = position_dodge()) +
   geom_errorbar(
      aes(
         ymin = mean - std,
         ymax = mean + std),
      width = 0.15,
      position=position_dodge(0.9)) +
   theme_linedraw() +
   labs(
      x = "Dose (mg/day)",
      y = "Length (mm)",
      fill = "Supplement")

### Standard error: +/- sem in y-axis in the aes of geom_errorbar
ggplot(
   data = tg_desc,
   aes(
      x = dose,
      y = mean,
      fill = supp)) +
   geom_bar(
      stat = "identity",
      color = "black",
      position = position_dodge()) +
   geom_errorbar(
      aes(
         ymin = mean - sem,
         ymax = mean + sem),
      width = 0.15,
      position=position_dodge(0.9)) +
   theme_linedraw() +
   labs(
      x = "Dose (mg/day)",
      y = "Length (mm)",
      fill = "Supplement")

### Confidence interval 95%: +/- sem in y-axis in the aes of geom_errorbar
ggplot(
   data = tg_desc,
   aes(
      x = dose,
      y = mean,
      fill = supp)) +
   geom_bar(
      stat = "identity",
      color = "black",
      position = position_dodge()) +
   geom_errorbar(
      aes(
         ymin = mean - ci,
         ymax = mean + ci),
      width = 0.15,
      position=position_dodge(0.9)) +
   theme_linedraw() +
   labs(
      x = "Dose (mg/day)",
      y = "Length (mm)",
      fill = "Supplement")

# Line graph with error bar
### This example use confidence interval 95%
ggplot(
   data = tg_desc,
   aes(
      x = dose,
      y = mean,
      colour = supp,
      group = supp)) +
   geom_line(
      position = position_dodge(0.1)) +
   geom_point(
      position = position_dodge(0.1)) +
   geom_errorbar(
      aes(
         ymax = mean + ci,
         ymin = mean - ci),
      width = 0.15,
      position = position_dodge(0.1)) +
   theme_linedraw() +
   labs(
      x = "Dose (mg/day)",
      y = "Length (mm)",
      fill = "Supplement")

# Bar Plot with Single Error Bar
### Just change ymin = mean, without minus ci.
ggplot(
   data = tg_desc,
   aes(
      x = dose,
      y = mean,
      fill = supp)) +
   geom_bar(
      stat = "identity",
      color = "black",
      position = position_dodge()) +
   geom_errorbar(
      aes(
         ymin = mean,
         ymax = mean + std),
      width = 0.15,
      position=position_dodge(0.9)) +
   theme_linedraw() +
   labs(
      x = "Dose (mg/day)",
      y = "Length (mm)",
      fill = "Supplement")

# Dot plot with Error Bar
### Suitable for x-axis that doesn't represent continuity
ggplot(
   data = tg_desc,
   aes(
      x = dose,
      y = mean,
      colour = supp)) +
   geom_point(
      position = position_dodge(0.1)) +
   geom_errorbar(
      aes(
         ymax = mean + ci,
         ymin = mean - ci),
      width = 0.15,
      position = position_dodge(0.1)) +
   theme_linedraw() +
   labs(
      x = "Dose (mg/day)",
      y = "Length (mm)",
      fill = "Supplement")