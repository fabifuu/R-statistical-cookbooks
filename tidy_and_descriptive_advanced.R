# Advanced Tidy and Descriptive v.1.0
# With example

# Forewords
## This is upgraded version of "Tidy and Descriptive" with really messy data
## that usually used in biology and biomedical science. This messy data is mainly
## data that we, human, compiled. Why? Human-compiled data are in nature, very 
## messy but easy to read. They have double--no--triple header or more!

# Library used
library(dplyr)
library(ggplot2)
library(tidyr)

# Set working directory
## Use your own directory
setwd("~/Bioinformatics/Biostatistics/")

# Import data: Plant Hormone Experiment
## We use a very messy data
data_raw <- 
   read.csv(file="./plant_hormone_messy.csv", header = FALSE)

## Explanation of raw data example
### This is a standard plant hormone and soil nutrition experiment. We use two
### different hormone (ABA and IAA). Each hormone have three different concentration.
### We use two different plant (Oryza sativa and Zea mays). Each plant have three
### different soil nutrition group (control, lack of N, and lack of NPK). Each soil
### nutrition group have 5 plant. The actual value measured is dry biomass in gram.

## Inspect data
### Determine whether the data is tidy or not, and find a way to tidying it.
data_raw

### Delete first and second row
### This is untidy header that will gathered later.
data_raw <- data_raw[-2,]
data_raw <- data_raw[-1,]
data_raw

### Change the first row as header
### Now the header is concentration value of each hormone. However, we lost
### original hormone name.
names(data_raw) <- data_raw[1,]
data_raw <- data_raw[-1,]
data_raw

### Change header name since they have white space, lost the original hormone name,
### and have identical name (two "0 mg/L" column).
### Create a list of header from left (1st column) to right (nth column).
header <- c(
   "Sample",
   "IAA_0",
   "IAA_5",
   "IAA_10",
   "IAA_15",
   "ABA_0",
   "ABA_5",
   "ABA_10",
   "ABA_15"
)
colnames(data_raw) <- header

### Extract header for IAA and ABA as a list
### We will use this list for grouping later
header_IAA <- header[c(2:5)]
header_IAA
header_ABA <- header[c(6:9)]
header_ABA
header_all <- header[c(2:9)]
header_all

# Tidying Data
## Group should be arranged with tidy-data structure 
## (see https://r4ds.had.co.nz/tidy-data.html)
## condition = treatment group, control group, etc.
## value = value or the data contain in the group (i.e. dry biomass) 

library(tidyr)
data_tidy <- data_raw %>%
   pivot_longer(
      header_all,
      names_to = "Concentration", #name of our condition (which is hormone conc.)
      values_to = "Dry_Biomass" #this is the value each condition have
   )
data_tidy

### Make sure that numeric value is numeric, and character value is character
data_tidy$Dry_Biomass <- as.numeric(data_tidy$Dry_Biomass)
data_tidy

### other example:
data_tidy$column <- as.character(data_tidy$column) #as character
data_tidy$column <- as.Date(data_tidy$column) #as date
data_tidy$column <- as.logical(data_tidy$column) #as binary logic (T/F)

### Separate IAA or ABA and concentration value in "Concentration" column
### as "Hormone" and "Concentration" column
data_tidy <- data_tidy %>%
   separate(
      Concentration, 
      into = c("Hormone","Concentration"),
      sep = "_"
   )
data_tidy

### Separate P1 in "Sample" column as "Plant_no"
data_tidy <- data_tidy %>%
   separate(
      Sample, 
      into = c("Sample", "Plant_no"),
      sep = " P"
   )
data_tidy

### Separate soil condition from speices name in "Sample" column
### Sample become two column: "Species" and "Soil_cond"
### We will capture the species name AND soil condition:
###   - species name is ALWAYS first and second words.
###   - soil condition is ALWAYS third words
###   - each words separated by white space
### Use extract() turns each group into a new column, given a regex to capture.
### Regex to capture string before & after second white space: ^([^ ]* [^ ]*)[ ](.*)$

data_tidy <-
   data_tidy %>%
   extract(
      Sample,
      into=c("Species", "Soil_cond"),
      regex="^([^ ]* [^ ]*)[ ](.*)$")
data_tidy

### Finally our data is tidy. :D
### Save as csv file
write.csv(data_tidy, file = "./plant_hormone_tidy.csv")

# Descriptive statistics using dplyr
library(dplyr)

## --- Note:
## ---- group_by() → Condition or sample type. You can use >1 group variable.
## ---- Argument inside summarize must be numeric only!

### TWO times grouping
### Grouping by Species AND Soil Condition
### We use double piping (See: tidy_and_descriptive.R)
data_tidy %>%
group_by(Species, Soil_cond) %>%
   summarise(
      mean = mean(Dry_Biomass),
      count = n(),
      median = median(Dry_Biomass),
      min = min(Dry_Biomass),
      '25%'= quantile(Dry_Biomass, probs = 0.25),
      '50%' = quantile(Dry_Biomass, probs = 0.5),
      '75%' = quantile(Dry_Biomass, probs = 0.75),
      max = max(Dry_Biomass),
      std = sd(Dry_Biomass), #standard deviation
      sem = sd(Dry_Biomass)/sqrt(n()), #standard error of mean
      ci = 1.96*sd(Dry_Biomass)/sqrt(n()) #confidence interval 95%
   )

### THREE times grouping
### Grouping by Species AND Soil Condition AND Hormone
data_tidy %>%
   group_by(Species, Soil_cond, Hormone) %>%
   summarise(
      mean = mean(Dry_Biomass),
      count = n(),
      median = median(Dry_Biomass),
      min = min(Dry_Biomass),
      '25%'= quantile(Dry_Biomass, probs = 0.25),
      '50%' = quantile(Dry_Biomass, probs = 0.5),
      '75%' = quantile(Dry_Biomass, probs = 0.75),
      max = max(Dry_Biomass),
      std = sd(Dry_Biomass),
      sem = sd(Dry_Biomass)/sqrt(n()),
      ci = 1.96*sd(Dry_Biomass)/sqrt(n())
   )

### FOUR times grouping
### Grouping by Species AND Soil Condition AND Hormone AND Concentration
### This is grouping mechanism that we want. 
plant_desc <- data_tidy %>%
   group_by(Species, Soil_cond, Hormone, Concentration) %>%
   summarise(
      mean = mean(Dry_Biomass),
      count = n(),
      median = median(Dry_Biomass),
      min = min(Dry_Biomass),
      '25%'= quantile(Dry_Biomass, probs = 0.25),
      '50%' = quantile(Dry_Biomass, probs = 0.5),
      '75%' = quantile(Dry_Biomass, probs = 0.75),
      max = max(Dry_Biomass),
      std = sd(Dry_Biomass),
      sem = sd(Dry_Biomass)/sqrt(n()),
      ci = 1.96*sd(Dry_Biomass)/sqrt(n())
   )
plant_desc

## save as csv file
write.csv(plant_desc, file = "./plant_hormone_descriptive.csv")

# Data Visualization

plant_desc$Concentration = as.integer(plant_desc$Concentration)
plant_desc

## Box Plot
### Try 2 factor of grouping: species and type of hormone.
### x is hormone and colour is species.
ggplot(
   data = data_tidy,
   aes(
      x = Hormone,
      y = Dry_Biomass,
      color = Species)) +
   geom_boxplot(
      position = position_dodge(0.88))

### Try 2 factor of grouping: soil condition and type of hormone.
ggplot(
   data = data_tidy,
   aes(
      x = Soil_cond,
      y = Dry_Biomass,
      color = Species)) +
   geom_boxplot(
      position = position_dodge(0.88))

### Try 3 factor of grouping: species, soil condition, and type of hormone.
ggplot(
   data = data_tidy,
   aes(
      x = Concentration,
      y = Dry_Biomass,
      color = Hormone)) +
   geom_boxplot(
      position = position_dodge(0.88)) +
   facet_grid(Species ~ Soil_cond)

# Line graph with error bar
### This example use confidence interval 95%
### See simple_error_bar.R for explanation
ggplot(
   data = plant_desc,
   aes(
      x = Concentration,
      y = mean,
      colour = Hormone,
      group = Hormone)) +
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
   theme_bw() +
   labs(
      x = "Concentration (mg/L)",
      y = "Dry Biomass (gram)") +
   facet_grid(Species ~ Soil_cond)
