# Tidy and Descriptive v.1.2
# With example

# Forewords
## From importing messy data into beautiful visualization and complete
## summary of descriptive statistics. Tailored towards biology and biomedical 
## beginner user.
## --fabifuu, 2021

# Library used
library(dplyr)
library(ggplot2)
library(tidyr)

# Import data
## We use a messy data
data_raw <- 
   read.csv(file="~/Bioinformatics/Biostatistics/plant_hormone_messy.csv", header = FALSE)

## Explanation of raw data example
### This is a standard RT-qPCR experiment to find the relative expression of p53 gene.
### We use four different cancer cell line. Each sample is treated with Meth (a drugs)
### in different Inhibitory Concentration (IC) and one negative control
### (without treatment).

## Inspect data
### Determine whether the data is tidy or not, and find a way to tidying it.
data_raw

### Delete first and second row
### This will be gathered in tidyR.
data_raw <- data_raw[-2,]
data_raw <- data_raw[-1,]
data_raw

### Change the first row as header
names(data_raw) <- data_raw[1,]
data_raw <- data_raw[-1,]
data_raw

### Change header name since they have white space 
### and identical name (two "0 mg/L" column)
header <- c(
   "Sample",
   "IAA_0mg/L",
   "IAA_5mg/L",
   "IAA_10mg/L",
   "IAA_15mg/L",
   "ABA_0mg/L",
   "ABA_1mg/L",
   "ABA_2mg/L",
   "ABA_3mg/L"
)
colnames(data_raw) <- header

### Select header for IAA and ABA
header_IAA <- header[c(1:5)]
header_IAA
header_ABA <- header[c(1,5:8)]
header_ABA
header_all <- header[c(2:9)]

# Tidying Data
## Group should be arranged with tidy-data structure 
## (see https://r4ds.had.co.nz/tidy-data.html)
## condition = treatment group, control group, etc.
## value = value or the data contain in the group (i.e. Ct value of qPCR) 

library(tidyr)
data_tidy <- data_raw %>%
   pivot_longer(
      header_all,
      names_to = "Concentration", #condition refer to group_1, group_2, etc.
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
      into = c("Hormone","Replicate"),
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

### Separate NPK in "Sample" column as "Soil_cond"
### Regex to separate string after two space: \s\S*\s(.*)
data_tidy <- data_tidy %>%
   separate(
      Sample, 
      into = c("Species", "Soil_cond"),
      sep = "~"
   )
data_tidy

### save as csv file
write.csv(data_tidy, file = "a/path/tp53_tidy.csv")

# Descriptive statistics using dplyr
library(dplyr)

## Note
## group_by → usually condition or sample type
## argument inside summarize → numeric only!

### Grouping by dosage
desc_dosage <- group_by(data_tidy, Dosage) %>%
   summarise(
      mean = mean(p53_expression),
      count = n(),
      median = median(p53_expression),
      min = min(p53_expression),
      '25%'= quantile(p53_expression, probs = 0.25),
      '50%' = quantile(p53_expression, probs = 0.5),
      '75%' = quantile(p53_expression, probs = 0.75),
      max = max(p53_expression),
      std = sd(p53_expression)
   )
desc_dosage

### Grouping by dosage
desc_sample <- group_by(data_tidy, Sampel) %>%
   summarise(
      mean = mean(p53_expression),
      count = n(),
      median = median(p53_expression),
      min = min(p53_expression),
      '25%'= quantile(p53_expression, probs = 0.25),
      '50%' = quantile(p53_expression, probs = 0.5),
      '75%' = quantile(p53_expression, probs = 0.75),
      max = max(p53_expression),
      std = sd(p53_expression)
   )
desc_sample

### Group by Sample & Dosage
### Note: I use double piping: a data frame piped into group_by. 
### And then, inside group_by, I choose two column for grouping.
desc_sample_both <- 
   data_tidy %>% 
   group_by(Sampel, Dosage) %>%
   summarise(
      mean = mean(p53_expression),
      count = n(),
      median = median(p53_expression),
      min = min(p53_expression),
      '25%'= quantile(p53_expression, probs = 0.25),
      '50%' = quantile(p53_expression, probs = 0.5),
      '75%' = quantile(p53_expression, probs = 0.75),
      max = max(p53_expression),
      std = sd(p53_expression)
   )
desc_sample_both

## save as csv file
write.csv(desc_sample_both, file = "/home/fabifuu/Bioinformatics/Biostatistics/descriptive_stats_p53.csv")