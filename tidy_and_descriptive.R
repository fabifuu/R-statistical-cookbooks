# Tidy and Descriptive v.1.1
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
   read.csv(file="~/Bioinformatics/Biostatistics/cancer_tp53_messy.csv", header = FALSE)

## Explanation of raw data example
### This is a standard RT-qPCR experiment to find the relative expression of p53 gene.
### We use four different cancer cell line. Each sample is treated with Meth (a drugs)
### in different Inhibitory Concentration (IC) and one negative control
### (without treatment).

## Inspect data
### Determine whether the data is tidy or not, and find a way to tidying it.
data_raw

### Delete first row since it's containing group that is not used
### as header. However, keep in mind that this deleted group will
### be used as group in tidy structure
data_raw <- data_raw[-1,]
data_raw

### If you need to delete first column, use the code below.
### In this example, we don't need to delete the first column.
data_raw <- data_raw[,-1]
data_raw

### Change the first row as header
names(data_raw) <- data_raw[1,]
data_raw <- data_raw[-1,]
data_raw

# Tidying Data
## Group should be arranged with tidy-data structure 
## (see https://r4ds.had.co.nz/tidy-data.html)
## condition = treatment group, control group, etc.
## value = value or the data contain in the group (i.e. Ct value of qPCR) 

library(tidyr)
data_tidy <- data_raw %>%
   pivot_longer(
      c(Neg, IC25, IC50, IC75), #gather each group
      names_to = "Dosage", #condition refer to group_1, group_2, etc.
      values_to = "p53_expression" #this is the value each condition have
   )
data_tidy

### Make sure that numeric value is numeric, and character value is character
data_tidy$p53_expression <- as.numeric(data_tidy$p53_expression)
data_tidy

### other example:
data_tidy$column <- as.character(data_tidy$column) #as character
data_tidy$column <- as.Date(data_tidy$column) #as date
data_tidy$column <- as.logical(data_tidy$column) #as binary logic (T/F)

### Separate " R1" as "Replicate" column
### This step make "Sampel" column more tidy and create "Replicate" column
data_tidy <- data_tidy %>%
   separate(
      Sampel, 
      into = c("Sampel","Replicate"),
      sep = " R" #I use <space> before <R> so it's tidy.
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
      std = sd(len), #standard deviation
      sem = sd(len)/sqrt(n()), #standard error of mean
      ci = 1.96*sd(len)/sqrt(n()) #confidence interval 95%
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
      std = sd(len),
      sem = sd(len)/sqrt(n()),
      ci = 1.96*sd(len)/sqrt(n())
   )
desc_sample

### Group by Sample & Dosage
### Note: I use double pipe operator: a data frame piped into group_by. 
### Then, inside group_by, choose two column you want to group.
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
      std = sd(len),
      sem = sd(len)/sqrt(n()),
      ci = 1.96*sd(len)/sqrt(n())
   )
desc_sample_both

## save as csv file
write.csv(desc_sample_both, file = "/home/fabifuu/Bioinformatics/Biostatistics/descriptive_stats_p53.csv")

# Data Visualization
library(ggplot2)

## Box Plot
### x = usually condition group (dosage, type of drugs, etc)
### y = numerical value contained in the x
### color = different sample. each sample have x in their data
### best theme with theme_linedraw()
### other type of theme: https://ggplot2.tidyverse.org/reference/ggtheme.html
### colour of graph: 
ggplot(data = data_tidy) + 
   geom_boxplot(
      mapping = aes(
         x = Dosage, 
         y = p53_expression, 
         color = Sampel), 
      position = position_dodge(0.88)) +
   theme_linedraw() +
   labs(x = "Dosage of Meth in Inhibitory Concentration (IC)") +
   labs(y = "p53 Expression (FPKM)")

## Bar Plot
ggplot(data = data_tidy) + 
   geom_bar(
      mapping = aes(
         x = Dosage, 
         y = p53_expression, 
         color = Sampel), 
      position = position_dodge(0.88)) +
   theme_linedraw() +
   labs(x = "Dosage of Meth in Inhibitory Concentration (IC)") +
   labs(y = "p53 Expression (FPKM)")

## Scatter plot

## Density plot

## Histogram plot