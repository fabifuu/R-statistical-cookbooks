# Error-Bar (EB) Package v.0.1 by Fabi
# Table of content
# 1. Tidying Data
# 2. Descriptive statistics
# 3. Bar EB type 1 (std error bar)
# 4. Bar EB type 2 (CI 95% error bar)
# 5. Point EB type 1
# 6. Point EB type 2

# TITLE HERE

# Import data
data <- read.csv(file="~/directory/of/your/file.csv")

# Tidying Data
# Pembagian grup sesuai dengan https://r4ds.had.co.nz/tidy-data.html 
library(tidyr)
tidy <- data %>% 
  gather(group.1, group.2, group.3, key = "group", value = "value")

# Descriptive statistics
# Mean, median, quartile, min, max, and std.
library(dplyr)
descriptive <- group_by(tidy, Genotype) %>%
  summarise(
    count = n(),
    mean = mean(Height),
    median = median(Height),
    min = min(Height),
    '25%'= quantile(Height, probs = 0.25),
    '50%' = quantile(Height, probs = 0.5),
    '75%' = quantile(Height, probs = 0.75),
    max = max(Height),
    std = sd(Height)
  )
write.csv(descriptive, file = "./output.csv")

tidy <- ToothGrowth

# BEB with std error bar
library(ggplot2)

  
ggplot(data = tidy) +
  geom_bar(
    mapping = aes(
      x = dose, 
      y = len,
      fill = supp),
    stat = "identity",
    color = "black",
    position = position_dodge()) +
  geom_errorbar(
    mapping = aes(
      ymin = len - sd,
      ymax = len + sd),
    width = 0.2)

library(ggplot2)
ToothGrowth # data set
df <- ToothGrowth # assign data set
df$dose <- as.factor(df$dose)
head(df)

# Calculate mean and standard deviation (std)
# data : a data frame
# varname : the name of a column containing the variable to be summariezed
# groupnames : vector of column names to be used as grouping variables

Mean, median, quartile, min, max, and std.
library(dplyr)
descriptive <- group_by(df, supp, dose) %>%
  summarise(
    count = n(),
    mean = mean(len),
    median = median(len),
    min = min(len),
    '25%'= quantile(len, probs = 0.25),
    '50%' = quantile(len, probs = 0.5),
    '75%' = quantile(len, probs = 0.75),
    max = max(len),
    std = sd(len)
  )



library(ggplot2)
ggplot(data = descriptive, mapping = aes(
  x = dose,
  y = mean,
  ymin = mean - std,
  ymax = mean + std,
  fill = supp)) +
  geom_bar(
    stat = "identity",
    position = position_dodge()) +
  geom_errorbar(
    width = .08, 
    position = position_dodge(0.5)) 

ggplot(data = descriptive, mapping = aes(
  x = dose,
  y = mean,
  ymin = mean - std,
  ymax = mean + std,
  fill = supp)) +
  geom_bar(
    stat = "identity",
    position = position_dodge())

ggplot(data = descriptive) +
  geom_bar(mapping = aes(
    x = dose,
    y = mean,
    ymin = mean - std,
    ymax = mean + std,
    fill = supp),
    stat = "identity",
    color = "black",
    position = position_dodge())