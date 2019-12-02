# ANOVA Package v.1.10 by Fabi
# Table of content
# Descriptive statistics
# ANOVA
# Multi-boxplot
# Assumption Check

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


# ANOVA
# group.1, group.2, etc. should be numerical (ORDINAL / MEASUREMENT data type)
# variabel should be "group of" those numerical value. variabel usually has NOMINAL data type

anova.1 <- aov(group.1 ~ variabel, data = data)
summary(anova.1)
anova.2 <- aov(group.2 ~ variabel, data = data)
summary(anova.2)
anova.3 <- aov(group.3 ~ variabel, data = data)
summary(anova.3)
anova.sum <- aov(group ~ variabel, data = tidy)
summary(anova.sum)


# Boxplot
# Kau pikir sendiri x, y, z, etc. itu apa.

library(ggplot2)
ggplot(data = tidy) + 
  geom_boxplot(
    mapping = aes(
      x = xxxx, 
      y = yyyy, 
      color = zzzz), 
    position = position_dodge(0.88)) + 
  scale_color_grey() +
  theme_bw() +
  labs(x = "xxxx") +
  labs(y = "yyyy")

# Assumption check
# Levene's Test
library(car)
leveneTest(group ~ variabel, tidy)

# Normality Tests
# Normality plot of residuals (Normal Q-Q)
plot(anova.sum, 2)

# Kolmogorov-Smirnoff
